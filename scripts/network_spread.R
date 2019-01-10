# ................................

# Network spread for SOTA

# Last updated: 01.07.19 

# ................................

# Set network you are working on
relNetwork = "Network 15"

# Working dir
wd = "I:/Emily/SOTA/draftStats"
setwd(wd)

library(DT)
library(ggplot2)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(openxlsx)
library(tidyverse)
library(plyr)
library(rgdal)
library(ggmap)
library(knitr)
library(kableExtra)

### Read in, set up data ----------------------------------------------

# Source data setup
setwd("scripts")
source(sprintf("%s/scripts/sota_schools_format.R", wd)) 
source(sprintf("%s/scripts/plotStandards.R", wd)) 
setwd("..")

# Read in demographic data from CPS
netDemos = read.csv(sprintf("%s/data/Demographics_LEPSPED_2018_indivSchools.csv",
                            wd), head = T)
netDemos = netDemos[3:nrow(netDemos),] # drop weird headers
colnames(netDemos) = c("Network", "SchoolId", "UnitName", "TotalEnrollment",
                       "numBilingual", "percBilingual", "numDiverse", "percDiverse",
                       "numFRL", "percFRL")

# ..................

# Subset rubric to relevant network and 17-18 data
netRub = rubric_list[rubric_list$SchoolYears == "2017-18" &
                       rubric_list$Network == relNetwork,]
netRub = netRub[!(is.na(netRub$SchoolId)),] # get weird of odd subset NAs

# Sort by CSC score
netRub = netRub[order(netRub$FinalScore, netRub$Nickname),]

# ..................

# Read in demographic data

raceDemos = read.csv(sprintf("%s/data/Demographics_RacialEthnic_2018_indivSchools.csv", wd), head = T)
otherDemos = read.csv(sprintf("%s/data/Demographics_LEPSPED_2018_indivSchools.csv", wd), head = T)

# Get rid of extraenous rows and columns
raceDemos = raceDemos[3:nrow(raceDemos),1:22]
otherDemos = otherDemos[3:nrow(otherDemos),]

# Re-process column names
colnames(raceDemos) = c("SchoolId", "UnitName", "Network", "TotalEnrollment",
                        "NumWhite", "PercWhite", "NumAfAm", "PercAfAm",
                        "NumAsianPI", "PercAsianPI", "NumNative", "PercNative",
                        "NumHispanic", "PercHispanic", "NumMulti", "PercMulti",
                        "NumAsian", "PercAsian", "NumHawaii", "PercHawaii",
                        "NumNA", "PercNA")
                        
colnames(otherDemos) = c("Network", "SchoolId", "UnitName", "TotalEnrollment", 
                         "NumBilingual", "PercBilingual", "NumSpED", "PercSpED", "NumFRL", "PercFRL")

### Barplot of CSC by network, all networks -------------------------

# subset 650 17-18 schools out
r78 = rubric_list[rubric_list$SchoolYears == "2017-18",]

# Count up data and get proportion of each label within a network
rAgg = prop.table(xtabs(~FinalScore + Network, data = r78), 2)
rAgg = round(100 * rAgg, 1)
rAgg = data.frame(rAgg)
rAgg$Network = as.character(rAgg$Network)
rAgg$label = paste(rAgg$Freq, "%", sep = "")

# Sort by proportion of excelling and strong
highAgg = rAgg[rAgg$FinalScore %in% c(1, 2),]
topAgg = with(highAgg, aggregate(Freq, list(Network), sum))
colnames(topAgg) = c("Network", "sum")
topAgg = topAgg[order(topAgg$sum),]
topAgg$order = 1:nrow(topAgg)
rAgg = merge(rAgg, topAgg[,c("Network", "order")], by = "Network")
rAgg <- rAgg %>%
  arrange(order) %>%
  mutate(Network = factor(Network, unique(Network))) 

# Drop rows where freq = 0 (networks with no 5s)
rAgg = rAgg[rAgg$Freq >0,]

# ............

# plot
all_net_csc_bar = ggplot(rAgg, aes(x = Network, y = Freq, fill = FinalScore)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = label), color = "white", size = 7, 
            position=position_stack(vjust= 0.5)) +
  scale_fill_manual(values = cscColors, labels = cscLabels) +
  sota_theme_bar_hor +
  xlab("")

### Barplot of CSC by network, separate HS and ES ------------------------------

# High schools

# Count up data and get proportion of each label within a network
rAggHS = prop.table(xtabs(~FinalScore + Network, 
                          data = r78[r78$SchoolType == "HS",]), 2)
rAggHS = round(100 * rAggHS, 1)
rAggHS = data.frame(rAggHS)
rAggHS$Network = as.character(rAggHS$Network)
rAggHS$label = paste(rAggHS$Freq, "%", sep = "")

# Sort by proportion of excelling and strong
highAggHS = rAggHS[rAggHS$FinalScore %in% c(1, 2),]
topAggHS = with(highAggHS, aggregate(Freq, list(Network), sum))
colnames(topAggHS) = c("Network", "sum")
topAggHS = topAggHS[order(topAggHS$sum),]
topAggHS$order = 1:nrow(topAggHS)
rAggHS = merge(rAggHS, topAggHS[,c("Network", "order")], by = "Network")
rAggHS <- rAggHS %>%
  arrange(order) %>%
  mutate(Network = factor(Network, unique(Network))) 

# Drop rows where freq = 0 (networks with no 5s)
rAggHS = rAggHS[rAggHS$Freq >0,]

hs_net_csc_bar = ggplot(rAggHS, 
                        aes(x = Network, y = Freq, fill = FinalScore)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = label), color = "white", size = 7, 
            position=position_stack(vjust= 0.5)) +
  scale_fill_manual(values = cscColors, labels = cscLabels) +
  sota_theme_bar_hor +
  xlab("") +
  ggtitle("High schools")

# .................................

# Elementary schools

# Count up data and get proportion of each label within a network
rAggES = prop.table(xtabs(~FinalScore + Network, 
                          data = r78[r78$SchoolType == "ES",]), 2)
rAggES = round(100 * rAggES, 1)
rAggES = data.frame(rAggES)
rAggES$Network = as.character(rAggES$Network)
rAggES$label = paste(rAggES$Freq, "%", sep = "")

# Sort by proportion of excelling and strong
highAggES = rAggES[rAggES$FinalScore %in% c(1, 2),]
topAggES = with(highAggES, aggregate(Freq, list(Network), sum))
colnames(topAggES) = c("Network", "sum")
topAggES = topAggES[order(topAggES$sum),]
topAggES$order = 1:nrow(topAggES)
rAggES = merge(rAggES, topAggES[,c("Network", "order")], by = "Network")
rAggES <- rAggES %>%
  arrange(order) %>%
  mutate(Network = factor(Network, unique(Network))) 

# Drop rows where freq = 0 (networks with no 5s)
rAggES = rAggES[rAggES$Freq >0,]

es_net_csc_bar = ggplot(rAggES, aes(x = Network, y = Freq, fill = FinalScore)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = label), color = "white", size = 7, 
            position=position_stack(vjust= 0.5)) +
  scale_fill_manual(values = cscColors, labels = cscLabels) +
  sota_theme_bar_hor +
  xlab("") +
  ggtitle("Elementary schools")

# ........................

# Combine ES and HS 
es_hs_net_csc_bar = grid.arrange(es_net_csc_bar, hs_net_csc_bar, ncol = 2)

### List school names and neighborhoods for selected network -------------------------------------

# Table names of schools
schoolNames = netRub$Nickname
schoolNames = data.frame(schoolNames)
colnames(schoolNames) = "name"

# goofy way to wrap single column up into 4 columns
schoolNames2 = schoolNames
tabDivisor = round(nrow(schoolNames2)/4, 0)
schoolNames2$divisor = c(rep('col_1', tabDivisor), 
                         rep('col_2', tabDivisor), 
                         rep('col_3', tabDivisor), 
                         rep('col_4', nrow(schoolNames2)-(3*tabDivisor)))
schoolNames2$dummy = c(c(1:tabDivisor), c(1:tabDivisor), c(1:tabDivisor), 
                       c(1:(nrow(schoolNames2)-(3*tabDivisor))))
schoolNamesWide = 
  schoolNames2 %>% spread(divisor, name)
schoolNamesWide$dummy = NULL

# Label colors, color-coded by CSC
schoolNameCols = netRub$FinalScore
schoolNameCols = recode(schoolNameCols, 
                        "1" = cscColors[1],
                        "2" = cscColors[2],
                        "3" = cscColors[3],
                        "4" = cscColors[4])
# Add white to the end for any cells that get filled with NA (if number of schools is not divisible by 4)
cellCountNA = tabDivisor - (nrow(schoolNames) %/% 4)
schoolNameCols = c(schoolNameCols, rep("white", cellCountNA))

# Create table, assign colors to each column appropriately
netSchoolTab = schoolNamesWide %>%
  mutate(col_1 = cell_spec(col_1, color = schoolNameCols[1:tabDivisor]),
         col_2 = cell_spec(col_2, color = schoolNameCols[(tabDivisor+1):(tabDivisor*2)]),
         col_3 = cell_spec(col_3, color = schoolNameCols[(tabDivisor*2+1):(tabDivisor*3)]),
         col_4 = cell_spec(col_4, color = schoolNameCols[(tabDivisor*3+1):(tabDivisor*4)])) %>%
  kable("html", escape = F, col.names = NULL, align =rep('c', 4)) %>%
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive", "bordered"))

# ............................................................................................

## Table of neighorhoods/community areas

# Unique community areas in this network
neighborhoodNames = data.frame(sort(unique(netRub$CommunityArea)))
colnames(neighborhoodNames) = "neighborhood"

# goofy wrap-up, 3 cols
neighNames2 = neighborhoodNames
nTabDivisor = round(nrow(neighNames2)/3, 0)
neighNames2$divisor = c(rep('col_1', nTabDivisor), 
                        rep('col_2', nTabDivisor), 
                        rep('col_3', nrow(neighNames2)-(2*nTabDivisor)))
neighNames2$dummy = c(c(1:nTabDivisor), c(1:nTabDivisor), c(1:(nrow(neighNames2)-(2*nTabDivisor))))

neighNamesWide = 
  neighNames2 %>% spread(divisor, neighborhood)
neighNamesWide$dummy = NULL

netNeighTab = neighNamesWide %>%
  kable("html", escape = F, col.names = NULL, align =rep('c', 3)) %>%
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive", "bordered"))


### Demographics -----------------------------------------------------------------------------



### Generate plots for network spread ----------------------------------------------------------

## Overall CSC scores pie

# calculate percentage of schools in each category
net_csc_df = data.frame(prop.table(table(netRub$FinalScore)))
colnames(net_csc_df) = c("FinalScore", "perc")
net_csc_df$label = paste(round(100*prop.table(table(netRub$FinalScore)), 1), "%", sep = "")


netPie = ggplot(net_csc_df, aes(x ="", y = perc, fill = FinalScore)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = pieLabSize) +
  sota_theme_pie +
  scale_fill_manual(values = cscColors[1:4]) +
  ggtitle("")


# ............................................