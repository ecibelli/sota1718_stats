
# SOTA draft stats

# Based on:
# I:\Data & Impact\Analyses\State of the Arts in Chicago Public Schools

# Last updated: 01.09.19 

# ................................

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

### Workspace and data setup -------------------------------------------

wd = "I:/Emily/SOTA/draftStats"
setwd(wd)

# I. Setup and sourcing
setwd("scripts")
source(sprintf("%s/scripts/sota_schools_pull_mod.R", wd))  # school data pull
rubric_list_full = rAll
rubric_list = r
rubric_list$Status <- ifelse(rubric_list$FinalScore < 5, "Complete", "Incomplete")
rubric_list_complete <- rubric_list[!rubric_list$FinalScore == 5,]

source(sprintf("%s/scripts/sota_partnerships.R", wd))  # source partners data pull

source(sprintf("%s/scripts/plotStandards.R", wd))  # plotting conventions

# Community area boundaries, last updated Jan 3, 2017
chitown<-readOGR(sprintf("%s/data/Boundaries - Community Areas (current)", wd),
                 "geo_export_272f8222-06a8-4f1f-a587-7d8438a475b9") 

chitown.df<-fortify(chitown, region="area_numbe")

setwd("..")

# ................................................................................

## Assign groups 

district_run <- c("Network 1", "Network 2", "Network 3", "Network 4", "Network 5", 
                  "Network 6", "Network 7", "Network 8",
                  "Network 9", "Network 10", "Network 11", "Network 12", 
                  "Network 13", "AUSL", "ISP", "Service Leadership Academies")

charter_contract <- c("Charter", "Contract")

other <- c("Options")

rubric_list$Group <- ifelse(rubric_list$CpsNewNetwork %in% district_run, "District-Run",
                            ifelse(rubric_list$CpsNewNetwork %in% charter_contract, 
                                   "Charter & Contract",
                                   ifelse(rubric_list$CpsNewNetwork %in% other, "Other", "NA")))

rubric_list_complete$Group <- ifelse(rubric_list_complete$CpsNewNetwork 
                                     %in% district_run, "District-Run",
                          ifelse(rubric_list_complete$CpsNewNetwork 
                                    %in% charter_contract, "Charter & Contract",
                        ifelse(rubric_list_complete$CpsNewNetwork %in% other, 
                                                   "Other", "NA")))

# .............................................

## Add new networks
# Note: $CpsNewNetwork is not the newest 2018-19 network
# For these, see: https://cps.edu/About_CPS/Departments/Documents/schoolsByNetwork_10022018.pdf

newNet = read.csv(sprintf("%s/data/newNetworks_fromCPS_8jan2019.csv", wd), head = T)  
# Restrict to ID and network
newNet = newNet[,c("SchoolId", "Network")]

## A few need to be manually corrected/added (e.g. closed schools not in new network list):
# Castellanos is missing
# I think in 18-19 merged with Cardenas, so this would be changed to ISP in the future
newNet = rbind(newNet, c(609826, "Network 7"))
# Jesse Owens ES was associated with a different ID at one point?
newNet = rbind(newNet, c(609932, "Network 13"))
# Instituto Lozano/JLA is options
newNet = rbind(newNet, c(400148, "Options"))

## Weird ones to note -.-.-.-.-.-.-.-
# Disney II ES Magnet School is Network 14, (its a combo school and grouped with its HS)
# Ogden ES - same deal in network 15
# Alcott HS: same deal in network 4 (grouped with its ES) 
# Camelot safe ES: the only ES options school

# Note: no new network info for Paul Robeson available as of 1/7/19
# Need to track down Alcott HS

# Find charters that don't appear in newNet
misChart = unique(rubric_list[rubric_list$CpsNewNetwork == "Charter",]$SchoolId)
misChart = misChart[!(misChart %in% newNet$SchoolId)]
misChart = misChart[complete.cases(misChart)]
misChart = data.frame(as.integer(as.character(misChart)),
                      rep("Charter", length(misChart)))
colnames(misChart) = c("SchoolId", "Network")
# Append to newNet
newNet = rbind(newNet, misChart)

# Add HS/ES Charter split
newNet$Network = as.character(newNet$Network)
chartES = unique(rubric_list_full[rubric_list_full$SchoolType == "ES" & 
                                    rubric_list_full$CpsNewNetwork == "Charter",]$SchoolId)
chartHS = unique(rubric_list_full[rubric_list_full$SchoolType == "HS" & 
                                    rubric_list_full$CpsNewNetwork == "Charter",]$SchoolId)
newNet[newNet$SchoolId %in% chartES,]$Network = "Charter ES"
newNet[newNet$SchoolId %in% chartHS,]$Network = "Charter HS"

# Factor networks to appear ES first, then HS
numNets = paste("Network", 1:17)
newNet$Network = fct_relevel(newNet$Network,
                                    numNets[1:13], "Charter ES",
									numNets[14:17], "Charter HS", "Contract", "Options",
                                    "ISP", "AUSL")

# Add new networks to data
newNet$SchoolId = as.numeric(as.character(newNet$SchoolId))
rubric_list = left_join(rubric_list, newNet, by = "SchoolId")
rubric_list_complete = left_join(rubric_list_complete, newNet, 
                                 by = "SchoolId")
rubric_list_full = left_join(rubric_list_full, newNet, 
                                 by = "SchoolId")
partner.distinctparts = left_join(partner.distinctparts, newNet,
                                  by = "SchoolId")
parnter.distinctparts = left_join(parnter.distinctparts, newNet,
                                  by = "SchoolId")

### Section 1: Key Findings (TBD) --------------------------------

### Section 3: District Information ------------------------------

school_numbers <- aggregate(rubric_list$SchoolId,
                            by = list(rubric_list$SchoolYears, 
                                      rubric_list$Group, 
                                      rubric_list$SchoolType),
                            FUN = NROW)

xref_school_numbers <- aggregate(school_numbers$x,
                                 by = list(school_numbers$Group.1, 
                                           school_numbers$Group.2),
                                 FUN = sum)

school_numbers <- merge(school_numbers, xref_school_numbers, by = c("Group.1", "Group.2"))
rm(xref_school_numbers)

school_numbers$Percent <- paste(round((school_numbers$x.x / school_numbers$x.y)*100, digits = 0), "%", sep = "")

school_numbers <- reshape(school_numbers,
                          timevar = "Group.1",
                          idvar = c("Group.2", "Group.3"),
                          direction = "wide")

school_numbers_names <- c("Group", "School Type","Num 2012-13", "Total 2012-13", "Percent 2012-13",
                          "Num 2013-14", "Total 2013-14", "Percent 2013-14",
                          "Num 2014-15", "Total 2014-15", "Percent 2014-15",
                          "Num 2015-16", "Total 2015-16", "Percent 2015-16",
                          "Num 2016-17", "Total 2016-17", "Percent 2016-17",
                          "Num 2017-18", "Total 2017-18", "Percent 2017-18")

colnames(school_numbers) <- school_numbers_names
school_numbers <- arrange(school_numbers, Group, `School Type`)

# Raw stats
numSchools1718 = sum(school_numbers$`Num 2017-18`, na.rm = T)
numES1718 = sum(school_numbers[school_numbers$`School Type` == "ES",]$`Num 2017-18`, na.rm = T)
numHS1718 = sum(school_numbers[school_numbers$`School Type` == "HS",]$`Num 2017-18`, na.rm = T)

enrollment_total_num = 351941
enrollment_total = "351,941"

### Section 3 (cont): Creative Schools Survey Participation ------------------

# Table 1: Survey completion rate by year, district overall

comp_rate <- aggregate(rubric_list$SchoolId,
                       by = list(rubric_list$SchoolYears, rubric_list$Status),
                       FUN = NROW)

xref_comp_rate <- aggregate(comp_rate$x,
                         by = list(comp_rate$Group.1),
                         FUN = sum)

comp_rate <- merge(comp_rate, xref_comp_rate, by = "Group.1")
rm(xref_comp_rate)
comp_rate$Percent <- paste(round((comp_rate$x.x / comp_rate$x.y)*100, digits = 0), 
                           "%", sep = "")
comp_rate_name <- list("SchoolYear", "Status", "Num", "N", "Percent")
colnames(comp_rate) <- comp_rate_name
rm(comp_rate_name)
comp_rate <- arrange(comp_rate, desc(SchoolYear), Status)


# ......

# Modify table for plotting
comp_rate_plot = comp_rate[comp_rate$Status == "Complete",]
comp_rate_plot$perc = as.numeric(as.character(gsub("%", "", comp_rate_plot$Percent)))

comp_rate_bar = ggplot(comp_rate_plot,
                       aes(x = SchoolYear, y = perc, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percent, color = SchoolYear), size = topTextSingleSize, vjust=-0.3) +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  scale_color_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  sota_theme_bar +
  ylim(0, 110) +
  xlab("") + ylab("") 

# ........................................................

# Table 2: Survey completion rate by year, ES and HS

comp_rate_type <- aggregate(rubric_list$SchoolId,
                            by = list(rubric_list$SchoolYears,
                                      rubric_list$SchoolType, 
                                      rubric_list$Status),
                            FUN = NROW)

xref_comp_rate_type <- aggregate(comp_rate_type$x,
                                 by = list(comp_rate_type$Group.1, 
                                           comp_rate_type$Group.2),
                                 FUN = sum)

comp_rate_type <- merge(comp_rate_type, xref_comp_rate_type, by = c("Group.1", "Group.2"))
rm(xref_comp_rate_type)

comp_rate_type$Percent <- paste(round((comp_rate_type$x.x / comp_rate_type$x.y)*100, digits = 0), "%", sep = "")
comp_rate_type_name <- list("SchoolYear", "SchoolType", "Status", "Num", "N", "Percent")
colnames(comp_rate_type) <- comp_rate_type_name

comp_rate_type <- comp_rate_type[!comp_rate_type$Status == "Incomplete",]
rm(comp_rate_type_name)
comp_rate_type <- arrange(comp_rate_type, SchoolYear, SchoolType)



# ......

# Modify table for plotting
comp_rate_type_plot = comp_rate_type[comp_rate_type$Status == "Complete",]
comp_rate_type_plot$perc = as.numeric(as.character(gsub("%", "", comp_rate_type_plot$Percent)))

comp_rate_type_plot$SchoolType = recode(comp_rate_type_plot$SchoolType, 
                            "ES" = "Elementary Schools",
							"HS" = "High Schools")

comp_rate_type_bar = ggplot(comp_rate_type_plot,
                       aes(x = SchoolYear, y = perc, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percent, color = SchoolYear), size = topTextMultiSize, vjust=-0.3) +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  scale_color_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  sota_theme_bar +
  ylim(0, 110) +
  xlab("") + ylab("") + 
  facet_wrap(~SchoolType, ncol = 2) +
  theme(strip.background = element_rect(colour = "white"))  

# ........................................................

# Table 3: Survey completion rate by year: district-run, charter/contract, and options (other)

comp_rate_other <- aggregate(rubric_list$SchoolId,
                             by = list(rubric_list$SchoolYears, 
                                       rubric_list$Group,
                                       rubric_list$Status),
                             FUN = NROW)

xref_comp_rate_other <- aggregate(comp_rate_other$x,
                                 by = list(comp_rate_other$Group.1, comp_rate_other$Group.2),
                                 FUN = sum)

comp_rate_other <- merge(comp_rate_other, xref_comp_rate_other,
                         by = c("Group.1", "Group.2"))
rm(xref_comp_rate_other)

comp_rate_other$Percent <- paste(round((comp_rate_other$x.x / comp_rate_other$x.y)*100, 
                                       digits = 0), "%", sep = "")
comp_rate_other_name <- list("SchoolYear", "SchoolType", "Status", "Num", "N", "Percent")
colnames(comp_rate_other) <- comp_rate_other_name
rm(comp_rate_other_name)
comp_rate_other <- arrange(comp_rate_other, SchoolYear, SchoolType)

# Change "other" to "options and other"
comp_rate_other$SchoolType = recode(comp_rate_other$SchoolType, 
                            "Other" = "Options & Other")

# ..........

# Modify for plotting

comp_rate_other_plot = comp_rate_other[comp_rate_other$SchoolType 
                                       %in% c("Charter & Contract",
                                              "District-Run",
                                              "Options & Other"),]
											  
# reorder school type so district comes first
comp_rate_other$SchoolType = fct_relevel(comp_rate_other$SchoolType, 
							c("District-Run", "Charter & Contract", "Options & Other"))	
											  
comp_rate_other_plot = comp_rate_other_plot[comp_rate_other_plot$Status == "Complete",]
comp_rate_other_plot$perc = as.numeric(as.character(gsub("%", "", comp_rate_other_plot$Percent)))
comp_rate_other_plot$SchoolType = fct_relevel(comp_rate_other_plot$SchoolType,
                                              "District-Run")
											  

comp_rate_other_bar = 
  ggplot(comp_rate_other_plot , aes(x = SchoolYear, y = perc, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  facet_wrap(~SchoolType, ncol = 3) +
  geom_text(aes(label = Percent, color = SchoolYear), size = topTextMultiSize, vjust=-0.3) +
  scale_fill_manual(values =c(rep(pastYearCol, 5), currentYearCol)) +
  scale_color_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  sota_theme_bar +
  ylim(0, 115) + 
  xlab("") + ylab("") +
  theme(strip.text.x = element_text(size = baseSizeDef+10)) +
  theme(strip.background = element_rect(colour = "white"))

# ................................................................................

# Table 4: Survey completion rate by year, network breakdown
# updated december 18 with new networks

comp_rate_net <- aggregate(rubric_list$SchoolId,
                           by = list(rubric_list$SchoolYears, 
                                     #rubric_list$CpsNewNetwork,
                                     rubric_list$Network,
                                     rubric_list$Status),
                           FUN = NROW)

xref_comp_net_type <- aggregate(comp_rate_net$x,
                                by = list(comp_rate_net$Group.1, 
                                          comp_rate_net$Group.2),
                                FUN = sum)

comp_rate_net <- merge(comp_rate_net, xref_comp_net_type, 
                       by = c("Group.1", "Group.2"))

comp_rate_net$Percent <- paste(round((comp_rate_net$x.x / comp_rate_net$x.y)*100, digits = 0),
                               "%", sep = "")
comp_rate_net_name <- list("SchoolYear", "Network", "Status", "Num", "N", "Percent")

colnames(comp_rate_net) <- comp_rate_net_name
comp_rate_net <- comp_rate_net[!comp_rate_net$Status == "Incomplete",]
comp_rate_net <- comp_rate_net[order(comp_rate_net$Network, comp_rate_net$SchoolYear),]
comp_rate_net <- arrange(comp_rate_net, Network, SchoolYear)

# Reorder factor levels
numNets = paste("Network", 1:17)
comp_rate_net$Network = fct_relevel(comp_rate_net$Network,
                                    numNets[1:13], "Charter ES",
									numNets[14:17], "Charter HS", "Contract", "Options",
                                    "ISP", "AUSL")
                                    

# .....

# Modify for plotting
comp_rate_net_plot = comp_rate_net
comp_rate_net_plot$Perc = as.numeric(as.character(
  gsub("%", "", comp_rate_net_plot$Percent)))

comp_rate_net_bar = 
  ggplot(comp_rate_net_plot , aes(x = SchoolYear, y = Perc, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Network, ncol = 5) +
  geom_text(aes(label = Percent, color = SchoolYear), size = 6, vjust=-0.3) +
  scale_fill_manual(values =c(rep(pastYearCol, 5), currentYearCol)) +
  scale_color_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  sota_theme_bar +
  ylim(0, 115) + 
  xlab("") + ylab("") +
  theme(strip.text.x = element_text(size = baseSizeDef+10)) +
  theme(strip.background = element_rect(colour = "white"))


### Section 3 (cont): Creative Schools Certifications  --------------------

# Table 5: Creative Schools Certifications, district overall

final <- aggregate(rubric_list$SchoolId,
                   by = list(rubric_list$SchoolYears, rubric_list$FinalScore),
                   FUN = NROW)
final_names <- list("SchoolYears", "Final", "Num")

colnames(final) <- final_names
final <- arrange(final, SchoolYears, Final)
xref_final <- aggregate(final$Num,
                        by = list(final$SchoolYears),
                        FUN = sum)
final <- merge(final, xref_final, by.x = "SchoolYears", 
               by.y = "Group.1", all.x = TRUE)

final$Percent <- paste(round((final$Num / final$x)*100, digits = 0), "%", sep = "")

finalLong = final

final <- reshape(final,
                 timevar = "SchoolYears",
                 idvar = c("Final"),
                 direction = "wide")

final_names <- list("FinalScore", 
                    "Num 2012-13", "Total 2012-13", "Percent 2012-13",
                    "Num 2013-14", "Total 2013-14", "Percent 2013-14",
                    "Num 2014-15", "Total 2014-15", "Percent 2014-15",
                    "Num 2015-16", "Total 2015-16", "Percent 2015-16",
                    "Num 2016-17", "Total 2016-17", "Percent 2016-17",
                    "Num 2017-18", "Total 2017-18", "Percent 2017-18")
colnames(final) <- final_names

# ..............

# For plotting

finalLong$percNumber = as.numeric(as.character(
  gsub("%", "", finalLong$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
finalLong$adjScore = ifelse(finalLong$Final %in% c(3,4,5),
                        finalLong$percNumber*-1,
                        finalLong$percNumber)

# Add labels
finalLong$CSCcat = rep(cscLabels, 6)
# factor score
finalLong$Final = as.factor(as.character(finalLong$Final))

# reorder negative categories to ensure they display in the correct order 
finalLong$FinalReorder = fct_relevel(finalLong$Final, c("1","2","5","4","3"))
# Also reorder colors so that they appear properly
cscColorsNegBarAll = cscColors[c(1,2,5,4,3)]
# guides(override.aes) will be used to fix the color order in the legend
# This is all a little goofy, but it works

# ...........

# Percentages are a little misleading  with so few reporting in early years
# Section out the total school numbers to provide context
final_total = finalLong[,c("SchoolYears", "x")]
final_total$duplicated = duplicated(final_total$SchoolYears)
final_total = final_total[final_total$duplicated == FALSE,]
final_total$Nlab = paste(final_total$x, "Schools", sep = " ")

# add height of each label (y val) = 1 + 2 percentages
final_total$height = 0
for (i in 1:nrow(final_total)) {
  # rows applying to that year and network group
  relRows = finalLong[finalLong$SchoolYears == 
                        final_total$SchoolYears[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Final %in% c(1,2),]$percNumber)
  # Apply to final_total
  final_total$height[i] = relSum
}

# ..........

# plot
final_csc_plot = ggplot(finalLong,
       aes(x = SchoolYears, y = adjScore, fill = FinalReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBarAll,
                    labels = cscLabels) +
  sota_theme_bar_leg +
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(fill = cscColors))) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
          data = final_total, vjust = -0.2, size = 4,
          color = "black")

# ....................................

# Table 6: Creative Schools Certifications, elementary schools

final_type <- aggregate(rubric_list$SchoolId,
                   by = list(rubric_list$SchoolYears,
                             rubric_list$FinalScore, rubric_list$SchoolType),
                   FUN = NROW)


final_names_type <- list("SchoolYears", "Final", "SchoolType", "Num")
colnames(final_type) <- final_names_type
xref_final_type <- aggregate(final_type$Num,
                        by = list(final_type$SchoolYears, final_type$SchoolType),
                        FUN = sum)

final_type <- merge(final_type, xref_final_type, by.x = c("SchoolYears", 
                                                          "SchoolType"),
                    by.y = c("Group.1", "Group.2"), all.x = TRUE)

final_type$Percent <- paste(round((final_type$Num / final_type$x)*100, digits = 0),
                            "%", sep = '')

final_type_es <- final_type[final_type$SchoolType == "ES",]
final_type_es <- final_type_es[,-2] # Delete SchoolType column

final_type_es <- reshape(final_type_es,
                      timevar = "SchoolYears",
                      idvar = c("Final"),
                      direction = "wide")
colnames(final_type_es) <- final_names
final_type_es <- arrange(final_type_es, FinalScore)

# .........

# Table 7: Creative Schools Certifications, high schools
final_type_hs <- final_type[final_type$SchoolType == "HS",]
final_type_hs <- final_type_hs[,-2] # Delete SchoolType column

final_type_hs <- reshape(final_type_hs,
                         timevar = "SchoolYears",
                         idvar = c("Final"),
                         direction = "wide")
colnames(final_type_hs) <- final_names
final_type_hs <- arrange(final_type_hs, FinalScore)

# ...........

# Plot HS and ES separately

final_type$percNumber = as.numeric(as.character(
  gsub("%", "", final_type$Percent)))

# sort by year and final score - it got scrambled above
final_type = final_type[
  with(final_type, order(SchoolYears, SchoolType, Final)),
  ]

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
final_type$adjScore = ifelse(final_type$Final %in% c(3,4,5),
                            final_type$percNumber*-1,
                            final_type$percNumber)

# Add labels
final_type$CSCcat = rep(cscLabels, 6)
# factor score
final_type$Final = as.factor(as.character(final_type$Final))

# reorder negative categories and colors to ensure correct order 
cscColorsNegBarAll = cscColors[c(1,2,5,4,3)]
final_type$FinalReorder = fct_relevel(final_type$Final, c("1","2","5","4","3"))

# relabel facets
type_names = c("ES" = "Elementary Schools", "HS" = "High Schools")

# .........

# calculate total schools in each year
final_type_total = final_type[,c("SchoolYears", "SchoolType", "x")]
final_type_total$dupeCheck = paste(final_type_total[,1], 
                                   final_type_total[,2], 
                                   final_type_total[,3],
                                   sep = "_")
final_type_total$duplicated = duplicated(final_type_total$dupeCheck)
final_type_total = final_type_total[final_type_total$duplicated == FALSE,]
final_type_total$Nlab = paste(final_type_total$x, "Schools", sep = " ")

# add height of each label (y val) = 1 + 2 percentages
final_type_total$height = 0
for (i in 1:nrow(final_type_total)) {
  # rows applying to that year and network group
  relRows = final_type[final_type$SchoolYears == 
                         final_type_total$SchoolYears[i] &
                         final_type$SchoolType == 
                         final_type_total$SchoolType[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(as.numeric(as.character(gsub("%", "", relRows[relRows$Final %in% c(1,2),]$Percent))))
  # Apply to final_type_total
  final_type_total$height[i] = relSum
}


# .........


# plot
final_type_csc_plot = ggplot(final_type,
                        aes(x = SchoolYears, y = adjScore, fill = FinalReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~SchoolType, labeller = as_labeller(type_names)) +
  scale_fill_manual(values = cscColorsNegBarAll,
                    labels = cscLabels) +
  sota_theme_bar_leg +
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(fill = cscColors))) +
   theme(strip.background = element_blank())  +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
          data = final_type_total, vjust = -0.2, size = 4,
          color = "black") 

# ..................................................................

# Table 8: Creative Schools Certification: district-run

final_other <- aggregate(rubric_list$SchoolId,
                         by = list(rubric_list$SchoolYears, rubric_list$Group, 
                                   rubric_list$FinalScore),
                         FUN = NROW)
xref_final_other <- aggregate(final_other$x,
                              by = list(final_other$Group.1, final_other$Group.2),
                              FUN = sum)
final_other <- merge(final_other, xref_final_other, by = c("Group.1", "Group.2"))
rm(xref_final_other)
final_other$Percent <- paste(round((final_other$x.x / final_other$x.y)*100, digits = 0), "%", sep = "")
final_other_name <- list("SchoolYears", "NetworkGroup", "Final", "Num", "N", "Percent")
colnames(final_other) <- final_other_name
rm(final_other_name)

final_districtrun <- final_other[final_other$NetworkGroup == "District-Run",]
final_districtrun <- final_districtrun[,-2]
final_districtrun <- reshape(final_districtrun,
                             timevar = "SchoolYears",
                             idvar = c("Final"),
                             direction = "wide")
colnames(final_districtrun) <- final_names
final_districtrun <- arrange(final_districtrun, `FinalScore`)

# Table 9: Creative Schools Certification: charter/contract

final_charter <- final_other[final_other$NetworkGroup == "Charter & Contract",]
final_charter <- final_charter[,-2]
final_charter <- reshape(final_charter,
                         timevar = "SchoolYears",
                         idvar = c("Final"),
                         direction = "wide")
colnames(final_charter) <- final_names
final_charter <- arrange(final_charter, `FinalScore`)

# Table 10: Creative Schools Certification: other

final_options <- final_other[final_other$NetworkGroup == "Other",]
final_options <- final_options[,-2]
final_options <- reshape(final_options,
                         timevar = "SchoolYears",
                         idvar = c("Final"),
                         direction = "wide")
colnames(final_options) <- final_names
final_options <- arrange(final_options, `FinalScore`)


# ............

# Plot by network group

final_other = final_other[final_other$NetworkGroup != "NA",]

final_other$percNumber = as.numeric(as.character(
  gsub("%", "", final_other$Percent)))

# sort by year and final score - it got scrambled above
final_other = final_other[
  with(final_other, order(SchoolYears, NetworkGroup, Final)),
  ]

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
final_other$adjScore = ifelse(final_other$Final %in% c(3,4,5),
                             final_other$percNumber*-1,
                             final_other$percNumber)

# Add labels
final_other$CSCcat = recode(final_other$Final, 
                            "1" = "Excelling",
                            "2" = "Strong",
                            "3" = "Developing",
                            "4" = "Emerging",
                            "5" = "Insufficient Data")

# Change "other" to "options", reorder network group
final_other$NetworkGroup = recode(final_other$NetworkGroup, 
                            "Other" = "Options and Other")
final_other$NetworkGroup = fct_relevel(final_other$NetworkGroup, 
							c("District-Run", "Charter & Contract", "Options and Other"))							

# factor score
final_other$Final = as.factor(as.character(final_other$Final))

# reorder negative categories to ensure they display in the correct order 
# Note: the legend itself won't appear in the correct order this way
cscColorsNegBar = cscColors[c(1,2,5,4,3)]
final_other$FinalReorder = fct_relevel(final_other$Final, 
                                       c("1","2","5","4","3"))
final_other$CSCreorder = fct_relevel(final_other$CSCcat, c("Excelling", "Strong", 
                                                         "Insufficient Data",
                                                         "Emerging", "Developing"))

# ...............

# Section out the total school numbers to provide context
final_other_total = final_other[,c("SchoolYears", "NetworkGroup", "N")]
final_other_total$dupeCheck = paste(final_other_total[,1], 
                                    final_other_total[,2], 
                                    final_other_total[,3],
                                    sep = "_")
final_other_total$duplicated = duplicated(final_other_total$dupeCheck)
final_other_total = final_other_total[final_other_total$duplicated == FALSE,]
final_other_total$Nlab = paste(final_other_total$N, "Schools", sep = " ")

# add height of each label (y val) = 1 + 2 percentages
final_other_total$height = 0
for (i in 1:nrow(final_other_total)) {
  # rows applying to that year and network group
  relRows = final_other[final_other$SchoolYears == 
                          final_other_total$SchoolYears[i] &
                          final_other$NetworkGroup == 
                          final_other_total$NetworkGroup[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(as.numeric(as.character(relRows[relRows$Final %in% c(1,2),]$percNumber)))
  # Apply to final_other_total
  final_other_total$height[i] = relSum
}

# ..............

# plot
final_other_csc_plot = ggplot(final_other,
                             aes(x = SchoolYears, y = adjScore, 
                                 fill = CSCreorder)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~NetworkGroup) +
  # Add labels on top of bars
  scale_fill_manual(values = cscColorsNegBarAll,
                    labels = cscLabels) +
  sota_theme_bar_leg + 
  guides(fill = guide_legend(override.aes = list(fill = cscColors))) +
  xlab("") + ylab("") +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = final_other_total, vjust = -0.2, size = 4,
            color = "black") 

# ...................................................................

# Table 11: Creative Schools Certification: network breakdown

final_net <- aggregate(rubric_list$SchoolId,
                       by = list(rubric_list$SchoolYears, rubric_list$Network, rubric_list$FinalScore),
                       FUN = NROW)
xref_final_net <- aggregate(final_net$x,
                            by = list(final_net$Group.1, final_net$Group.2),
                            FUN = sum)
final_net <- merge(final_net, xref_final_net, by = c("Group.1", "Group.2"))
rm(xref_final_net)
final_net$Percent <- paste(round((final_net$x.x / final_net$x.y)*100, digits = 0),
                           "%", sep = "")
final_net_name <- list("SchoolYears", "Network", "FinalScore", "Num", "N", "Percent")
colnames(final_net) <- final_net_name
final_net <- final_net[order(final_net$Network, final_net$SchoolYears),]
final_net <- arrange(final_net, Network, SchoolYears, FinalScore)

# ............

# Plot by network 

final_net_plot_df = final_net[final_net$Network != "",]

final_net_plot_df$percNumber = as.numeric(as.character(
  gsub("%", "", final_net_plot_df$Percent)))

# sort by year and final score - it got scrambled above
final_net_plot_df = final_net_plot_df[
  with(final_net_plot_df, order(SchoolYears, Network, FinalScore)),
  ]

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
final_net_plot_df$adjScore = ifelse(final_net_plot_df$FinalScore %in% c(3,4,5),
                              final_net_plot_df$percNumber*-1,
                              final_net_plot_df$percNumber)

# Add labels
final_net_plot_df$CSCcat = recode(final_net_plot_df$FinalScore, 
                            "1" = "Excelling",
                            "2" = "Strong",
                            "3" = "Developing",
                            "4" = "Emerging",
                            "5" = "Insufficient Data")


# factor score
final_net_plot_df$FinalScore = as.factor(as.character(final_net_plot_df$FinalScore))

# reorder negative categories to ensure they display in the correct order 
# Note: the legend itself won't appear in the correct order this way
cscColorsNegBar = cscColors[c(1,2,5,4,3)]
final_net_plot_df$FinalReorder = fct_relevel(final_net_plot_df$FinalScore, c("1","2","5","4","3"))
final_net_plot_df$CSCreorder = fct_relevel(final_net_plot_df$CSCcat, c("Excelling", "Strong", 
                                                           "Insufficient Data",
                                                           "Emerging", "Developing"))

# Reorder factor levels
numNets = paste("Network", 1:17)
final_net_plot_df$Network = fct_relevel(final_net_plot_df$Network,
                                    numNets[1:13], "Charter ES",
									numNets[14:17], "Charter HS", "Contract", "Options",
                                    "ISP", "AUSL")

# plot
final_net_csc_plot = ggplot(final_net_plot_df,
                              aes(x = SchoolYears, y = adjScore, fill = CSCreorder)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Network, ncol = 5) +
  # Add labels on top of bars
  scale_fill_manual(values = cscColorsNegBar) +
  sota_theme_bar_leg +
  #ylim(0, 60) + # make sure  labels don't get cut off
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank())

### Section 3 (cont): Staffing ---------------------------------------

## Note that, for most of the staffing analyses, only reporting schools are considered. Also, when conducting analyses on each of the rubric criteria, it will be important to work from the rubric_list_complete data frame. This is necessary because sometimes there are schools that complete one portion of the survey (e.g., staffing), but do not complete the entire survey. To double check your work, be sure that the N for each year in the rubric_list_complete data frame matches what you have calculated as the total number of complete surveys.

# Calculate minimum number of staffing needed to meet 1:350 ratio district-wide
minFTEcount = round(enrollment_total_num/350, 0)

# ........................................................................

# Table 12A: FTE Count, district overall
staff_count <- aggregate(teacher$RealFraction,
                         by = list(teacher$SchoolYears),
                         FUN = sum)
staff_count_names <- c("School Year", "Number of FTEs")
colnames(staff_count) <- staff_count_names

# 2012-13 data uses CPS Office of Accountability numbers; see pg 30 of 2016-17 SOTA
staff_count[1,2] = 1163.4

# Plot
staff_count_plot_df = staff_count
colnames(staff_count_plot_df) = c("SchoolYear", "numFTE")
staff_count_plot_df$numFTE = round(staff_count_plot_df$numFTE, 1)

staff_count_bar = ggplot(staff_count_plot_df, 
       aes(x = SchoolYear, y = numFTE, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  # Add labels on top of bars
  geom_text(aes(label = numFTE, color = SchoolYear), size = topTextSingleSize-1, vjust=-0.3) +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  scale_color_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  sota_theme_bar +
  ylim(0, max(staff_count_plot_df$numFTE) +
         max(staff_count_plot_df$numFTE)*.1) +
  xlab("") + ylab("") 

# ...............................................................

# Table 12B: Counts of FTEs
staffing_categories <- select(rubric_list_complete, SchoolId, UnitName, 
                              SchoolType, SchoolYears, #TotalFTE, 
                              TotalFTECount,
                              FinalScore)

staffing_categories$StaffingLabel =ifelse(
      staffing_categories$TotalFTECount >= 4.0, "4 or more",
      ifelse(staffing_categories$TotalFTECount >= 3.0 & 
               staffing_categories$TotalFTECount < 4.0, "3 - 3.5",
             ifelse(staffing_categories$TotalFTECount >= 2.0 & 
                      staffing_categories$TotalFTECount < 3.0, "2 - 2.5",
                    ifelse(staffing_categories$TotalFTECount >= 1.0 & 
                             staffing_categories$TotalFTECount < 2.0, "1 - 1.5",
                           ifelse(staffing_categories$TotalFTECount > 0 & 
                                    staffing_categories$TotalFTECount < 1.0, "0.5",
                                  ifelse(staffing_categories$TotalFTECount == 0, 
                                         "0", "NA"))))))

staff_labels <- aggregate(staffing_categories$SchoolId,
                          by = list(staffing_categories$SchoolYears, staffing_categories$SchoolType, staffing_categories$StaffingLabel),
                          FUN = NROW)

xref_staff_labels <- aggregate(staff_labels$x,
                               by = list(staff_labels$Group.1, 
                                         staff_labels$Group.2),
                               FUN = sum)

staff_labels <- merge(staff_labels, xref_staff_labels, 
                      by = c("Group.1", "Group.2"))
staff_labels$Percent <- paste(round(
  (staff_labels$x.x / staff_labels$x.y)*100, digits = 0), "%", sep = "")

staff_labels_name <- list("SchoolYears", "SchoolType", 
                          "Category", "Num", "N", "Percent")
colnames(staff_labels) <- staff_labels_name
rm(staff_labels_name)

staff_labels <- arrange(staff_labels, desc(SchoolYears),
                        SchoolType, desc(Category))

# ...............

# Plot FTE bands by ES/HS, 17-18 only

staff_labels_plot_df = staff_labels[staff_labels$SchoolYears == 
                                      "2017-18",]
staff_labels_plot_df$percNumber = as.numeric(as.character(
  gsub("%", "", staff_labels_plot_df$Percent)))
staffBlues = brewer.pal(7, "Blues")[2:7]
staff_labels_plot_df$Type = recode(staff_labels_plot_df$SchoolType,
                                    "ES" = "Elementary Schools", 
                                    "HS" = "High Schools")

staff_bands_bar = ggplot(staff_labels_plot_df, 
                         aes(x = Category, y = percNumber, 
                             fill = Category)) +
  geom_bar(stat = "identity") +
  # Add labels on top of bars
  geom_text(aes(label = Percent, color = Category),
            size = topTextMultiSize, vjust=-0.3) +
  scale_fill_manual(values = staffBlues) +
  scale_color_manual(values = staffBlues) +
  sota_theme_bar +
  ylim(0, max(staff_labels_plot_df$percNumber) +
         max(staff_labels_plot_df$percNumber)*.1) +
  ylab("") + xlab("Number of FTEs") +
  facet_wrap(~Type, ncol = 2) +
  theme(strip.background = element_blank())

# ................................................................

# Table 12C: FTEs by arts discipline, district overall

teachers_agg <- aggregate(teacher$RealFraction,
                          by = list(teacher$SchoolYears, 
                                    teacher$PrimaryFocus),
                          FUN = sum)

xref_teachers <- left_join(teacher, rubric_list_full[,c("SchoolId",
                                                        "SchoolYears",
                                                        "SchoolType")],
                           by = c("SchoolId" = "SchoolId",
                                  "SchoolYears" = "SchoolYears"))

teachers_agg_schooltype <- aggregate(xref_teachers$RealFraction,
                                     by = list(xref_teachers$SchoolYears, 
                                               xref_teachers$SchoolType,
                                               xref_teachers$PrimaryFocus),
                                     FUN = sum)

teachers_agg_schooltype <- arrange(teachers_agg_schooltype, desc(Group.1),
                                   Group.2, Group.3)
teachers_agg_schooltype$x <- round(teachers_agg_schooltype$x, digits = 1)

teachers_agg_schooltype_xref <- aggregate(teachers_agg_schooltype$x, by = 
                                            list(teachers_agg_schooltype$Group.1,
                                                 teachers_agg_schooltype$Group.2),
                                          FUN = sum)

teachers_agg_schooltype <- left_join(teachers_agg_schooltype, 
                                     teachers_agg_schooltype_xref, 
                                     by = c("Group.1" = "Group.1", 
                                            "Group.2" = "Group.2"))

# Next, remove some blank rows. Recall that, each year, we are able to collect some staffing information through CPS for schools that do not report on the survey. For these instructors, we are not always able to determine their primary discipline.
teachers_agg_schooltype = teachers_agg_schooltype[
  !teachers_agg_schooltype$Group.3 == "",]

teachers_agg_schooltype$Percent <- paste(round((teachers_agg_schooltype$x.x /
                                                  teachers_agg_schooltype$x.y)*100,
                                               digits = 1), "%", sep = "")

names(teachers_agg_schooltype) <- c("SchoolYears", "SchoolType",
                                    "ArtsDiscipline", "FTEsByDiscipline",
                                    "TotalFTEs", "Percent")

# Next, remove some blank rows. Recall that, each year, we are able to collect some staffing information through CPS for schools that do not report on the survey. For these instructors, we are not always able to determine their primary discipline.

teachers_agg <- teachers_agg[!teachers_agg$Group.2 == "",]

xref_teachers_agg <- aggregate(teachers_agg$x,
                               by = list(teachers_agg$Group.1),
                               FUN = sum)
teachers_agg <- merge(teachers_agg, xref_teachers_agg, by = "Group.1")
rm(xref_teachers_agg)

teachers_agg$Percent <- paste(round((teachers_agg$x.x / teachers_agg$x.y)*100,
                                    digits = 0), "%", sep = "")
teachers_agg_name <- list("SchoolYears", "Discipline", "Num", "N", "Percent")
colnames(teachers_agg) <- teachers_agg_name
rm(teachers_agg_name)
teachers_agg <- arrange(teachers_agg, desc(SchoolYears), desc(Num))

# ................

# Plot disciplines over time

teachers_agg_plot_df = teachers_agg
teachers_agg_plot_df$percNumber = as.numeric(as.character(
  gsub("%", "", teachers_agg_plot_df$Percent)))
teachers_agg_plot_df$Discipline = recode(teachers_agg_plot_df$Discipline,
                                         "VisualArts" = "Visual Arts",
                                         "MediaArts" = "Media Arts",
										 "LiteraryArts" = "Literary Arts")

disc_time_bar = ggplot(teachers_agg_plot_df, 
                         aes(x = SchoolYears, y = percNumber, fill = Discipline)) +
  geom_bar(stat = "identity") +
  # Add labels on top of bars
  geom_text(aes(label = Percent, color = Discipline), size = topTextMultiSize, vjust=-0.3) +
  scale_fill_manual(values = progColors[1:6]) +
  scale_color_manual(values = progColors[1:6]) +
  sota_theme_bar +
  ylim(0, max(teachers_agg_plot_df$percNumber) +
         max(teachers_agg_plot_df$percNumber)*.2) +
  xlab("") + ylab("") +
  facet_wrap(~Discipline, ncol = 3) +
  theme(strip.background = element_blank())

# ....

# Plot disciplines, just 2017-18

disc_perc_bar = ggplot(
  teachers_agg_plot_df[teachers_agg_plot_df$SchoolYears == "2017-18",], 
                       aes(x = Discipline, y = percNumber, 
                           fill = Discipline)) +
  geom_bar(stat = "identity") +
  # Add labels on top of bars
  geom_text(aes(label = Percent, color = Discipline), size = topTextSingleSize, vjust=-0.3) +
  scale_fill_manual(values = progColors[1:6]) +
  scale_color_manual(values = progColors[1:6]) +
  sota_theme_bar +
  ylim(0, max(teachers_agg_plot_df$percNumber) +
         max(teachers_agg_plot_df$percNumber)*.2) +
  xlab("") + ylab("") 

# ......

# Plot disciplines by school type, 2017-18

teachers_type_plot_df = teachers_agg_schooltype
teachers_type_plot_df$percNumber = as.numeric(as.character(
  gsub("%", "", teachers_type_plot_df$Percent)))
teachers_type_plot_df$Discipline = recode(teachers_type_plot_df$ArtsDiscipline,
                                         "VisualArts" = "Visual Arts",
                                         "MediaArts" = "Media Arts")


										 
teachers_type_plot_df$SchoolType = recode(teachers_type_plot_df$SchoolType, 
                            "ES" = "Elementary Schools",
							"HS" = "High Schools")

disc_type_bar = ggplot(
  teachers_type_plot_df[teachers_type_plot_df$SchoolYears == "2017-18",], 
                       aes(x = Discipline, y = percNumber, 
                           fill = Discipline)) +
  geom_bar(stat = "identity") +
  # Add labels on top of bars
  geom_text(aes(label = Percent, color = Discipline),
            size = topTextMultiSize, vjust=-0.3) +
  scale_fill_manual(values = progColors[1:6]) +
  scale_color_manual(values = progColors[1:6]) +
  sota_theme_bar +
  ylim(0, max(teachers_type_plot_df$percNumber) +
         max(teachers_type_plot_df$percNumber)*.2) +
  xlab("") + ylab("") +
  facet_wrap(~SchoolType, ncol = 2)


### Section 3 (cont): Staffing CSC categories ------------

# Table 13: Staffing categories, district overall

staff <- aggregate(rubric_list_complete$SchoolId,
                       by = list(rubric_list_complete$SchoolYears, 
                                 rubric_list_complete$Fte),
                       FUN = NROW)

xref_staff <- aggregate(staff$x,
                            by = list(staff$Group.1),
                            FUN = sum)
staff <- merge(staff, xref_staff, by = "Group.1")
rm(xref_staff)

staff$Percent <- paste(round((staff$x.x / staff$x.y)*100, digits = 0), 
                       "%", sep = "")
staff_name <- list("SchoolYears", "Status", "Num", "N", "Percent")
colnames(staff) <- staff_name
rm(staff_name)

staff <- arrange(staff, SchoolYears, Status)
staff_long = staff

staff <- reshape(staff,
                 timevar = "SchoolYears",
                 idvar = c("Status"),
                 direction = "wide")

staff_names <- list("Staff Score", 
                    "Num 2012-13", "Total 2012-13", "Percent 2012-13",
                    "Num 2013-14", "Total 2013-14", "Percent 2013-14",
                    "Num 2014-15", "Total 2014-15", "Percent 2014-15",
                    "Num 2015-16", "Total 2015-16", "Percent 2015-16",
                    "Num 2016-17", "Total 2016-17", "Percent 2016-17",
                    "Num 2017-18", "Total 2017-18", "Percent 2017-18")
colnames(staff) <- staff_names

# ....

# Prepare for plotting

colnames(staff_long)[2] = "Staff"

staff_long$percNumber = as.numeric(as.character(
  gsub("%", "", staff_long$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
staff_long$adjScore = ifelse(staff_long$Staff %in% c(3,4),
                               staff_long$percNumber*-1,
                               staff_long$percNumber)

# Add labels
staff_long$CSCcat =  recode(staff_long$Staff, 
                              "1" = "Excelling",
                              "2" = "Strong",
                              "3" = "Developing",
                              "4" = "Emerging")
# factor score
staff_long$Staff = as.factor(as.character(staff_long$Staff))

# Reorder negative categories (3 and 4) to ensure they display in the correct order
staff_long$StaffReorder = fct_relevel(staff_long$Staff, c("1","2","4","3"))
# Also reorder colors so that they appear properly
cscColorsNegBar = cscColors[c(1,2,4,3)]
# guides(override.aes) will be used to fix the color order in the legend
# This is all a little goofy, but it works

# .........

# get total school numbers
staff_total = staff_long[,c("SchoolYears", "N")]
staff_total$dupeCheck = paste(staff_total[,1], 
                                staff_total[,2], sep = "_")
staff_total$duplicated = duplicated(staff_total$dupeCheck)
staff_total = staff_total[staff_total$duplicated == FALSE,]
staff_total$Nlab = paste(staff_total$N, "Schools", sep = " ")

# add height of each label (y val) = 1 + 2 percentages
staff_total$height = 0
for (i in 1:nrow(staff_total)) {
  # rows applying to that year and network group
  relRows = staff_long[staff_long$SchoolYears == 
                           staff_total$SchoolYears[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Staff %in% c(1,2),]$percNumber)
  # Apply to staff_total
  staff_total$height[i] = relSum
}

# .....

# plot
staff_plot = 
  ggplot(staff_long,
                      aes(x = SchoolYears, y = adjScore, fill = StaffReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar, # colors ordered to match 1 2 4 3
                    labels = cscLabels[1:4]) + # Labels in correct order for legend
  sota_theme_bar_leg +
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = staff_total, vjust = -0.2, size = 4,
            color = "black") +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4])))


# ..............................................................................
# Table 14: Staffing categories, elementary schools

staff_type <- aggregate(rubric_list_complete$SchoolId,
                        by = list(rubric_list_complete$SchoolYears,
                                  rubric_list_complete$Fte, 
                                  rubric_list_complete$SchoolType),
                        FUN = NROW)

staff_names_type <- list("SchoolYears", "Staffing", "SchoolType", "Num")
colnames(staff_type) <- staff_names_type
xref_staff_type <- aggregate(staff_type$Num,
                             by = list(staff_type$SchoolYears, 
                                       staff_type$SchoolType),
                             FUN = sum)

# Merge based on two columns, SchoolYears and SchoolType
staff_type <- merge(staff_type, xref_staff_type, 
                    by.x = c("SchoolYears", "SchoolType"), 
                    by.y = c("Group.1", "Group.2"), all.x = TRUE)
staff_type$Percent <- paste(round((staff_type$Num / staff_type$x)*100, 
                                  digits = 0), "%", sep = "")

staff_type_es <- staff_type[staff_type$SchoolType == "ES",]
staff_type_es <- staff_type_es[,-2] # Delete SchoolType column
staff_type_es <- reshape(staff_type_es,
                         timevar = "SchoolYears",
                         idvar = c("Staffing"),
                         direction = "wide")
colnames(staff_type_es) <- staff_names
staff_type_es <- arrange(staff_type_es, `Staff Score`)

# .......

# Table 15: Staffing categories, high schools
staff_type_hs <- staff_type[staff_type$SchoolType == "HS",]
staff_type_hs <- staff_type_hs[,-2] # Delete SchoolType column
staff_type_hs <- reshape(staff_type_hs,
                         timevar = "SchoolYears",
                         idvar = c("Staffing"),
                         direction = "wide")
colnames(staff_type_hs) <- staff_names
staff_type_hs <- arrange(staff_type_hs, `Staff Score`)

# .....

# Prepare for plotting

colnames(staff_type)[3] = "Staff"

staff_type$percNumber = as.numeric(as.character(
  gsub("%", "", staff_type$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
staff_type$adjScore = ifelse(staff_type$Staff %in% c(3,4),
                             staff_type$percNumber*-1,
                             staff_type$percNumber)

# Relabel school type
staff_type$SchoolType = recode(staff_type$SchoolType, 
                                        "ES" = "Elementary Schools",
                                        "HS" = "High Schools")

# Add labels
staff_type$CSCcat =  recode(staff_type$Staff, 
                            "1" = "Excelling",
                            "2" = "Strong",
                            "3" = "Developing",
                            "4" = "Emerging")
# factor score
staff_type$Staff = as.factor(as.character(staff_type$Staff))

# Reorder negative categories (3 and 4) to ensure they display in the correct order
staff_type$StaffReorder = fct_relevel(staff_type$Staff, c("1","2","4","3"))
# Also reorder colors so that they appear properly
cscColorsNegBar = cscColors[c(1,2,4,3)]
# guides(override.aes) will be used to fix the color order in the legend
# This is all a little goofy, but it works

# .......

# get total school numbers
staff_type_total = staff_type[,c("SchoolYears", "x", "SchoolType")]
staff_type_total$dupeCheck = paste(staff_type_total[,1], 
                                   staff_type_total[,2],
                                   staff_type_total[,3], sep = "_")
staff_type_total$duplicated = duplicated(staff_type_total$dupeCheck)
staff_type_total = staff_type_total[staff_type_total$duplicated == FALSE,]
staff_type_total$Nlab = paste(staff_type_total$x, "Schools", sep = " ")

# add height of each label (y val) = 1 + 2 percentages
staff_type_total$height = 0
for (i in 1:nrow(staff_type_total)) {
  # rows applying to that year and network group
  relRows = staff_type[staff_type$SchoolYears == 
                         staff_type_total$SchoolYears[i] &
                         staff_type$SchoolType == 
                         staff_type_total$SchoolType[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Staff %in% c(1,2),]$percNumber)
  # Apply to staff_type_total
  staff_type_total$height[i] = relSum
}

# ..........

# plot
staff_type_plot = 
  ggplot(staff_type,
         aes(x = SchoolYears, y = adjScore, fill = StaffReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar, # colors ordered to match 1 2 4 3
                    labels = cscLabels[1:4]) + # Labels in correct order for legend
  sota_theme_bar_leg +
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = staff_type_total, vjust = -0.2, size = 4,
            color = "black") +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4]))) +
  facet_wrap(~SchoolType, ncol = 2)

# ...............................................................

# Table 16: Staffing Ratios, district overall
rubric_list_complete$RatioBand <- 
  ifelse(rubric_list_complete$EnrollmentRatio > 0 & 
           rubric_list_complete$EnrollmentRatio <= 350, "1:350 or better",
         ifelse(rubric_list_complete$EnrollmentRatio > 350 
                & rubric_list_complete$EnrollmentRatio <= 500, "1:351 - 1:500",
                ifelse(rubric_list_complete$EnrollmentRatio > 500 & 
                         rubric_list_complete$EnrollmentRatio <= 750, 
                       "1:501 - 1:750",
                       ifelse(rubric_list_complete$EnrollmentRatio > 750, 
                              "Greater than 750",
                              ifelse(rubric_list_complete$EnrollmentRatio == 0, 
                                     "No FTE", "NA")))))

ratio <- aggregate(rubric_list_complete$SchoolId,
                   by = list(rubric_list_complete$SchoolYears, 
                             rubric_list_complete$RatioBand),
                   FUN = NROW)

xref_ratio <- aggregate(ratio$x,
                        by = list(ratio$Group.1),
                        FUN = sum)
ratio <- merge(ratio, xref_ratio, by = c("Group.1"))
rm(xref_ratio)

ratio$Percent <- paste(round((ratio$x.x / ratio$x.y)*100, digits = 0), "%")
ratio_name <- list("School Year", "Staffing Ratio", "Num", "N", "Percent")
colnames(ratio) <- ratio_name
rm(ratio_name)

ratio <- arrange(ratio, `School Year`)
ratio <- reshape(ratio,
                 timevar = "School Year",
                 idvar = c("Staffing Ratio"),
                 direction = "wide")

ratio_names <- list("Ratio", 
                    "Num 2012-13", "Total 2012-13", "Percent 2012-13",
                    "Num 2013-14", "Total 2013-14", "Percent 2013-14",
                    "Num 2014-15", "Total 2014-15", "Percent 2014-15",
                    "Num 2015-16", "Total 2015-16", "Percent 2015-16",
                    "Num 2016-17", "Total 2016-17", "Percent 2016-17",
                    "Num 2017-18", "Total 2017-18", "Percent 2017-18")
colnames(ratio) <- ratio_names

# .............................................................

# Table 17: Staffing Ratios, elementary schools

ratio_type <- aggregate(rubric_list_complete$SchoolId,
                        by = list(rubric_list_complete$SchoolYears, 
                                  rubric_list_complete$RatioBand, 
                                  rubric_list_complete$SchoolType),
                        FUN = NROW)
ratio_names_type <- list("SchoolYears", "Ratio", "SchoolType", "Num")
colnames(ratio_type) <- ratio_names_type
rm(ratio_names_type)

xref_ratio_type <- aggregate(ratio_type$Num,
                             by = list(ratio_type$SchoolYears, 
                                       ratio_type$SchoolType),
                             FUN = sum)
ratio_type <- merge(ratio_type, xref_ratio_type, 
                    by.x = c("SchoolYears", "SchoolType"),
                    by.y = c("Group.1", "Group.2"), all.x = TRUE)
ratio_type$Percent <- paste(round((ratio_type$Num / ratio_type$x)*100,
                                  digits = 1), "%", sep = "")

ratio_type_es <- ratio_type[ratio_type$SchoolType == "ES",]
ratio_type_es <- ratio_type_es[,-2] # Delete SchoolType column
ratio_type_es <- reshape(ratio_type_es,
                         timevar = "SchoolYears",
                         idvar = c("Ratio"),
                         direction = "wide")
colnames(ratio_type_es) <- ratio_names
ratio_type_es <- arrange(ratio_type_es, `Ratio`)

# .........

# Table 18: Staffing Ratios, high schools
ratio_type_hs <- ratio_type[ratio_type$SchoolType == "HS",]
ratio_type_hs <- ratio_type_hs[,-2] # Delete SchoolType column
ratio_type_hs <- reshape(ratio_type_hs,
                         timevar = "SchoolYears",
                         idvar = c("Ratio"),
                         direction = "wide")
colnames(ratio_type_hs) <- ratio_names
ratio_type_hs <- arrange(ratio_type_hs, `Ratio`)

# ......

# Plot 0 FTE over time, ES/HS

# Modify table for plotting
zero_fte_type = ratio_type[ratio_type$Ratio == "No FTE",]
zero_fte_type$percNumber = as.numeric(as.character(
  gsub("%", "", zero_fte_type$Percent)))

zero_fte_type$SchoolType = recode(zero_fte_type$SchoolType, 
                                        "ES" = "Elementary Schools",
                                        "HS" = "High Schools")

zero_fte_type_bar = ggplot(zero_fte_type,
                            aes(x = SchoolYears, 
                                y = percNumber, fill = SchoolYears)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percent, color = SchoolYears), 
            size = topTextMultiSize, vjust=-0.3) +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  scale_color_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  sota_theme_bar +
  xlab("") + ylab("") + 
  ylim(0, max(zero_fte_type$percNumber)+1) +
  facet_wrap(~SchoolType, ncol = 2) +
  theme(strip.background = element_rect(colour = "white"))  

# ..............................................................

# Table 19: Staffing Ratios, district-run

ratio_other <- aggregate(rubric_list_complete$SchoolId,
                         by = list(rubric_list_complete$SchoolYears, rubric_list_complete$Group, rubric_list_complete$RatioBand),
                         FUN = NROW)
xref_ratio_other <- aggregate(ratio_other$x,
                              by = list(ratio_other$Group.1, ratio_other$Group.2),
                              FUN = sum)
ratio_other <- merge(ratio_other, xref_ratio_other, 
                     by = c("Group.1", "Group.2"))
rm(xref_ratio_other)
ratio_other$Percent <- paste(round((ratio_other$x.x / ratio_other$x.y)*100, 
                                   digits = 0), "%", sep = "")
ratio_other_name <- list("SchoolYear", "NetworkGroup", "Ratio", "Num", "N", "Percent")
colnames(ratio_other) <- ratio_other_name
rm(ratio_other_name)

# ............

ratio_districtrun <- ratio_other[ratio_other$NetworkGroup == "District-Run",]
ratio_districtrun <- ratio_districtrun[,-2]
ratio_districtrun <- reshape(ratio_districtrun,
                             timevar = "SchoolYear",
                             idvar = c("Ratio"),
                             direction = "wide")
colnames(ratio_districtrun) <- ratio_names
ratio_districtrun <- arrange(ratio_districtrun, `Ratio`)

# ..........

# Table 20: Staffing Ratios, charter/contract
ratio_charter <- ratio_other[ratio_other$NetworkGroup == "Charter & Contract",]
ratio_charter <- ratio_charter[,-2]
ratio_charter <- reshape(ratio_charter,
                         timevar = "SchoolYear",
                         idvar = c("Ratio"),
                         direction = "wide")
colnames(ratio_charter) <- ratio_names
ratio_charter <- arrange(ratio_charter, `Ratio`)

# ..........

# Table 21: Staffing Ratios, other
ratio_options <- ratio_other[ratio_other$NetworkGroup == "Other",]
ratio_options <- ratio_options[,-2]
ratio_options <- reshape(ratio_options,
                         timevar = "SchoolYear",
                         idvar = c("Ratio"),
                         direction = "wide")
colnames(ratio_options) <- ratio_names
ratio_options <- arrange(ratio_options, `Ratio`)


# ...........

# Plot 0 FTE by network group

# Modify table for plotting
zero_fte_group = ratio_other[ratio_other$Ratio == "No FTE",]
zero_fte_group$percNumber = as.numeric(as.character(
  gsub("%", "", zero_fte_group$Percent)))

zero_fte_group$NetworkGroup = recode(zero_fte_group$NetworkGroup, 
                                     "Other" = "Options & Other")

zero_fte_group = zero_fte_group[!(is.na(zero_fte_group$NetworkGroup)),]
zero_fte_group = zero_fte_group[zero_fte_group$NetworkGroup != "NA",]

# reorder school type so district comes first
zero_fte_group$NetworkGroup = fct_relevel(zero_fte_group$NetworkGroup, 
                                         c("District-Run", 
                                           "Charter & Contract", 
                                           "Options & Other"))	

# Plot
zero_fte_group_bar = ggplot(zero_fte_group,
                            aes(x = SchoolYear, 
                                y = percNumber, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percent, color = SchoolYear), 
            size = topTextMultiSize, vjust=-0.3) +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  scale_color_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  sota_theme_bar +
  xlab("") + ylab("") + 
  ylim(0, max(zero_fte_group$percNumber)+1) +
  facet_wrap(~NetworkGroup, ncol = 3) 


# ......................................................................

# Table 22: Staffing Ratios, network breakdown

ratio_net <- aggregate(rubric_list_complete$SchoolId,
                       by = list(rubric_list_complete$SchoolYears, 
                                 rubric_list_complete$CpsNewNetwork, 
                                 rubric_list_complete$RatioBand),
                       FUN = NROW)

xref_ratio_net <- aggregate(ratio_net$x,
                            by = list(ratio_net$Group.1, ratio_net$Group.2),
                            FUN = sum)

ratio_net <- merge(ratio_net, xref_ratio_net, 
                   by = c("Group.1", "Group.2"))
rm(xref_ratio_net)

ratio_net$Percent <- paste(round((ratio_net$x.x / ratio_net$x.y)*100, 
                                 digits = 0), "%", sep = "")
ratio_net_name <- list("SchoolYear", "Network", "Status", "Num", "N", "Percent")
colnames(ratio_net) <- ratio_net_name
ratio_net <- ratio_net[order(ratio_net$Network, ratio_net$SchoolYear, ratio_net$Status),]

# Ad-hoc analysis, not included in Rmd
xref <- subset(rubric_list_complete, rubric_list_complete$SchoolYears == "2016-17")
xref <- subset(xref, xref$Fte == 3 | xref$Fte == 4)
sum(xref$TotalEnrollment)
rm(xref)

### Section 3 (cont): Minutes of Instruction ----------------------------------

# This is an elementary school only metric, so we need to first create separate ES and HS data frames
rubric_list_complete$Group <- ifelse(rubric_list_complete$CpsNewNetwork %in% 
                                       district_run, "District-Run",
                                ifelse(rubric_list_complete$CpsNewNetwork %in% 
                                         charter_contract, "Charter & Contract",
                                   ifelse(rubric_list_complete$CpsNewNetwork %in% other,
                                          "Other", "NA")))

rubric_list_complete_es <- rubric_list_complete[rubric_list_complete$SchoolType == "ES",]
rubric_list_complete_hs <- rubric_list_complete[rubric_list_complete$SchoolType == "HS",]

# ..................................................................

# Table 23: Minutes of Instruction categories, district overall

minutes <- aggregate(rubric_list_complete_es$SchoolId,
                   by = list(rubric_list_complete_es$SchoolYears,
                             rubric_list_complete_es$Minutes),
                   FUN = NROW)

xref_minutes <- aggregate(minutes$x,
                        by = list(minutes$Group.1),
                        FUN = sum)

minutes <- merge(minutes, xref_minutes, by = "Group.1")
rm(xref_minutes)

minutes$Percent <- paste(round((minutes$x.x / minutes$x.y)*100, digits = 0), "%", sep = "")
minutes_name <- list("SchoolYears", "Minutes", "Num", "N", "Percent")
colnames(minutes) <- minutes_name
rm(minutes_name)

minutes <- arrange(minutes, SchoolYears, Minutes)
minutes_long = minutes

minutes <- reshape(minutes,
                 timevar = "SchoolYears",
                 idvar = c("Minutes"),
                 direction = "wide")

minutes_names <- list("Minutes", "Num 2012-13", "Total 2012-13", "Percent 2012-13",
                    "Num 2013-14", "Total 2013-14", "Percent 2013-14",
                    "Num 2014-15", "Total 2014-15", "Percent 2014-15",
                    "Num 2015-16", "Total 2015-16", "Percent 2015-16",
                    "Num 2016-17", "Total 2016-17", "Percent 2016-17",
                    "Num 2017-18", "Total 2017-18", "Percent 2017-18")
colnames(minutes) <- minutes_names

# .............

# plot

minutes_long$percNumber = as.numeric(as.character(
  gsub("%", "", minutes_long$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
minutes_long$adjScore = ifelse(minutes_long$Minutes %in% c(3,4),
                            minutes_long$percNumber*-1,
                            minutes_long$percNumber)

# Add labels
minutes_long$CSCcat =  recode(minutes_long$Minutes, 
                              "1" = "Excelling",
                              "2" = "Strong",
                              "3" = "Developing",
                              "4" = "Emerging")
# factor score
minutes_long$Minutes = as.factor(as.character(minutes_long$Minutes))

# reorder negative categories to ensure they display in the correct order 
# Note: the legend itself won't appear in the correct order this way
cscColorsNegBar = cscColors[c(1,2,4,3)]
minutes_long$MinutesReorder = fct_relevel(minutes_long$Minutes, c("1","2","4","3"))

# .....

# Percentages are a little misleading  with so few reporting in early years
# Section out the total school numbers to provide context
minutes_total = minutes_long[,c("SchoolYears", "N")]
minutes_total$dupeCheck = paste(minutes_total[,1], 
                                minutes_total[,2], sep = "_")
minutes_total$duplicated = duplicated(minutes_total$dupeCheck)
minutes_total = minutes_total[minutes_total$duplicated == FALSE,]
minutes_total$Nlab = paste("Total:", minutes_total$N, sep = " ")

# add height of each label (y val) = 1 + 2 percentages
minutes_total$height = 0
for (i in 1:nrow(minutes_total)) {
  # rows applying to that year and network group
  relRows = minutes_long[minutes_long$SchoolYears == 
                           minutes_total$SchoolYears[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Minutes %in% c(1,2),]$percNumber)
  # Apply to minutes_total
  minutes_total$height[i] = relSum
}

# .....

# plot
minutes_plot = ggplot(minutes_long,
                        aes(x = SchoolYears, y = adjScore,
                            fill = MinutesReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar,
                    labels = cscLabels[1:4]) +
  sota_theme_bar_leg +
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = minutes_total, vjust = -0.2, size = 4,
            color = "black")  +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4])))

# .........................................................

# Table 24: Minutes of Instruction categories, district-run

minutes_other <- aggregate(rubric_list_complete_es$SchoolId,
                         by = list(rubric_list_complete_es$SchoolYears, 
                                   rubric_list_complete_es$Group, 
                                   rubric_list_complete_es$Minutes),
                         FUN = NROW)

xref_minutes_other <- aggregate(minutes_other$x,
                              by = list(minutes_other$Group.1, minutes_other$Group.2),
                              FUN = sum)

minutes_other <- merge(minutes_other, xref_minutes_other, by = c("Group.1", "Group.2"))
rm(xref_minutes_other)

minutes_other$Percent <- paste(round((minutes_other$x.x / minutes_other$x.y)*100, 
                                     digits = 0), "%", sep = "")
minutes_other_name <- list("SchoolYears", "NetworkGroup", "Minutes", "Num", "N", "Percent")
colnames(minutes_other) <- minutes_other_name
rm(minutes_other_name)

# ........

# Plot minutes by network type

minutes_other$percNumber = as.numeric(as.character(
  gsub("%", "", minutes_other$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
minutes_other$adjScore = ifelse(minutes_other$Minutes %in% c(3,4),
                               minutes_other$percNumber*-1,
                               minutes_other$percNumber)

# Add labels
minutes_other$CSCcat =  recode(minutes_other$Minutes, 
                              "1" = "Excelling",
                              "2" = "Strong",
                              "3" = "Developing",
                              "4" = "Emerging")
# factor score
minutes_other$Minutes = as.factor(as.character(minutes_other$Minutes))

# reorder negative categories to ensure they display in the correct order 
# Note: the legend itself won't appear in the correct order this way
cscColorsNegBar = cscColors[c(1,2,4,3)]
minutes_other$MinutesReorder = fct_relevel(minutes_other$Minutes,
                                           c("1","2","4","3"))


# restrict to district-run and charter/contract (options barely exist for ES)
minutes_other_plot = minutes_other[minutes_other$NetworkGroup %in% c("District-Run",
                                                                     "Charter & Contract"),]
minutes_other_plot$NetworkGroup = fct_relevel(minutes_other_plot$NetworkGroup,
                                              c("District-Run", "Charter & Contract"))

# ....

# Percentages are a little misleading  with so few reporting in early years
# Section out the total school numbers to provide context
minutes_other_total = minutes_other_plot[,c("SchoolYears", "NetworkGroup", "N")]
minutes_other_total$dupeCheck = paste(minutes_other_total[,1], minutes_other_total[,2],
                                      minutes_other_total[,3], sep = "_")
minutes_other_total$duplicated = duplicated(minutes_other_total$dupeCheck)
minutes_other_total = minutes_other_total[minutes_other_total$duplicated == FALSE,]
minutes_other_total$Nlab = paste(minutes_other_total$N, "Schools", sep = " ")

# add height of each label (y val) = 1 + 2 percentages
minutes_other_total$height = 0
for (i in 1:nrow(minutes_other_total)) {
  # rows applying to that year and network group
  relRows = minutes_other_plot[minutes_other_plot$SchoolYears == 
                                 minutes_other_total$SchoolYears[i] &
                                 minutes_other_plot$NetworkGroup ==
                                 minutes_other_total$NetworkGroup[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Minutes %in% c(1,2),]$percNumber)
  # Apply to minutes_other_total
  minutes_other_total$height[i] = relSum
}

# plot
minutes_dcco_plot = ggplot(minutes_other_plot,
                      aes(x = SchoolYears, y = adjScore, fill = MinutesReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar,
                    labels = cscLabels[1:4]) +
  sota_theme_bar_leg +
  #ylim(0, 60) + # make sure  labels don't get cut off
  xlab("") + ylab("") +
  # Percentages above each stacked segment
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~NetworkGroup, ncol = 2) +
  scale_y_continuous(limits = c(NA, max(minutes_other_plot$adjScore + 10))) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = minutes_other_total, vjust = -0.2, size = 3,
            color = "black")  +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4])))

### Section 3 (cont): Percent Access --------------------------------------------

# Table 27: Percent Access, district overall
access <- aggregate(rubric_list_complete_es$SchoolId,
                    by = list(rubric_list_complete_es$SchoolYears, 
                              rubric_list_complete_es$Access),
                    FUN = NROW)

xref_access <- aggregate(access$x,
                         by = list(access$Group.1),
                         FUN = sum)
access <- merge(access, xref_access, by = "Group.1")
rm(xref_access)

access$Percent <- paste(round((access$x.x / access$x.y)*100, digits = 0),
                        "%", sep = "")
access_name <- list("SchoolYears", "Access", "Num", "N", "Percent")
colnames(access) <- access_name
rm(access_name)

access <- arrange(access, SchoolYears, Access)
access_long = access

access <- reshape(access,
                   timevar = "SchoolYears",
                   idvar = c("Access"),
                   direction = "wide")
access_names <- list("Access", "Num 2012-13", "Total 2012-13", "Percent 2012-13",
                      "Num 2013-14", "Total 2013-14", "Percent 2013-14",
                      "Num 2014-15", "Total 2014-15", "Percent 2014-15",
                      "Num 2015-16", "Total 2015-16", "Percent 2015-16",
                      "Num 2016-17", "Total 2016-17", "Percent 2016-17",
                      "Num 2017-18", "Total 2017-18", "Percent 2017-18")
colnames(access) <- access_names

# .............

# set up plot

access_long$percNumber = as.numeric(as.character(
  gsub("%", "", access_long$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
access_long$adjScore = ifelse(access_long$Access %in% c(3,4),
                              access_long$percNumber*-1,
                              access_long$percNumber)

# Add labels
access_long$CSCcat =  recode(access_long$Access, 
                             "1" = "Excelling",
                             "2" = "Strong",
                             "3" = "Developing",
                             "4" = "Emerging")
# factor score
access_long$Access = as.factor(as.character(access_long$Access))

# reorder negative categories to ensure they display in the correct order 
# Note: the legend itself won't appear in the correct order this way
cscColorsNegBar = cscColors[c(1,2,4,3)]
access_long$AccessReorder = fct_relevel(access_long$Access, c("1","2","4","3"))


# .............

# Percentages are a little misleading  with so few reporting in early years
# Section out the total school numbers to provide context
access_total = access_long[,c("SchoolYears", "N")]
access_total$dupeCheck = paste(access_total[,1], 
                               access_total[,2], sep = "_")
access_total$duplicated = duplicated(access_total$dupeCheck)
access_total = access_total[access_total$duplicated == FALSE,]
access_total$Nlab = paste("Total:", access_total$N, sep = " ")

# add height of each label (y val) = 1 + 2 percentages
access_total$height = 0
for (i in 1:nrow(access_total)) {
  # rows applying to that year and network group
  relRows = access_long[access_long$SchoolYears == 
                          access_total$SchoolYears[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Access %in% c(1,2),]$percNumber)
  # Apply to access_total
  access_total$height[i] = relSum
}

# .............

# plot
access_plot = ggplot(access_long,
                     aes(x = SchoolYears, y = adjScore, fill = AccessReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar,
                    labels = cscLabels[1:4]) +
  sota_theme_bar_leg +
  #ylim(0, 60) + # make sure  labels don't get cut off
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = access_total, vjust = -0.2, size = 4,
            color = "black") +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4])))

# ........................................................

# Table ???: Access categories, district-run

access_other <- aggregate(rubric_list_complete_es$SchoolId,
                          by = list(rubric_list_complete_es$SchoolYears, 
                                    rubric_list_complete_es$Group, 
                                    rubric_list_complete_es$Access),
                          FUN = NROW)

xref_access_other <- aggregate(access_other$x,
                               by = list(access_other$Group.1, access_other$Group.2),
                               FUN = sum)

access_other <- merge(access_other, xref_access_other, by = c("Group.1", "Group.2"))
rm(xref_access_other)

access_other$Percent <- paste(round((access_other$x.x / access_other$x.y)*100, 
                                    digits = 0), "%", sep = "")
access_other_name <- list("SchoolYears", "NetworkGroup", "Access", "Num", "N", "Percent")
colnames(access_other) <- access_other_name
rm(access_other_name)

# ........

# Plot access by network type

access_other$percNumber = as.numeric(as.character(
  gsub("%", "", access_other$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
access_other$adjScore = ifelse(access_other$Access %in% c(3,4),
                               access_other$percNumber*-1,
                               access_other$percNumber)

# Add labels
access_other$CSCcat =  recode(access_other$Access, 
                              "1" = "Excelling",
                              "2" = "Strong",
                              "3" = "Developing",
                              "4" = "Emerging")
# factor score
access_other$Access = as.factor(as.character(access_other$Access))

# reorder negative categories to ensure they display in the correct order 
# Note: the legend itself won't appear in the correct order this way
cscColorsNegBar = cscColors[c(1,2,4,3)]
access_other$AccessReorder = fct_relevel(access_other$Access, c("1","2","4","3"))

# restrict to district-run and charter/contract (options barely exist for ES)
access_other_plot = access_other[access_other$NetworkGroup %in% c("District-Run",
                                                                  "Charter & Contract"),]
access_other_plot$NetworkGroup = fct_relevel(access_other_plot$NetworkGroup,
                                             c("District-Run", "Charter & Contract"))

# ....

# Percentages are a little misleading  with so few reporting in early years
# Section out the total school numbers to provide context
access_other_total = access_other_plot[,c("SchoolYears", "NetworkGroup", "N")]
access_other_total$dupeCheck = paste(access_other_total[,1], access_other_total[,2],
                                     access_other_total[,3], sep = "_")
access_other_total$duplicated = duplicated(access_other_total$dupeCheck)
access_other_total = access_other_total[access_other_total$duplicated == FALSE,]
access_other_total$Nlab = paste("Total:", access_other_total$N, sep = " ")

# add height of each label (y val) = 1 + 2 percentages
access_other_total$height = 0
for (i in 1:nrow(access_other_total)) {
  # rows applying to that year and network group
  relRows = access_other_plot[access_other_plot$SchoolYears == 
                                access_other_total$SchoolYears[i] &
                                access_other_plot$NetworkGroup ==
                                access_other_total$NetworkGroup[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Access %in% c(1,2),]$percNumber)
  # Apply to access_other_total
  access_other_total$height[i] = relSum
}

# .....

# plot
access_dcco_plot = ggplot(access_other_plot,
                          aes(x = SchoolYears, y = adjScore, 
                              fill = AccessReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar,
                    labels = cscLabels[1:4]) +
  sota_theme_bar_leg +
  #ylim(0, 60) + # make sure  labels don't get cut off
  xlab("") + ylab("") +
  # Percentages above each stacked segment
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~NetworkGroup, ncol = 2) +
  scale_y_continuous(limits = c(NA, max(access_other_plot$adjScore + 10))) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = access_other_total, vjust = -0.2, size = 3,
            color = "black")  +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4])))

### Section 3 (cont): HS Disciplines & Depth -----------------------------------

# Table 28: HS Disciplines & Depth, district overall

disciplines <- aggregate(rubric_list_complete_hs$SchoolId,
                         by = list(rubric_list_complete_hs$SchoolYears,
                                   rubric_list_complete_hs$Disciplines),
                         FUN = NROW)

xref_disciplines <- aggregate(disciplines$x,
                         by = list(disciplines$Group.1),
                         FUN = sum)
disciplines <- merge(disciplines, xref_disciplines, by = "Group.1")
rm(xref_disciplines)
disciplines$Percent <- paste(round((disciplines$x.x / disciplines$x.y)*100,
                                   digits = 0), "%", sep = "")

disciplines_name <- list("SchoolYears", "Disciplines", "Num", "N", "Percent")
colnames(disciplines) <- disciplines_name
rm(disciplines_name)

disciplines <- arrange(disciplines, SchoolYears, Disciplines)
disciplines_long = disciplines

disciplines <- reshape(disciplines,
                  timevar = "SchoolYears",
                  idvar = c("Disciplines"),
                  direction = "wide")
disciplines_names <- list("Disciplines", 
                     "Num 2012-13", "Total 2012-13", "Percent 2012-13",
                     "Num 2013-14", "Total 2013-14", "Percent 2013-14",
                     "Num 2014-15", "Total 2014-15", "Percent 2014-15",
                     "Num 2015-16", "Total 2015-16", "Percent 2015-16",
                     "Num 2016-17", "Total 2016-17", "Percent 2016-17",
                     "Num 2017-18", "Total 2017-18", "Percent 2017-18")
colnames(disciplines) <- disciplines_names


# ..............

# set up for plotting

disciplines_long$percNumber = as.numeric(as.character(
  gsub("%", "", disciplines_long$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
disciplines_long$adjScore = ifelse(disciplines_long$Disciplines %in% c(3,4),
                                   disciplines_long$percNumber*-1,
                                   disciplines_long$percNumber)

# Add labels
disciplines_long$CSCcat =  recode(disciplines_long$Disciplines, 
                                  "1" = "Excelling",
                                  "2" = "Strong",
                                  "3" = "Developing",
                                  "4" = "Emerging")
# factor score
disciplines_long$Disciplines = as.factor(as.character(disciplines_long$Disciplines))

# reorder negative categories to ensure they display in the correct order 
cscColorsNegBar = cscColors[c(1,2,4,3)]
disciplines_long$DisciplinesReorder = 
  fct_relevel(disciplines_long$Disciplines, c("1","2","4","3"))

# .....

# Percentages are a little misleading  with so few reporting in early years
# Section out the total school numbers to provide context
disciplines_total = disciplines_long[,c("SchoolYears", "N")]
disciplines_total$dupeCheck = paste(disciplines_total[,1], 
                                    disciplines_total[,2], sep = "_")
disciplines_total$duplicated = duplicated(disciplines_total$dupeCheck)
disciplines_total = disciplines_total[disciplines_total$duplicated == FALSE,]
disciplines_total$Nlab = paste("Total:", disciplines_total$N, sep = " ")

# add height of each label (y val) = 1 + 2 percentages
disciplines_total$height = 0
for (i in 1:nrow(disciplines_total)) {
  # rows applying to that year and network group
  relRows = disciplines_long[disciplines_long$SchoolYears == 
                               disciplines_total$SchoolYears[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Disciplines %in% c(1,2),]$percNumber)
  # Apply to disciplines_total
  disciplines_total$height[i] = relSum
}

# .....

# plot
disciplines_plot = ggplot(disciplines_long,
                          aes(x = SchoolYears, y = adjScore,
                              fill = DisciplinesReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar,
                    labels = cscLabels[1:4]) +
  sota_theme_bar_leg +
  xlab("") + ylab("") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = disciplines_total, vjust = -0.2, size = 4,
            color = "black")  +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4]))) 

# .........................................................................

# Table 29: HS Disciplines & Depth, district-run

disciplines_other <- aggregate(rubric_list_complete_hs$SchoolId,
                               by = list(rubric_list_complete_hs$SchoolYears, rubric_list_complete_hs$Group, rubric_list_complete_hs$Disciplines),
                               FUN = NROW)

xref_disciplines_other <- aggregate(disciplines_other$x,
                                    by = list(disciplines_other$Group.1, 
                                              disciplines_other$Group.2),
                                    FUN = sum)

disciplines_other <- merge(disciplines_other, xref_disciplines_other,
                           by = c("Group.1", "Group.2"))
rm(xref_disciplines_other)

disciplines_other$Percent <- paste(round((disciplines_other$x.x / 
                                            disciplines_other$x.y)*100,
                                         digits = 0), "%", sep = "")

disciplines_other_name <- list("SchoolYears", "NetworkGroup", "Disciplines", "Num", "N", "Percent")
colnames(disciplines_other) <- disciplines_other_name
rm(disciplines_other_name)

disciplines_districtrun <- disciplines_other[disciplines_other$NetworkGroup ==
                                               "District-Run",]
disciplines_districtrun <- disciplines_districtrun[,-2]
disciplines_districtrun <- reshape(disciplines_districtrun,
                               timevar = "SchoolYears",
                               idvar = c("Disciplines"),
                               direction = "wide")
colnames(disciplines_districtrun) <- disciplines_names
disciplines_districtrun <- arrange(disciplines_districtrun, `Disciplines`)

# .............

# Table 30: HS Disciplines & Depth, charter/contract
disciplines_charter <- disciplines_other[disciplines_other$NetworkGroup == 
                                           "Charter & Contract",]
disciplines_charter <- disciplines_charter[,-2]
disciplines_charter <- reshape(disciplines_charter,
                               timevar = "SchoolYears",
                               idvar = c("Disciplines"),
                               direction = "wide")
colnames(disciplines_charter) <- disciplines_names
disciplines_charter <- arrange(disciplines_charter, `Disciplines`)

# .......

# Table 31: HS Disciplines & Depth, other
disciplines_options <- disciplines_other[disciplines_other$NetworkGroup == 
                                           "Other",]
disciplines_options <- disciplines_options[,-2]
disciplines_options <- reshape(disciplines_options,
                               timevar = "SchoolYears",
                               idvar = c("Disciplines"),
                               direction = "wide")
# no data from 12-13; need to insert na columns to get colnames right
disciplines_options = add_column(disciplines_options, c = NA,
           .after = "Disciplines")
disciplines_options = add_column(disciplines_options, b = NA,
                                 .after = "Disciplines")
disciplines_options = add_column(disciplines_options, a = NA,
                                 .after = "Disciplines")
colnames(disciplines_options) <- disciplines_names
disciplines_options <- arrange(disciplines_options, `Disciplines`)

# .............

# Set up for plotting by network group

disciplines_other$percNumber = as.numeric(as.character(
  gsub("%", "", disciplines_other$Percent)))

# adjust percentages - scores > 2 will be negative to center 0 at the 2/3 boundary
disciplines_other$adjScore = ifelse(disciplines_other$Disciplines %in% c(3,4),
                                    disciplines_other$percNumber*-1,
                                    disciplines_other$percNumber)

# Add labels
disciplines_other$CSCcat =  recode(disciplines_other$Disciplines, 
                                   "1" = "Excelling",
                                   "2" = "Strong",
                                   "3" = "Developing",
                                   "4" = "Emerging")
# factor score
disciplines_other$Disciplines = as.factor(as.character(disciplines_other$Disciplines))

# reorder negative categories to ensure they display in the correct order 
# Note: the legend itself won't appear in the correct order this way
cscColorsNegBar = cscColors[c(1,2,4,3)]
disciplines_other$DisciplinesReorder = fct_relevel(disciplines_other$Disciplines,
                                                   c("1","2","4","3"))

# Reorder and recode network groups
disciplines_other$NetworkGroup = fct_relevel(disciplines_other$NetworkGroup,
                                              c("District-Run", 
                                                "Charter & Contract", "Other"))
disciplines_other$NetworkGroup = recode(disciplines_other$NetworkGroup, 
                            "Other" = "Options")

# ....

# Percentages are a little misleading  with so few reporting in early years
# Section out the total school numbers to provide context
disciplines_other_total = disciplines_other[,c("SchoolYears", "NetworkGroup", "N")]
disciplines_other_total$dupeCheck = paste(disciplines_other_total[,1], disciplines_other_total[,2],
                                          disciplines_other_total[,3], sep = "_")
disciplines_other_total$duplicated = duplicated(disciplines_other_total$dupeCheck)
disciplines_other_total = disciplines_other_total[disciplines_other_total$duplicated == FALSE,]
disciplines_other_total$Nlab = paste("Total:", disciplines_other_total$N, sep = " ")

# add height of each label (y val) = 1 + 2 percentages
disciplines_other_total$height = 0
for (i in 1:nrow(disciplines_other_total)) {
  # rows applying to that year and network group
  relRows = disciplines_other[disciplines_other$SchoolYears == 
                                     disciplines_other_total$SchoolYears[i] &
                                     disciplines_other$NetworkGroup ==
                                     disciplines_other_total$NetworkGroup[i],]
  # sum the percentage of schools in category 1 or 2
  relSum = sum(relRows[relRows$Disciplines %in% c(1,2),]$percNumber)
  # Apply to disciplines_other_total
  disciplines_other_total$height[i] = relSum
}


# ........


# plot
disciplines_dcco_plot = ggplot(disciplines_other,
                               aes(x = SchoolYears, y = adjScore, 
                                   fill = DisciplinesReorder)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cscColorsNegBar,
                    labels = cscLabels[1:4]) +
  sota_theme_bar_leg +
  #ylim(0, 60) + # make sure  labels don't get cut off
  xlab("") + ylab("") +
  # Percentages above each stacked segment
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~NetworkGroup, ncol = 3) +
  scale_y_continuous(limits = c(NA, max(disciplines_other$adjScore + 30))) +
  # Number of schools
  geom_text(aes(label = Nlab, x = SchoolYears, y = height, fill = NULL), 
            data = disciplines_other_total, vjust = -0.2, size = 3,
            color = "black")  +
  guides(fill = guide_legend(override.aes = list(fill = cscColors[1:4])))

### Section 3 (cont): Additional Arts Assets (tables) -----------------

# Table 32: Does the school have a dedicated budget for the arts? All schools
budget <- aggregate(rubric_list_complete$SchoolId,
                    by = list(rubric_list_complete$SchoolYears, 
                              rubric_list_complete$HasBudget),
                    FUN = NROW)

xref_budget <- aggregate(budget$x,
                         by = list(budget$Group.1),
                         FUN = sum)
budget <- merge(budget, xref_budget, by = c("Group.1"))
rm(xref_budget)

budget$Percent <- paste(round((budget$x.x / budget$x.y)*100, digits = 0), 
                        "%", sep = "")
budget_name <- list("School Year", "Has Budget?", "Num", "N", "Percent")
colnames(budget) <- budget_name
rm(budget_name)

budget <- arrange(budget, desc(`School Year`), desc(`Has Budget?`))

# .....

# Table 33: Does the school have a dedicated budget for the arts? Elementary schools
budget_es <- aggregate(rubric_list_complete_es$SchoolId,
                       by = list(rubric_list_complete_es$SchoolYears, 
                                 rubric_list_complete_es$HasBudget),
                       FUN = NROW)

xref_budget_es <- aggregate(budget_es$x,
                            by = list(budget_es$Group.1),
                            FUN = sum)
budget_es <- merge(budget_es, xref_budget_es, by = c("Group.1"))
rm(xref_budget_es)

budget_es$Percent <- paste(round((budget_es$x.x / budget_es$x.y)*100,
                                 digits = 0), "%", sep = "")
budget_es_name <- list("School Year", "Has Budget?", "Num", "N", "Percent")
colnames(budget_es) <- budget_es_name
rm(budget_es_name)

budget_es <- arrange(budget_es, desc(`School Year`), desc(`Has Budget?`))

# .....

# Table 34: Does the school have a dedicated budget for the arts? High schools

budget_hs <- aggregate(rubric_list_complete_hs$SchoolId,
                       by = list(rubric_list_complete_hs$SchoolYears, 
                                 rubric_list_complete_hs$HasBudget),
                       FUN = NROW)

xref_budget_hs <- aggregate(budget_hs$x,
                            by = list(budget_hs$Group.1),
                            FUN = sum)
budget_hs <- merge(budget_hs, xref_budget_hs, by = c("Group.1"))
rm(xref_budget_hs)
budget_hs$Percent <- paste(round((budget_hs$x.x / budget_hs$x.y)*100, 
                                 digits = 0), "%", sep = "")

budget_hs_name <- list("School Year", "Has Budget?", "Num", "N", "Percent")
colnames(budget_hs) <- budget_hs_name
rm(budget_hs_name)

budget_hs <- arrange(budget_hs, desc(`School Year`), desc(`Has Budget?`))

# .......................................................................


# Table 35: Does the school use arts integration strategies? All schools
artsint <- aggregate(rubric_list_complete$SchoolId,
                    by = list(rubric_list_complete$SchoolYears, rubric_list_complete$HasIntegration),
                    FUN = NROW)
xref_artsint <- aggregate(artsint$x,
                         by = list(artsint$Group.1),
                         FUN = sum)
artsint <- merge(artsint, xref_artsint, by = c("Group.1"))
rm(xref_artsint)
artsint$Percent <- paste(round((artsint$x.x / artsint$x.y)*100, digits = 0), "%", sep = "")
artsint_name <- list("School Year", "Has Arts Integration?", "Num", "N", "Percent")
colnames(artsint) <- artsint_name
rm(artsint_name)
artsint <- arrange(artsint, desc(`School Year`), desc(`Has Arts Integration?`))

# .......

# Table 36: Does the school use arts integration strategies? Elementary schools

artsint_es <- aggregate(rubric_list_complete_es$SchoolId,
                     by = list(rubric_list_complete_es$SchoolYears, 
                               rubric_list_complete_es$HasIntegration),
                     FUN = NROW)
xref_artsint_es <- aggregate(artsint_es$x,
                             by = list(artsint_es$Group.1),
                             FUN = sum)
artsint_es <- merge(artsint_es, xref_artsint_es, by = c("Group.1"))
rm(xref_artsint_es)
artsint_es$Percent <- paste(round((artsint_es$x.x / artsint_es$x.y)*100, digits = 0), "%", sep = "")
artsint_name_es <- list("School Year", "Has Arts Integration?", "Num", "N", "Percent")
colnames(artsint_es) <- artsint_name_es
rm(artsint_name_es)

artsint_es <- arrange(artsint_es, desc(`School Year`), 
                      desc(`Has Arts Integration?`))

# .....

# Table 37: Does the school use arts integration strategies? High schools

artsint_hs <- aggregate(rubric_list_complete_hs$SchoolId,
                        by = list(rubric_list_complete_hs$SchoolYears, 
                                  rubric_list_complete_hs$HasIntegration),
                        FUN = NROW)
xref_artsint_hs <- aggregate(artsint_hs$x,
                             by = list(artsint_hs$Group.1),
                             FUN = sum)
artsint_hs <- merge(artsint_hs, xref_artsint_hs, by = c("Group.1"))
rm(xref_artsint_hs)
artsint_hs$Percent <- paste(round((artsint_hs$x.x / artsint_hs$x.y)*100, digits = 0), "%", sep = "")

artsint_name_hs <- list("School Year", "Has Arts Integration?", "Num", "N", "Percent")
colnames(artsint_hs) <- artsint_name_hs
rm(artsint_name_hs)

artsint_hs <- arrange(artsint_hs, desc(`School Year`), 
                      desc(`Has Arts Integration?`))

# ......................................................................

# Table 38: Do teachers at the school have access to arts-specific professional development? All schools

profdev <- aggregate(rubric_list_complete$SchoolId,
                     by = list(rubric_list_complete$SchoolYears, rubric_list_complete$HasProfDev),
                     FUN = NROW)
xref_profdev <- aggregate(profdev$x,
                          by = list(profdev$Group.1),
                          FUN = sum)
profdev <- merge(profdev, xref_profdev, by = c("Group.1"))
rm(xref_profdev)
profdev$Percent <- paste(round((profdev$x.x / profdev$x.y)*100, digits = 0), "%", sep = "")
profdev_name <- list("School Year", "Has Professional Development?", "Num", "N", "Percent")
colnames(profdev) <- profdev_name
rm(profdev_name)
profdev <- arrange(profdev, desc(`School Year`), desc(`Has Professional Development?`))

# ..........

# Table 39: Do teachers at the school have access to arts-specific professional development? Elementary schools
profdev_es <- aggregate(rubric_list_complete_es$SchoolId,
                     by = list(rubric_list_complete_es$SchoolYears, rubric_list_complete_es$HasProfDev),
                     FUN = NROW)
xref_profdev_es <- aggregate(profdev_es$x,
                          by = list(profdev_es$Group.1),
                          FUN = sum)
profdev_es <- merge(profdev_es, xref_profdev_es, by = c("Group.1"))
rm(xref_profdev_es)
profdev_es$Percent <- paste(round((profdev_es$x.x / profdev_es$x.y)*100, digits = 0), "%", sep = "")
profdev_name_es <- list("School Year", "Has Professional Development?", "Num", "N", "Percent")
colnames(profdev_es) <- profdev_name_es
rm(profdev_name_es)
profdev_es <- arrange(profdev_es, desc(`School Year`), desc(`Has Professional Development?`))

# ..........

# Table 40: Do teachers at the school have access to arts-specific professional development? High schools
profdev_hs <- aggregate(rubric_list_complete_hs$SchoolId,
                        by = list(rubric_list_complete_hs$SchoolYears, rubric_list_complete_hs$HasProfDev),
                        FUN = NROW)
xref_profdev_hs <- aggregate(profdev_hs$x,
                             by = list(profdev_hs$Group.1),
                             FUN = sum)
profdev_hs <- merge(profdev_hs, xref_profdev_hs, by = c("Group.1"))
rm(xref_profdev_hs)
profdev_hs$Percent <- paste(round((profdev_hs$x.x / profdev_hs$x.y)*100, digits = 0), "%", sep = "")
profdev_name_hs <- list("School Year", "Has Professional Development?", "Num", "N", "Percent")
colnames(profdev_hs) <- profdev_name_hs
rm(profdev_name_hs)
profdev_hs <- arrange(profdev_hs, desc(`School Year`), desc(`Has Professional Development?`))

# .......................................................................

# Table 41: Does the school have at least one partnership with an external arts partner? All schools
part <- aggregate(rubric_list_complete$SchoolId,
                     by = list(rubric_list_complete$SchoolYears, rubric_list_complete$HasPartners),
                     FUN = NROW)
xref_part <- aggregate(part$x,
                          by = list(part$Group.1),
                          FUN = sum)
part <- merge(part, xref_part, by = c("Group.1"))
rm(xref_part)
part$Percent <- paste(round((part$x.x / part$x.y)*100, digits = 0), "%", sep = "")
part_name <- list("School Year", "Has Partners?", "Num", "N", "Percent")
colnames(part) <- part_name
rm(part_name)
part <- arrange(part, desc(`School Year`), desc(`Has Partners?`))

# ...........

# Table 42: Does the school have at least one partnership with an external arts partner? Elementary schools
part_es <- aggregate(rubric_list_complete_es$SchoolId,
                     by = list(rubric_list_complete_es$SchoolYears, rubric_list_complete_es$HasPartners),
                     FUN = NROW)
xref_part_es <- aggregate(part_es$x,
                          by = list(part_es$Group.1),
                          FUN = sum)
part_es <- merge(part_es, xref_part_es, by = c("Group.1"))
rm(xref_part_es)
part_es$Percent <- paste(round((part_es$x.x / part_es$x.y)*100, digits = 0), "%", sep = "")
part_name_es <- list("School Year", "Has Partners?", "Num", "N", "Percent")
colnames(part_es) <- part_name_es
rm(part_name_es)
part_es <- arrange(part_es, desc(`School Year`), desc(`Has Partners?`))

# .........

# Table 43: Does the school have at least one partnership with an external arts partner? High schools
part_hs <- aggregate(rubric_list_complete_hs$SchoolId,
                     by = list(rubric_list_complete_hs$SchoolYears, rubric_list_complete_hs$HasPartners),
                     FUN = NROW)
xref_part_hs <- aggregate(part_hs$x,
                          by = list(part_hs$Group.1),
                          FUN = sum)
part_hs <- merge(part_hs, xref_part_hs, by = c("Group.1"))
rm(xref_part_hs)
part_hs$Percent <- paste(round((part_hs$x.x / part_hs$x.y)*100, digits = 0), "%", sep = "")
part_name_hs <- list("School Year", "Has Partners?", "Num", "N", "Percent")
colnames(part_hs) <- part_name_hs
rm(part_name_hs)
part_hs <- arrange(part_hs, desc(`School Year`), desc(`Has Partners?`))

# ........................................................................

# Table 44: Does the school have opportunities for parent and community engagement in the arts? All schools
eng <- aggregate(rubric_list_complete$SchoolId,
                  by = list(rubric_list_complete$SchoolYears, rubric_list_complete$HasEngagement),
                  FUN = NROW)
xref_eng <- aggregate(eng$x,
                       by = list(eng$Group.1),
                       FUN = sum)
eng <- merge(eng, xref_eng, by = c("Group.1"))
rm(xref_eng)
eng$Percent <- paste(round((eng$x.x / eng$x.y)*100, digits = 0), "%", sep = "")
eng_name <- list("School Year", "Has Engagement?", "Num", "N", "Percent")
colnames(eng) <- eng_name
rm(eng_name)
eng <- arrange(eng, desc(`School Year`), desc(`Has Engagement?`))

# .........

# Table 45: Does the school have opportunities for parent and community engagement in the arts? Elementary schools
eng_es <- aggregate(rubric_list_complete_es$SchoolId,
                    by = list(rubric_list_complete_es$SchoolYears, rubric_list_complete_es$HasEngagement),
                    FUN = NROW)
xref_eng_es <- aggregate(eng_es$x,
                         by = list(eng_es$Group.1),
                         FUN = sum)
eng_es <- merge(eng_es, xref_eng_es, by = c("Group.1"))
rm(xref_eng_es)
eng_es$Percent <- paste(round((eng_es$x.x / eng_es$x.y)*100, digits = 0), "%", sep = "")
eng_name_es <- list("School Year", "Has Engagement?", "Num", "N", "Percent")
colnames(eng_es) <- eng_name_es
rm(eng_name_es)
eng_es <- arrange(eng_es, desc(`School Year`), desc(`Has Engagement?`))

# ......

# Table 46: Does the school have opportunities for parent and community engagement in the arts? High schools
eng_hs <- aggregate(rubric_list_complete_hs$SchoolId,
                    by = list(rubric_list_complete_hs$SchoolYears, rubric_list_complete_hs$HasEngagement),
                    FUN = NROW)
xref_eng_hs <- aggregate(eng_hs$x,
                         by = list(eng_hs$Group.1),
                         FUN = sum)
eng_hs <- merge(eng_hs, xref_eng_hs, by = c("Group.1"))
rm(xref_eng_hs)
eng_hs$Percent <- paste(round((eng_hs$x.x / eng_hs$x.y)*100, digits = 0), "%", sep = "")
eng_name_hs <- list("School Year", "Has Engagement?", "Num", "N", "Percent")
colnames(eng_hs) <- eng_name_hs
rm(eng_name_hs)
eng_hs <- arrange(eng_hs, desc(`School Year`), desc(`Has Engagement?`))

### Section 3 (cont): Additional Arts Assets (plots) ---------------------------- 

# Add column to each table identifying the asset
budget$asset = "Budget"
budget_es$asset = "Budget"
budget_hs$asset = "Budget"

artsint$asset = "Arts Integration"
artsint_es$asset = "Arts Integration"
artsint_hs$asset = "Arts Integration"

profdev$asset = "Prof. Development"
profdev_es$asset = "Prof. Development"
profdev_hs$asset = "Prof. Development"

part$asset = "Partnerships"
part_es$asset = "Partnerships"
part_hs$asset = "Partnerships"

eng$asset = "Comm. Engagement"
eng_es$asset = "Comm. Engagement"
eng_hs$asset = "Comm. Engagement"

# Add column to each table identifying type (ES, HS, or all)
budget$type = "All"
budget_es$type = "Elementary Schools"
budget_hs$type = "High Schools"

artsint$type = "All"
artsint_es$type = "Elementary Schools"
artsint_hs$type = "High Schools"

profdev$type = "All"
profdev_es$type = "Elementary Schools"
profdev_hs$type = "High Schools"

part$type = "All"
part_es$type = "Elementary Schools"
part_hs$type = "High Schools"

eng$type = "All"
eng_es$type = "Elementary Schools"
eng_hs$type = "High Schools"

# Bind them all up
asset_tab = rbindlist(list(budget, budget_es, budget_hs,
                  artsint, artsint_es, artsint_hs,
                  profdev, profdev_es, profdev_hs,
                  part, part_es, part_hs,
                  eng, eng_es, eng_hs))
colnames(asset_tab) = c("SchoolYear", "HasAsset", "Num", "N",
                        "Percent", "Asset", "Type")

# Create numeric version of percent column
asset_tab$percNumber = as.numeric(as.character(gsub("%", "", asset_tab$Percent)))

# Create label version of percent where missing asset % is empty
asset_tab$percLab = ifelse(asset_tab$HasAsset == 1, asset_tab$Percent, "")

# Convert HasAsset to factor
asset_tab$HasAsset = as.factor(as.character(asset_tab$HasAsset))

# ..........................

# All assets by year, pie

# weird hack to avoid scaling issue with faceting
# https://github.com/tidyverse/ggplot2/issues/2815
cp <- coord_polar(theta = "y", start = 0)
cp$is_free <- function() TRUE

asset_all_pie = ggplot(asset_tab[asset_tab$Type == "All",],
       aes(x = SchoolYear, y = percNumber, fill = HasAsset)) +
  geom_bar(width = 1, stat = "identity") +
  cp +
  geom_text(aes(label = percLab), x = 0, y = -1,
            color = pastYearCol, size = pieLabSize) +
  sota_theme_pie +
  scale_fill_manual(values = c(greyCol, currentYearCol)) +
  facet_grid(rows = vars(Asset), cols = vars(SchoolYear), scales = "free")

# ............................

# All assets by year, bar

asset_all_bar = ggplot(asset_tab[asset_tab$Type == "All" &
                                   asset_tab$HasAsset == 1,],
                       aes(x = SchoolYear, y = percNumber, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  sota_theme_bar +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  # Add labels on top of bars
  geom_text(aes(label = Percent, color = SchoolYear), size = 4, vjust=-0.3) +
  scale_color_manual(values = c(rep(pastYearCol,5), currentYearCol)) +
  facet_wrap(~Asset, ncol = 5) +
  ylim(0, 105) +
  xlab("School Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank())

# ..............................

# All assets by year, ES/HS split, bar

asset_es_hs_bar = ggplot(asset_tab[asset_tab$Type != "All" &
                                   asset_tab$HasAsset == 1,],
                       aes(x = SchoolYear, y = percNumber, fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  sota_theme_bar +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  # Add labels on top of bars
  geom_text(aes(label = Percent, color = SchoolYear), size = 4, vjust=-0.3) +
  scale_color_manual(values = c(rep(pastYearCol,5), currentYearCol)) +
  facet_grid(cols = vars(Asset), rows = vars(Type)) +
  ylim(0, 105) +
  xlab("School Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank())

### Section 4: Partnerships -------------------------------------------

# Table 47: Partner Count

partner_count <- aggregate(partner.active$Id,
                           by = list(partner.active$SchoolYears),
                           FUN = NROW)
partner_count_names <- c("School Year", "Number of Partners")
colnames(partner_count) <- partner_count_names
partner_count <- arrange(partner_count, `School Year`)
partner_count <- partner_count[-c(1:9),]

partner_count_plot_df = partner_count
colnames(partner_count_plot_df) = c("SchoolYear", "Count")

partner_count_bar = ggplot(partner_count_plot_df,
                       aes(x = SchoolYear, y = Count,
                           fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  sota_theme_bar +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  ylim(0, max(partner_count_plot_df$Count) + 50) +
  xlab("School Year") +
  # Add labels on top of bars
  geom_text(aes(label = Count, color = SchoolYear), size = 8, vjust=-0.2) +
  scale_color_manual(values = c(rep(pastYearCol,5), currentYearCol)) 


# .................................................................

# Table 48: Average number of partnerships per school

partner_mean <- aggregate(rubric_list_complete$NumPartners,
                          by = list(rubric_list_complete$SchoolYears),
                          FUN = mean)
partner_mean_names <- c("SchoolYear", "Average Number of Partners per School")
colnames(partner_mean) <- partner_mean_names
rm(partner_mean_names)
partner_mean[,2] = round(partner_mean[,2], 2)

partner_mean_plot_df = partner_mean
colnames(partner_mean_plot_df)[2] = "AvPart"

partner_mean_bar = ggplot(partner_mean_plot_df,
                            aes(x = SchoolYear, y = AvPart, 
                                fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  sota_theme_bar +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  ylim(0, max(partner_mean_plot_df$AvPart) +
         max(partner_mean_plot_df$AvPart)*.2) +
  xlab("School Year") +
  # Add labels on top of bars
  geom_text(aes(label = AvPart, color = SchoolYear),
            size = topTextSingleSize, vjust=-0.2) +
  scale_color_manual(values = c(rep(pastYearCol,5), currentYearCol)) 

# .....................................................................

# Table 48B: Active partners per school, by CPS Network
# First, use xref_comp_net_type to get # schools in each network each school year
xref_comp_net_type <- arrange(xref_comp_net_type, Group.2, Group.1)
xref_comp_net_type <- xref_comp_net_type[-c(1:2),]

# Next, aggregate active partners by CPS Network
partner_networks <- aggregate(partner.distinctparts$OrgName,
                        by = list(partner.distinctparts$SchoolYears,
                        #parnter.distinctparts$CpsNewNetwork
                        partner.distinctparts$Network),
                                   FUN = NROW)

partner_networks = partner_networks[!partner_networks$Group.1
                                    %in% c("2011-12", "2010-11", "2009-10", 
                                           "2008-09", "2007-08", "2006-07",
                                           "2005-06", "2004-05", "2003-04"),]

partner_networks <- arrange(partner_networks, Group.2, Group.1)

# Merge data frames
partner_networks <- merge(partner_networks, xref_comp_net_type, 
                          by = c("Group.1", "Group.2"))
partner_networks_names <- c("School Year", "Network", 
                            "Num Partners", "Num Schools")
colnames(partner_networks) <- partner_networks_names

# Reorder networks
numNets = paste("Network", 1:17)
partner_networks$Network = fct_relevel(partner_networks$Network,
									numNets[1:13], "Charter ES",
									numNets[14:17], "Charter HS", "Contract", "Options",
                                    "ISP", "AUSL")
			


# Calculate partners per school
partner_networks$`Partners per School` <- 
  round(partner_networks$`Num Partners` / partner_networks$`Num Schools`, 
        digits = 1)
partner_networks <- arrange(partner_networks, Network, `School Year`)

partner_net_plot_df = partner_networks
colnames(partner_net_plot_df) = c("SchoolYear", "Network", "numPart", 
                               "numSchool", "partPerSchool")

# Plot
partner_net_bar = ggplot(partner_net_plot_df,
                          aes(x = SchoolYear, y = partPerSchool, 
                              fill = SchoolYear)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Network, ncol = 4, scales = "free") +
  sota_theme_bar +
  scale_fill_manual(values = c(rep(pastYearCol, 5), currentYearCol)) +
  ylim(0, max(partner_net_plot_df$partPerSchool) +
         max(partner_net_plot_df$partPerSchool)*.25) +
  xlab("School Year") +
  # Add labels on top of bars
  geom_text(aes(label = partPerSchool, color = SchoolYear), 
            size = topTextSingleSize-1, vjust=-0.3) +
  scale_color_manual(values = c(rep(pastYearCol,5), currentYearCol)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# .........................................................

# Table 49: Arts program types, over the years

program_types <- aggregate(partner.programs$ContentType,
                           by = list(partner.programs$SchoolYears,
                                     partner.programs$ContentType),
                           FUN = NROW)

program_types = program_types[!program_types$Group.1
                                    %in% c("2011-12", "2010-11", "2009-10", 
                                           "2008-09", "2007-08", "2006-07",
                                           "2005-06", "2004-05", "2003-04"),]

xref_program_types <- aggregate(program_types$x,
                                    by = list(program_types$Group.1),
                                    FUN = sum)
program_types <- merge(program_types, xref_program_types, by = "Group.1")
rm(xref_program_types)

program_types$Percent <- paste(round((
  program_types$x.x / program_types$x.y)*100, digits = 1), "%", sep = "")

program_types_names <- list("SchoolYear", "Program Type", "Num", "N", "Percent")
colnames(program_types) <- program_types_names
rm(program_types_names)

program_types <- arrange(program_types, desc(SchoolYear), desc(Num))

# .....


# Plot-friendly df
program_types_plot_df = program_types
colnames(program_types_plot_df)[2] = "ProgramType"
program_types_plot_df$percNumber = as.numeric(as.character(
  gsub("%", "", program_types_plot_df$Percent)))

# Recode programming types
program_types_plot_df$progType = recode(program_types_plot_df$ProgramType, 
                            "FT" = "Field Trip",
                            "RESO" = "Resources",
                            "OST" = "Out-of-School Time",
                            "ISP" = "In-School Programming",
                            "RESI" = "Residencies",
                            "PD" = "Professional Development",
                            "O" = "Other")

# Plot 
program_types_bar = ggplot(program_types_plot_df,
                          aes(x = SchoolYear, y = percNumber, 
                              fill = progType)) +
  geom_bar(stat = "identity") +
  sota_theme_bar_leg +
  scale_fill_manual(values = progColors, name = "Programming Type") +
  xlab("School Year") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5))

# .............................................................................

# Table 50: Arts partner disciplines

disciplines_names <- c("Dance", "Media Arts", "Music",
                       "Theater", "Visual Arts", "Literary Arts", "Other")
part_disc_subset <- subset(partner.disc,
                           partner.disc$DisciplineType %in% disciplines_names)
rm(disciplines_names)

# # Standardize disciplines
# # Remove everything after a dash
# partner.disc$orgDisc = gsub("-.*", "", partner.disc$DisciplineType, fixed = FALSE)
# # Standardize label and capitalization
# partner.disc[partner.disc$orgDisc == "music",]$orgDisc = "Music"
# partner.disc[partner.disc$orgDisc == "theater",]$orgDisc = "Theater"
# partner.disc[partner.disc$orgDisc == "dance",]$orgDisc = "Dance"
# partner.disc[partner.disc$orgDisc == "visual",]$orgDisc = "Visual Arts"
# partner.disc[partner.disc$orgDisc == "media",]$orgDisc = "Media Arts"

disc_agg <- aggregate(part_disc_subset$Id,
                      by = list(part_disc_subset$SchoolYears, 
                                part_disc_subset$DisciplineType),
                      FUN = NROW)

# This line of code directly below does not work because we do not have arts discipline information for 2012-13 in the OrganizationDiscipline table
active_1 <- nrow(xref <- subset(partner.disc, partner.disc$SchoolYears == "2012-13") %>% distinct(xref, Name))
active_2 <- nrow(xref <- subset(partner.disc, partner.disc$SchoolYears == "2013-14") %>% distinct(xref, Name))
active_3 <- nrow(xref <- subset(partner.disc, partner.disc$SchoolYears == "2014-15") %>% distinct(xref, Name))
active_4 <- nrow(xref <- subset(partner.disc, partner.disc$SchoolYears == "2015-16") %>% distinct(xref, Name))
active_5 <- nrow(xref <- subset(partner.disc, partner.disc$SchoolYears == "2016-17") %>% distinct(xref, Name))
active_6 <- nrow(xref <- subset(partner.disc, partner.disc$SchoolYears == "2017-18") %>% distinct(xref, Name))

disc_agg$N <- NA
disc_agg$N[which(disc_agg$Group.1 == "2013-14")] <- active_2
disc_agg$N[which(disc_agg$Group.1 == "2014-15")] <- active_3
disc_agg$N[which(disc_agg$Group.1 == "2015-16")] <- active_4
disc_agg$N[which(disc_agg$Group.1 == "2016-17")] <- active_5
disc_agg$N[which(disc_agg$Group.1 == "2017-18")] <- active_6

disc_agg$Percent <- paste(round((disc_agg$x / disc_agg$N)*100, digits = 0),
                          "%", sep = "")
disc_agg_name <- list("School Year", "Arts Discipline", "Num", "N", "Percent")
colnames(disc_agg) <- disc_agg_name
rm(disc_agg_name)
disc_agg <- arrange(disc_agg, desc(`School Year`), `Arts Discipline`)

# ....

# Prep for plotting
disc_agg_plot_df = disc_agg
colnames(disc_agg_plot_df)[1:2] = c("SchoolYear", "Discipline")
disc_agg_plot_df$percNumber = as.numeric(as.character(
  gsub("%", "", disc_agg_plot_df$Percent)))

# Order disciplines by their popularity in 2017-18
disc78 = disc_agg_plot_df[disc_agg_plot_df$SchoolYear == "2017-18",]
disc78 <- disc78 %>%
  arrange(percNumber) %>%               # sort your dataframe
  mutate(Discipline = factor(Discipline, unique(Discipline))) 
discOrder = rev(levels(disc78$Discipline))
disc_agg_plot_df$Discipline = fct_relevel(disc_agg_plot_df$Discipline, 
                                          discOrder)

# Plot 
partner_disc_bar = ggplot(disc_agg_plot_df,
                           aes(x = SchoolYear, y = percNumber, 
                               fill = Discipline)) +
  geom_bar(stat = "identity") +
  sota_theme_bar_leg +
  scale_fill_manual(values = progColors, name = "Arts Discipline") +
  xlab("School Year") +
  geom_text(aes(label = Percent),
            color = "white",
            position = position_stack(vjust = 0.5))

# Alternate plot with counts
partner_disc_count_bar = ggplot(disc_agg_plot_df,
                          aes(x = SchoolYear, y = Num, 
                              fill = Discipline)) +
  geom_bar(stat = "identity") +
  sota_theme_bar_leg +
  scale_fill_manual(values = progColors, name = "Arts Discipline") +
  xlab("School Year") +
  geom_text(aes(label = Num),
            color = "white",
            position = position_stack(vjust = 0.5))

### Section 4 (cont): Partnerships by geography (TBD) ----------------

# !!!
# Talk to Nicole about classification of programs into high/med/low impact
# !!!

### Section 4 (cont): Partnerships meeting 16-17 needs (TBD!!!) -------------