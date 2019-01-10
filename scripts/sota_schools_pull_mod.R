# Modified somewhat from past iterations to 
# (1) capture updated 17-18 staffing data, and
# (2) handle a miscategorization of 2012-13 HS/ES data.

source("I:/Emily/SOTA/draftStats/scripts/db_connection.R")

# Setup and run database connection
library(tidyverse)
library(data.table)
library(openxlsx)

# I. Pull school rubric for each school year
r <- dbGetQuery(myconn, "SELECT s.Id, s.UnitName, s.Nickname, osr.*, u.SchoolType, s.CommunityArea, s.CpsNewNetwork, s.Latitude, s.Longitude
                FROM School s
                INNER JOIN OfficialSchoolRubric osr ON s.Id = osr.SchoolId
                INNER JOIN [User] u ON osr.SchoolId = u.SchoolId AND osr.SchoolYears = u.SchoolYears
                WHERE u.IsActive = 1")
r$SchoolType[r$SchoolType == 0] <- "None"
r$SchoolType[r$SchoolType == 1 | r$SchoolType == 3] <- "ES"
r$SchoolType[r$SchoolType == 2] <- "HS"
cols_remove <- c("CreateUserId", "CreateDate", "LastMaintUserId", "LastUpdated")
r <- r[,-which(names(r) %in% cols_remove)]
r[,c("Fte","Minutes","Access","Disciplines")][r[,c("Fte","Minutes","Access","Disciplines")]==0] <- NA

# Change TotalFTE column name to avoid redundancy/confusion with the wrong column in r6
colnames(r)[which(colnames(r) == "TotalFTE")] = "TotalFTECount"

# ..............................................

# Fix 2012-13 school type assignments 

# many HS are labeled as ES in 2012-13 data in database (should be 527 ES, 154 HS)
# (based on #s in previous SotA reports vs. output generated from DB)
# Correct these
earlyRub = r[r$SchoolYears == "2012-13",]
laterRub = r[r$SchoolYears != "2012-13",]

earlyRub$SchoolType = as.character(earlyRub$SchoolType)
for ( i in 1:nrow(earlyRub)) {
  # Find each school from 12-13 in the 13-14 list
  relSchool = earlyRub$SchoolId[i]
  nextMatch = laterRub[laterRub$SchoolYears == "2013-14" & laterRub$SchoolId == relSchool,]
  # If there is a match (i.e. if the school still existed in 13-14), apply that label
  if (nrow(nextMatch) > 0) {
    # Grab school type 
    relType = nextMatch$SchoolType[1]
    # replace schoolTYpe in earlyRub
    earlyRub$SchoolType[i] = relType
    rm(relType)
  }
  rm(relSchool, nextMatch)
}

# still 6 more to be corrected. (probably in schools that closed after 12-13)
# Helpful: I:\Data & Impact\State of the Arts Reports\2015-16\2012-13 Data Adjustments

earlyRub[earlyRub$UnitName %in% c(
  "Mason High School", "Banner North High School", "Banner Academy West",
  "Milburn Alternative High School", "Banner Academy South",
  "Vivian E. Summers Alternative High School"),]$SchoolType = "HS"

# recombine 12-13 data to the rest
r = rbind(earlyRub, laterRub)

# Note: there is one more misalignment wiht previous datasets. 
# In past SOTAs, the ES/HS split is reported as 484/180, while the database has 483/181.
# The issue seems to be the conflating  of Camelot Safe Academy HS (Id# 610585) with Camelot Safe Academy ES (ID# 610572).
# See: I:/Data & Impact/State of the Arts Reports/2015-16/Data Check/CSC 2014-15 - 664 Schools.xlsx
# In this spreadsheet, the ES is listed twice (once under each ID number), and that error may have been carried forward.
# In this case, the DB classification seems to be the accurate one, so stick with it.

# ............................................

## For SY 2015-16, CPS adopted a new way to count schools. The new method excluded certain types of schools, such as ALOPs, from the "official" list of schools.
## Therefore, for school years following SY 2015-16 we could have two lists for each year: a "full" rubric and a shortened rubric
## The full rubric is, essentially, a list of all the surveys, so it should match the number in the artlook Schools admin portal
## The shortened rubric, on the other hand, takes the full rubric and removes ALOPs, charter combo schools (whose ID starts with a '1'), 
##    and any other school types that are necessary that year; it is what is used to calculate SotA data and is here

# Save a version before we do this for FTE calculating
rAll = r
# Do some renaming
rAll <- rAll %>% select(SchoolYears,SchoolId,everything()) %>% mutate(Id=NULL,UnitName.1=NULL,Id.1=NULL)

xref_alops <- c(610555,610581,610582,610580,610566,610567,610569,610570,610571,610568,610557,610574,400173)
xref_chartercombos <- c(100115,100162,100121)
xref_districtcombos <- c(609774,610573,610515,610101,610357)
exclude <- c(610555,610581,610582,610580,610566,610567,610569,610570,610571,610557,610568,400173,400177,100115,100162,100121)

r <- r[r$SchoolYears!='2015-16' | (r$SchoolYears=='2015-16' & !r$Id %in% c(xref_chartercombos,xref_alops,xref_districtcombos)),]
r <- r[r$SchoolYears!='2016-17' | (r$SchoolYears=='2016-17' & !r$Id %in% exclude),]

r <- r %>% select(SchoolYears,SchoolId,everything()) %>% mutate(Id=NULL,UnitName.1=NULL,Id.1=NULL)

# Do some renaming
#r <- r %>% select(SchoolYears,SchoolId,everything()) %>% mutate(Id=NULL,UnitName.1=NULL,Id.1=NULL)

# 6. 2017-18 CSC
#source("I:/Emily/SOTA/draftStats/scripts/staffing_clean_2018.R") 
r6 = read.xlsx("I:/Data & Impact/Analyses/SOTA/SOTA1718/output/RubricWithChanges.xlsx") # this is the output of staffing_clean_2018.R 
r6 <- r6 %>% select(SchoolYears,SchoolId,everything()) %>% mutate(Id=NULL)#,UnitName.1=NULL,Id.1=NULL)
#r6Saver = r6

rcols <- names(r)

r6 <- r6 %>% dplyr::rename(FinalScore = realOfficialScore, Fte = realFTE, TotalFTECount = realTotalFTE) %>% select(rcols) # tweaked to select correct scores related to staffing from r6
r <- bind_rows(r,r6)

# Save a version prior to school removal
# Use this for most of the reporting (Fry may be interested in all schools their grantees work with, regardless of SOTA reporting)
r6All = r6
rAll = bind_rows(rAll, r6All)

# Re-exclude schools above from 17-18 data, plus one more (400148) based on staffing_clean_2018.R
exclude18 = c(exclude, 400148)
r <- r[r$SchoolYears!='2017-18' | (r$SchoolYears=='2017-18' & !r$SchoolId %in% exclude18),]

# ................................................................................

# II. Pull teacher data
teacher <- dbGetQuery(myconn, "SELECT SchoolId, SchoolYears, LastName, FirstName, MiddleInitial, WorkHoursType,
                                               PrimaryFocus, SecondaryFocus, PartTimeFraction
                                        FROM Teacher")
teacher$RealFraction <- ifelse(teacher$WorkHoursType == 0, teacher$PartTimeFraction, 1)
teacher <- teacher[!c(teacher$SchoolId < 100000 | teacher$SchoolId > 619999), ]  # remove test schools, district employees, and other nonsense
teacher = teacher[teacher$SchoolYears != "2017-18",] # Drop 17-18 data; not most updated

# 2017-18 staffing data (not yet in DB)
teacher6 = read.xlsx("I:/Data & Impact/Analyses/SOTA/SOTA1718/output/TeacherRosterForUpload.xlsx")
teacher6$Email = NULL
teacher6$RealFraction <- ifelse(teacher6$WorkHoursType == 0, teacher6$PartTimeFraction, 1)
teacher6 <- teacher6[!c(teacher6$SchoolId < 100000 | teacher6$SchoolId > 619999), ]  # remove test schools, district employees, and other nonsense

# Merge all staffing data
teacher = rbind(teacher, teacher6)