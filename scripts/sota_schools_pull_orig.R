
#rm(list=ls())

source("db_connection.R")

# Setup and run database connection
library(tidyverse)
library(data.table)

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

## For SY 2015-16, CPS adopted a new way to count schools. The new method excluded certain types of schools, such as ALOPs, from the "official" list of schools.
## Therefore, for school years following SY 2015-16 we could have two lists for each year: a "full" rubric and a shortened rubric
## The full rubric is, essentially, a list of all the surveys, so it should match the number in the artlook Schools admin portal
## The shortened rubric, on the other hand, takes the full rubric and removes ALOPs, charter combo schools (whose ID starts with a '1'), 
##    and any other school types that are necessary that year; it is what is used to calculate SotA data and is here

xref_alops <- c(610555,610581,610582,610580,610566,610567,610569,610570,610571,610568,610557,610574,400173)
xref_chartercombos <- c(100115,100162,100121)
xref_districtcombos <- c(609774,610573,610515,610101,610357)
exclude <- c(610555,610581,610582,610580,610566,610567,610569,610570,610571,610557,610568,400173,400177,100115,100162,100121)

r <- r[r$SchoolYears!='2015-16' | (r$SchoolYears=='2015-16' & !r$Id %in% c(xref_chartercombos,xref_alops,xref_districtcombos)),]
r <- r[r$SchoolYears!='2016-17' | (r$SchoolYears=='2016-17' & !r$Id %in% exclude),]
r <- r %>% select(SchoolYears,SchoolId,everything()) %>% mutate(Id=NULL,UnitName.1=NULL,Id.1=NULL)

# 6. 2017-18 CSC
#source("staffing_clean_2018.R")
rcols <- names(r)
r6 <- r6 %>% rename(FinalScore = OfficialScore, Fte = FTE) %>% select(rcols)
r <- bind_rows(r,r6)

