# ..............................................................................

# SOTA 2017-18
# Funding: calculate CPS-based funding

# ..............................................................................

### Note: for FTE inclusion logic, include: 
### I:\Data & Impact\Analyses\SOTA\SOTA1718\staffing_clean_2018.R
### which is described in I:\Data & Impact\Analyses\SOTA\SOTA1718\README.md

### For salary calculations logic, see:
### I:\Data & Impact\Data Files\SY 2016-17\CPS Data\Staffing\2016-17 Staffing Decision Documentation.doc

### Sanity check with last year's data:
### I:\Data & Impact\Analyses\[ARCHIVE] State-Of-The-Arts-in-CPS\graphics\Funding Data.xlsx

wd = "I:/Emily/SOTA/draftStats"
setwd(wd)

library(openxlsx)

### Set up data -------------------------------------------------------------------

# Pull in rubric data
setwd("scripts")
source("I:/Emily/SOTA/draftStats/scripts/sota_schools_pull_mod.R")
setwd("..")

r78 = r[r$SchoolYears == "2017-18",]
rAll78 = rAll[rAll$SchoolYears == "2017-18",] # all schools, not just the 650 in SOTA

# Read in the official teacher roster - output of staffing_clean_2018.R
ofcRoster = read.xlsx("I:/Data & Impact/Analyses/SOTA/SOTA1718/output/TeacherRoster.xlsx")

# Read in Art Kim's latest staffing data with budget info
akStaff = read.csv("I:/Data & Impact/Data Files/SY 2017-18/CPS Data/Staffing/Positions - Art - 2018-12-17 for 6-19-18 - Sheet 1.csv", head = T)

# Pull budget data from DB
budget = dbGetQuery(myconn,"SELECT SchoolId, Budget 
                              FROM SchoolYearDetails 
                              WHERE SchoolYears='2017-18'")

### Reconcile salary data ------------------------------------------------------------

# the akStaff$Annl.Sal.PosEncumb and akStaff$Annl.BenefitCost can *nearly* just be summed. But first, rows that duplicate an employee ID AND a Position # in one school must be removed. (Other repeats of employee id indicate their salary split between two rows, but duped posIDs seem to be duplicates. See William Estrada for an example of this.)

# Fix position # column name
colnames(akStaff)[which(colnames(akStaff) == "Pos..")] = "PosNum"

# Create a dupe checker column in line with the above
akStaff$dupeCheck = paste(akStaff$Schl.ID, akStaff$Empl.ID, akStaff$PosNum, sep = "_")
akStaff$duplicated = duplicated(akStaff$dupeCheck)

# Sanity check: is William Estrada in duplicated?
"Estrada" %in% akStaff[akStaff$duplicated == TRUE,]$Last.Name

# Drop the dupes 
akStaff = akStaff[akStaff$duplicated == FALSE,]

# ..........................................................

# Now we can add the salary data from akStaff to each row in ofcRoster

# note that 4 employee ids are duped in ofcRoster:
ofcRosTab = table(ofcRoster$Empl.ID)
ofcRosTab[ofcRosTab > 1]

### Sum up all data and plot ---------------------------------------------------------

# Assuming no schools excluded
TotalSchoolBased = sum(budget$Budget, na.rm = T)



pastYears = read.xlsx("I:/Data & Impact/Analyses/[ARCHIVE] State-Of-The-Arts-in-CPS/graphics/Funding Data.xlsx")
pastYears = pastYears[1:5,]