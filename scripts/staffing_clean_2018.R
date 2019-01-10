#rm(list = ls())
#gc()

## setup
library(readxl) # better for reading in excel b/c doesn't change var names
library(openxlsx) # better for writing to excel b/c allows for multiple sheets
library(dplyr)
library(tidyverse)
library(data.table)
library(googlesheets)

#############################################################################################
## This first commented-out section was used to generate some excel sheets that are preserved in the last few lines of the code.
## The code here is preserved to document data cleaning that was done to generate those excel files
## After the excel sheets were created, manual checking was done of ~127 rows of teachers. 
## The revisions based on on that manual checking is saved in another excel file. 
## The first part of this file that is NOT commented out is pulling together the clean data. 
#############################################################################################
#
source("db_connection.R")
#
###############################################
## CSC
###############################################
#rubric_5_full <- dbGetQuery(myconn, "SELECT s.Id, s.UnitName, s.Nickname, sr.SchoolId, sr.FTE, sr.Minutes, sr.Access, 
#                            sr.Disciplines, sr.HasBudget, sr.HasIntegration, sr.HasProfDev, sr.HasPartners, sr.HasEngagement, 
#                            sr.AccessPercent, sr.AverageMinutes, sr.TotalEnrollment, sr.EnrollmentRatio, sr.PreliminaryScore, 
#                            sr.OfficialScore, sr.FinalScore, sr.TotalFTE, sr.HasSpace, sr.TotalDisciplines, sr.DisciplineNames, sr.PartnerDisciplineNames, 
#                            sr.NumPartners, sr.SchoolYears, u.SchoolType, s.CommunityArea, s.CpsNewNetwork, s.Latitude, s.Longitude
#                            FROM School s
#                            INNER JOIN SchoolRubric sr ON s.Id = sr.SchoolId
#                            INNER JOIN [User] u ON sr.SchoolId = u.SchoolId AND sr.SchoolYears = u.SchoolYears
#                            WHERE u.IsActive = 1 AND sr.SchoolYears = '2016-17'")
#
#for (j in seq_len(ncol(rubric_5_full))) {
#  if (class(rubric_5_full[, j]) == "character")
#    rubric_5_full[, j] <- iconv(rubric_5_full[, j], from = "latin1", to = "UTF-8")
#}
##write.xlsx(rubric_5_full,"rubric_5_full.xlsx")
##rubric_5_full<-read.xlsx("rubric_5_full.xlsx")
#rubric_5_full$SchoolType[rubric_5_full$SchoolType == 0] <- "None"
#rubric_5_full$SchoolType[rubric_5_full$SchoolType == 1 | rubric_5_full$SchoolType == 3] <- "ES"
#rubric_5_full$SchoolType[rubric_5_full$SchoolType == 2] <- "HS"
#
rubric_6_full <- dbGetQuery(myconn, "SELECT s.Id, s.UnitName, s.Nickname, sr.SchoolId, sr.FTE, sr.Minutes, sr.Access, 
                            sr.Disciplines, sr.HasBudget, sr.HasIntegration, sr.HasProfDev, sr.HasPartners, sr.HasEngagement, 
                            sr.AccessPercent, sr.AverageMinutes, sr.TotalEnrollment, sr.EnrollmentRatio, sr.PreliminaryScore, 
                            sr.OfficialScore, sr.TotalFTE, sr.HasSpace, sr.TotalDisciplines, sr.DisciplineNames, sr.PartnerDisciplineNames, 
                            sr.NumPartners, sr.SchoolYears, u.SchoolType, s.CommunityArea, s.CpsNewNetwork, s.Latitude, s.Longitude
                            FROM School s
                            INNER JOIN SchoolRubric sr ON s.Id = sr.SchoolId
                            INNER JOIN [User] u ON sr.SchoolId = u.SchoolId AND sr.SchoolYears = u.SchoolYears
                            WHERE u.IsActive = 1 AND sr.SchoolYears = '2017-18'")

for (j in seq_len(ncol(rubric_6_full))) {
  if (class(rubric_6_full[, j]) == "character")
    rubric_6_full[, j] <- iconv(rubric_6_full[, j], from = "latin1", to = "UTF-8")
}
#write.xlsx(rubric_6_full,"rubric_6_full.xlsx")
#rubric_6_full<-read.xlsx("rubric_6_full.xlsx")
rubric_6_full$SchoolType[rubric_6_full$SchoolType == 0] <- "None"
rubric_6_full$SchoolType[rubric_6_full$SchoolType == 1 | rubric_6_full$SchoolType == 3] <- "ES"
rubric_6_full$SchoolType[rubric_6_full$SchoolType == 2] <- "HS"
rubric_6_full$TotalEnrollment[rubric_6_full$SchoolId==400177]<-648
rubric_6_full$TotalEnrollment[rubric_6_full$SchoolId==400044]<-256
#
## Reconcile from 666 to x
## Subtract 12 ALOPs and 4 Charter-Combo Schools
exclude_6 <- c(610580,610581,610582,610566,610567,610569,610570,610571,610557,610568,400173,400177,100115,100162,100121,400148) # also removed 400148
#rubric_6 <- rubric_6_full[!rubric_6_full$SchoolId %in% exclude_6,]
#
#
###############################################
## II. Teacher data
###############################################
#teacher <- dbGetQuery(myconn, "SELECT SchoolId, SchoolYears, LastName, FirstName, MiddleInitial, Email, WorkHoursType,
#                      PrimaryFocus, SecondaryFocus, PartTimeFraction
#                      FROM Teacher WHERE SchoolYears = '2017-18'")
#
#for (j in seq_len(ncol(teacher))) {
#  if (class(teacher[, j]) == "character")
#    teacher[, j] <- iconv(teacher[, j], from = "latin1", to = "UTF-8")
#}
##write.xlsx(teacher,"teacher.xlsx")
##teacher<-read.xlsx("teacher.xlsx")
#teacher$RealFraction <- ifelse(teacher$WorkHoursType == 0, teacher$PartTimeFraction, 1)
#
#pos1<-data.table(read.xlsx("data/Positions - Art - 2017-11-02x.xlsx"))
##pos2<-read.xlsx("data/Positions - Art - 2018-02-01x.xlsx")
##pos3<-read.xlsx("data/Positions - Art - 2018-04-12x.xlsx")
#pos4<-data.table(read.xlsx("data/Positions - Art - 2018-06-19x.xlsx"))
#
################################################
##### Clean CPS Staffing Data from Art Kim ####
###############################################
### approach here is for each employee to keep only unique positions within their school and sum the FTEs of those positions
### commented-out code tests for each position file what that does to employees that are listed multiple times in position files
### Seems too good to be true b/c so much simpler than what I did last year, so preserving some other work in the "Extras" section below
##p<-pos4
##p<-p[!is.na(Empl.ID) & !is.na(Schl.ID)]
##pt<-p[, ':=' (emp.fte = sum(FTE), rows = .N), by=c("Empl.ID")]
##p<-p[!duplicated(p,by=c("Empl.ID","Pos.#"))]
##p[, ':=' (emp.fte = sum(FTE), rows = .N), by=c("Empl.ID")]
##p<-p[!duplicated(Empl.ID),c("Empl.ID","emp.fte")]
##pt<-pt[!duplicated(Empl.ID),c("Empl.ID","emp.fte")]
##pm<-merge(p,pt,by="Empl.ID")
##table(pm$emp.fte.x,pm$emp.fte.y) # rows show final FTE for each Empl ID using this approach; cols show how many FTE those individuals add up to in original position file 
#
#colext <-c("PointInTimeDt","Program.Descr","Job.Profile")
#keepcol<-c("Schl.ID","DeptType","Empl.ID","Last.Name","First.Name","MidInit","Email","FTE")
#p1<-pos1[!is.na(Empl.ID) & !is.na(Schl.ID)]
#p1<-p1[!duplicated(p1,by=c("Empl.ID","Pos.#"))]
#p1[, ':=' (FTE = sum(FTE), rows = .N), by=c("Empl.ID")]
#p1<-p1[!duplicated(Empl.ID), c(keepcol,colext), with=F]
#
#p4<-pos4[!is.na(Empl.ID) & !is.na(Schl.ID)]
#p4<-p4[!duplicated(p4,by=c("Empl.ID","Pos.#"))]
#p4[, ':=' (FTE = sum(FTE), rows = .N), by=c("Empl.ID")]
#p4<-p4[!duplicated(Empl.ID), c(keepcol,colext), with=F]
#
#cps<-merge(p1,p4,all=T)
##Deal with Disney
#cps[cps$Empl.ID %in% c("000236896","000249537","000239436"), "Schl.ID"]<-"610564"
##Deal with Ogden
#cps[cps$Empl.ID %in% c("000227017","000239163","000249197","000222077"), "Schl.ID"]<-"610529"
##Deal with Alcott
#cps[cps$Empl.ID %in% c("000148040"), "Schl.ID"]<-"610524"
#cps<-cps[order(Empl.ID,-PointInTimeDt),] 
#cps<-cps[, ':=' (SchoolId = as.integer(Schl.ID), BothDates = duplicated(cps, by="Empl.ID") | duplicated(cps, by="Empl.ID", fromLast = T))]
#cps<-unique(cps, by=keepcol)
#cps<-cps[, ':=' (Schl.ID = NULL, mgnm = paste(SchoolId,tolower(Last.Name),tolower(First.Name)),
#                 dup = duplicated(cps, by="Empl.ID") | duplicated(cps, by="Empl.ID", fromLast = T))]
#cps<-cps %>% mutate(Last.Name=str_trim(Last.Name), First.Name=str_trim(First.Name))
## three possibilities to explain multiple obs of an empl ID (assuming coding of keepcol fields is unchanged):
#  # 1. Individual is in 2 different schools - wait to see what school they show up in for the survey  
#  # 2. Individual has different FTE in beginning / end of year - wait to see what FTE they show in the survey
#  # 3. Both
#
###############################################
##### Clean teacher data                   ####
###############################################
#teacher <- teacher %>% mutate(LastName=str_trim(LastName), FirstName=str_trim(FirstName))
#teacher<-data.table(teacher)
#teacher[SchoolId==609901 & FirstName=="Mary", FirstName := "Mary Lee"]
#teacher[SchoolId==610257 & FirstName=="JONATHAN", FirstName := "Jon"]
#teacher[SchoolId==609967 & FirstName=="Terry", FirstName := "Terence"]
#teacher[SchoolId==610524 & FirstName=="Medina", FirstName := "Alyssa"]
#teacher[SchoolId==609694 & FirstName=="Mathew", FirstName := "Matthew"]
#
#teacher[SchoolId==610334 & LastName=="Harland", LastName := "Harland Young"]
#teacher[SchoolId==610257 & LastName=="HEARON", LastName := "Hearon"]
#teacher[SchoolId==609804 & LastName=="Hines III", LastName := "Hines"]
#teacher[SchoolId==610513 & LastName=="Jones", LastName := "Holley-Jones"]
#teacher[SchoolId==609711 & LastName=="Pulfus", LastName := "Pulphus"]
#teacher[SchoolId==609842 & LastName=="Minarik", LastName := "Lundstrom"]
#teacher[SchoolId==609798 & LastName=="Schroeder", LastName := "McCann"]
#teacher[SchoolId==609792 & LastName=="Schouweiler", LastName := "Soto"]
#teacher[SchoolId==610524 & LastName=="Alyssa", LastName := "Medina"]
#teacher[SchoolId==610558 & LastName=="Bauer", LastName := "Baurer"]
#teacher[SchoolId==610529 & LastName=="Castelucci-Cabral", LastName := "Castellucci Cabral"]
#teacher[SchoolId==610504 & LastName=="Rachel", FirstName := "Rachel"]
#teacher[SchoolId==610504 & LastName=="Rachel", LastName := "Krueger"]
#
#teacher <- teacher %>% mutate(mgnm = paste(SchoolId,tolower(LastName),tolower(FirstName)))
#  
########################################################
##### things to delete or deal with in final data   ####
########################################################
badprofiles<-c('9-12 REG Beauty Culture','9-12 REG Carpentry','9-12 REG Chef Training','9-12 REG Creative Writing','9-12 REG Drafting','9-12 REG General Vocational',
               '9-12 REG Home Economics','9-12 REG Horticulture','9-12 REG Industrial Art','9-12 REG Office Occupations','9-12 REG Plumbing','9-12 REG Welding',
               '9-12 REG Wood Shop','K-8 REG Library','Reassign Teachers- k to 12')
badprograms<-c("Agricultural Academy","Architectural Drafting","Career Employment Preparation","Carpentry","Coop Work Training","Cosmetology","Cte - Business Systems",
               "Cte - Project Lead The Way","Cte Law And Public Safety","Culinary Arts","Horticulture-Vocational","Improvement Of Instruction","Industrial Arts",
               "Library Service","Library Service-Elementary","Library Service - Elementary","Mathematics","Mathematics-Hs","Oep - Computer Education","Reduced Class Size K-8 (Push-I",
               "Oep - Learning Center","Plumbing Pipe Fit-Vocational","Reach Reassigned Teachers","Special Education Instruction","Vocational Student Services")
badschools <- c(610584,610585,610555)
#baddomains <- c("ccchoir.org","forwardmomentumchicago.org","freespiritmedia.org","hydeparkart.org")
#baddomains.maybe <- c("aol.com","aqs.Org","ccc.edu","gmail.com","hotmail.com","icloud.com","il.bridgescape.com","live.com","nush.org","ombudsman.com","sbcglobal.net","yahoo.com","youth-guidance.org")
#
###############################################
##### Merge Art Kim and artlook Staffing   ####
###############################################
#df <- full_join(cps,teacher,by="mgnm") # merge based on school / name combo
#
## generate single set of names / ways of testing for duplicates
#df <- df %>% mutate(Survey = !is.na(SchoolId.y), CPS = !is.na(SchoolId.x), match.hrs = FTE==RealFraction) # Indicators for whether data come from survey and/or CPS data and for whether the FTE in the two sources match
#df <- df %>% mutate(SchoolId = if_else(is.na(SchoolId.x),as.numeric(SchoolId.y),as.numeric(SchoolId.x)), Email = if_else(is.na(Email.x),Email.y,Email.x), 
#                    LastName = if_else(is.na(LastName),Last.Name,LastName), FirstName = if_else(is.na(FirstName),First.Name,FirstName))  # Create single set of school IDs, emails, and names
#df <- df %>% mutate(name = paste(tolower(LastName),tolower(FirstName)), SchoolId.x=NULL, SchoolId.y=NULL, Email.y=NULL, Email.x=NULL, SchoolYears=NULL, Last.Name=NULL, First.Name=NULL)
#df <- df %>% mutate(badprof = Job.Profile %in% badprofiles, badprog = Program.Descr %in% badprograms) # ID rows with bad profiles and programs
#df <- df %>% mutate(badboth = badprof==T & badprog==T) 
#df <- df %>% mutate(dup.mgnm = duplicated(df$mgnm) | duplicated(df$mgnm, fromLast = T), dup.name = duplicated(df$name) | duplicated(df$name, fromLast = T))  # ID duplicates
### Deal with cases with duplicate name/school combos (ie, person shows up multiple times in one school during the year)
### This only happens if person is listed with different FTE totals in Oct & June OR if person is only in the survey
#df <- df %>% group_by(mgnm) %>% mutate(test.hrs = sum(match.hrs,na.rm = T)) # keep if person is only in survey or keep version of FTE from CPS data that matches survey  
#df <- df %>% group_by(mgnm) %>% filter(dup.mgnm==F | (dup.mgnm==T & (test.hrs %in% c(0,2) | match.hrs==T))) %>% mutate(test.hrs=NULL)
#df <- df %>% separate(Email, sep = "@", into = c("EmailNm","EmailDom"), remove = F)
#df <- df %>% filter(!EmailDom %in% baddomains & !SchoolId %in% badschools)
#df <- df %>% mutate(realFTE=ifelse(is.na(FTE),RealFraction,FTE))
#df <- df[with(df,order(name, SchoolId)),]
#
## RC flagged instructors
#flag.inst <- gs_title("Flagged Arts Teachers for CSC 17-18")
#flag.inst <- gs_read(flag.inst, ws = 1, range = cell_rows(c(2:100)))
#flag.inst <- flag.inst %>% mutate(name = paste(tolower(`LAST `),tolower(FIRST)))
#flagged<- unlist(df[df$name %in% flag.inst$name, "name"])
#       
#spryteach <- unlist(df[df$SchoolId==610357,"name"])
#movers<-c("baurer irica","fosses vasiliki","serment ricardo","swenson morgan")
#diffpeople<-c("hamilton sean","moore tracey")
#
#df.s <- df %>% filter(dup.name==T | EmailDom %in% baddomains.maybe | name %in% flagged | name %in% spryteach | name %in% movers | name %in% diffpeople)
#df.g <- anti_join(df,df.s, by=c(names(df)))
#df.s$id <- paste0("id",str_pad(seq(1:nrow(df.s)),4,side="left",pad="0"))
#
#write.xlsx(df.g,"output/goodMerge.xlsx")
#write.xlsx(df.s,"output/toCorrect.xlsx")

df.g <- read.xlsx("I:/Data & Impact/Analyses/SOTA/SOTA1718/output/goodMerge.xlsx")
df.s <- read.xlsx("I:/Data & Impact/Analyses/SOTA/SOTA1718/output/beenCorrected.xlsx")
df.s <- select(df.s, -c(id,Notes))

# create one list of teachers that includes bad job profiles and one that doesn't
df <- bind_rows(df.g,df.s)
df <- df %>% mutate(oldFTE=ifelse(is.na(FTE),RealFraction,FTE))
df <- left_join(df,rubric_6_full[,c("SchoolId","UnitName")], by="SchoolId")
df2 <- df %>% filter(Survey==T | (Survey==F & (badprof==F & badprog==F)))
df2$SchoolYears <- '2017-18'
df3 <- df2 %>% filter(realFTE>0) %>% mutate(WorkHoursType = if_else(realFTE>=1,1,0), PartTimeFraction = realFTE)

#df <- df %>% group_by(SchoolId) %>% mutate(realTotalFTE = sum(realFTE))
#df2 <- df2 %>% group_by(SchoolId) %>% mutate(realTotalFTE = sum(realFTE))
kpcol<-c("SchoolId","UnitName","DeptType","Empl.ID","LastName","FirstName","MiddleInitial","Email","PointInTimeDt","Program.Descr","Job.Profile","realFTE",
         "PrimaryFocus","SecondaryFocus","Survey","CPS","BothDates","badprof","badprog")
kpcol2<-c("LastName","FirstName","MiddleInitial","WorkHoursType","PrimaryFocus","SecondaryFocus","SchoolId","SchoolYears","PartTimeFraction","Email")
write.xlsx(df2[order(df2$SchoolId,df2$name),kpcol],sprintf("%s/output/TeacherRoster.xlsx", wd))
write.xlsx(df3[order(df3$SchoolId,df3$name),kpcol2],sprintf("%s/output/TeacherRosterForUpload.xlsx", wd))

ds2 <- data.table(df2)
ds2 <- ds2[, .(realTotalFTE = sum(realFTE)), by=SchoolId]

r6<-full_join(rubric_6_full,ds2,by="SchoolId")
r6$realTotalFTE <- r6$realTotalFTE %>% replace_na(0)
r6 <- r6 %>% mutate(realEnrollmentRatio = realTotalFTE/TotalEnrollment) 
r6 <- data.table(r6)

# create new FTE measures
r6[realTotalFTE==0,':=' (realFTE = 4)]
r6[realTotalFTE==.5 & realEnrollmentRatio < 1/350,':=' (realFTE = 3)]
r6[realTotalFTE>=1 & realEnrollmentRatio < 1/350,':=' (realFTE = 2)]
r6[realEnrollmentRatio >= 1/350,':=' (realFTE = 1)]
r6[FTE==5, ':=' (realFTE = 5)]
# clean up
r6[,c("FTE","Minutes","Access","Disciplines")][r6[,c("FTE","Minutes","Access","Disciplines")]==0] <- NA
r6 <- data.frame(r6) %>% mutate(realPreliminaryScore = apply(r6[,c("realFTE","Minutes","Access","Disciplines")], 1, function(x) max(x, na.rm = T)))
r6 <- r6 %>% mutate(realOfficialScore = realPreliminaryScore + OfficialScore - PreliminaryScore)
r6 <- data.frame(r6) %>% mutate(chgFTE = FTE != realFTE) %>% mutate(impact = realFTE >= PreliminaryScore)

#write.xlsx(r6,"output/RubricWithChanges.xlsx")
#write.xlsx(select(r6[order(r6$SchoolId),],SchoolId, UnitName, FinalScore = realOfficialScore),"output/Rubric1718forCPS.xlsx")

## create audit table
#audit <- data.table(r6)
#audit <- audit[,c("FTE","PreliminaryScore","OfficialScore","EnrollmentRatio","TotalFTE") := NULL]
#names(audit) <- sub("real","",names(audit))
#audit <- audit %>% select(SchoolId, UnitName, Nickname, everything())
#names(audit) <- c("SchoolId","UnitName","Nickname", paste0(names(audit)[!names(audit) %in% c("SchoolId","UnitName","Nickname")],".18"))
#r5 <- rubric_5_full
#names(r5) <- c("SchoolId", paste0(names(r5)[names(r5)!="SchoolId"],".17"))
#a1 <- full_join(audit[,c("SchoolId","UnitName","Nickname","OfficialScore.18","FTE.18","Minutes.18","Access.18","Disciplines.18","AccessPercent.18","AverageMinutes.18","TotalFTE.18","TotalEnrollment.18","EnrollmentRatio.18","PreliminaryScore.18")],
#                r5[,c("SchoolId","FinalScore.17","FTE.17","Minutes.17","Access.17","Disciplines.17","AccessPercent.17","AverageMinutes.17","TotalFTE.17","TotalEnrollment.17","EnrollmentRatio.17","PreliminaryScore.17")],
#                by="SchoolId")
#a1[,names(a1)[grep("^FTE.|^Minutes.|^Access.|^Disciplines.",names(a1))]][a1[,names(a1)[grep("^FTE.|^Minutes.|^Access.|^Disciplines.",names(a1))]]==0] <- NA
#a1 <- a1 %>% mutate(OfficialScore.d=FinalScore.17-OfficialScore.18, FTE.d=FTE.17-FTE.18, Minutes.d=Minutes.17-Minutes.18, Access.d=Access.17-Access.18, Disciplines.d=Disciplines.17-Disciplines.18, PreliminaryScore.d=PreliminaryScore.17-PreliminaryScore.18)
#a1.1 <- a1 %>% filter(FinalScore.17==5 | OfficialScore.18==5) 
#a1.1[,c("OfficialScore.d","FTE.d","Minutes.d","Access.d","Disciplines.d","PreliminaryScore.d")] <- NA
#a1.2 <- a1 %>% filter(FinalScore.17!=5 & OfficialScore.18!=5) 
#a1 <- bind_rows(a1.1,a1.2)
#a1 <- a1[order(a1$SchoolId),]
#
#write.xlsx(a1,"AuditData.xlsx")

table(rubric_6_full$TotalFTE,r6$realTotalFTE)
table(rubric_6_full$FTE,r6$realFTE)
table(df$oldFTE,df$realFTE)
prop.table(table(r6$realOfficialScore))
prop.table(table(r6[!r6$SchoolId %in% exclude_6,]$realOfficialScore))
