##########################################################
# Name of file: Scotland File Processing
# Data Release: Delayed Discharges monthly publication
# Original author(s): Jennifer Noall (spss version: James Mc Nally, Deanna Campbell, Peter McClurg)
# Original date: 21/10/2019 (spss version: 08/2016)
# Latest update author (if not using version control)
# Latest update date (if not using version control)
# Latest update description (if not using version control)
# Type of script: Preparation
# Written/run on: R Studio SERVER
# Version of R that the script was most recently run on: ?
# Description of content: This syntax takes a Scotland csv file and carries out validation using the data definitions as at July 2016.  
# Approximate run time: TBC
##########################################################


### 1.Housekeeping ----


#install.packages("janitor")
#install.packages("openxlsx")
#install.packages("here")

library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(haven)
library(lubridate)
library(stringr)
library(openxlsx)
library(tidyverse)
library(janitor)
library(stringi)

# Define filepaths - move into 'set up environment' script.

filepath<-("//conf/delayed_discharges/RAP development/2019_07/Outputs/")

censusdate <- ("25/07/2019")

#first date of month
firstdom <- ("01/07/2019")

#last date of the month
lastdom <- ("31/07/2019")


# Manual steps:

# Before reading data in, need to copy from EACH board's BO report (FULL DATA DOWNLOAD tab) and past into a new workbook then save as csv.
# NB: any changes to BO report mean that variable list below will need updated

# BEFORE READING IN CSV FILE, OPEN IN EXCEL, DO FIND&REPLACE: COMMA WITH SPACE
# This will remove any commas in specialty description - eg Ear, Nose & Throat - which will cause problems once file read in

### 2.Get Scotland_validated file for latest month ----


# Read in file
datafile<-read_spss(paste0(filepath,"Allboards_R.sav"))

# Create variable with first / last day of month value (defined above).
datafile <- mutate(datafile, firstdom=firstdom)
datafile <- mutate(datafile, lastdom=lastdom)

# Check and recode any cases with missing monthflag
datafile$monthflag[is.na(datafile$monthflag)] <- "Jul-19"
table(datafile$monthflag)

# Review frequency tables
table(datafile$nhs_board)
table(datafile$monthflag)
table(datafile$discharge_hospital_nat_code)
table(datafile$local_authority_code)
table(datafile$discharge_specialty_nat_code)
table(datafile$discharge_to_code)
table(datafile$dd_code_1)
table(datafile$dd_code_2)
table(datafile$sex_code)

# Recodes:
#CHI NUMBER - Remove leading / trailing spaces & add leading 0 if missing.
trimws (datafile$chi_number)

datafile<-datafile  %>%
  mutate(chi_number=as.character(chi_number)) %>% 
  mutate(chi_number = ifelse(nchar(chi_number) == 9, paste0("0", chi_number), chi_number))
View(datafile$chi_number)
count(datafile, substr(chi_number, 9, 9))

#Change numeric to a string character for chi_number

datafile  %>%
  mutate(chi_number=as.character(chi_number))


# Recode discharge reason:
datafile$discharge_to_code[datafile$discharge_to_code =="01"] <- "1"
datafile$discharge_to_code[datafile$discharge_to_code =="02"] <- "2"
datafile$discharge_to_code[datafile$discharge_to_code =="03"] <- "3"
datafile$discharge_to_code[datafile$discharge_to_code =="04"] <- "4"
datafile$discharge_to_code[datafile$discharge_to_code =="05"] <- "5"

count(datafile,discharge_to_code)


# Recode out of area:
datafile$out_of_area_case_indicator[datafile$out_of_area_case_indicator =="Yes"] <- "Y"
datafile$out_of_area_case_indicator[datafile$out_of_area_case_indicator =="No"] <- "N"
datafile$out_of_area_case_indicator[datafile$out_of_area_case_indicator =="no"] <- "N"
datafile$out_of_area_case_indicator[datafile$out_of_area_case_indicator ==""] <- "N"

count(datafile,out_of_area_case_indicator)

# Recode sex
datafile$sex_code[datafile$sex_code=="1"] <- "Male"
datafile$sex_code[datafile$sex_code=="M"] <- "Male"
datafile$sex_code[datafile$sex_code=="2"] <- "Female"
datafile$sex_code[datafile$sex_code=="F"] <- "Female"

count(datafile,sex_code)


# If Gender is blank check CHINo 9th charcter (1,3,5,7,9=Male, 0,2,4,6,8=female)
table(datafile$sex_code)

datafile<-datafile %>% 
  mutate(sex_code=
           case_when(
             sex_code!=""~sex_code,
             substr(chi_number,9,9)%in%c("1","3","5","7","9") ~ "Male",
             substr(chi_number,9,9)%in%c("0","2","4","6","8") ~ "Female",
             TRUE~"Unknown"
           ))

# Recode delay reason
# Set blank codes to 11A
datafile$dd_code_1[datafile$dd_code_1==" " & datafile$dd_code_2==" "] <- "11A"

# recode code 9's
datafile$dd_code_1[datafile$dd_code_1=="09"] <- "9"

# CHECK FOR SMALL x in delaycodes.
table(datafile$dd_code_1)
table(datafile$dd_code_2)

# reason code variables to upper case
toupper(datafile$dd_code_1)
toupper(datafile$dd_code_2)

# recode old delay reasons
datafile$dd_code_1[datafile$dd_code_1=="41"] <- "25E"
datafile$dd_code_1[datafile$dd_code_1=="41A"] <- "23C"
datafile$dd_code_1[datafile$dd_code_1=="41B"] <- "23D"
datafile$dd_code_1[datafile$dd_code_1=="43"] <- "24A"
datafile$dd_code_1[datafile$dd_code_1=="27A."] <- "27"
datafile$dd_code_1[datafile$dd_code_1=="62"] <- "67"
datafile$dd_code_1[datafile$dd_code_1=="63"] <- "67"

# First remove all spaces from postcode variable
datafile <- datafile %>% 
  mutate(postcode = gsub("\\s", "", postcode),
       

# Then add space (or spaces) at appropriate juncture (depending on
# the number of characters) to get the postcode into 7-character
# format
postcode = case_when(
  is.na(postcode) ~ NA_character_,
  str_length(postcode) == 5 ~ sub("(.{2})", "\\1  ", postcode),
  str_length(postcode) == 6 ~ sub("(.{3})", "\\1 ", postcode),
  str_length(postcode) == 7 ~ sub("(.{4})", "\\1 ", postcode),
  TRUE ~ postcode))

# Recode LA
datafile$local_authority_code[datafile$local_authority_code==""] <- "Missing"
datafile$local_authority_code[datafile$local_authority_code=="1" | datafile$local_authority_code=="01" | datafile$local_authority_code=="Aberdeen"] <- "Aberdeen City"
datafile$local_authority_code[datafile$local_authority_code=="2" | datafile$local_authority_code=="02"] <- "Aberdeenshire"
datafile$local_authority_code[datafile$local_authority_code=="3" | datafile$local_authority_code=="03"] <- "Angus"
datafile$local_authority_code[datafile$local_authority_code=="4" | datafile$local_authority_code=="04"] <- "Argyll & Bute"
datafile$local_authority_code[datafile$local_authority_code=="5" | datafile$local_authority_code=="05"] <- "Scottish Borders"
datafile$local_authority_code[datafile$local_authority_code=="6" | datafile$local_authority_code=="06"] <- "Clackmannanshire"
datafile$local_authority_code[datafile$local_authority_code=="7" | datafile$local_authority_code=="07"] <- "West Dunbartonshire"
datafile$local_authority_code[datafile$local_authority_code=="8" | datafile$local_authority_code=="08"] <- "Dumfries & Galloway"
datafile$local_authority_code[datafile$local_authority_code=="9" | datafile$local_authority_code=="09"] <- "Dundee City"
datafile$local_authority_code[datafile$local_authority_code=="10"] <- "East Ayrshire"
datafile$local_authority_code[datafile$local_authority_code=="11"] <- "East Dunbartonshire"
datafile$local_authority_code[datafile$local_authority_code=="12"] <- "East Lothian"
datafile$local_authority_code[datafile$local_authority_code=="13"] <- "East Renfrewshire"
datafile$local_authority_code[datafile$local_authority_code=="14"] <- "City of Edinburgh"
datafile$local_authority_code[datafile$local_authority_code=="15"] <- "Falkirk"
datafile$local_authority_code[datafile$local_authority_code=="16"] <- "Fife"
datafile$local_authority_code[datafile$local_authority_code=="17"] <- "Glasgow City"
datafile$local_authority_code[datafile$local_authority_code=="18"] <- "Highland"
datafile$local_authority_code[datafile$local_authority_code=="19"] <- "Inverclyde"
datafile$local_authority_code[datafile$local_authority_code=="20"] <- "Midlothian"
datafile$local_authority_code[datafile$local_authority_code=="21"] <- "Moray"
datafile$local_authority_code[datafile$local_authority_code=="22"] <- "North Ayrshire"
datafile$local_authority_code[datafile$local_authority_code=="23"] <- "North Lanarkshire"
datafile$local_authority_code[datafile$local_authority_code=="24"] <- "Orkney"
datafile$local_authority_code[datafile$local_authority_code=="25"] <- "Perth & Kinross"
datafile$local_authority_code[datafile$local_authority_code=="26"] <- "Renfrewshire"
datafile$local_authority_code[datafile$local_authority_code=="27"] <- "Shetland"
datafile$local_authority_code[datafile$local_authority_code=="28"] <- "South Ayrshire"
datafile$local_authority_code[datafile$local_authority_code=="29"] <- "South Lanarkshire"
datafile$local_authority_code[datafile$local_authority_code=="30"] <- "Stirling"
datafile$local_authority_code[datafile$local_authority_code=="31"] <- "West Lothian"
datafile$local_authority_code[datafile$local_authority_code=="32"] <- "Comhairle nan Eilean Siar"
datafile$local_authority_code[datafile$local_authority_code=="90"] <- "Other"

table(datafile$local_authority_code)

datafile$local_authority_code[datafile$local_authority_code=="Argyll & Bute Council"] <- "Argyll & Bute"
datafile$local_authority_code[datafile$local_authority_code=="Eilean Siar"] <- "Comhairle nan Eilean Siar"
datafile$local_authority_code[datafile$local_authority_code=="shetland Islands"] <- "Shetland"
datafile$local_authority_code[datafile$local_authority_code=="Shetland Islands"] <- "Shetland"

### 3.Exclusions ----

# AGE AT RMD DATE (Ready for Medical Discharge).

# compute dob2 and RMD in yyyymmdd format

datafile <-
  mutate(datafile, dob2 = as.character (date_of_birth))

datafile$dob2 <- dmy(datafile$dob2)
datafile$date_declared_medically_fit <- dmy(datafile$date_declared_medically_fit)

# compute Age at Ready for Discharge Date.

# Create interval() using DOB2 and Ready for Discharge Date.
# Then time_length() gives the exact length in "year"
# Note that floor() rounds down to nearest integer to prevent 46.5 years giving age as 47
datafile <- datafile %>% 
  mutate(age_at_rdd = interval(start = dob2, end = date_declared_medically_fit)) %>% 
  mutate(age_at_rdd = floor(time_length(age_at_rdd,unit = "year")))

# Check no missing values for age_at_rdd - all should be false.
age_rdd_missing <- is.na(datafile$age_at_rdd)
unique(age_rdd_missing)

# Ensure ages 18+ only
unique(datafile$age_at_rdd)

# Keep only hospital locations (i.e. codes ending in H plus carehome in Grampian with NHS staffed beds)

datafile<-datafile%>% 
  filter(str_sub(discharge_hospital_nat_code,5,5)=="H" | discharge_hospital_nat_code=="N465R")

# ZERO DELAYS
# 1. Identify any records where Ready for Discharge Date = Discharge Date as not delays. 
datafile <- mutate(datafile, RDDsameasDD = case_when(discharge_date == date_declared_medically_fit ~ 1))


datafile$RDDsameasDD <- ifelse(datafile$discharge_date == date_declared_medically_fit,1,0)

datafile <- mutate(datafile, RDDsameasDD = 
                 if_else(discharge_date == date_declared_medically_fit, 1, 0))
               

# Check how many
unique(datafile$RDDsameasDD)

# Remove zero delays & delete variable
datafile <- filter(datafile, RDDsameasDD = 0)
datafile <- select(datafile, -RDDsameasDD)

#2. Remove any records where Ready for Discharge Date = Last Day of Month (LastDoM) - these do not become delays until the 1st of the following month.
datafile <- mutate(datafile, RDDsameasLastDoM = case_when(date_declared_medically_fit == lastdom ~ 1))

# Check how many
unique(datafile$RDDsameasLastDoM)

# Remove zero delays & delete variable
datafile <- filter(datafile, RDDsameasLastDoM = 0)
datafile <- select(datafile, -RDDsameasLastDoM)

### 4.Derivations ----

# Derive age groups
datafile <- mutate(datafile, 
               age_grp = case_when(
                 age_at_rdd <75 ~ "18-74", 
                  age_at_rdd >=75 ~ "75+"))

count(datafile, age_grp, age_at_rdd) %>%
  spread(age_grp, n)

# Create Reason Code Groupings

# 1. High level reason code grouping - post July 2016
  # Transport grouped under H&SC reasons as per BO report

datafile<-datafile %>% 
  mutate(reasongrp_highlevel=
           case_when(
             dd_code_1=="100" ~ "Code 100",
             dd_code_1=="9" ~ "Code 9",
             dd_code_1 %in% c("11A","11B","23C","23D","24A","24B","24C","24D","24E","24F","27A","25A","25D","25E","25F","44") ~ "Health and Social Care Reasons",
             dd_code_1 %in% c("51","52","61","67","71","72","73","74" ~ "Patient/Carer/Family-related reasons")))

# 2. Reason code grouping - post July 2016
datafile<-datafile %>% 
  mutate(reasongrp=
           case_when(
             dd_code_1 %in% c("11A","11B") ~ "H&SC - Community Care Assessment",
             dd_code_1 %in% c("23C","23D") ~ "H&SC - Funding",
             dd_code_1 %in% c("24A","24B","24C","24D","24E","24F","27A") ~ "H&SC - Place Availability",
             dd_code_1 %in% c("25A","25D","25E","25F") ~ "H&SC - Care Arrangements",
             dd_code_1 %in% c("44") ~ "H&SC - Transport",
             dd_code_1 %in% c("51","52") ~ "Patient/Carer/Family-related reasons: Legal/Financial", 
             dd_code_1 %in% c("61","67") ~ "Patient/Carer/Family-related reasons: Disagreements",
             dd_code_1 %in% c("71","72","73","74") ~ "Patient/Carer/Family-related reasons: Other",
             dd_code_2 %in% c("71X","25X","24EX","24DX") ~ "Other code 9 reasons (not AWI)",
             dd_code_2 %in% c("51X") ~ "Adults with Incapacity Act"))

# 3. Reason Codes INDIVIDUAL - Post July 2016.
datafile<-datafile %>% 
  mutate(delay_description=
           case_when(
           dd_code_1=="11A" ~ "Awaiting commencement of post-hospital social care assessment (including transfer to another area team). Social care includes home care and social work OT",
           dd_code_1=="11B" ~ "Awaiting completion of post-hospital social care assessment (including transfer to another area team). Social care includes home care and social work OT",
           dd_code_1=="23C" ~ "Non-availability of statutory funding to purchase Care Home Place",
           dd_code_1=="23D" ~ "Non-availability of statutory funding to purchase any Other Care Package",
           dd_code_1=="24A" ~ "Awaiting place availability in Local Authority Residential Home",
           dd_code_1=="24B" ~ "Awaiting place availability in Independent Residential Home",
           dd_code_1=="24C" ~ "Awaiting place availability in Nursing Home",
           dd_code_1=="24D" ~ "Awaiting place availability in Specialist Residential Facility for younger age groups (<65)",
           dd_code_1=="24E" ~ "Awaiting place availability in Specialist Residential Facility for older age groups (65+)",
           dd_code_1=="24F" ~ "Awaiting place availability in care home (EMI/Dementia bed required)",
           dd_code_1=="27A" ~ "Awaiting place availability in an Intermediate Care facility",
           dd_code_1=="25A" ~ "Awaiting completion of arrangements for Care Home placement",
           dd_code_1=="25D" ~ "Awaiting completion of arrangements - in order to live in their own home – awaiting social support (non-availability of services)",
           dd_code_1=="25E" ~ "Awaiting completion of arrangements - in order to live in their own home – awaiting procurement/delivery of equipment/adaptations fitted",
           dd_code_1=="25F" ~ "Awaiting completion of arrangements - Re-housing provision (including sheltered housing and homeless patients)",
           dd_code_1=="51" ~ "Legal issues (including intervention by patient’s lawyer) - e.g. informed consent and/or adult protection issues",
           dd_code_1=="52" ~ "Financial and personal assets problem - e.g. confirming financial assessment",
           dd_code_1=="61" ~ "Internal family dispute issues (including dispute between patient and carer)",
           dd_code_1=="67" ~ "Disagreement between patient/carer/family and health and social  care",
           dd_code_1=="71" ~ "Patient exercising statutory right of choice",
           dd_code_1=="72" ~ "Patient does not qualify for care",
           dd_code_1=="73" ~ "Family/relatives arranging care",
           dd_code_1=="74" ~ "Other patient/carer/family-related reason",
           dd_code_1=="44" ~ "Awaiting availability of transport",
           dd_code_1=="100" ~ "Reprovisioning/Recommissioning (see data definitions manual section 2.3)"))
           
datafile$delay_description[datafile$dd_code_1=="9" & datafile$dd_code_2=="24DX"] <- "Awaiting place availability in Specialist Facility for high level younger age groups (<65) where the Facility is not currently available and no interim option is appropriate"
datafile$delay_description[datafile$dd_code_1=="9" & datafile$dd_code_2=="24EX"] <- "Awaiting place availability in Specialist Facility for high level older age groups (65+) where the Facility is not currently available and an interim option is not appropriate"
datafile$delay_description[datafile$dd_code_1=="9" & datafile$dd_code_2=="26X"] <- "Care Home/facility closed"
datafile$delay_description[datafile$dd_code_1=="9" & datafile$dd_code_2=="46X"] <- "Ward closed – patient well but cannot be discharged due to closure"
datafile$delay_description[datafile$dd_code_1=="9" & datafile$dd_code_2=="25X"] <- "Awaiting completion of complex care arrangements - in order to live in their own home"
datafile$delay_description[datafile$dd_code_1=="9" & datafile$dd_code_2=="51X"] <- "Adults with Incapacity Act"
datafile$delay_description[datafile$dd_code_1=="9" & datafile$dd_code_2=="71X"] <- "Patient exercising statutory right of choice – interim placement is not possible or reasonable"

# Create census flag
datafile <- mutate(datafile, Census_Date=censusdate) %>% 
  mutate(datafile, Census_Date = as.character (Census_Date))

datafile$Census_Date <- dmy(datafile$Census_Date)
datafile$admission_date <- dmy(datafile$admission_date)
datafile$date_referred_for_sw_assessment <- dmy(datafile$date_referred_for_sw_assessment)
datafile$date_declared_medically_fit <- dmy(datafile$date_declared_medically_fit)
datafile$discharge_date <- dmy(datafile$discharge_date)

datafile <- datafile %>% 
  mutate(CENSUSFLAG=
           case_when(
             discharge_date=="N/A" & date_declared_medically_fit == Census_Date & dd_code_2 != "26X" | dd_code_2!= "46X" ~ "Y",
             discharge_date >= Census_Date & date_declared_medically_fit == Census_Date & dd_code_2 != "26X" | dd_code_2!= "46X" ~ "Y",
             discharge_date <= Census_Date & dd_code_2 != "26X" | dd_code_2!= "46X" ~ "",
             discharge_date == date_declared_medically_fit & dd_code_2 != "26X" | dd_code_2 != "46X" ~ "",
             discharge_date >= Census_Date & dd_code_2 != "26X" | dd_code_2 != "46X" ~ ""
             
# Flag those discharged up to 3 working days after census (NOTE census is always last THURS of month).
  
# *1/ Identify Census Date + 3 working days:
datafile <- datafile %>% 
  mutate(datafile, CensusDate_Plus3WorkingDays = censusdate + 5) 

datafile$CensusDate_Plus3WorkingDays <- dmy(datafile$CensusDate_Plus3WorkingDays)

# *2/Flag those with a discharge date le CensusDate_Plus3WorkingDays and check against BO variable CENSUSDISCHARGEWITHIN3WORKINGDAYS.

datafile <- datafile %>% 
  mutate(Dischargewithin3daysCensus=
           case_when(
             CENSUSFLAG=="Y" & discharge_date < CensusDate_Plus3WorkingDays& dd_code_1 != "100" & dd_code_2 != "26X" | dd_code_2!= "46X" ~ "Y"

             
# *Change 'Y' to count for DischargeWithin3DaysCensus.

datafile <- datafile %>% 
  mutate(datafile,
         Dischargewithin3daysCensus = case_when(
           Dischargewithin3daysCensus == "Y" ~ 1,
           Dischargewithin3daysCensus == "" ~ 0

#Check - should be zero cases returned.
censusflagcheck <- filter(datafile, Dischargewithin3daysCensus = 1 & CENSUSFLAG == "")
           
count(censusflagcheck, chi_number, CENSUSFLAG) %>%
  spread(CENSUSFLAG, n)


# Calculate OBDs in the current month

# Create variable with month start date
datafile <- datafile %>%  
  mutate(datafile, current_month_start = monthflag)

datafile$current_month_start <- dmy(datafile$current_month_start)

# Create variable with month end date (add 1 month to start date)
datafile <- 
  mutate(datafile, current_month_end = current_month_start %m+% months(1))

# Then subtract one day to get the last day of the month.
datafile <- 
  mutate(datafile, current_month_end = current_month_end %m-% days(1))

### 4.Next section ----

# Flag whether readyfordischargedatein current month
datafile <-
  mutate(datafile, DRMDInMonth =
    case_when(date_declared_medically_fit >= current_month_start & date_declared_medically_fit <= current_month_end ~ "Y"))

# Flag whether discharge date in current month
datafile <-
  mutate(datafile, DateDischargeInMonth = 
           case_when(discharge_date >= current_month_start & discharge_date <= current_month_end ~ "Y"))



### END OF SCRIPT ###