##########################################################
# Name of file: Trend file
# Data Release: Delayed Discharges monthly publication
# Original author(s): Jennifer Noall (spss version: James Mc Nally)
# Original date: 09/09/19 (spss version: 30/11/2017)
# Latest update author (if not using version control)
# Latest update date (if not using version control)
# Latest update description (if not using version control)
# Type of script: Preparation
# Written/run on: R Studio SERVER
# Version of R that the script was most recently run on: ?
# Description of content: Updates trendfile with DD records from most recent publication month.
# Approximate run time: TBC
##########################################################


### 1.Housekeeping ----
# This section should be the only section of the script which requires manual changes 
# for future updates and includes:
#   loading packages
#   setting filepaths and extract dates
#   functions (defined here or sourced from another file)
#   setting plot parameter
#   specifying codes (e.g. ICD-10 codes)

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


#filename for latest month needs to be manually updated - can we automate this somehow?


### 2.Get Scotland_validated file for latest month ----

# Read in file
datafile <-
  read_spss("/conf/delayed_discharges/Data files/Single Submissions (July 2016 onwards)/2019_07/Data/scotland/SCOTLAND_validated.sav")

# Convert PatientDOB to date variable, add variable 'cennum' (census number for latest month)
datafile2 <- datafile %>% 
  mutate(PatientDOB=as.Date(PatientDOB)) %>% 
  mutate(cennum=
           if_else(MONTHFLAG=='Jul 2019',164, 1, 0)) %>% 
  mutate(CalYr=
           if_else(MONTHFLAG=='Jul 2019',2019, 1, 0)) %>% 
  mutate(FinYr=
           if_else(MONTHFLAG=='Jul 2019',2019/20, 1, 0))

  # Fix finyr recode
datafile2$FinYr[datafile2$FinYr==100.95] <- "2019/20"

datafile2 <- mutate(datafile2, CalYr = as.character(CalYr)) %>% 
            mutate(cennum = as.character(cennum))

# Add specialty description
#datafile2 <- arrange(datafile2, SpecialtyCode)
    
#lookup_spec <-
  #read_sav("/conf/delayed_discharges/Data files/Single Submissions (July 2016 onwards)/Specialty.sav")

#datafile2 <- left_join(datafile2, lookup_spec,
                       #by = c("SpecialtyCode" = "SpecialtyCode"))
  
# Add hospital name
lookup_hosp <-
read_sav("/conf/delayed_discharges/Data files/Single Submissions (July 2016 onwards)/location.sav")

datafile2 <- left_join(datafile2, lookup_hosp,
by = c("HealthLocationCode" = "Location"))

# Remove additional variables pulled through from lookup
datafile2 <- select(datafile2, -Add1:-filler)

# Save files
write_sav(datafile2, "/conf/delayed_discharges/RAP development/2019_07/Data/scotland/temp1.sav")

temp1 <-
  read_spss("/conf/delayed_discharges/RAP development/2019_07/Data/scotland/temp1.sav")

temp1 <-
  select(temp1, -DuplicateCHI, -hospname)
  
write_sav(temp1, "/conf/delayed_discharges/RAP development/2019_07/Data/scotland/temp2.sav")

### 3.Add latest monthly file to current trend file ----

# Add files together to update trend file

temp2 <- read_spss("/conf/delayed_discharges/RAP development/2019_07/Data/scotland/temp2.sav")

updated_trend <- bind_rows(temp2, trend)

count(updated_trend,DischargeReason)

# Change discharge reason from text to code
updated_trend$DischargeReason[updated_trend$DischargeReason=="Placement"] <- "1"
updated_trend$DischargeReason[updated_trend$DischargeReason=="Continuing Care NHS (MEL)"] <- "1"
updated_trend$DischargeReason[updated_trend$DischargeReason=="Discharge Home with Home Care"] <- "2"
updated_trend$DischargeReason[updated_trend$DischargeReason=="Discharge Home"] <- "3"
updated_trend$DischargeReason[updated_trend$DischargeReason=="Death"] <- "4"
updated_trend$DischargeReason[updated_trend$DischargeReason=="Not Fit For Discharge"] <- "5"

count(updated_trend, DischargeReason)
                        
# Save out as SPSS file

updated_trend <- arrange(updated_trend, cennum, CHINo)

write_sav(updated_trend, "/conf/delayed_discharges/RAP development/2019_07/Outputs/TrendFile_JUL16-JUL19_R.sav")

# Save out to Excel

write.xlsx(updated_trend,"/conf/delayed_discharges/RAP development/2019_07/Outputs/TrendFile_JUL16-JUL19_R.xlsx")

### 4.Checks ----

monthflag_cennum_patients <- updated_trend %>% 
  group_by(cennum,MONTHFLAG) %>% 
  summarise(no_of_patients = sum(NoofPatients)) %>%
  ungroup()

age_checks <- updated_trend %>% 
  group_by(AGEATRDD, AgeGrouping) %>% 
  summarise(count = n()) %>% 
  ungroup()

location_hb_patients <- updated_trend %>% 
  group_by(Healthboard,HealthLocationCode) %>% 
  summarise(no_of_patients = sum(NoofPatients)) %>%
  ungroup()

hscp_la_patients <- updated_trend %>% 
  group_by(LocalAuthorityArea,HSCP2019_derived) %>% 
  summarise(no_of_patients = sum(NoofPatients)) %>%
  ungroup()

delay_reason <- updated_trend %>% 
  group_by(REASONFORDELAY, REASONFORDELAYSECONDARY) %>% 
  summarise(no_of_patients = sum(NoofPatients)) %>%
  ungroup()

Gender <-
  count(updated_trend, Gender)

Outofareacaseindicator <-
  count(updated_trend, Outofareacaseindicator)

SpecialtyDesc <-
  count(updated_trend, SpecialtyDesc)

DischargeReason <-
  count(updated_trend, DischargeReason)

HSCPLocality_derived <-
  count(updated_trend, HSCPLocality_derived)

Readyfordischargedate <-
  count(updated_trend, Readyfordischargedate)

DateDischarge <-
  count(updated_trend, DateDischarge)
  
RDD_Datedischarge <-
  filter(updated_trend, Readyfordischargedate==DateDischarge)

trend_aug <-read_excel("/conf/delayed_discharges/RAP development/2019_07/Outputs/TrendFile_JUL16-AUG19.xlsx") %>% 
filter(cennum != "165")

write.xlsx(trend_aug,"/conf/delayed_discharges/RAP development/2019_07/Outputs/TrendFile_JUL16-JUL19_SPSS.xlsx")
write_sav(trend_aug,"/conf/delayed_discharges/RAP development/2019_07/Outputs/TrendFile_JUL16-JUL19_SPSS.sav")

### END OF SCRIPT ###