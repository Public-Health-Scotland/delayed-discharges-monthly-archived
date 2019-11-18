##########################################################
# Name of file: Provisional and Variance Figures
# Data Release: Delayed Discharges monthly publication
# Original author(s): Peter McClurg (spss version: James Mc Nally)
# Original date: 09/10/19 (spss version: 30/11/2017)
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
library(dplyr)
library(tidyr)
library(haven)
library(lubridate)
library(stringr)
library(openxlsx)
library(tidyverse)
library(janitor)
library(stats)
devtools::install_github("Health-SocialCare-Scotland/phimethods")

### 1.Filepaths for latest month

filepath<-("/conf/delayed_discharges/RAP development/2019_07/Outputs/")

censusdate<-dmy("25/07/2019")
monthstart<-dmy("01/07/2019")
monthend<-dmy("31/07/2019")

Monthflag<-("Jul-2019")

### 2.Get Scotland_allboards file for latest month ----

# Read in file
datafile<-read_spss(paste0(filepath,"Allboards_R.sav"))

#INITIAL DATA CHECKS
#Check for missing values - applies to data not created in BO (Island Boards and D&G)

#RENAME VARIABLES TO COINCIDE WITH WHAT'S USED THROUGHOUT EXISTING SYNTAX

datafile <- datafile %>% 
  rename(Healthboard = nhs_board,
         Monthflag = monthflag,
         HealthLocationCode = discharge_hospital_nat_code,
         CHINo=chi_number,
         PatientPostcode=postcode,
         LocalAuthorityCode=local_authority_code,
         PatientDOB=date_of_birth,
         SpecialtyCode=discharge_specialty_nat_code,
         DateReferralReceived=date_referred_for_sw_assessment,
         Readyfordischargedate=date_declared_medically_fit,
         REASONFORDELAY=dd_code_1,
         REASONFORDELAYSECONDARY=dd_code_2,
         Outofareacaseindicator=out_of_area_case_indicator,
         OriginalAdmissionDate=admission_date,
         Gender=sex_code,
         DateDischarge=discharge_date,
         DischargeReason=discharge_to_code)


#REVIEW FREQUENCY TABLES 
table(datafile$Healthboard)
table(datafile$LocalAuthorityCode)
table(datafile$SpecialtyCode)
table(datafile$REASONFORDELAY)
table(datafile$REASONFORDELAYSECONDARY)
table(datafile$Gender)

#change any REASONFORDELAY blank codes to 11A.
datafile$REASONFORDELAY[datafile$REASONFORDELAY==" "] <- "11A"

table(datafile$REASONFORDELAY)    #check frequency of REASONFORDELAY

# Check if any RDD=DD and remove from analysis
# May not need this as this has already been checked in previous script 2.? check with Jennifer?
datafile <- filter(datafile, Readyfordischargedate!=DateDischarge)


# Recode Gender
table(datafile$Gender)  #precheck frequencies

datafile$Gender[datafile$Gender=="1"] <- "Male"
datafile$Gender[datafile$Gender=="M"] <- "Male"
datafile$Gender[datafile$Gender=="2"] <- "Female"
datafile$Gender[datafile$Gender=="F"] <- "Female"
#datafile$Gender[datafile$Gender==""] <- "Female"
table(datafile$Gender)  #precheck frequencies
#Check to see if unknown gender code has a chi number and code based on 9th digit within CHI.
## How do I select the 9th character of the CHI and check if it's 0,2,4,6,8 for felame and 1,3,5,7,9 for male
View(datafile$CHINo)
# Issue is that there some CHINo with 9 length and some with 10 length
# Need to amend those with 9 length to become 10 by adding the 0 to the start


#str_pad(datafile$CHINo, 10, pad = "0") # could use this or the following script

datafile<-datafile  %>%
  mutate(CHINo=as.character(CHINo)) %>% 
  mutate(CHINo = ifelse(nchar(CHINo) == 9, paste0("0", CHINo), CHINo))
View(datafile$CHINo)
count(datafile, substr(CHINo, 9, 9))
#Change numeric to a string character for CHINo

datafile  %>%
  mutate(CHINo=as.character(CHINo))

# If Gender is blank check CHINo 9th charcter (1,3,5,7,9=Male, 0,2,4,6,8=female)
table(datafile$Gender)

datafile<-datafile %>% 
  mutate(Gender=
           case_when(
             Gender!=""~Gender,
             substr(CHINo,9,9)%in%c("1","3","5","7","9") ~ "Male",
             substr(CHINo,9,9)%in%c("0","2","4","6","8") ~ "Female",
             TRUE~"Unknown"
           ))

table(datafile$Gender)

#Amend REASONFORDELAY from '09' to '9'
#View(datafile$REASONFORDELAY)

datafile$REASONFORDELAY[datafile$REASONFORDELAY=="09"] <- "9"
table(datafile$REASONFORDELAY)

#Check LocalAuthorityCode has been changed ( done in previous script 1.)
table(datafile$LocalAuthorityCode)

#Recode LocalAuthorityCode to Missing if blank
datafile$LocalAuthorityCode[datafile$LocalAuthorityCode==""] <- "Missing"

# HealthLocationCode 1st character to establish Healthboard

# Fix Argyll & Bute boundry issue with Healthlocationcode=="C"



       
#datafile<-datafile %>%
#  mutate(Healthboard2=
#    if_else(substr(HealthLocationCode,1,1)=="A","NHS Ayrshire & Arran",
#    if_else(substr(HealthLocationCode,1,1)=="B","NHS Borders", 
#    if_else(substr(HealthLocationCode,1,1)=="F","NHS Fife",
#    if_else(substr(HealthLocationCode,1,1)=="V","NHS Forth Valley",
 #   if_else(substr(HealthLocationCode,1,1)=="G","NHS Greater Glasgow & Clyde",
#    if_else(substr(HealthLocationCode,1,1)=="H","NHS Highland",
 #   if_else(substr(HealthLocationCode,1,1)=="N","NHS Grampian",
#    if_else(substr(HealthLocationCode,1,1)=="L","NHS Lanarkshire",
 #   if_else(substr(HealthLocationCode,1,1)=="S","NHS Lothian",
#    if_else(substr(HealthLocationCode,1,1)=="Y","NHS Dumfries & Galloway",
#    if_else(substr(HealthLocationCode,1,1)=="T","NHS Tayside",
#    if_else(substr(HealthLocationCode,1,1)=="Z","NHS Shetland",
#    if_else(substr(HealthLocationCode,1,1)=="W","NHS Western Isles",
#    if_else(substr(HealthLocationCode,1,1)=="R","NHS Orkney","Unknown")))))))))))))))


#Now resort using the above file and the unknowns
#datafile<-datafile %>%
#  mutate(Healthboard=
# if_else(substr(HealthLocationCode,1,1)=="C" & Healthboard2=="Unknown","NHS Greater Glasgow & Clyde",
#if_else(substr(HealthLocationCode,1,1)=="H" & Healthboard2=="Unknown","NHS Highland",Healthboard)))
                                                           

#table(datafile$Healthboard=="Unknown")
table(datafile$Healthboard)


    
###. Change discharge reason from code to text



datafile<-datafile%>% mutate(DischargeReason=
if_else(DischargeReason %in%c("1","01"), "Placement",
if_else(DischargeReason %in%c("2","02"), "Discharge Home with Home Care",
if_else(DischargeReason %in%c("3","03"), "Discharge Home",
if_else(DischargeReason %in%c("4","04"), "Death",
if_else(DischargeReason %in%c("5","05"), "Not Fit For Discharge", " "))))))
                                                             
# table(datafile$DischargeReason)      

# If monthflag is missing add it
class(datafile$Monthflag)
datafile$Monthflag[datafile$Monthflag==" "] <- Monthflag
table(datafile$Monthflag)

#Add a leading zero to CHINo

str_pad(datafile$CHINo, 10, pad = "0")

#Check string length of CHINo
#summary(str_length(datafile$CHINo))


#Create dob2 in yyyymmdd format
datafile<-datafile%>% 
  mutate(dob2=as.numeric(paste0(str_sub(PatientDOB,7,10),str_sub(PatientDOB,4,5),
                             str_sub(PatientDOB,1,2))))
#Check class
#class(datafile$dob2)


#Create rmddate as a string
datafile<-datafile%>% 
  mutate(rmddate=as.numeric(paste0(str_sub(Readyfordischargedate,7,10),str_sub(Readyfordischargedate,4,5),
                                str_sub(Readyfordischargedate,1,2))))


#Compute Age at RMD Date ( Ready for Medical Discharge Date )

datafile<-datafile %>% 
  mutate(AGEATREADYFORDISCHARGEDATE2=trunc((rmddate-dob2)/10000))



#check if AGEATREADYFORDISCHARGEDATE2 IS MISSING
DATAFILE2 <- filter(datafile, is.na(AGEATREADYFORDISCHARGEDATE2))

#Ensure 18+ only selected
datafile<-filter(datafile,AGEATREADYFORDISCHARGEDATE2>=18)
#table(datafile$AGEATREADYFORDISCHARGEDATE2)

#check age is within defined parameters ( must be 18 + up to no more than 110)
summary(datafile$AGEATREADYFORDISCHARGEDATE2)

datafile<-datafile%>% mutate(AgeGrouping=
if_else(AGEATREADYFORDISCHARGEDATE2<75, "18-74",
if_else(AGEATREADYFORDISCHARGEDATE2>=75, "75+", " ")))

#Check AgeGrouping
table(datafile$AgeGrouping)

#Keep only Hospital Locations and N465R location ( Grampian carehome with NHS Beds )

datafile<-datafile%>% 
  filter(str_sub(HealthLocationCode,5,5)=="H"|HealthLocationCode=="N465R")

#check HealthLocationCode
table(datafile$HealthLocationCode)

#PREcheck HealthLocationCode
table(datafile$REASONFORDELAY)

#Recode old delay codes

datafile$REASONFORDELAY[datafile$REASONFORDELAY=="41"] <- "25E"
datafile$REASONFORDELAY[datafile$REASONFORDELAY=="41A"] <- "23C"
datafile$REASONFORDELAY[datafile$REASONFORDELAY=="41B"] <- "23D"
datafile$REASONFORDELAY[datafile$REASONFORDELAY=="43"] <- "24A"
datafile$REASONFORDELAY[datafile$REASONFORDELAY=="62"] <- "67"
datafile$REASONFORDELAY[datafile$REASONFORDELAY=="63"] <- "67"

#POSTcheck HealthLocationCode
table(datafile$REASONFORDELAY)
table(datafile$AGEATREADYFORDISCHARGEDATE2)

#ENSURE ALL REASONFORDELAY CODES AND REASONFORDELAYSECONDARY CODES UPPERCASE

table(datafile$REASONFORDELAYSECONDARY)


#CONVERT TO UPPERCASE  ( had to cenvert ALL Variables to UPPERCASE )
datafile%>% mutate_if(is.character, str_to_upper) -> datafile



#High Level Reason Code Grouping
datafile<-datafile%>% mutate(REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check=
if_else(REASONFORDELAY=="100", "Code 100",
if_else(REASONFORDELAY=="9", "Code 9",
if_else(REASONFORDELAY%in%c("11A","11B","23C","23D","24A","24B","24C","24C","24D","24E","24F","27A","25A","25D","25E","25F","44"), "Health and Social Care Reasons",
if_else(REASONFORDELAY%in%c("51","52","61","67","71","72","73","74"),"Patient/Carer/Family-related reasons","Other")))))

#Check output
table(datafile$REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check)

#Reason code groupings
#High Level Reason Code Grouping
datafile<-datafile%>% mutate(REASONCODEGROUPINGSPOSTJULY2016_Check=
if_else(REASONFORDELAY%in%c("11A","11B"), "H&SC - Community Care Assessment",
if_else(REASONFORDELAY%in%c("23C","23D"), "H&SC - Funding",
if_else(REASONFORDELAY%in%c("24A","24B","24C","24D","24E","24F","27A")|REASONFORDELAYSECONDARY%in%c("24DX","24EX"), "H&SC - Place Availability",
if_else(REASONFORDELAY%in%c("25A","25D","25E","25F","44")|REASONFORDELAYSECONDARY%in%c("25X"), "H&SC - Care Arrangements",
if_else(REASONFORDELAY%in%c("51","52")|REASONFORDELAYSECONDARY%in%c("51X"), "Patient/Carer/Family-related reasons:Legal/Financial",
if_else(REASONFORDELAY%in%c("61","67"),"Patient/Carer/Family-related reasons:Disagreements",
if_else(REASONFORDELAY%in%c("71","72","73","74")|REASONFORDELAYSECONDARY%in%c("71X"), "Patient/Carer/Family-related reasons:Other"," "))))))))

#Check output
table(datafile$REASONCODEGROUPINGSPOSTJULY2016_Check) #cant check with tables yet as this is record total ( not just census)
##check as 93 showing as blank-is this code 100?

#Individual Level Reason Code Grouping
datafile<-datafile%>% mutate(DELAY_DESCRIPTION=
if_else(REASONFORDELAY%in%c("11A","11B"), "Awaiting commencement of post-hospital social care assessment(including transfer to another area team). Social Care includes home care and social work OT",
if_else(REASONFORDELAY%in%c("23C"), "Non-availability of statutory funding to purchase Care Home place",
if_else(REASONFORDELAY%in%c("23D"), "Non-availability of statutory funding to purchase any Other Care Home Package", 
if_else(REASONFORDELAY%in%c("24A"), "Awaiting place availablity in Local Authority Residential Home",
if_else(REASONFORDELAY%in%c("24B"), "Awaiting place availablity in Independent Residential Home",
if_else(REASONFORDELAY%in%c("24A"), "Awaiting place availablity in Local Authority Residential Home",
if_else(REASONFORDELAY%in%c("24C"), "Awaiting place availability in Nursing Home",
if_else(REASONFORDELAY%in%c("24D"), "Awaiting place availability in Specialist Residential Facility for younger age groups(<65)",
if_else(REASONFORDELAY=="9"& REASONFORDELAYSECONDARY=="24DX", "Awaiting place availability in Specialist Facility for high level younger age groups (<65) where the Facility is not currently available and no interim option is appropriate",
if_else(REASONFORDELAY=="24E", "Awaiting place availability in Specialist Residential Facility for older age groups(65+)",
if_else(REASONFORDELAY=="9"& REASONFORDELAYSECONDARY=="24EX","Awaiting place availability in Specialist Residential Facility for older age groups(65+) where the Facility is not currently available and an interim option is not appropriate",
if_else(REASONFORDELAY=="24F", "Awaiting place availability in care home (EMI/Dementia bed required)",
if_else(REASONFORDELAY=="9"& REASONFORDELAYSECONDARY=="26X", "Care Home/Facility Closed",
if_else(REASONFORDELAY=="27A", "Awaiting place availability in an intermediate Care facility", 
if_else(REASONFORDELAY=="9"& REASONFORDELAYSECONDARY=="46X", "Ward Closed - patient well but cannot be discharged due to closure",
if_else(REASONFORDELAY=="25A", "Awaiting completion of arrangements for Care Home Placement",
if_else(REASONFORDELAY=="25D", "Awaiting completion of arrangements - in order to live in their own home - awaiting social support(non-availability of service",
if_else(REASONFORDELAY=="25E", "Awaiting completion of arrangements - in order to live in their own home - awaiting procurement/delivery of equipment/adaptations fitted",
if_else(REASONFORDELAY=="25F", "Awaiting completion of arraangements - Re-housing provision(including sheltered housing and homeless patients)",
if_else(REASONFORDELAY=="9"& REASONFORDELAYSECONDARY=="25X", "Awaiting completion of complex care arrangements - in order to live within their own home",
if_else(REASONFORDELAY=="51", "Legal issues (including intervention by patient's lawyer) - e.g. informed consent and / or adult protection issues",
if_else(REASONFORDELAY=="9"& REASONFORDELAYSECONDARY=="51X", "Adults with Incapacity Act",
if_else(REASONFORDELAY=="52", "Financial and personal assets problem - e.g. confirming financial assessment",
if_else(REASONFORDELAY=="61","Internal family dispute issues (including dispute between patient and carer)",
if_else(REASONFORDELAY=="67","Disagreement between patient/carer/family and health and social care",
if_else(REASONFORDELAY=="71","Patient exercising statutory right of choice",
if_else(REASONFORDELAY=="9"& REASONFORDELAYSECONDARY=="71X", "Patient exercising statutory right of choice - interim placement is not possible or reasonable",
if_else(REASONFORDELAY=="72","Patient does not qualify for care",
if_else(REASONFORDELAY=="73","Family/relatives arranging care",
if_else(REASONFORDELAY=="74","Other patient/carer/family-related reason",
if_else(REASONFORDELAY=="44","Awaiting availability of transport",
if_else(REASONFORDELAY=="100","Reprovisioning/Recommissioning(see data definitions manual section 2.3)"," ")))))))))))))))))))))))))))))))))

table(datafile$DELAY_DESCRIPTION) #check

# check that there is no blank REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check
datafile %>% filter(REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check == "") %>% pull(CHINo) #None showing as no CHINo appears

#select out any code 42 as a safeguard.

datafile<-datafile %>% filter(REASONFORDELAY!="42")

# check code 42
datafile %>% filter(REASONFORDELAY =="42") %>% pull(CHINo) # None showing up

#change from string to Date format
datafile<-datafile%>% mutate(PatientDOB = dmy(PatientDOB),
       DateReferralReceived = dmy(DateReferralReceived),
       Readyfordischargedate = dmy(Readyfordischargedate),
       OriginalAdmissionDate = dmy(OriginalAdmissionDate),
       DateDischarge = dmy(DateDischarge))
 

#compute new variable census flag - ignoring 26X and 46X

datafile<-datafile %>% mutate(CENSUSFLAG=
if_else(is.na(DateDischarge) & Readyfordischargedate<censusdate & REASONFORDELAYSECONDARY!=c("26X","46X"),"Y",
if_else(DateDischarge>=censusdate & Readyfordischargedate<censusdate & REASONFORDELAYSECONDARY!=c("26X","46X"),"Y",
if_else(DateDischarge<=censusdate & REASONFORDELAYSECONDARY!=c("26X","46X"),"",
if_else(DateDischarge==Readyfordischargedate & REASONFORDELAYSECONDARY!=c("26X","46X"),"",
if_else(DateDischarge>=censusdate & REASONFORDELAYSECONDARY!=c("26X","46X"),"",""))))))

table(datafile$CENSUSFLAG) #Matches syntax at this point 1586 v 1585 - one extra record is at census point for N465R )

datafile2<-datafile %>% filter(HealthLocationCode=="N465R") 

#sort cases by CHI

datafile<-datafile[order(datafile$CHINo),]

# Check for duplicates
table(duplicated(datafile$CHINo)) # 213 CHINo duplicated

#Create duplicatedCHI variable
datafile<-datafile %>% mutate(duplicatedCHI=
 if_else(duplicated(datafile$CHINo)=="TRUE","1","0"))

table(datafile$duplicatedCHI) # check that above command has worked

##line 551 in syntax referenced

#calculate bed days in current month
datafile<-datafile %>% mutate(CurrentMonthStart=monthstart)
datafile<-datafile %>% mutate(CurrentMonthEnd=monthend)



#Flag if Readyfordischargedate in current month
datafile<-datafile %>% mutate(DRMDInMonth=
  if_else(Readyfordischargedate>=monthstart & Readyfordischargedate<=monthend,"Y"," "))

#Flag if dischargedate in current month
#added in the !is.na element to ensure any N/A DateDischarge is counted too in totals
datafile<-datafile %>% 
  mutate(DateDischargeInMonth= if_else((DateDischarge>=monthstart & DateDischarge<=monthend)
                                       & !is.na(DateDischarge),"Y",""))


#table(datafile$DRMDInMonth)  # check that 
##add in NA for Date Discharge to be monthend

datafile<-datafile %>% mutate(OBDs_intheMonth=
    if_else(DRMDInMonth=="Y" & DateDischargeInMonth=="Y",difftime(DateDischarge, Readyfordischargedate, units = "days"),
    if_else(DRMDInMonth==" " & DateDischargeInMonth=="Y",difftime(DateDischarge, CurrentMonthStart, units = "days")+1,
    if_else(DRMDInMonth=="Y" & DateDischargeInMonth!="Y",difftime(CurrentMonthEnd, Readyfordischargedate, units = "days"),
    if_else(DRMDInMonth==" " & DateDischargeInMonth!="Y",difftime(CurrentMonthEnd, CurrentMonthStart, units = "days")+1,0)))))

table(datafile$DRMDInMonth)           # matches syntax output
table(datafile$DateDischargeInMonth)  # matches syntax output 
table (datafile$OBDs_intheMonth)      # matches syntax output

datafile<-datafile %>% mutate(NoofPatients=1)
table(datafile$NoofPatients) #3790 matches.

write_sav(datafile,paste0(filepath,"scotland_temp.sav"))

datafile2 <- filter(datafile, REASONFORDELAY!="100" & CENSUSFLAG=="Y")


#aggregate
Census_hb <- datafile2 %>% 
  group_by(Healthboard, REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check) %>% 
  summarise(CensusTotal = sum(NoofPatients)) %>%
  ungroup()
glimpse(Census_hb) # matches syntax output
head(Census_hb,42)
table(Census_hb$Healthboard)
table(datafile$Healthboard)

#aggregate
Census_LA <- datafile2 %>% 
  group_by(Healthboard, LocalAuthorityCode, REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check) %>% 
  summarise(CensusTotal = sum(NoofPatients)) %>%
  ungroup()
glimpse(Census_LA) # matches syntax output
head(Census_LA,42)
### END OF SCRIPT ###