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
library(haven)
library(stats)
devtools::install_github("Health-SocialCare-Scotland/phimethods")

### 1.Filepaths for latest month

filepath<-("/conf/delayed_discharges/RAP development/2019_07/Outputs/")
filepath2<-("/conf/delayed_discharges/RAP development/2019_07/Data/scotland/")


censusdate<-dmy("25/07/2019")
monthstart<-dmy("01/07/2019")
monthend<-dmy("31/07/2019")

Monthflag<-("Jul 2019")

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
         LocalAuthorityArea=local_authority_code,
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
table(datafile$LocalAuthorityArea)
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


datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="1"] <- "Aberdeen City"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="2"] <- "Aberdeenshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="3"] <- "Angus"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="4"] <- "Argyll & Bute"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="5"] <- "Scottish Borders"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="6"] <- "Clackmannanshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="7"] <- "West Dunbartonshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="8"] <- "Dumfries & Galloway"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="9"] <- "Dundee City"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="10"] <- "East Ayrshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="11"] <- "East Dunbartonshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="12"] <- "East Lothian"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="13"] <- "East Renfrewshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="14"] <- "City of Edinburgh"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="15"] <- "Falkirk"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="16"] <- "Fife"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="17"] <- "Glasgow City"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="18"] <- "Highland"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="19"] <- "Inverclyde"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="20"] <- "Midlothian"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="21"] <- "Moray"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="22"] <- "North Ayrshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="23"] <- "North Lanarkshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="24"] <- "Orkney"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="25"] <- "Perth & Kinross"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="26"] <- "Renfrewshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="27"] <- "Shetland"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="28"] <- "South Ayrshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="29"] <- "South Lanarkshire"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="30"] <- "Stirling"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="31"] <- "West Lothian"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="32"] <- "Comhairle nan Eilean Siar"
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea=="90"] <- "Unknown"

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
table(datafile$Healthboard)
#Amend REASONFORDELAY from '09' to '9'
#View(datafile$REASONFORDELAY)

datafile$REASONFORDELAY[datafile$REASONFORDELAY=="09"] <- "9"
table(datafile$REASONFORDELAY)

#Check LocalAuthorityArea has been changed ( done in previous script 1.)
table(datafile$LocalAuthorityArea)

#Recode LocalAuthorityArea to Missing if blank
datafile$LocalAuthorityArea[datafile$LocalAuthorityArea==""] <- "Missing"

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

# datafile<-datafile%>%
#   mutate(Healthboard=
#            case_when(str_sub(HealthLocationCode,1,1) =="C"& str_sub(Healthboard,1,1) =="H" ~ "NHS Highland",
#                      str_sub(HealthLocationCode,1,1) =="C"& str_sub(Healthboard,1,1) =="G" ~ "NHS Greater Glasgow & Clyde"))


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

#datafile<-datafile%>% 
#filter(str_sub(HealthLocationCode,5,5)=="H"|HealthLocationCode=="N465R")

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
datafile<-datafile%>% 
  filter(str_sub(HealthLocationCode,5,5)=="H")


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
'%!in%' <- function(x,y)!('%in%'(x,y))
                          
datafile<-datafile %>% mutate(CENSUSFLAG=
if_else(is.na(DateDischarge) & Readyfordischargedate<censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y",
if_else(DateDischarge>=censusdate & Readyfordischargedate<censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y",
if_else(DateDischarge<=censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"",
if_else(DateDischarge==Readyfordischargedate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in% c("26X","46X"),"",
if_else(DateDischarge>=censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"",""))))))


#datafile9 <- filter(datafile, REASONFORDELAY!="100" & CENSUSFLAG=="Y")
#datafile10 <- filter(datafile, REASONFORDELAYSECONDARY%!in%c("26X","46X"))
#table(datafile9$REASONFORDELAYSECONDARY)
#datafile9<-datafile %>% filter(CENSUSFLAG=="Y" & REASONFORDELAYSECONDARY==c("26X","46X"))
# Why is 26X still in there? Why is there two records with a 24B for a secondary code?
table(datafile$CENSUSFLAG) #Matches syntax at this point 1586 v 1585 - one extra record is at census point for N465R )
table(datafile$Healthboard)



#datafile2<-datafile %>% filter(HealthLocationCode=="N465R") 

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
table(datafile$REASONFORDELAYSECONDARY) # checking to see if there are any 26X or 46X

datafile2 <- filter(datafile, REASONFORDELAY!="100" & CENSUSFLAG=="Y")
table(datafile2$REASONFORDELAYSECONDARY) # checking to see if there are any 26X or 46X

         

#aggregate
Census_hb <- datafile2 %>% 
  group_by(Healthboard, REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check) %>% 
  summarise(CensusTotal = sum(NoofPatients)) %>%
  ungroup()
glimpse(Census_hb) # matches syntax output
head(Census_hb,42)


#aggregate at LA level
Census_LA <- datafile2 %>% 
  group_by(Healthboard, LocalAuthorityArea, REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check) %>% 
  summarise(CensusTotal = sum(NoofPatients)) %>%
  ungroup()
glimpse(Census_LA) # matches syntax output
head(Census_LA,42)

#aggregate OBDs at HB level
datafile3 <- filter(datafile, REASONFORDELAY!="100")
OBDs_HB <- datafile3 %>% 
  group_by(Healthboard, REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check) %>% 
  summarise(OBDs_intheMonth = sum(OBDs_intheMonth)) %>%
  ungroup()
glimpse(OBDs_HB)# matches syntax output

#aggregate OBDs at LA level

OBDs_LA <- datafile3 %>% 
  group_by(Healthboard, LocalAuthorityArea, REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check) %>% 
  summarise(OBDs_intheMonth = sum(OBDs_intheMonth)) %>%
  ungroup()
glimpse(OBDs_LA)# matches syntax output

#Line 686 of syntax to start Tuesday

#Add tables

datafile4<-bind_rows(Census_hb,Census_LA,OBDs_HB,OBDs_LA)

datafile4<-datafile4 %>% mutate(REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check=
                                if_else(REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check!="Code 9","HSC/PCF","Code 9"))

datafile4 <- datafile4 %>% 
  rename(DelayCategory=REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check) #rename variable
glimpse(datafile4$CensusTotal)

ProvisionalCensustotal <- datafile4 %>% 
  group_by(Healthboard, LocalAuthorityArea, DelayCategory) %>% 
  summarise(CensusTotal=sum(CensusTotal,na.rm=TRUE)) %>% 
  ungroup()
glimpse(ProvisionalCensustotal)
ProvisionalOBDtotal <- datafile4 %>% 
  group_by(Healthboard, LocalAuthorityArea, DelayCategory) %>% 
  summarise(OBDs_intheMonth = sum(OBDs_intheMonth,na.rm=TRUE)) %>%
  ungroup()

#Produce Provisional Census and OBD Totals Table
ProvisionalCensusandOBDTotals<-bind_rows(ProvisionalCensustotal,ProvisionalOBDtotal)

write_csv(ProvisionalCensusandOBDTotals,paste0(filepath,"Provisional Census and OBD totals.csv"))


table(datafile$HealthLocationCode)

#Line 751 in spss syntax onwards.

#Read in sav file
datafile<-read_sav(paste0(filepath,"scotland_temp.sav"))


table(datafile$NoofPatients)


##Calculate Del;ay at Census at Census POint
datafile<-datafile %>% mutate(DelayatCensus=
  if_else(CENSUSFLAG=="Y" & Readyfordischargedate<censusdate,difftime(censusdate, Readyfordischargedate, units = "days"),0))

#sum(datafile$DelayatCensus) # check the total matches the syntax output totals - slight difference due to N465R?

datafile<-datafile%>% mutate(DelayLengthGroup=
  if_else(DelayatCensus >= 1 & DelayatCensus <= 3, "1-3 days",
  if_else(DelayatCensus > 3 & DelayatCensus <= 14, "3-14 days",
  if_else(DelayatCensus > 14 & DelayatCensus <= 28, "2-4 weeks",
  if_else(DelayatCensus > 28 & DelayatCensus <= 42, "4-6 weeks",
  if_else(DelayatCensus > 42 & DelayatCensus <= 84, "6-12 weeks",
  if_else(DelayatCensus > 84 & DelayatCensus <= 182.625, "3-6 months",
  if_else(DelayatCensus > 182.625 & DelayatCensus < 365, "6-12 months",
  if_else(DelayatCensus >= 365, "12 months or more","N/A")))))))))

table(datafile$DelayLengthGroup)

#Create counts for each delay period
datafile$Delay1to3days[datafile$DelayatCensus >=1 &datafile$DelayatCensus<=3] <-1
datafile$Delay3to14days[datafile$DelayatCensus >3 &datafile$DelayatCensus<=14] <-1
datafile$Delay2to4weeks[datafile$DelayatCensus >14 &datafile$DelayatCensus<=28] <-1
datafile$Delay4to6weeks[datafile$DelayatCensus >28 &datafile$DelayatCensus<=42] <-1
datafile$Delay6to12weeks[datafile$DelayatCensus >42 &datafile$DelayatCensus<=84] <-1
datafile$Delay3to6months[datafile$DelayatCensus >84 &datafile$DelayatCensus<=182.625] <-1
datafile$Delay6to12months[datafile$DelayatCensus >182.625 &datafile$DelayatCensus<=365] <-1
datafile$DelayOver12months[datafile$DelayatCensus >365] <-1
datafile$DelayOver3days[datafile$DelayatCensus >3] <-1
datafile$DelayUnder2wks[datafile$DelayatCensus <=14] <-1
datafile$DelayOver2wks[datafile$DelayatCensus>14] <-1
datafile$DelayOver4wks[datafile$DelayatCensus >28] <-1
datafile$DelayOver6wks[datafile$DelayatCensus >42] <-1

table(datafile$Delay1to3days)


###Read in sav file
#data_validated<-read_sav(paste0(filepath2,"SCOTLAND_validated.sav"))


#Rename variable
datafile <- datafile %>% rename(hosp = HealthLocationCode)
         
#match in hospital names

# Add hospital name
lookup_hosp <-
  read_sav("/conf/delayed_discharges/Data files/Single Submissions (July 2016 onwards)/Acute hospital lookup.sav")

datafile <- left_join(datafile, lookup_hosp,
                       by = c("hosp" = "hosp"))

#Create count for each type of location - should we classify L465R as acute too as it's acute beds being allocated?
datafile<-datafile %>% mutate(acute=
                                  if_else(is.na(hospname),0,1))

datafile<-datafile %>% mutate(gpled=
                                if_else(acute %!in%c("1") & SpecialtyCode=="E12",1,0))


datafile<-datafile %>% mutate(notgpled=
                                if_else(acute %!in%c("1") & gpled %!in%c("1"),1,0))

#Check that each row only has one of either acute, carehome, gpled or notgpled

datafile<-datafile %>% mutate(total=acute+gpled+notgpled)
table(datafile$total)

Check<-filter(datafile,total>1)
table(Check) # nothing found in check

#remove unwanted variables
datafile<-select(datafile,-total,-hospname)

#Rename variable back to its original name
datafile <- datafile %>% rename(HealthLocationCode = hosp)

#select out code 100s
datafile2<-filter(datafile,REASONFORDELAY!="100")

#Add year
datafile2<-datafile2 %>% mutate(year="2018/19")

#rename variables

datafile2 <- datafile2 %>% 
  rename(hbname =Healthboard,
         month = Monthflag,
         chi=CHINo,
         la=LocalAuthorityArea,
         age=AgeGrouping,
         daysdelayed=DelayatCensus,
         reas1=REASONCODEGROUPINGSHIGHLEVELPOSTJULY2016_Check,
         reas2=REASONCODEGROUPINGSPOSTJULY2016_Check,
         reas3=DELAY_DESCRIPTION,
         totpats=NoofPatients,
         delayreason=REASONFORDELAY,
         secReason=REASONFORDELAYSECONDARY)

#subgroupings
datafile2$reas2[datafile2$reas3=="Adults with Incpacity Act"] <- "Adults with Incapacity Act"
datafile2$reas2[datafile2$reas1=="Code 9" & datafile2$reas3 %!in%c("Adults with Incapacity Act")] <- "Other code 9 reasons(not AWI)"
datafile2$reas2[datafile2$reas3=="Awaiting availablity of transport"] <- "Transport"


write_sav(datafile2,paste0(filepath,"SCOTLAND_PROVISIONAL_TEMP_R.sav"))

read_spss(datafile2,paste0(filepath,"SCOTLAND_PROVISIONAL_TEMP_R.sav"))
datafile3 <- haven::read_spss(paste0(filepath,"SCOTLAND_PROVISIONAL_TEMP_R.sav"))



datafile4<-filter(datafile3,CENSUSFLAG=="Y")
datafile4$level<-"1"
datafile4$areaname<-"Scotland"

Scotlandbymainreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE),meandelay=mean(daysdelayed),
            mediandelay=median(daysdelayed)) %>% 
  ungroup()

Scotlandbysubreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas2) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE),meandelay=mean(daysdelayed),
            mediandelay=median(daysdelayed)) %>% 
  ungroup()


#Breakdown for HBs (Level=2)
datafile4$level<-"2"
datafile4<-datafile4 %>% mutate(areaname=hbname)


HBbymainreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

HBbysubreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas2) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#Breakdown for LAs (Level=2)
datafile4$level<-"3"
datafile4<-datafile4 %>% mutate(areaname=la)


LAbymainreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

{LAbysubreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas2) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()}

#add files together

Scot_HB_LA<-bind_rows(Scotlandbymainreasongrouping,Scotlandbysubreasongrouping,HBbymainreasongrouping,HBbysubreasongrouping,
                  LAbymainreasongrouping,LAbysubreasongrouping)

datafile5<-select(Scot_HB_LA,-meandelay,-mediandelay)

#if reas1 is blank, reas1 = reas2

datafile6<-datafile5%>% mutate(reas1=
                               if_else(is.na(reas1), reas2,reas1))

#remove reas2
datafile6<-select(datafile6,-reas2)
datafile6$age<-"All"
ScotHBLAallages<- datafile6 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

ScotHBLAallreasonexcHSCPatFamtotal<-bind_rows(Scot_HB_LA,ScotHBLAallages)
table(ScotHBLAallreasonexcHSCPatFamtotal$reas1)

#Calculate total number of delays excluding code9s.
datafile7<-filter(datafile6, reas1%in%c("Health and Social Care Reasons","Patient/Carer/Family-related reasons"))
datafile7$reas1<-"All Delays excl. Code 9"

ScotHBLAallreasonsincHSCPatFamtotal<- datafile7 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#calculate the number of delays for all reasons

datafile8<-filter(ScotHBLAallreasonexcHSCPatFamtotal, reas1%in%c("Health and Social Care Reasons","Code 9",
                                                                 "Patient/Carer/Family-related reasons"))
table(datafile8$reas1) # check totals against the syntax output ( 320 total is correct )

datafile8$reas1<-"All"
ScotHBLAallreasonsalldelaystotal<- datafile8 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#add files
CensusdataPROVISIONALforVARIANCETABLES<-
  bind_rows(ScotHBLAallreasonsalldelaystotal,ScotHBLAallreasonsincHSCPatFamtotal,ScotHBLAallreasonexcHSCPatFamtotal)


### END OF SCRIPT ###