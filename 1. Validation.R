#Validation script for Monthly delayed discharges file creation

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
library(xlsx)
library(tidylog)
devtools::install_github("Health-SocialCare-Scotland/phimethods")

### 1.Filepaths for latest month

filepath<-("/conf/delayed_discharges/RAP development/2019_07/Outputs/")
filepath2<-("/conf/delayed_discharges/RAP development/2019_07/Data/glasgow/")


censusdate<-ymd("2019/07/25")
monthstart<-ymd("2019/07/01")
monthend<-ymd("2019/07/31")

Monthflag<-("Jul 2019")
Healthboard<-("glasgow")

### Get data file ( csv )

datafile<-read_csv(paste0(filepath2,Healthboard,".csv"))

# initial data checks

datafile <- datafile %>% 
  rename(Healthboard = `NHS Board`,
         HealthLocationCode = `Discharge Hospital Nat Code`,
         CHINo=`CHI Number`,
         PatientPostcode=Postcode,
         LocalAuthorityArea=`Local Authority Code`,
         PatientDOB=`Date of Birth`,
         SpecialtyCode=`Discharge Specialty Nat Code`,
         DateReferralReceived=`Date Referred for SW Assessment`,
         Readyfordischargedate=`Date Declared Medically Fit`,
         REASONFORDELAY=DD_Code_1,
         REASONFORDELAYSECONDARY=DD_Code_2,
         Outofareacaseindicator=`Out of Area Case Indicator`,
         OriginalAdmissionDate=`Admission Date`,
         Gender=`Sex Code`,
         DateDischarge=`Discharge Date`,
         DischargeReason=`Discharge To Code`)


#REVIEW FREQUENCY TABLES 
table(datafile$Healthboard)
table(datafile$LocalAuthorityArea)
table(datafile$SpecialtyCode)
table(datafile$REASONFORDELAY)
table(datafile$REASONFORDELAYSECONDARY)
table(datafile$Gender)

trimws(datafile$CHINo) # trim the chino to ensure no rogue spaces at beginning or end

#str_pad(datafile$CHINo, 10, pad = "0") # could use this or the following script

datafile<-datafile  %>%
  mutate(CHINo=as.character(CHINo)) %>% 
  mutate(CHINo = ifelse(nchar(CHINo) == 9, paste0("0", CHINo), CHINo))
View(datafile$CHINo)
count(datafile, substr(CHINo, 9, 9))
#Change numeric to a string character for CHINo

datafile  %>%
  mutate(CHINo=as.character(CHINo))

###. Change discharge reason from code to text



datafile<-datafile%>% mutate(DischargeReason=
    if_else(DischargeReason %in%c("1","01"), "Placement",
    if_else(DischargeReason %in%c("2","02"), "Discharge Home with Home Care",
    if_else(DischargeReason %in%c("3","03"), "Discharge Home",
    if_else(DischargeReason %in%c("4","04"), "Death",
    if_else(DischargeReason %in%c("5","05"), "Not Fit For Discharge", " "))))))

table(datafile$DischargeReason)

#Check that variable length of postcode is 7 or less
datafile<-datafile %>% mutate(postcode_chars=
  if_else(nchar(datafile$PatientPostcode)>7,1,0))

table(datafile$postcode_chars) # no postcodes with more than 7 characters

# Fix formatting of postcode variable
datafile <-  datafile %>%
  
  # First remove all spaces from postcode variable
  mutate(PatientPostcode = gsub("\\s", "", PatientPostcode),
         
         # Then add space (or spaces) at appropriate juncture (depending on
         # the number of characters) to get the postcode into 7-character
         # format
         PatientPostcode = case_when(
           is.na(PatientPostcode) ~ NA_character_,
           str_length(PatientPostcode) == 5 ~ sub("(.{2})", "\\1  ", PatientPostcode),
           str_length(PatientPostcode) == 6 ~ sub("(.{3})", "\\1 ", PatientPostcode),
           TRUE ~ PatientPostcode))


#datafile$FirstDoM <- monthstart  # FirstDoM becomes monthstart
#datafile$LastDoM <- monthend # LastDoM becomes monthend

#Create dob2 in yyyymmdd format
datafile<-datafile%>% 
  mutate(dob2=as.numeric(paste0(str_sub(PatientDOB,7,10),str_sub(PatientDOB,4,5),
                                str_sub(PatientDOB,1,2))))

#Create rmddate as a string
datafile<-datafile%>% 
  mutate(rmddate=as.numeric(paste0(str_sub(Readyfordischargedate,7,10),str_sub(Readyfordischargedate,4,5),
                                   str_sub(Readyfordischargedate,1,2))))


#Compute Age at RMD Date ( Ready for Medical Discharge Date )

datafile<-datafile %>% 
  mutate(AGEATRDD=trunc((rmddate-dob2)/10000))

# Check there are no cases with a missing AgeatRDD
df_missingageatrdd<-filter(datafile,is.na(datafile$AGEATRDD))

#select if Ageatrdd>=18

table(datafile$AGEATRDD)

#amend dates to same formats
datafile$Readyfordischargedate<-format(as.Date(datafile$Readyfordischargedate,"%d/%m/%Y"),"%Y/%m/%d")
datafile$OriginalAdmissionDate<-format(as.Date(datafile$OriginalAdmissionDate,"%d/%m/%Y"),"%Y/%m/%d")
datafile$DateDischarge<-format(as.Date(datafile$DateDischarge,"%d/%m/%Y"),"%Y/%m/%d")
datafile$PatientDOB<-format(as.Date(datafile$PatientDOB,"%d/%m/%Y"),"%Y/%m/%d")
monthstart<-format(as.Date(monthstart,"%d/%m/%Y"),"%Y/%m/%d")
monthend<-format(as.Date(monthend,"%d/%m/%Y"),"%Y/%m/%d")


#Keep only hospital locations or N465R (in Grampian)
datafile<-datafile%>% 
  filter(str_sub(HealthLocationCode,5,5)=="H" | HealthLocationCode=="N465R")


### Check if any RDD=DD ( select if RDD<>DD)  ----
datafile <- filter(datafile, is.na(DateDischarge) | Readyfordischargedate!=DateDischarge)

#ensure no records where RDD=LastDOM

datafile<-filter(datafile,Readyfordischargedate!=monthend)

# compute Age Groupings ( ensures no-one aged under 18 is selected)
datafile<-datafile%>% mutate(AgeGrouping=
                    if_else(AGEATRDD<75, "18-74",
                    if_else(AGEATRDD>=75, "75+", " ")))

#Check AgeGrouping
table(datafile$AgeGrouping)


datafile<-datafile %>% mutate(REASONGRP_HIGHLEVEL=
                      if_else(REASONFORDELAY=="100","Code 100",
                      if_else(REASONFORDELAY=="9","Code 9",
                      if_else(REASONFORDELAY %in%c("11A","11B","23C","23D","24A","24B","24C","24D","24E","24F","27A","25A","25D","25E","25F","44"),"Health and Social Care Reasons",
                      if_else(REASONFORDELAY%in%c("51","52","61","67","71","72","73","74"),"Patient/Carer/Family-related reasons","")))))

table(datafile$REASONGRP_HIGHLEVEL) # check no outliers that haven't been coded

#High Level Reason Code Grouping
datafile<-datafile%>% mutate(REASONGRP=
        if_else(REASONFORDELAY%in%c("11A","11B"), "H&SC - Community Care Assessment",
        if_else(REASONFORDELAY%in%c("23C","23D"), "H&SC - Funding",
        if_else(REASONFORDELAY%in%c("24A","24B","24C","24D","24E","24F","27A")|REASONFORDELAYSECONDARY%in%c("24DX","24EX"), "H&SC - Place Availability",
        if_else(REASONFORDELAY%in%c("25A","25D","25E","25F")|REASONFORDELAYSECONDARY%in%c("25X"), "H&SC - Care Arrangements",
        if_else(REASONFORDELAY=="44","H&SC-Transport",
        if_else(REASONFORDELAY%in%c("51","52"), "Patient/Carer/Family-related reasons:Legal/Financial",
        if_else(REASONFORDELAY%in%c("61","67"),"Patient/Carer/Family-related reasons:Disagreements",
        if_else(REASONFORDELAY%in%c("71","72","73","74"), "Patient/Carer/Family-related reasons:Other",
        if_else(REASONFORDELAYSECONDARY%in%c("71X","25X","24EX","24DX"),"Other Code 9 reasons",
        if_else(REASONFORDELAYSECONDARY=="51X","Adults with Incapacity Act"," ")))))))))))

#Check output
table(datafile$REASONGRP) #cant check with tables yet as this is record total ( not just census)



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

#Compute new variable census flag - ignoring code 26X and 46X.

#compute new variable census flag - ignoring 26X and 46X
'%!in%' <- function(x,y)!('%in%'(x,y))
'%notin%' <- Negate('%in%')
datafile<-datafile %>% mutate(Census_Date=censusdate)

datafile<-datafile %>% mutate(CENSUSFLAG=
          if_else(is.na(DateDischarge) & Readyfordischargedate<Census_Date & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y",
          if_else(DateDischarge>=Census_Date & Readyfordischargedate<censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y",
          if_else(DateDischarge<=Census_Date & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"",
          if_else(DateDischarge==Readyfordischargedate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in% c("26X","46X"),"",
          if_else(DateDischarge>=Census_Date & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"",""))))))

table(datafile$CENSUSFLAG) # OK here 209 matches census output table publication
#Flag those discharged up to 3 working days after census

#Add in variable CensusDatePlus3WorkingDays

datafile<-datafile%>% mutate(CensusDatePlus3WorkingDays=Census_Date+5)

#Flag those with a dischargedate le CensusDatePlus3WorkingDays

datafile<-datafile %>% mutate(Dischargewithin3daysCensus=
                                if_else(CENSUSFLAG=="Y" & DateDischarge<=CensusDatePlus3WorkingDays & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y"," "))

table (datafile$Dischargewithin3daysCensus)
#change "Y" to a count for DischargeWithin3Days
datafile$Dischargewithin3daysCensus[datafile$Dischargewithin3daysCensus=="Y"] <- 1
datafile$Dischargewithin3daysCensus[datafile$Dischargewithin3daysCensus==""] <- 0
datafile$Dischargewithin3daysCensus[is.na(datafile$Dischargewithin3daysCensus)] <- 0


datafile<-datafile %>% mutate(Dischargewithin3daysCensus=as.numeric(Dischargewithin3daysCensus)) # change variable to numeric
datafile$Dischargewithin3daysCensus[is.na(datafile$Dischargewithin3daysCensus)] <- 0
datafile_check<-datafile %>% filter(Dischargewithin3daysCensus==1 & is.na(CENSUSFLAG))


#Calculate Bed Days in Current Month



#convert dates to same format 
datafile$CensusDatePlus3WorkingDays<-format(as.Date(datafile$CensusDatePlus3WorkingDays,"%Y-%m-%d"),"%Y/%m/%d")
  

#calculate bed days in current month
#datafile<-datafile %>% mutate(CurrentMonthStart=monthstart)
#datafile<-datafile %>% mutate(CurrentMonthEnd=monthend)

#convert dates to same format 
#datafile$CurrentMonthStart<-format(as.Date(datafile$CurrentMonthStart,"%Y-%m-%d"),"%Y/%m/%d")
#datafile$CurrentMonthEnd<-format(as.Date(datafile$CurrentMonthEnd,"%Y-%m-%d"),"%Y/%m/%d")

#test commit works 



#Flag if Readyfordischargedate in current month
datafile<-datafile %>% mutate(DRMDInMonth=
                                if_else(Readyfordischargedate>=monthstart & Readyfordischargedate<=monthend,"Y"," "))

#Flag if dischargedate in current month
#added in the !is.na element to ensure any N/A DateDischarge is counted too in totals
datafile<-datafile %>% 
  mutate(DateDischargeInMonth= if_else((DateDischarge>=monthstart & DateDischarge<=monthend)
                                       & !is.na(DateDischarge),"Y",""))


table(datafile$DRMDInMonth)  # check that 
##add in NA for Date Discharge to be monthend

datafile<-datafile %>% mutate(OBDs_intheMonth=
                    if_else(DRMDInMonth=="Y" & DateDischargeInMonth=="Y",difftime(DateDischarge, Readyfordischargedate, units = "days"),
                    if_else(DRMDInMonth==" " & DateDischargeInMonth=="Y",difftime(DateDischarge, monthstart, units = "days")+1,
                    if_else(DRMDInMonth=="Y" & DateDischargeInMonth!="Y",difftime(monthend, Readyfordischargedate, units = "days"),
                    if_else(DRMDInMonth==" " & DateDischargeInMonth!="Y",difftime(monthend, monthstart, units = "days")+1,0)))))

table(datafile$DRMDInMonth)           # matches syntax output
table(datafile$DateDischargeInMonth)  # matches syntax output 
table(datafile$OBDs_intheMonth)      # matches syntax output
table(datafile$HealthLocationCode) # Checking wh
datafile<-datafile %>% mutate(NoofPatients=1)
table(datafile$NoofPatients) #525
table(datafile$REASONFORDELAY)
#Create Query Flags


#Create rmddate as a string
datafile<-datafile%>% 
  mutate(query_CHI_DOB=
           if_else(paste0(str_sub(PatientDOB,9,10),str_sub(PatientDOB,6,7),str_sub(PatientDOB,3,4))!=str_sub(CHINo,1,6),"Y"," "))

table(datafile$query_CHI_DOB)


###Issues with checking postcode reference file ( one has spaces one doesn't)
#Read in P0stcode directory file
Postcodedirectory<-read_sav("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.sav")
         
#Remove spaces from postcode within datafile

#stringr::str_remove_all(datafile$PatientPostcode," ")

# add flag to file showing that postcode is in the Postcodedirectory ( don't flag NK010AA postcodes as these are homeless )

datafile<-datafile %>% dplyr::mutate(flag_pc=if_else(PatientPostcode %in% Postcodedirectory$pc7 | PatientPostcode=="NK010AA",1,0))

#create new flag for any invalid postcodes
datafile<-datafile %>% mutate(query_PCodeInvalid=
                                if_else(flag_pc==1," ","Y"))


# raed in specialty file

Specialtyfile<-read_sav(paste0("/conf/linkage/output/lookups/Unicode/National Reference Files/specialt.sav"))

#check specialties are correct

datafile<-datafile %>% dplyr::mutate(flag_spec=if_else(SpecialtyCode %in% Specialtyfile$speccode,1,0))

datafile<-datafile %>% mutate(query_SpecInvalid=
                                if_else(flag_spec==0,"Y"," "))


#Check data discharged is in current month

datafile<-datafile %>%  mutate(query_Month=
                                 if_else(is.na(DateDischarge) & DateDischarge<monthstart,"Y"," "))

#Check RDD in current month

datafile<-datafile %>%  mutate(query_RDD=
                                 if_else(Readyfordischargedate>monthend,"Y"," "))

#Check for Missing Discharge Reason

datafile<-datafile %>%  mutate(query_MissingDischReas=
                                 if_else(!is.na(DateDischarge) & DischargeReason==" ","Y"," "))

#Check for Missing Discharge Date

datafile<-datafile %>%  mutate(query_MissingDischDate=
                                 if_else(is.na(DateDischarge) & DischargeReason!=" ","Y"," "))

#Check for DischReasonInvalid

datafile<-datafile %>%  mutate(query_DischReasInvalid=
                                 if_else(DischargeReason%!in%c("Death","Discharge Home","Discharge Home with Home Care",
                                                               "Not Fit For Discharge","Placement"," "),"Y"," "))

#Check for DiscontinuedCode

datafile<-datafile %>%  mutate(query_DiscontinuedCode=
                                 if_else(REASONFORDELAY%in%c("42","62","63"),"Y",
                                 if_else(REASONFORDELAYSECONDARY=="42X","Y"," ")))

#Check for missing 11A date referral codes ( existing 11A codes with no date of referral)

datafile<-datafile %>%  mutate(query_MissingDateRefRec_11A=
                                 if_else(REASONFORDELAY=="11A" & is.na(DateReferralReceived),"Y"," "))
table(datafile$REASONFORDELAY) #check that codes haven't changed
table(datafile$REASONFORDELAYSECONDARY) #check that codes haven't changed

#Set Blank Codes to 11A

datafile<-datafile %>%  mutate(REASONFORDELAY=
                                 if_else(is.na(REASONFORDELAY) & is.na(REASONFORDELAYSECONDARY),"11A",REASONFORDELAY))
table(datafile$REASONFORDELAY) #check that codes haven't changed
table(datafile$REASONFORDELAYSECONDARY) #check that codes haven't changed
#Ensure Primary codes do not end in an 'X'

datafile<-datafile %>%  mutate(query_Reas1endsX=
                                 if_else(paste0(str_sub(REASONFORDELAY,3,3))=="X","Y"," "))

#If Reas1<>Code 9, ensure Reas2=blank 

datafile<-datafile %>%  mutate(query_Reas2Invalid=
                                 if_else(REASONFORDELAY!="9" & REASONFORDELAYSECONDARY!=" ","Y"," "))

#If Reas1=Code 9, Reas2 cannot be blank 

datafile<-datafile %>%  mutate(query_Reas2Invalid=
                                 if_else(REASONFORDELAY=="9" & REASONFORDELAYSECONDARY!=" ","Y"," "))

#If Reas1=Code 9, Reas2 must end with 'X'

datafile<-datafile %>%  mutate(query_Reas2Invalid=
                        if_else(REASONFORDELAY=="9" & str_ends(REASONFORDELAYSECONDARY,"X")=="FALSE","Y"," "))

table(datafile$REASONFORDELAY)
table(datafile$REASONFORDELAYSECONDARY)
#Flag Local Codes
datafile<-datafile %>%  mutate(query_LocalCode=
                                 if_else(
                                   !(REASONFORDELAY %in% c("11A","11B","23C","24A","24B","24C","24D","24E","24F","27A",
                                                           "25A","25D","25E","25F","43","51","52","61","67","71","72","73",
                                                           "74","44"," ","9","09","41","100")) |
                                     (!is.na(REASONFORDELAYSECONDARY) & 
                                        !(REASONFORDELAYSECONDARY %in% c("11A","11B","23C","24A","24B","24C","24D","24E","24F","27A",
                                                                         "25A","25D","25E","25F","43","51","52","61","67","71","72","73",
                                                                         "74","44"," ","9","09","41","100","24DX", "24EX","25X","51X","71X",
                                                                         "26X","46X"))),
                                   "Y"," "))

table(datafile$query_LocalCode)
#update query flags for blanks

datafile<-datafile %>%  mutate(query_location=
                                 if_else(HealthLocationCode==" ","Y"," "))

datafile<-datafile %>%  mutate(query_CHI=
                                 if_else(is.na(CHINo),"Y"," "))

datafile<-datafile %>%  mutate(query_LA=
                                 if_else(LocalAuthorityArea%in%c("Missing"," "),"Y"," "))

datafile<-datafile %>%  mutate(query_PCode=
                                 if_else(PatientPostcode==" ","Y"," "))

datafile<-datafile %>%  mutate(query_DOB=
                                 if_else(PatientDOB==" ","Y"," "))

datafile<-datafile %>%  mutate(query_Gender=
                                 if_else(Gender==" ","Y"," "))

datafile<-datafile %>%  mutate(query_Spec=
                                 if_else(SpecialtyCode==" ","Y"," "))

datafile<-datafile %>%  mutate(query_RDD=
                                 if_else(is.na(Readyfordischargedate),"Y"," "))

datafile<-datafile %>%  mutate(query_Reas1=
                                 if_else(is.na(REASONFORDELAY),"Y"," "))

datafile<-datafile %>%  mutate(query_AdmDate=
                                 if_else(is.na(OriginalAdmissionDate),"Y"," "))



#Query flags for date errors

datafile<-datafile %>%  mutate(query_RDDltAdmDate=
                                 if_else(Readyfordischargedate<OriginalAdmissionDate,"Y"," "))

datafile<-datafile %>%  mutate(query_DischDateltRDD=
                                 if_else(DateDischarge<Readyfordischargedate,"Y"," "))

datafile<-datafile %>%  mutate(query_DischDateltAdmDate=
                                 if_else(DateDischarge<OriginalAdmissionDate,"Y"," "))
#OBDle0 error variable

datafile<-datafile %>%  mutate(query_OBDle0=
                                 if_else(CENSUSFLAG=="Y" & OBDs_intheMonth<=0,"Y"," "))

# check for duplicate CHINo and flag

datafile<-datafile %>% tidylog::group_by(CHINo) %>%
  tidylog::mutate(
    #DuplicateCHI = dplyr::row_number(),
                  DuplicateCHI = if_else(max(dplyr::row_number())>1,"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$DuplicateCHI)

# matches output from spss ( 17 duplicate chi records, 16 with two and one with 3)

# check for duplicate census records and flag
datafile<-arrange(datafile,CHINo,desc(CENSUSFLAG))


# check for duplicate CENSUS RECORDS and flag

datafile<-datafile %>% tidylog::group_by(CHINo,CENSUSFLAG) %>%
  tidylog::mutate(query_DuplicateCHI_Census = max(row_number())>1 & CENSUSFLAG=="Y") %>%
  #query_DuplicateCHI_Census == if_else(max(row_number())>1 & CENSUSFLAG="Y"),"Y"," ") %>%
  dplyr::ungroup()

table(datafile$query_DuplicateCHI_Census)


# need to capture paired records with errors to investigate reason for duplicate - so update Error code for both.
datafile<-arrange(datafile,CHINo,desc(query_DuplicateCHI_Census))

datafile<-datafile %>% tidylog::group_by(CHINo,query_DuplicateCHI_Census) %>%
  tidylog::mutate(query_DuplicateCHI_Census = max(row_number())>1 & CENSUSFLAG=="Y") %>%
  #query_DuplicateCHI_Census == if_else(max(row_number())>1 & CENSUSFLAG="Y"),"Y"," ") %>%
  dplyr::ungroup()

table(datafile$query_DuplicateCHI_Census)
#Where CHI number and Admission Date match - check same RDD date on multiple records?
datafile<-arrange(datafile,CHINo,OriginalAdmissionDate,Readyfordischargedate,DateDischarge)

datafile<-datafile %>% tidylog::group_by(CHINo,OriginalAdmissionDate,Readyfordischargedate) %>%
  tidylog::mutate(query_OverlappingDates = if_else(max(row_number())>1,"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#RDD before Discharge Date on previous record.
#if CHNO=lagCHINO and OriginalAdmissionDate=lagOriginalADmissionDate and REadyfordischargedate<lagReadyfordischargedate and missing lagDateDischarge
datafile<-datafile %>% tidylog::group_by(CHINo,OriginalAdmissionDate,Readyfordischargedate) %>%
  tidylog::mutate(query_OverlappingDates = if_else(max(row_number())>1 & Readyfordischargedate<lag(Readyfordischargedate), "Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#Different RDDs but previous record missing a DateDischarge.
datafile<-datafile %>% tidylog::group_by(CHINo,OriginalAdmissionDate,Readyfordischargedate) %>%
  tidylog::mutate(query_OverlappingDates = if_else(CHINo==lag(CHINo) & OriginalAdmissionDate==lag(OriginalAdmissionDate) & 
                                                     Readyfordischargedate==lag(Readyfordischargedate) & DateDischarge!=lag(DateDischarge),"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#Different admission and ready for discharge dates where RDD on second record before RDD on first record
datafile<-datafile %>% tidylog::group_by(CHINo,DateDischarge,Readyfordischargedate) %>%
  tidylog::mutate(query_OverlappingDates = if_else(CHINo==lag(CHINo) & Readyfordischargedate<lag(Readyfordischargedate) 
                                                   | DateDischarge<lag(Readyfordischargedate) | Readyfordischargedate<lag(DateDischarge),"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#Need to capture paired records with errors to investigate reason for duplicate - so update Error code for both.

datafile<-arrange(datafile,CHINo,desc(query_OverlappingDates))


datafile<-datafile %>% tidylog::group_by(CHINo) %>%
  tidylog::mutate(query_OverlappingDates = if_else(CHINo==lag(CHINo),lag(query_OverlappingDates)," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#sort cases by CHINo OriginalAdmissionDate ReadyforDischargeDate DateDischarge

datafile<-arrange(datafile,CHINo,OriginalAdmissionDate,Readyfordischargedate,DateDischarge)

#compute Noofpatients=1
datafile$NoofPatients==1

#table(datafile$NoofPatients)

write_sav(datafile,paste0(filepath,"glasgow_temp.sav"))
write.xlsx(datafile,paste0(filepath,"glasgow_temp.xlsx"))



#Create Error file with row per CHI per ERROR

datafile<-read_sav(paste0(filepath,"glasgow_temp.sav"))


datafile<-datafile %>% mutate(query=
                    if_else(query_Month=="Y"|query_location=="Y"| query_CHI=="Y"|query_LA=="Y"
                            | query_PCode=="Y"| query_PCodeInvalid=="Y"|query_Gender=="Y"| query_Spec=="Y"
                            | query_SpecInvalid=="Y"| query_RDD=="Y"|query_Reas1=="Y"| query_AdmDate=="Y"| query_CHI_DOB=="Y"
                            | query_LocalCode=="Y"| query_DiscontinuedCode=="Y"| query_Reas1endsX=="Y"| query_Reas2Invalid=="Y"
                            | query_RDDltAdmDate=="Y"| query_DischDateltRDD=="Y"| query_DischDateltAdmDate=="Y"|query_MissingDischReas=="Y"| query_MissingDischDate=="Y"
                            | query_DischReasInvalid=="Y"| query_OverlappingDates=="Y"| query_DuplicateCHI_Census=="Y"| query_OBDle0=="Y"
                            | query_MissingDateRefRec_11A=="Y","Y"," "))
#Checks on query file
table(datafile$query)  #448 - must be all of the query_LocalCode
table(datafile$query_Month) #None
table(datafile$query_location) #None
table(datafile$query_CHI) # None
table(datafile$query_LA) # None
table(datafile$query_PCode) # None
table(datafile$query_PCodeInvalid) # None
table(datafile$query_Gender) # None
table(datafile$query_Spec) # None
table(datafile$query_SpecInvalid) # None
table(datafile$query_RDD) #None
table(datafile$query_Reas1) # None
table(datafile$query_AdmDate) # None
table(datafile$query_CHI_DOB) # None
table(datafile$query_LocalCode) # 448 - Check
table(datafile$query_DiscontinuedCode) # None
table(datafile$query_Reas1endsX)  # None
table(datafile$query_Reas2Invalid)
table(datafile$query_RDDltAdmDate) # None
table(datafile$query_DischDateltRDD) # None
table(datafile$query_DischDateltAdmDate) # None
table(datafile$query_DischReasInvalid) # None
table(datafile$query_DischReasInvalid) # None
table(datafile$query_OverlappingDates) # None
table(datafile$query_DuplicateCHI_Census) # 0 Check
table(datafile$query_OBDle0) # None
table(datafile$query_MissingDateRefRec_11A) # None
table(datafile$query_MissingDischDate) # None
table(datafile$query_MissingDischReas) # check 182

#select if query is showing  - May not need this as it selects out all rows
query_list<-datafile %>% filter(datafile$query=="Y")

#Note: No query_HospInBoard, query_DoB or query_Reas2noCode9 queries generated previously so ignored in next command.

query_list2 = subset(query_list, select = c(Monthflag, Census_Date, Healthboard, HealthLocationCode, LocalAuthorityArea,
                                            PatientPostcode, DuplicateCHI, CHINo, PatientDOB, SpecialtyCode, CENSUSFLAG,
                                            OriginalAdmissionDate, DateReferralReceived, Readyfordischargedate, DateDischarge,
                                            DischargeReason, REASONFORDELAY, REASONFORDELAYSECONDARY, Outofareacaseindicator, 
                                            query_Month, query_location, query_CHI, query_LA, query_PCode, 
                                            query_PCodeInvalid, query_Gender, query_Spec, query_SpecInvalid, 
                                            query_RDD, query_Reas1, query_AdmDate, query_CHI_DOB, query_LocalCode, 
                                            query_DiscontinuedCode, query_Reas1endsX, query_Reas2Invalid, 
                                            query_RDDltAdmDate, query_DischDateltRDD, query_DischDateltAdmDate, query_MissingDischReas, 
                                            query_MissingDischDate, query_DischReasInvalid, query_OverlappingDates, query_DuplicateCHI_Census, 
                                            query_OBDle0, query_MissingDateRefRec_11A, NoofPatients) )

write_sav(query_list2,paste0(filepath,Healthboard,"_Query_List.sav"))
write.xlsx(query_list2,paste0(filepath,Healthboard,"_Query_List.xlsx"))
# isn't saving out a blank file as no errors!
# If no Query_List.xlsx shows up this means there are no queries.


#recode query_Month TO query_MissingDateRefRec_11A where 'Y' becomes 1.
#need to get number of column for query_Month and query_MissingDateRefRec_11A ( 41 to 47 )

grep("query_CHI_DOB", colnames(datafile))
grep("query_OverlappingDates", colnames(datafile))

datafile[ ,36:66][datafile[ , 36:66] == "Y"] <- as.numeric(1)

datafile<-datafile %>% mutate(query_Reas1=as.numeric(query_Reas1))
datafile<-datafile %>% mutate(query_AdmDate=as.numeric(query_AdmDate))


#above includes alter type query_Month TO query_MissingDateRefRec_11A ( string becomes a numeric)
typeof(datafile$query_Reas1)
#aggregate file

datafile2 <- datafile %>% 
  group_by(Healthboard, Monthflag) %>% 
  summarise(query_Month = sum(as.numeric(query_Month)),
            query_location=sum(as.numeric(query_location)),
            query_CHI=sum(as.numeric(query_CHI)),
            query_LA=sum(as.numeric(query_LA)),
            query_PCode=sum(as.numeric(query_PCode)),
            query_PCodeInvalid=sum(as.numeric(query_PCodeInvalid)),
            query_Gender=sum(as.numeric(query_Gender)),
            query_Spec=sum(as.numeric(query_Spec)),
            query_SpecInvalid=sum(as.numeric(query_SpecInvalid)),
            query_RDD=sum(as.numeric(query_RDD)),
            query_Reas1=sum(as.numeric(query_Reas1)),
            query_AdmDate=sum(as.numeric(query_AdmDate)),
            query_CHI_DOB=sum(as.numeric(query_CHI_DOB)),
            query_LocalCode=sum(as.numeric(query_LocalCode)),
            query_DiscontinuedCode=sum(as.numeric(query_DiscontinuedCode)),
            query_Reas1endsX=sum(as.numeric(query_Reas1endsX)),
            query_Reas2Invalid=sum(as.numeric(query_Reas2Invalid)),
            query_RDDltAdmDate=sum(as.numeric(query_RDDltAdmDate)),
            query_DischDateltRDD=sum(as.numeric(query_DischDateltRDD)),
            query_DischDateltAdmDate=sum(as.numeric(query_DischDateltAdmDate)),
            query_MissingDischReas=sum(as.numeric(query_MissingDischReas)),
            query_MissingDischDate=sum(as.numeric(query_MissingDischDate)),
            query_DischReasInvalid=sum(as.numeric(query_DischReasInvalid)),
            query_OverlappingDates=sum(as.numeric(query_OverlappingDates)),
            query_DuplicateCHI_Census=sum(as.numeric(query_DuplicateCHI_Census)),
            query_OBDle0=sum(as.numeric(query_OBDle0)),
            query_MissingDateRefRec_11A=sum(as.numeric(query_MissingDateRefRec_11A)))%>%
  ungroup()





#Restructure file

datafile2 %<>%
  pivot_longer(
    cols = starts_with("query"),
    names_to = "Query_Type",
    values_to = "Total"
  )

            

#Provisional Census / OBD figures

datafile<-read_sav(paste0(filepath,"glasgow_temp.sav"))

#Create a provsional HB census total - excl. Code 100.

datafile3<-datafile %>% mutate(REASONFORDELAY!="100" & (CENSUSFLAG=="Y" | Dischargewithin3daysCensus==1))

Census_hb <- datafile3 %>% 
  group_by(Healthboard, Dischargewithin3daysCensus, REASONGRP_HIGHLEVEL) %>% 
  summarise(census=n()) %>% 
  ungroup()

#Create a Provisional HB/LA census total - excl Code 100.

Census_la<- datafile3 %>% 
  group_by(Healthboard, LocalAuthorityArea, Dischargewithin3daysCensus, REASONGRP_HIGHLEVEL) %>% 
  summarise(census=n()) %>% 
  ungroup()


#Create a provisional HB OBD total - excl. Code 100.
datafile$OBDs_intheMonth[is.na(datafile$OBDs_intheMonth)] <- 0 # Need to change NA to 0 before aggregate

OBDs_HB<-datafile %>% mutate(REASONFORDELAY!="100") %>% 
  group_by(Healthboard, REASONGRP_HIGHLEVEL) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth)) %>% 
  ungroup()

#Create a provisional HB OBD total - excl. code 100.

OBDs_LA<-datafile %>% mutate(REASONFORDELAY!="100") %>% 
  group_by(Healthboard, LocalAuthorityArea, REASONGRP_HIGHLEVEL) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth)) %>% 
  ungroup()


#Combine provisional census / OBD files.

Prov<-bind_rows(Census_la,OBDs_HB,OBDs_LA)


Prov$REASONGRP_HIGHLEVEL[Prov$REASONGRP_HIGHLEVEL!="Code 9"] <- "HSC/PCF"

#Rename variable

Prov <- Prov %>% 
  rename(DelayCategory = REASONGRP_HIGHLEVEL)
         

# Need to amend the NA in Dischargewithin3daysCensus and census to ensure it works in line 751 - 752.
Prov$Dischargewithin3daysCensus[is.na(Prov$Dischargewithin3daysCensus)] <- 0
Prov$census[is.na(Prov$census)] <- 0

Prov<-Prov %>% mutate(Dischargewithin3daysCensus=
                        if_else(Dischargewithin3daysCensus==1,census,Dischargewithin3daysCensus))


#Now set census value to 0 if there is a value in Dischargewithin3dayscensus column.

Prov<-Prov %>% mutate(census=
                        if_else(Dischargewithin3daysCensus>0,0,census))
#rename census as CensusTotal
Prov <- Prov %>% 
  rename(CensusTotal = census)

#aggregate 

ProvCensusOBD<-Prov%>% 
  group_by(Healthboard, LocalAuthorityArea, DelayCategory) %>% 
  summarise(Dischargewithin3daysCensus=sum(Dischargewithin3daysCensus),
            CensusTotal=sum(CensusTotal),
            OBDs_intheMonth=sum(OBDs_intheMonth,na.rm = TRUE)) %>% 
  ungroup()




