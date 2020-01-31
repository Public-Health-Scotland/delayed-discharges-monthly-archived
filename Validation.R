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


### Get data file ( csv )

datafile<-read_csv(paste0(filepath2,"glasgow.csv"))

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


#Keep only hospital locations or N465R (in Grampian)
datafile<-datafile%>% 
  filter(str_sub(HealthLocationCode,5,5)=="H" | HealthLocationCode=="N465R")


### Check if any RDD=DD ( select if RDD<>DD)  ----
datafile <- filter(datafile, is.na(DateDischarge) | Readyfordischargedate!=DateDischarge)

#ensure no records where RDD=LastDOM

datafile<-filter(datafile,Readyfordischargedate!=monthend)

# compute Age Groupings
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

datafile<-datafile %>% mutate(CENSUSFLAG=
          if_else(is.na(DateDischarge) & Readyfordischargedate<censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y",
          if_else(DateDischarge>=censusdate & Readyfordischargedate<censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y",
          if_else(DateDischarge<=censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"",
          if_else(DateDischarge==Readyfordischargedate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in% c("26X","46X"),"",
          if_else(DateDischarge>=censusdate & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"",""))))))

#Flag those discharged up to 3 working days after census

#Add in variable CensusDatePlus3WorkingDays
datafile<-datafile%>% mutate(CensusDatePlus3WorkingDays=censusdate+5)

#Flag those with a dischargedate le CensusDatePlus3WorkingDays

datafile<-datafile %>% mutate(Dischargewithin3daysCensus=
                                if_else(CENSUSFLAG=="Y" & DateDischarge<=CensusDatePlus3WorkingDays & REASONFORDELAY!="100" & REASONFORDELAYSECONDARY %!in%c("26X","46X"),"Y",""))


#change "Y" to a count for DischargeWithin3Days
datafile$Dischargewithin3daysCensus[datafile$Dischargewithin3daysCensus=="Y"] <- 1
datafile$Dischargewithin3daysCensus[datafile$Dischargewithin3daysCensus==""] <- 0
datafile$Dischargewithin3daysCensus[is.na(datafile$Dischargewithin3daysCensus)] <- 0


datafile<-datafile %>% mutate(Dischargewithin3daysCensus=as.numeric(Dischargewithin3daysCensus)) # change variable to numeric

datafile_check<-datafile %>% filter(datafile$Dischargewithin3daysCensus==1 & is.na(datafile$CENSUSFLAG))


#Calculate Bed Days in Current Month



#convert dates to same format 
datafile$CensusDatePlus3WorkingDays<-format(as.Date(datafile$CensusDatePlus3WorkingDays,"%Y-%m-%d"),"%Y/%m/%d")


#calculate bed days in current month
datafile<-datafile %>% mutate(CurrentMonthStart=monthstart)
datafile<-datafile %>% mutate(CurrentMonthEnd=monthend)

#convert dates to same format 
datafile$CurrentMonthStart<-format(as.Date(datafile$CurrentMonthStart,"%Y-%m-%d"),"%Y/%m/%d")
datafile$CurrentMonthEnd<-format(as.Date(datafile$CurrentMonthEnd,"%Y-%m-%d"),"%Y/%m/%d")
datafile$Readyfordischargedate<-format(as.Date(datafile$Readyfordischargedate,"%d/%m/%Y"),"%Y/%m/%d")
datafile$DateDischarge<-format(as.Date(datafile$DateDischarge,"%d/%m/%Y"),"%Y/%m/%d")
datafile$PatientDOB<-format(as.Date(datafile$PatientDOB,"%d/%m/%Y"),"%Y/%m/%d")
monthstart<-format(as.Date(monthstart,"%d/%m/%Y"),"%Y/%m/%d")
monthend<-format(as.Date(monthend,"%d/%m/%Y"),"%Y/%m/%d")





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
                    if_else(DRMDInMonth==" " & DateDischargeInMonth=="Y",difftime(DateDischarge, CurrentMonthStart, units = "days")+1,
                    if_else(DRMDInMonth=="Y" & DateDischargeInMonth!="Y",difftime(CurrentMonthEnd, Readyfordischargedate, units = "days"),
                    if_else(DRMDInMonth==" " & DateDischargeInMonth!="Y",difftime(CurrentMonthEnd, CurrentMonthStart, units = "days")+1,0)))))

table(datafile$DRMDInMonth)           # matches syntax output
table(datafile$DateDischargeInMonth)  # matches syntax output 
table(datafile$OBDs_intheMonth)      # matches syntax output

datafile<-datafile %>% mutate(NoofPatients=1)
table(datafile$NoofPatients) #525

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
                                if_else(flag_pc==0,"Y"," "))


# raed in specialty file

Specialtyfile<-read_sav(paste0("/conf/linkage/output/lookups/Unicode/National Reference Files/specialt.sav"))

#check specialties are correct

datafile<-datafile %>% dplyr::mutate(flag_spec=if_else(SpecialtyCode %in% Specialtyfile$speccode,1,0))

datafile<-datafile %>% mutate(query_SpecInvalid=
                                if_else(flag_spec==0,"Y"," "))


#Check data dischraged is in current month

datafile<-datafile %>%  mutate(query_Month=
                                 if_else(is.na(DateDischarge) & DateDischarge<monthstart,"Y"," "))

#Check RDD in current month

datafile<-datafile %>%  mutate(query_RDD=
                                 if_else(Readyfordischargedate>monthend,"Y"," "))

#Check for Missing Discharge Reason

datafile<-datafile %>%  mutate(query_MissingDischReas=
                                 if_else(is.na(DateDischarge) & DischargeReason==" ","Y"," "))

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

#Set Blank Codes to 11A

datafile<-datafile %>%  mutate(REASONFORDELAY=
                                 if_else(REASONFORDELAY==" " & REASONFORDELAYSECONDARY==" ","11A"," "))

#Ensure Primary codes do not end in an 'X'

datafile<-datafile %>%  mutate(query_Reas1endsX=
                                 if_else(paste0(str_sub(REASONFORDELAY,3,3))=="X","Y"," "))

#If Reas1<>Code 9, ensure Reas2=blank 

datafile<-datafile %>%  mutate(query_Reas2invalid=
                                 if_else(REASONFORDELAY!="9" & REASONFORDELAYSECONDARY!=" ","Y"," "))

#If Reas1=Code 9, Reas2 cannot be blank 

datafile<-datafile %>%  mutate(query_Reas2invalid=
                                 if_else(REASONFORDELAY=="9" & REASONFORDELAYSECONDARY!=" ","Y"," "))

#If Reas1=Code 9, Reas2 must end with 'X'

datafile<-datafile %>%  mutate(query_Reas2invalid=
                        if_else(REASONFORDELAY=="9" & str_ends(REASONFORDELAYSECONDARY,"X")=="FALSE","Y"," "))

#Flag Local Codes

datafile<-datafile %>%  mutate(query_LocalCode=
                                 if_else(REASONFORDELAY%!in%c("11A","11B","23C","24A","24B","24C","24D","24E","24F","27A",
                                                              "25A","25D","25E","25F","43","51","52","61","67","71","72","73",
                                                              "74","44"," ","9","09","41","100")|REASONFORDELAYSECONDARY%!in%c("24DX",
                                                              "24EX","25X","51X","71X","26X","46X"),"Y"," "))



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
                                 if_else(REASONFORDELAY==" ","Y"," "))

datafile<-datafile %>%  mutate(query_AdmDate=
                                 if_else(is.na(OriginalAdmissionDate),"Y"," "))



#Query flags for date errors

datafile<-datafile %>%  mutate(query_RDDlyAdmDate=
                                 if_else(Readyfordischargedate<OriginalAdmissionDate,"Y"," "))

datafile<-datafile %>%  mutate(query_DischDateltRDD=
                                 if_else(DateDischarge<Readyfordischargedate,"Y"," "))

datafile<-datafile %>%  mutate(query_DischDateltAdmDate=
                                 if_else(DateDischarge<OriginalAdmissionDate,"Y"," "))

#OBDle0 error variable

datafile<-datafile %>%  mutate(query_OBDle0=
                                 if_else(CENSUSFLAG=="Y" & OBDs_intheMonth<=0,"Y"," "))

# check for duplicate CHINo and flag

datafile<-datafile %>% tidylog::group_by(datafile$CHINo) %>%
  tidylog::mutate(DuplicateCHI = dplyr::row_number(),
                  query_DuplicateCHI = if_else(max(row_number())>1,"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$DuplicateCHI)

# matches output from spss ( 17 duplicate chi records, 16 with two and one with 3)

# check for duplicate census records and flag
datafile<-arrange(datafile,datafile$CHINo,desc(datafile$CENSUSFLAG))


# check for duplicate CENSUS RECORDS and flag

datafile<-datafile %>% tidylog::group_by(datafile$CHINo,datafile$CENSUSFLAG) %>%
  tidylog::mutate(DuplicateCHI = dplyr::row_number(),
                  query_DuplicateCHI_Census = if_else(max(row_number())>1,"Y"," ")) %>%
  dplyr::ungroup()




datafile<-datafile %>% mutate(query=
                    if_else(query_Month=="Y"|query_Location=="Y"| query_HospInBoard=="Y"| query_HospInBoard=="Y"| query_CHI=="Y"|query_LA=="Y"
                            | query_PCode=="Y"| query_PCodeInvalid=="Y"|query_DoB=="Y"| query_Gender=="Y"| query_Spec=="Y"
                            | query_SpecInvalid=="Y"| query_RDD=="Y"|query_Reas1=="Y"| query_AdmDate=="Y"| query_CHI_DoB=="Y"
                            | query_LocalCode=="Y"| query_DiscontinuedCode=="Y"| query_Reas1endsX=="Y"| query_Reas2noCode9=="Y"| query_Reas2Invalid=="Y"
                            | query_RDDltAdmDate=="Y"| query_DischDateltRDD=="Y"| query_DischDateltAdmDate=="Y"|query_MissingDischReas=="Y"| query_MissingDischDate=="Y"
                            | query_DischReasInvalid=="Y"| query_OverlappingDates=="Y"| query_Duplicate_CHI_Census=="Y"| query_OBDle0=="Y"
                            | query_MissingDateRefRec_11A=="Y","Y"," "))
