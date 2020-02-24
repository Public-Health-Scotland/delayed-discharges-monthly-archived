#Validation script for Monthly delayed discharges file creation

 
#bring in environment 
source("00_setup_environment.R")

census_date<-ymd("2019/07/25")
month_start<-ymd("2019/07/01")
month_end<-ymd("2019/07/31")

Monthflag<-("Jul 2019")
nhs_board<-("glasgow")

filepath<-("/conf/delayed_discharges/RAP development/2019_07/Outputs/")
filepath2<-("/conf/delayed_discharges/RAP development/2019_07/Data/glasgow/")

### Get data file ( csv )

datafile<-read.csv(paste0(filepath2,nhs_board,".csv"))

datafile <-
  datafile %>% clean_names()


#REVIEW FREQUENCY TABLES 
table(datafile$nhs_board)
table(datafile$local_authority_code)
table(datafile$discharge_specialty_nat_code)
table(datafile$dd_code_1)
table(datafile$dd_code_2)
table(datafile$sex_code)

trimws(datafile$chi_number) # trim the chi_number to ensure no rogue spaces at beginning or end

#str_pad(datafile$chi_number, 10, pad = "0") # could use this or the following script

datafile<-datafile  %>%
  mutate(chi_number=as.character(chi_number)) %>% 
  mutate(chi_number = ifelse(nchar(chi_number) == 9, paste0("0", chi_number), chi_number))
View(datafile$chi_number)
count(datafile, substr(chi_number, 9, 9))
#Change numeric to a string character for chi_number

datafile  %>%
  mutate(chi_number=as.character(chi_number))

###. Change discharge reason from code to text



datafile<-datafile%>% mutate(discharge_to_code=
    if_else(discharge_to_code %in%c("1","01"), "Placement",
    if_else(discharge_to_code %in%c("2","02"), "Discharge Home with Home Care",
    if_else(discharge_to_code %in%c("3","03"), "Discharge Home",
    if_else(discharge_to_code %in%c("4","04"), "Death",
    if_else(discharge_to_code %in%c("5","05"), "Not Fit For Discharge", " "))))))

table(datafile$discharge_to_code)

#Check that variable length of postcode is 7 or less
datafile<-datafile %>% mutate(postcode_chars=
  if_else(nchar(datafile$postcode)>7,1,0))

table(datafile$postcode_chars) # no postcodes with more than 7 characters

# Fix formatting of postcode variable
datafile <-  datafile %>%
  
  # First remove all spaces from postcode variable
  mutate(postcode = gsub("\\s", "", postcode),
         
         # Then add space (or spaces) at appropriate juncture (depending on
         # the number of characters) to get the postcode into 7-character
         # format
         postcode = case_when(
           is.na(postcode) ~ NA_character_,
           str_length(postcode) == 5 ~ sub("(.{2})", "\\1  ", postcode),
           str_length(postcode) == 6 ~ sub("(.{3})", "\\1 ", postcode),
           TRUE ~ postcode))


#datafile$FirstDoM <- month_start  # FirstDoM becomes month_start
#datafile$LastDoM <- month_end # LastDoM becomes month_end

#Create dob2 in yyyymmdd format
datafile<-datafile%>% 
  mutate(dob2=as.numeric(paste0(str_sub(date_of_birth,7,10),str_sub(date_of_birth,4,5),
                                str_sub(date_of_birth,1,2))))

#Create ready_medical_discharge_date as a string
datafile<-datafile%>% 
  mutate(ready_medical_discharge_date=as.numeric(paste0(str_sub(date_declared_medically_fit,7,10),str_sub(date_declared_medically_fit,4,5),
                                   str_sub(date_declared_medically_fit,1,2))))


#Compute Age at RMD Date ( Ready for Medical Discharge Date )

datafile<-datafile %>% 
  mutate(age_at_rdd=trunc((ready_medical_discharge_date-dob2)/10000))

# Check there are no cases with a missing age_at_rdd
df_missingage_at_rdd<-filter(datafile,is.na(datafile$age_at_rdd))

#select if age_at_rdd>=18

table(datafile$age_at_rdd)

#amend dates to same formats
datafile$date_declared_medically_fit<-format(as.Date(datafile$date_declared_medically_fit,"%d/%m/%Y"),"%Y/%m/%d")
datafile$admission_date<-format(as.Date(datafile$admission_date,"%d/%m/%Y"),"%Y/%m/%d")
datafile$discharge_date<-format(as.Date(datafile$discharge_date,"%d/%m/%Y"),"%Y/%m/%d")
datafile$date_of_birth<-format(as.Date(datafile$date_of_birth,"%d/%m/%Y"),"%Y/%m/%d")
month_start<-format(as.Date(month_start,"%d/%m/%Y"),"%Y/%m/%d")
month_end<-format(as.Date(month_end,"%d/%m/%Y"),"%Y/%m/%d")


#Keep only hospital locations or N465R (in Grampian)
datafile<-datafile%>% 
  filter(str_sub(discharge_hospital_nat_code,5,5)=="H" | discharge_hospital_nat_code=="N465R")


### Check if any RDD=DD ( select if RDD<>DD)  ----
datafile <- filter(datafile, is.na(discharge_date) | date_declared_medically_fit!=discharge_date)

#ensure no records where RDD=LastDOM

datafile<-filter(datafile,date_declared_medically_fit!=month_end)

# compute Age Groupings ( ensures no-one aged under 18 is selected)
datafile<-datafile%>% mutate(age_grp=
                    if_else(age_at_rdd<75, "18-74",
                    if_else(age_at_rdd>=75, "75+", " ")))

#Check age_grp
table(datafile$age_grp)


datafile<-datafile %>% mutate(reason_group_high_level=
                      if_else(dd_code_1=="100","Code 100",
                      if_else(dd_code_1=="9","Code 9",
                      if_else(dd_code_1 %in%c("11A","11B","23C","23D","24A","24B","24C","24D","24E","24F","27A","25A","25D","25E","25F","44"),"Health and Social Care Reasons",
                      if_else(dd_code_1%in%c("51","52","61","67","71","72","73","74"),"Patient/Carer/Family-related reasons","")))))

table(datafile$reason_group_high_level) # check no outliers that haven't been coded

#High Level Reason Code Grouping
datafile<-datafile%>% mutate(reason_group=
        if_else(dd_code_1%in%c("11A","11B"), "H&SC - Community Care Assessment",
        if_else(dd_code_1%in%c("23C","23D"), "H&SC - Funding",
        if_else(dd_code_1%in%c("24A","24B","24C","24D","24E","24F","27A")|dd_code_2%in%c("24DX","24EX"), "H&SC - Place Availability",
        if_else(dd_code_1%in%c("25A","25D","25E","25F")|dd_code_2%in%c("25X"), "H&SC - Care Arrangements",
        if_else(dd_code_1=="44","H&SC-Transport",
        if_else(dd_code_1%in%c("51","52"), "Patient/Carer/Family-related reasons:Legal/Financial",
        if_else(dd_code_1%in%c("61","67"),"Patient/Carer/Family-related reasons:Disagreements",
        if_else(dd_code_1%in%c("71","72","73","74"), "Patient/Carer/Family-related reasons:Other",
        if_else(dd_code_2%in%c("71X","25X","24EX","24DX"),"Other Code 9 reasons",
        if_else(dd_code_2=="51X","Adults with Incapacity Act"," ")))))))))))

#Check output
table(datafile$reason_group) #cant check with tables yet as this is record total ( not just census)



datafile<-datafile%>% mutate(DELAY_DESCRIPTION=
          if_else(dd_code_1%in%c("11A","11B"), "Awaiting commencement of post-hospital social care assessment(including transfer to another area team). Social Care includes home care and social work OT",
          if_else(dd_code_1%in%c("23C"), "Non-availability of statutory funding to purchase Care Home place",
          if_else(dd_code_1%in%c("23D"), "Non-availability of statutory funding to purchase any Other Care Home Package", 
          if_else(dd_code_1%in%c("24A"), "Awaiting place availablity in Local Authority Residential Home",
          if_else(dd_code_1%in%c("24B"), "Awaiting place availablity in Independent Residential Home",
          if_else(dd_code_1%in%c("24A"), "Awaiting place availablity in Local Authority Residential Home",
          if_else(dd_code_1%in%c("24C"), "Awaiting place availability in Nursing Home",
          if_else(dd_code_1%in%c("24D"), "Awaiting place availability in Specialist Residential Facility for younger age groups(<65)",
          if_else(dd_code_1=="9"& dd_code_2=="24DX", "Awaiting place availability in Specialist Facility for high level younger age groups (<65) where the Facility is not currently available and no interim option is appropriate",
          if_else(dd_code_1=="24E", "Awaiting place availability in Specialist Residential Facility for older age groups(65+)",
          if_else(dd_code_1=="9"& dd_code_2=="24EX","Awaiting place availability in Specialist Residential Facility for older age groups(65+) where the Facility is not currently available and an interim option is not appropriate",
          if_else(dd_code_1=="24F", "Awaiting place availability in care home (EMI/Dementia bed required)",
          if_else(dd_code_1=="9"& dd_code_2=="26X", "Care Home/Facility Closed",
          if_else(dd_code_1=="27A", "Awaiting place availability in an intermediate Care facility", 
          if_else(dd_code_1=="9"& dd_code_2=="46X", "Ward Closed - patient well but cannot be discharged due to closure",
          if_else(dd_code_1=="25A", "Awaiting completion of arrangements for Care Home Placement",
          if_else(dd_code_1=="25D", "Awaiting completion of arrangements - in order to live in their own home - awaiting social support(non-availability of service",
          if_else(dd_code_1=="25E", "Awaiting completion of arrangements - in order to live in their own home - awaiting procurement/delivery of equipment/adaptations fitted",
          if_else(dd_code_1=="25F", "Awaiting completion of arraangements - Re-housing provision(including sheltered housing and homeless patients)",
          if_else(dd_code_1=="9"& dd_code_2=="25X", "Awaiting completion of complex care arrangements - in order to live within their own home",
          if_else(dd_code_1=="51", "Legal issues (including intervention by patient's lawyer) - e.g. informed consent and / or adult protection issues",
          if_else(dd_code_1=="9"& dd_code_2=="51X", "Adults with Incapacity Act",
          if_else(dd_code_1=="52", "Financial and personal assets problem - e.g. confirming financial assessment",
          if_else(dd_code_1=="61","Internal family dispute issues (including dispute between patient and carer)",
          if_else(dd_code_1=="67","Disagreement between patient/carer/family and health and social care",
          if_else(dd_code_1=="71","Patient exercising statutory right of choice",
          if_else(dd_code_1=="9"& dd_code_2=="71X", "Patient exercising statutory right of choice - interim placement is not possible or reasonable",
          if_else(dd_code_1=="72","Patient does not qualify for care",
          if_else(dd_code_1=="73","Family/relatives arranging care",
          if_else(dd_code_1=="74","Other patient/carer/family-related reason",
          if_else(dd_code_1=="44","Awaiting availability of transport",
          if_else(dd_code_1=="100","Reprovisioning/Recommissioning(see data definitions manual section 2.3)"," ")))))))))))))))))))))))))))))))))

table(datafile$DELAY_DESCRIPTION) #check

#Compute new variable census flag - ignoring code 26X and 46X.

#compute new variable census flag - ignoring 26X and 46X
'%!in%' <- function(x,y)!('%in%'(x,y))
'%notin%' <- Negate('%in%')
datafile<-datafile %>% mutate(Census_Date=census_date)

datafile<-datafile %>% mutate(census_flag=
          if_else(is.na(discharge_date) & date_declared_medically_fit<Census_Date & dd_code_1!="100" & dd_code_2 %!in%c("26X","46X"),"Y",
          if_else(discharge_date>=Census_Date & date_declared_medically_fit<census_date & dd_code_1!="100" & dd_code_2 %!in%c("26X","46X"),"Y",
          if_else(discharge_date<=Census_Date & dd_code_1!="100" & dd_code_2 %!in%c("26X","46X"),"",
          if_else(discharge_date==date_declared_medically_fit & dd_code_1!="100" & dd_code_2 %!in% c("26X","46X"),"",
          if_else(discharge_date>=Census_Date & dd_code_1!="100" & dd_code_2 %!in%c("26X","46X"),"",""))))))

table(datafile$census_flag) # OK here 209 matches census output table publication
#Flag those discharged up to 3 working days after census

#Add in variable census_datePlus3WorkingDays

datafile<-datafile%>% mutate(census_datePlus3WorkingDays=Census_Date+5)

#Flag those with a dischargedate le census_datePlus3WorkingDays

datafile<-datafile %>% mutate(discharge_within_3_days_census=
                                if_else(census_flag=="Y" & discharge_date<=census_datePlus3WorkingDays & dd_code_1!="100" & dd_code_2 %!in%c("26X","46X"),"Y"," "))

table (datafile$discharge_within_3_days_census)
#change "Y" to a count for DischargeWithin3Days
datafile$discharge_within_3_days_census[datafile$discharge_within_3_days_census=="Y"] <- 1
datafile$discharge_within_3_days_census[datafile$discharge_within_3_days_census==""] <- 0
datafile$discharge_within_3_days_census[is.na(datafile$discharge_within_3_days_census)] <- 0


datafile<-datafile %>% mutate(discharge_within_3_days_census=as.numeric(discharge_within_3_days_census)) # change variable to numeric
datafile$discharge_within_3_days_census[is.na(datafile$discharge_within_3_days_census)] <- 0
datafile_check<-datafile %>% filter(discharge_within_3_days_census==1 & is.na(census_flag))


#Calculate Bed Days in Current Month



#convert dates to same format 
datafile$census_datePlus3WorkingDays<-format(as.Date(datafile$census_datePlus3WorkingDays,"%Y-%m-%d"),"%Y/%m/%d")
  

#calculate bed days in current month
#datafile<-datafile %>% mutate(Currentmonth_start=month_start)
#datafile<-datafile %>% mutate(Currentmonth_end=month_end)

#convert dates to same format 
#datafile$Currentmonth_start<-format(as.Date(datafile$Currentmonth_start,"%Y-%m-%d"),"%Y/%m/%d")
#datafile$Currentmonth_end<-format(as.Date(datafile$Currentmonth_end,"%Y-%m-%d"),"%Y/%m/%d")

#test commit works 



#Flag if date_declared_medically_fit in current month
datafile<-datafile %>% mutate(DRMDInMonth=
                                if_else(date_declared_medically_fit>=month_start & date_declared_medically_fit<=month_end,"Y"," "))

#Flag if dischargedate in current month
#added in the !is.na element to ensure any N/A discharge_date is counted too in totals
datafile<-datafile %>% 
  mutate(discharge_dateInMonth= if_else((discharge_date>=month_start & discharge_date<=month_end)
                                       & !is.na(discharge_date),"Y",""))


table(datafile$DRMDInMonth)  # check that 
##add in NA for Date Discharge to be month_end

datafile<-datafile %>% mutate(OBDs_intheMonth=
                    if_else(DRMDInMonth=="Y" & discharge_dateInMonth=="Y",difftime(discharge_date, date_declared_medically_fit, units = "days"),
                    if_else(DRMDInMonth==" " & discharge_dateInMonth=="Y",difftime(discharge_date, month_start, units = "days")+1,
                    if_else(DRMDInMonth=="Y" & discharge_dateInMonth!="Y",difftime(month_end, date_declared_medically_fit, units = "days"),
                    if_else(DRMDInMonth==" " & discharge_dateInMonth!="Y",difftime(month_end, month_start, units = "days")+1,0)))))

table(datafile$DRMDInMonth)           # matches syntax output
table(datafile$discharge_dateInMonth)  # matches syntax output 
table(datafile$OBDs_intheMonth)      # matches syntax output
table(datafile$discharge_hospital_nat_code) # Checking wh
datafile<-datafile %>% mutate(NoofPatients=1)
table(datafile$NoofPatients) #525
table(datafile$dd_code_1)
#Create Query Flags


#Create ready_medical_discharge_date as a string
datafile<-datafile%>% 
  mutate(query_CHI_DOB=
           if_else(paste0(str_sub(date_of_birth,9,10),str_sub(date_of_birth,6,7),str_sub(date_of_birth,3,4))!=str_sub(chi_number,1,6),"Y"," "))

table(datafile$query_CHI_DOB)


###Issues with checking postcode reference file ( one has spaces one doesn't)
#Read in P0stcode directory file
Postcodedirectory<-read_sav("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.sav")
         
#Remove spaces from postcode within datafile

#stringr::str_remove_all(datafile$postcode," ")

# add flag to file showing that postcode is in the Postcodedirectory ( don't flag NK010AA postcodes as these are homeless )

datafile<-datafile %>% dplyr::mutate(flag_pc=if_else(postcode %in% Postcodedirectory$pc7 | postcode=="NK010AA",1,0))

#create new flag for any invalid postcodes
datafile<-datafile %>% mutate(query_PCodeInvalid=
                                if_else(flag_pc==1," ","Y"))


# raed in specialty file

Specialtyfile<-read_sav(paste0("/conf/linkage/output/lookups/Unicode/National Reference Files/specialt.sav"))

#check specialties are correct

datafile<-datafile %>% dplyr::mutate(flag_spec=if_else(discharge_specialty_nat_code %in% Specialtyfile$speccode,1,0))

datafile<-datafile %>% mutate(query_SpecInvalid=
                                if_else(flag_spec==0,"Y"," "))


#Check data discharged is in current month

datafile<-datafile %>%  mutate(query_Month=
                                 if_else(is.na(discharge_date) & discharge_date<month_start,"Y"," "))

#Check RDD in current month

datafile<-datafile %>%  mutate(query_RDD=
                                 if_else(date_declared_medically_fit>month_end,"Y"," "))

#Check for Missing Discharge Reason

datafile<-datafile %>%  mutate(query_MissingDischReas=
                                 if_else(!is.na(discharge_date) & discharge_to_code==" ","Y"," "))

#Check for Missing Discharge Date

datafile<-datafile %>%  mutate(query_MissingDischDate=
                                 if_else(is.na(discharge_date) & discharge_to_code!=" ","Y"," "))

#Check for DischReasonInvalid

datafile<-datafile %>%  mutate(query_DischReasInvalid=
                                 if_else(discharge_to_code%!in%c("Death","Discharge Home","Discharge Home with Home Care",
                                                               "Not Fit For Discharge","Placement"," "),"Y"," "))

#Check for DiscontinuedCode

datafile<-datafile %>%  mutate(query_DiscontinuedCode=
                                 if_else(dd_code_1%in%c("42","62","63"),"Y",
                                 if_else(dd_code_2=="42X","Y"," ")))


#Check for missing 11A date referral codes ( existing 11A codes with no date of referral)

datafile<-datafile %>%  mutate(query_MissingDateRefRec_11A=
                                 if_else(dd_code_1=="11A" & is.na(DateReferralReceived),"Y"," "))
table(datafile$dd_code_1) #check that codes haven't changed
table(datafile$dd_code_2) #check that codes haven't changed

#Set Blank Codes to 11A

datafile<-datafile %>%  mutate(dd_code_1=
                                 if_else(is.na(dd_code_1) & is.na(dd_code_2),"11A",dd_code_1))
table(datafile$dd_code_1) #check that codes haven't changed
table(datafile$dd_code_2) #check that codes haven't changed
#Ensure Primary codes do not end in an 'X'

datafile<-datafile %>%  mutate(query_Reas1endsX=
                                 if_else(paste0(str_sub(dd_code_1,3,3))=="X","Y"," "))

#If Reas1<>Code 9, ensure Reas2=blank 

datafile<-datafile %>%  mutate(query_Reas2Invalid=
                                 if_else(dd_code_1!="9" & dd_code_2!=" ","Y"," "))

#If Reas1=Code 9, Reas2 cannot be blank 

datafile<-datafile %>%  mutate(query_Reas2Invalid=
                                 if_else(dd_code_1=="9" & dd_code_2!=" ","Y"," "))

#If Reas1=Code 9, Reas2 must end with 'X'

datafile<-datafile %>%  mutate(query_Reas2Invalid=
                        if_else(dd_code_1=="9" & str_ends(dd_code_2,"X")=="FALSE","Y"," "))

table(datafile$dd_code_1)
table(datafile$dd_code_2)
#Flag Local Codes
datafile<-datafile %>%  mutate(query_LocalCode=
                                 if_else(
                                   !(dd_code_1 %in% c("11A","11B","23C","24A","24B","24C","24D","24E","24F","27A",
                                                           "25A","25D","25E","25F","43","51","52","61","67","71","72","73",
                                                           "74","44"," ","9","09","41","100")) |
                                     (!is.na(dd_code_2) & 
                                        !(dd_code_2 %in% c("11A","11B","23C","24A","24B","24C","24D","24E","24F","27A",
                                                                         "25A","25D","25E","25F","43","51","52","61","67","71","72","73",
                                                                         "74","44"," ","9","09","41","100","24DX", "24EX","25X","51X","71X",
                                                                         "26X","46X"))),
                                   "Y"," "))

table(datafile$query_LocalCode)
#update query flags for blanks

datafile<-datafile %>%  mutate(query_location=
                                 if_else(discharge_hospital_nat_code==" ","Y"," "))

datafile<-datafile %>%  mutate(query_CHI=
                                 if_else(is.na(chi_number),"Y"," "))

datafile<-datafile %>%  mutate(query_LA=
                                 if_else(local_authority_code%in%c("Missing"," "),"Y"," "))

datafile<-datafile %>%  mutate(query_PCode=
                                 if_else(postcode==" ","Y"," "))

datafile<-datafile %>%  mutate(query_DOB=
                                 if_else(date_of_birth==" ","Y"," "))

datafile<-datafile %>%  mutate(query_sex_code=
                                 if_else(sex_code==" ","Y"," "))

datafile<-datafile %>%  mutate(query_Spec=
                                 if_else(discharge_specialty_nat_code==" ","Y"," "))

datafile<-datafile %>%  mutate(query_RDD=
                                 if_else(is.na(date_declared_medically_fit),"Y"," "))

datafile<-datafile %>%  mutate(query_Reas1=
                                 if_else(is.na(dd_code_1),"Y"," "))

datafile<-datafile %>%  mutate(query_AdmDate=
                                 if_else(is.na(admission_date),"Y"," "))



#Query flags for date errors

datafile<-datafile %>%  mutate(query_RDDltAdmDate=
                                 if_else(date_declared_medically_fit<admission_date,"Y"," "))

datafile<-datafile %>%  mutate(query_DischDateltRDD=
                                 if_else(discharge_date<date_declared_medically_fit,"Y"," "))

datafile<-datafile %>%  mutate(query_DischDateltAdmDate=
                                 if_else(discharge_date<admission_date,"Y"," "))
#OBDle0 error variable

datafile<-datafile %>%  mutate(query_OBDle0=
                                 if_else(census_flag=="Y" & OBDs_intheMonth<=0,"Y"," "))

# check for duplicate chi_number and flag

datafile<-datafile %>% tidylog::group_by(chi_number) %>%
  tidylog::mutate(
    #DuplicateCHI = dplyr::row_number(),
                  DuplicateCHI = if_else(max(dplyr::row_number())>1,"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$DuplicateCHI)

# matches output from spss ( 17 duplicate chi records, 16 with two and one with 3)

# check for duplicate census records and flag
datafile<-arrange(datafile,chi_number,desc(census_flag))


# check for duplicate CENSUS RECORDS and flag

datafile<-datafile %>% tidylog::group_by(chi_number,census_flag) %>%
  tidylog::mutate(query_DuplicateCHI_Census = max(row_number())>1 & census_flag=="Y") %>%
  #query_DuplicateCHI_Census == if_else(max(row_number())>1 & census_flag="Y"),"Y"," ") %>%
  dplyr::ungroup()

table(datafile$query_DuplicateCHI_Census)


# need to capture paired records with errors to investigate reason for duplicate - so update Error code for both.
datafile<-arrange(datafile,chi_number,desc(query_DuplicateCHI_Census))

datafile<-datafile %>% tidylog::group_by(chi_number,query_DuplicateCHI_Census) %>%
  tidylog::mutate(query_DuplicateCHI_Census = max(row_number())>1 & census_flag=="Y") %>%
  #query_DuplicateCHI_Census == if_else(max(row_number())>1 & census_flag="Y"),"Y"," ") %>%
  dplyr::ungroup()

table(datafile$query_DuplicateCHI_Census)
#Where CHI number and Admission Date match - check same RDD date on multiple records?
datafile<-arrange(datafile,chi_number,admission_date,date_declared_medically_fit,discharge_date)

datafile<-datafile %>% tidylog::group_by(chi_number,admission_date,date_declared_medically_fit) %>%
  tidylog::mutate(query_OverlappingDates = if_else(max(row_number())>1,"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#Where CHI number and Admission Date match - check same Death date on multiple records?
datafile<-arrange(datafile,chi_number,admission_date,date_declared_medically_fit,discharge_date)

datafile<-datafile %>% tidylog::group_by(chi_number,admission_date,date_declared_medically_fit) %>%
  tidylog::mutate(query_OverlappingDates = if_else(chi_number==lag(chi_number) & admission_date==lag(admission_date) & 
                                                     date_declared_medically_fit==lag(date_declared_medically_fit) & discharge_date!=lag(discharge_date) & discharge_to_code==lag(discharge_to_code),"Y"," ")) %>%
  dplyr::ungroup()



#RDD before Discharge Date on previous record.
#if CHNO=lagchi_number and admission_date=lagadmission_date and date_declared_medically_fit<lagdate_declared_medically_fit and missing lagdischarge_date
datafile<-datafile %>% tidylog::group_by(chi_number,admission_date,date_declared_medically_fit) %>%
  tidylog::mutate(query_OverlappingDates = if_else(max(row_number())>1 & date_declared_medically_fit<lag(date_declared_medically_fit), "Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#Different RDDs but previous record missing a discharge_date.
datafile<-datafile %>% tidylog::group_by(chi_number,admission_date,date_declared_medically_fit) %>%
  tidylog::mutate(query_OverlappingDates = if_else(chi_number==lag(chi_number) & admission_date==lag(admission_date) & 
                                                     date_declared_medically_fit==lag(date_declared_medically_fit) & discharge_date!=lag(discharge_date),"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#Different admission and ready for discharge dates where RDD on second record before RDD on first record
datafile<-datafile %>% tidylog::group_by(chi_number,discharge_date,date_declared_medically_fit) %>%
  tidylog::mutate(query_OverlappingDates = if_else(chi_number==lag(chi_number) & date_declared_medically_fit<lag(date_declared_medically_fit) 
                                                   | discharge_date<lag(date_declared_medically_fit) | date_declared_medically_fit<lag(discharge_date),"Y"," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#Need to capture paired records with errors to investigate reason for duplicate - so update Error code for both.

datafile<-arrange(datafile,chi_number,desc(query_OverlappingDates))


datafile<-datafile %>% tidylog::group_by(chi_number) %>%
  tidylog::mutate(query_OverlappingDates = if_else(chi_number==lag(chi_number),lag(query_OverlappingDates)," ")) %>%
  dplyr::ungroup()

table(datafile$query_OverlappingDates)

#sort cases by chi_number admission_date date_declared_medically_fit discharge_date

datafile<-arrange(datafile,chi_number,admission_date,date_declared_medically_fit,discharge_date)

#compute Noofpatients=1
datafile$NoofPatients==1

#table(datafile$NoofPatients)

write_sav(datafile,paste0(filepath,"glasgow_temp.sav"))
write.xlsx(datafile,paste0(filepath,"glasgow_temp.xlsx"))



#Create Error file with row per CHI per ERROR

table(datafile$census_flag)

datafile<-datafile %>% mutate(query=
                    if_else(query_Month=="Y"|query_location=="Y"| query_CHI=="Y"|query_LA=="Y"
                            | query_PCode=="Y"| query_PCodeInvalid=="Y"|query_sex_code=="Y"| query_Spec=="Y"
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
table(datafile$query_sex_code) # None
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

query_list2 = subset(query_list, select = c(Monthflag, Census_Date, nhs_board, discharge_hospital_nat_code, local_authority_code,
                                            postcode, DuplicateCHI, chi_number, date_of_birth, discharge_specialty_nat_code, census_flag,
                                            admission_date, DateReferralReceived, date_declared_medically_fit, discharge_date,
                                            discharge_to_code, dd_code_1, dd_code_2, Outofareacaseindicator, 
                                            query_Month, query_location, query_CHI, query_LA, query_PCode, 
                                            query_PCodeInvalid, query_sex_code, query_Spec, query_SpecInvalid, 
                                            query_RDD, query_Reas1, query_AdmDate, query_CHI_DOB, query_LocalCode, 
                                            query_DiscontinuedCode, query_Reas1endsX, query_Reas2Invalid, 
                                            query_RDDltAdmDate, query_DischDateltRDD, query_DischDateltAdmDate, query_MissingDischReas, 
                                            query_MissingDischDate, query_DischReasInvalid, query_OverlappingDates, query_DuplicateCHI_Census, 
                                            query_OBDle0, query_MissingDateRefRec_11A, NoofPatients) )

write_sav(query_list2,paste0(filepath,nhs_board,"_Query_List.sav"))
write.xlsx(query_list2,paste0(filepath,nhs_board,"_Query_List.xlsx"))
# isn't saving out a blank file as no errors!
# If no Query_List.xlsx shows up this means there are no queries.


#recode query_Month TO query_MissingDateRefRec_11A where 'Y' becomes 1.
#need to get number of column for query_variables( columns 36 to 65 )

grep("query_CHI_DOB", colnames(datafile))
grep("query_OverlappingDates", colnames(datafile))

#recoding these queries with a 1 where there is a "Y" to enable aggregation later
datafile[ ,34:64][datafile[ , 34:64] == "Y"] <- as.numeric(1)

datafile<-datafile %>% mutate(query_Reas1=as.numeric(query_Reas1))
datafile<-datafile %>% mutate(query_AdmDate=as.numeric(query_AdmDate))


#above includes alter type query_Month TO query_MissingDateRefRec_11A ( string becomes a numeric)
class(datafile$query_Reas1)
typeof(datafile$query_Reas1)

#aggregate file
table(datafile$query_Reas1)
datafile2 <- datafile %>% 
  group_by(nhs_board, Monthflag) %>% 
  summarise(query_Month = sum(as.numeric(query_Month)),
            query_location=sum(as.numeric(query_location)),
            query_CHI=sum(as.numeric(query_CHI)),
            query_LA=sum(as.numeric(query_LA)),
            query_PCode=sum(as.numeric(query_PCode)),
            query_PCodeInvalid=sum(as.numeric(query_PCodeInvalid)),
            query_sex_code=sum(as.numeric(query_sex_code)),
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

datafile3<-datafile %>% filter(dd_code_1!="100" & (census_flag=="Y" | discharge_within_3_days_census==1))

Census_hb <- datafile3 %>% 
  group_by(nhs_board, discharge_within_3_days_census, reason_group_high_level) %>% 
  summarise(census=n()) %>% 
  ungroup()
 
#Create a Provisional HB/LA census total - excl Code 100.

Census_la<- datafile3 %>% 
  group_by(nhs_board, local_authority_code, discharge_within_3_days_census, reason_group_high_level) %>% 
  summarise(census=n()) %>% 
  ungroup()


#Create a provisional HB OBD total - excl. Code 100.
datafile$OBDs_intheMonth[is.na(datafile$OBDs_intheMonth)] <- 0 # Need to change NA to 0 before aggregate


OBDs_HB<-datafile %>% filter(dd_code_1!="100") %>% 
  group_by(nhs_board, reason_group_high_level) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth)) %>% 
  ungroup()

#Create a provisional HB OBD total - excl. code 100.

OBDs_LA<-datafile %>% filter(dd_code_1!="100") %>% 
  group_by(nhs_board, local_authority_code, reason_group_high_level) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth)) %>% 
  ungroup()


#Combine provisional census / OBD files.

Prov<-bind_rows(Census_la,OBDs_HB,OBDs_LA)


Prov$reason_group_high_level[Prov$reason_group_high_level!="Code 9"] <- "HSC/PCF"

#Rename variable

Prov <- Prov %>% 
  rename(DelayCategory = reason_group_high_level)
         

# Need to amend the NA in discharge_within_3_days_census and census to ensure it works in line 751 - 752.
Prov$discharge_within_3_days_census[is.na(Prov$discharge_within_3_days_census)] <- 0
Prov$census[is.na(Prov$census)] <- 0

Prov<-Prov %>% mutate(discharge_within_3_days_census=
                        if_else(discharge_within_3_days_census==1,census,discharge_within_3_days_census))


#Now set census value to 0 if there is a value in discharge_within_3_days_census column.

Prov<-Prov %>% mutate(census=
                        if_else(discharge_within_3_days_census>0,0,census))
#rename census as census_total
Prov <- Prov %>% 
  rename(census_total = census)

#aggregate 

ProvCensusOBD<-Prov%>% 
  group_by(nhs_board, local_authority_code, DelayCategory) %>% 
  summarise(discharge_within_3_days_census=sum(discharge_within_3_days_census),
            census_total=sum(census_total),
            OBDs_intheMonth=sum(OBDs_intheMonth,na.rm = TRUE)) %>% 
  ungroup()

      
   

