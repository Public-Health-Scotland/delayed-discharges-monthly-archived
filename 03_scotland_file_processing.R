##########################################################
# Name of file: Scotland File Processing
# Data Release: Delayed Discharges monthly publication
# Original author(s): Jennifer Noall (spss version: James Mc Nally, 
# Deanna Campbell, Peter McClurg)
# Original date: 21/10/2019 (spss version: 08/2016)
# Latest update author (if not using version control)
# Latest update date (if not using version control)
# Latest update description (if not using version control)
# Type of script: Preparation
# Written/run on: R Studio SERVER
# Version of R that the script was most recently run on: 3.5.1
# Description of content: This syntax takes a Scotland csv file and carries out 
# validation using the data definitions as at July 2016.  
# Approximate run time: TBC
##########################################################

# Original SPSS script: \\Isdsf00d03\delayed_discharges\RAP development\SPSS syntax\1-Scotland file Processing.sps

### 1.Housekeeping ----
source("00_setup_environment.R")

# Manual steps:

# Before reading data in, need to copy from EACH board's BO report 
# (FULL DATA DOWNLOAD tab) and past into a new workbook then save as csv.
# NB: any changes to BO report mean that variable list below will need updated

# BEFORE READING IN CSV FILE, OPEN IN EXCEL, DO FIND&REPLACE: COMMA WITH SPACE
# This will remove any commas in specialty description 
# - eg Ear, Nose & Throat - which will cause problems once file read in


### 2.Get Scotland_validated file for latest month ----

# Read in file
datafile <- read_spss(paste0(filepath,"Allboards_R.sav"))

# Create variable with first / last day of month value (defined above).
datafile <- datafile %>%
  mutate(first_dom <- first_dom,
         last_dom <- last_dom)


# Review frequency tables
table(datafile$monthflag)
table(datafile$nhs_board)
table(datafile$monthflag)
table(datafile$discharge_hospital_nat_code)
table(datafile$local_authority_code)
table(datafile$discharge_specialty_nat_code)
table(datafile$discharge_to_code)
table(datafile$dd_code_1)
table(datafile$dd_code_2)
table(datafile$sex_code)
table(datafile$date_declared_medically_fit)

# Format variables and recode
datafile <- datafile %>%
  #CHI NUMBER - Remove leading / trailing spaces & add leading 0 if missing.
  rename(location <- discharge_hospital_nat_code) %>%
  mutate(chi_number <- trimws(datafile$chi_number),
         chi_number <- ifelse(nchar(chi_number) == 9, paste0("0", chi_number), 
                              chi_number),
# View(chi_number)
# count(substr(chi_number, 9, 9)),

         #Format dates
        # Check and recode any cases with missing monthflag
        monthflag <- replace_na(first_dom),
         date_of_birth <- lubridate::dmy(date_of_birth),
         dob2 <- date_of_birth,
         date_referred_for_sw_assessment <- 
                              lubridate::dmy(date_referred_for_sw_assessment),
         date_declared_medically_fit <- 
                              lubridate::dmy(date_declared_medically_fit),
# SPSS syntax used to use 2 variables to calculate OBDs in the month based on 
# full length of delay, not from latest RDD (RDD2)- no longer needed?
Readyfordischargedate <- date_declared_medically_fit,
         admission_date <- lubridate::dmy(admission_date),
         discharge_date <- lubridate::dmy(discharge_date),
# "All formats failed to parse. No formats found." error - special character 
# in data causing to fail?
#   date_declared_medically_fit <- lubridate::dmy(date_declared_medically_fit),
date_declared_medically_fit <- as.Date(date_declared_medically_fit, "%d/%m/%y"),
         # Recode discharge reason:
         discharge_to_code <- case_when(discharge_to_code == "01" ~ "1",
                                       discharge_to_code == "02" ~ "2",
                                       discharge_to_code == "03" ~ "3",
                                       discharge_to_code == "04" ~ "4",
                                       discharge_to_code == "05" ~ "5"),
         # Recode out of area:
         out_of_area_case_indicator <- 
                          case_when(out_of_area_case_indicator == "Yes" ~ "Y",
                                    out_of_area_case_indicator == "No" ~ "N",
                                    out_of_area_case_indicator == "no" ~ "N",
                                    out_of_area_case_indicator == "" ~ "N"),
         # Recode sex
         sex_code <- case_when(sex_code == "1" ~ "Male",
                               sex_code == "M" ~ "Male",
                               sex_code == "2" ~ "Female",
                               sex_code == "F" ~ "Female"))
# Checks
count(datafile,discharge_to_code)
count(datafile,out_of_area_case_indicator)
count(datafile,sex_code)


# If Gender is blank check CHINo 9th charcter (1,3,5,7,9=Male, 0,2,4,6,8=female)
datafile<-datafile %>% 
  mutate(sex_code <- case_when(sex_code != "" ~ sex_code,
                              substr(chi_number,9,9) %in% c("1","3","5","7","9")
                              ~ "Male",
                              substr(chi_number,9,9) %in% c("0","2","4","6","8")
                              ~ "Female",
                              TRUE ~ "Unknown"),
         
         # Recode delay reason
         # Set blank codes to 11A
         dd_code_1 <- case_when(dd_code_1 == " " & dd_code_2 == " " ~ "11A",
                               # recode code 9's
                               dd_code_1 == "09" ~ "9"),
         # reason code variables to upper case
         dd_code_1 <- toupper(dd_code_1),
         dd_code_2 <- toupper(dd_code_2),
         # recode old delay reasons
         dd_code_1 <- case_when(dd_code_1 == "41" ~ "25E",
                               dd_code_1 == "41A" ~ "23C",
                               dd_code_1 == "41B" ~ "23D",
                               dd_code_1 == "43" ~ "24A",
                               dd_code_1 == "27A." ~ "27",
                               dd_code_1 == "62" ~ "67",
                               dd_code_1 == "63" ~ "67"),
         
  # Format postcode variable into pc7
  postcode <- phimethods::postcode(postcode, format = "pc7"),
  
  # Recode LA
  local_authority_code <- case_when(
    local_authority_code == "" ~ "Missing",
    local_authority_code == "1" | local_authority_code == "01"
    | local_authority_code == "Aberdeen" ~ "Aberdeen City",
    local_authority_code == "2" | local_authority_code == "02" ~ "Aberdeenshire",
    local_authority_code == "3" | local_authority_code == "03" ~ "Angus",
    local_authority_code == "4" | local_authority_code == "04"
    | local_authority_code == "Argyll & Bute Council" ~ "Argyll & Bute", 
    local_authority_code == "5" | local_authority_code == "05" ~ "Scottish Borders",
    local_authority_code == "6" | local_authority_code == "06" ~ "Clackmannanshire",
    local_authority_code == "7" | local_authority_code == "07" ~ "West Dunbartonshire",
    local_authority_code == "8" | local_authority_code == "08" ~ "Dumfries & Galloway",
    local_authority_code == "9" | local_authority_code == "09" ~ "Dundee City",
    local_authority_code == "10" ~ "East Ayrshire",
    local_authority_code == "11" ~ "East Dunbartonshire",
    local_authority_code == "12" ~ "East Lothian",
    local_authority_code == "13" ~ "East Renfrewshire",
    local_authority_code == "14" ~ "City of Edinburgh",
    local_authority_code == "15" ~ "Falkirk",
    local_authority_code == "16" ~ "Fife",
    local_authority_code == "17" ~ "Glasgow City",
    local_authority_code == "18" ~ "Highland",
    local_authority_code == "19" ~ "Inverclyde",
    local_authority_code == "20" ~ "Midlothian",
    local_authority_code == "21" ~ "Moray",
    local_authority_code == "22" ~ "North Ayrshire",
    local_authority_code == "23" ~ "North Lanarkshire",
    local_authority_code == "24" ~ "Orkney",
    local_authority_code == "25" ~ "Perth & Kinross",
    local_authority_code == "26" ~ "Renfrewshire",
    local_authority_code == "27" | local_authority_code == "shetland Islands"
    | local_authority_code == "Shetland Islands" ~ "Shetland",
    local_authority_code == "28" ~ "South Ayrshire",
    local_authority_code == "29" ~ "South Lanarkshire",
    local_authority_code == "30" ~ "Stirling",
    local_authority_code == "31" ~ "West Lothian",
    local_authority_code == "32" | local_authority_code == "Eilean Siar"
    ~ "Comhairle nan Eilean Siar",
    local_authority_code == "90" ~ "Other"))


### 3.Exclusions ----

# AGE AT RMD DATE (Ready for Medical Discharge).

# compute Age at Ready for Discharge Date.
# Create interval() using DOB2 and Ready for Discharge Date.
# Then time_length() gives the exact length in "year"
# Note that floor() rounds down to nearest integer to prevent 46.5 years giving
# age as 47
datafile <- datafile %>%
  mutate(age_at_rdd <- lubridate::interval(dob2, date_declared_medically_fit),
         age_at_rdd <- floor(time_length(age_at_rdd, unit = "year")),
         # Check no missing values for age_at_rdd - all should be false.
         age_rdd_missing <- is.na(age_at_rdd))

#unique(age_rdd_missing)

# Ensure ages 18+ only
#unique(datafile$age_at_rdd)

# Keep only hospital locations (i.e. codes ending in H plus carehome in Grampian
#with NHS staffed beds)
datafile <- datafile %>% 
  filter(str_sub(location,5,5) == "H" 
         | location == "N465R") %>%
  
  # ZERO DELAYS
  # 1. Identify any records where Ready for Discharge Date = Discharge Date as 
  # not delays. 
  mutate(RDDsameasDD <- ifelse(!is.na(discharge_date) & 
                                 !is.na(date_declared_medically_fit),
                               difftime(discharge_date, 
                                        date_declared_medically_fit, 
                                        units="days") == 0, FALSE)) %>%
  # Remove zero delays & delete variable
  filter(RDDsameasDD == FALSE) %>%
  select(-RDDsameasDD) %>%

  #2. Remove any records where Ready for Discharge Date = Last Day of Month
  # (last_dom) - these do not become delays until the 1st of the following month.
#  mutate(RDDsameaslast_dom <- case_when(date_declared_medically_fit == last_dom ~ 1)) %>%
  
  # Check how many
  #unique(RDDsameaslast_dom)
  
  # Remove zero delays & delete variable
#  filter(RDDsameaslast_dom == 0) %>%

  mutate(RDDsameaslast_dom <- ifelse(!is.na(date_declared_medically_fit),
                                    date_declared_medically_fit == last_dom, 
                                    FALSE)) %>%
  filter(RDDsameaslast_dom == FALSE) %>%
  select(-RDDsameaslast_dom)

### 4.Derivations ----

# Derive age groups
datafile <- datafile %>%
  mutate(age_grp <- case_when(age_at_rdd <75 ~ "18-74",
                             age_at_rdd >=75 ~ "75+"))

count(datafile, age_grp, age_at_rdd) %>%
  spread(age_grp, n)

# Create Reason Code Groupings

# 1. High level reason code grouping - post July 2016
# Transport grouped under H&SC reasons as per BO report

datafile <- datafile %>% 
  mutate(reasongrp_highlevel <- 
           case_when(is.na(dd_code_1) ~ NA_character_,
                     dd_code_1 == "100" ~ "Code 100", 
                     dd_code_1 == "9" ~ "Code 9",
                     dd_code_1 %in% c("11A","11B","23C","23D","24A","24B","24C","24D",
                                      "24E","24F","27A","25A","25D","25E","25F","44")
                     ~ "Health and Social Care Reasons",
                     dd_code_1 %in% c("51","52","61","67","71","72","73","74")
                                      ~ "Patient/Carer/Family-related reasons"), 
         reasongrp =
           case_when(
             dd_code_1 %in% c("11A","11B") ~ "H&SC - Community Care Assessment",
             dd_code_1 %in% c("23C","23D") ~ "H&SC - Funding",
             dd_code_1 %in% c("24A","24B","24C","24D","24E","24F","27A")
             ~ "H&SC - Place Availability",
             dd_code_1 %in% c("25A","25D","25E","25F")
             ~ "H&SC - Care Arrangements",
             dd_code_1 %in% c("44") ~ "H&SC - Transport",
             dd_code_1 %in% c("51","52") 
             ~ "Patient/Carer/Family-related reasons: Legal/Financial", 
             dd_code_1 %in% c("61","67")
             ~ "Patient/Carer/Family-related reasons: Disagreements",
             dd_code_1 %in% c("71","72","73","74")
             ~ "Patient/Carer/Family-related reasons: Other",
             dd_code_2 %in% c("71X","25X","24EX","24DX")
             ~ "Other code 9 reasons (not AWI)",
             dd_code_2 %in% c("51X") ~ "Adults with Incapacity Act"),
         # 3. Reason Codes INDIVIDUAL - Post July 2016.
         delay_description =
           case_when(
           dd_code_1 == "11A"
           ~ "Awaiting commencement of post-hospital social care assessment (including transfer to another area team). Social care includes home care and social work OT",
           dd_code_1 == "11B" 
           ~ "Awaiting completion of post-hospital social care assessment (including transfer to another area team). Social care includes home care and social work OT",
           dd_code_1 == "23C" 
           ~ "Non-availability of statutory funding to purchase Care Home Place",
           dd_code_1 == "23D" 
           ~ "Non-availability of statutory funding to purchase any Other Care Package",
           dd_code_1 == "24A" 
           ~ "Awaiting place availability in Local Authority Residential Home",
           dd_code_1 == "24B" 
           ~ "Awaiting place availability in Independent Residential Home",
           dd_code_1 == "24C" 
           ~ "Awaiting place availability in Nursing Home",
           dd_code_1 == "24D" 
           ~ "Awaiting place availability in Specialist Residential Facility for younger age groups (<65)",
           dd_code_1 == "24E" 
           ~ "Awaiting place availability in Specialist Residential Facility for older age groups (65+)",
           dd_code_1 == "24F" 
           ~ "Awaiting place availability in care home (EMI/Dementia bed required)",
           dd_code_1 == "27A" 
           ~ "Awaiting place availability in an Intermediate Care facility",
           dd_code_1 == "25A" 
           ~ "Awaiting completion of arrangements for Care Home placement",
           dd_code_1 == "25D" 
           ~ "Awaiting completion of arrangements - in order to live in their own home – awaiting social support (non-availability of services)",
           dd_code_1 == "25E" 
           ~ "Awaiting completion of arrangements - in order to live in their own home – awaiting procurement/delivery of equipment/adaptations fitted",
           dd_code_1 == "25F" 
           ~ "Awaiting completion of arrangements - Re-housing provision (including sheltered housing and homeless patients)",
           dd_code_1 == "51" 
           ~ "Legal issues (including intervention by patient’s lawyer) - e.g. informed consent and/or adult protection issues",
           dd_code_1 == "52" 
           ~ "Financial and personal assets problem - e.g. confirming financial assessment",
           dd_code_1 == "61" 
           ~ "Internal family dispute issues (including dispute between patient and carer)",
           dd_code_1 == "67" 
           ~ "Disagreement between patient/carer/family and health and social  care",
           dd_code_1 == "71" 
           ~ "Patient exercising statutory right of choice",
           dd_code_1 == "72" 
           ~ "Patient does not qualify for care",
           dd_code_1 == "73" 
           ~ "Family/relatives arranging care",
           dd_code_1 == "74" 
           ~ "Other patient/carer/family-related reason",
           dd_code_1 == "44" 
           ~ "Awaiting availability of transport",
           dd_code_1 == "100" 
           ~ "Reprovisioning/Recommissioning (see data definitions manual section 2.3)",
           dd_code_1 == "9" & dd_code_2 == "24DX" 
           ~ "Awaiting place availability in Specialist Facility for high level younger age groups (<65) where the Facility is not currently available and no interim option is appropriate",
           dd_code_1 == "9" & dd_code_2 == "24EX" 
           ~ "Awaiting place availability in Specialist Facility for high level older age groups (65+) where the Facility is not currently available and an interim option is not appropriate",
           dd_code_1 == "9" & dd_code_2 == "26X" ~ "Care Home/facility closed",
           dd_code_1 == "9" & dd_code_2 == "46X" 
           ~ "Ward closed – patient well but cannot be discharged due to closure",
           dd_code_1 == "9" & dd_code_2 == "25X" 
           ~ "Awaiting completion of complex care arrangements - in order to live in their own home",
           dd_code_1 == "9" & dd_code_2 == "51X" 
           ~ "Adults with Incapacity Act",
           dd_code_1 == "9" & dd_code_2 == "71X" 
           ~ "Patient exercising statutory right of choice – interim placement is not possible or reasonable"),
         # Create census flag
         CENSUSFLAG <- case_when(discharge_date == NA_character_
                                & date_declared_medically_fit == census_date 
                                & (dd_code_2 != "26X" | dd_code_2 != "46X") 
                                ~ "Y",
                                discharge_date >= census_date 
                                & date_declared_medically_fit == census_date 
                                & (dd_code_2 != "26X" | dd_code_2 != "46X") 
                                ~ "Y",
                                discharge_date <= census_date 
                                & (dd_code_2 != "26X" | dd_code_2 != "46X") 
                                ~ "",
                                discharge_date == date_declared_medically_fit 
                                & (dd_code_2 != "26X" | dd_code_2 != "46X") 
                                ~ "",
                                discharge_date >= census_date 
                                & (dd_code_2 != "26X" | dd_code_2 != "46X") 
                                ~ ""),
         
         # Flag those discharged up to 3 working days after census 
         # (NOTE census is always last THURS of month).
         
         # 1 Identify Census Date + 3 working days:
         census_date_Plus3WorkingDays <- census_date + days(5),
         
         # 2 Flag those with a discharge date le census_date_Plus3WorkingDays 
         # and check against BO variable CENSUSDISCHARGEWITHIN3WORKINGDAYS.
         Dischargewithin3daysCensus =
           case_when(discharge_date == NA_character_ ~ "NA",
                     CENSUSFLAG == "Y" & 
                       discharge_date < census_date_Plus3WorkingDays& 
                       dd_code_1 != "100" &
                       (dd_code_2 != "26X" & dd_code_2 != "46X") ~ "Y"),
         
         # Change 'Y' to count for DischargeWithin3DaysCensus.
         Dischargewithin3daysCensus <- case_when(
           Dischargewithin3daysCensus == "Y" ~ 1,
           Dischargewithin3daysCensus == "" ~ 0),
         
         # Check - should be zero cases returned.
         # censusflagcheck <- filter(datafile, Dischargewithin3daysCensus = 1 & CENSUSFLAG == "")
         
         # count(censusflagcheck, chi_number, CENSUSFLAG) %>%
         #   spread(CENSUSFLAG, n)
         
         
         # Calculate OBDs in the current month
         
         # Create variable with month start date
         current_month_start <- monthflag,
         # Create variable with month end date (add 1 month to start date)
         current_month_end <- current_month_start %m+% months(1),
         # Then subtract one day to get the last day of the month.
         current_month_end <- current_month_end %m-% days(1),
         
         ### 4.Next section ----

         # Flag whether date_declared_medically_fit in current month
         DRMDInMonth <- case_when(
           date_declared_medically_fit >= current_month_start & 
             date_declared_medically_fit <= current_month_end ~ "Y"),
         
         # Flag whether discharge date in current month
         DateDischargeInMonth <- case_when(
           discharge_date >= current_month_start & 
             discharge_date <= current_month_end ~ "Y"),
         
         # Now there are 2 variables which will make it easier to calculate OBDs
         # in the month.
         
         # NB: when substracting one date from another, SPSS does this as a 
         # numeric sum - eg 31-1 = 30. So if a patient is ready for discharge 
         # WITHIN current month, and we want to know how many days they have 
         # waited by the end of the month, the above calculation works, because 
         # it doesn't include day 1 as a delay - which is fine because we don't 
         # count this (RDD) either.
         
         #However, for patients with a ready for discharge date (RDD) BEFORE the 
         # current month: 
         # if we want to know how long they have waitied by the end of the month,
         # the SPSS calculation of month end minus month start (31-1) = 30 is 
         # inaccurate in terms of capturing days delayed. This is why the 
         # computations below ADD 1 to records where DRMD is not in current 
         # month.  DC 040816.
         
         OBDs_intheMonth <- case_when(
           DRMDInMonth == "Y" & DateDischargeInMonth == "Y" ~
             as.period(lubridate::interval(date_declared_medically_fit, 
                                           discharge_date), "days"),
           DRMDInMonth == NA_character_ & DateDischargeInMonth == "Y" ~
             as.period(lubridate::interval(first_dom, discharge_date), "days"),# + days(1),
           DRMDInMonth == "Y" & DateDischargeInMonth != "Y" ~
             as.period(lubridate::interval(date_declared_medically_fit, 
                                           last_dom), "days"),
           DRMDInMonth == NA_character_ & DateDischargeInMonth != "Y" ~ 
             as.period(lubridate::interval(first_dom, last_dom), "days")# + days(1)
         ),
         # OBDS in the current month - END.
         
         # Check for records where OriginalDateReadyforDischarge (ORDD) <> 
         # DateReadyforMedicalDischarge (DRMD).
         #   mutate(ORDDneDRMD <- if(OriginalDateReadyforDischarge != DateReadyforMedicalDischarge)
         #     ~  "Y") %>%
         #   filter(ORDDneDRMD == 'Y' & 
         #            OriginalDateReadyforDischarge > DateReadyforMedicalDischarge) %>% 
         # #unique(datafile$Healthboard)
         #   mutate(RDD2 <- case_when(ORDDneDRMD == 'Y' & OriginalDateReadyforDischarge > 
         #                             DateReadyforMedicalDischarge 
         #                           ~ OriginalDateReadyforDischarge,
         #                           ORDDneDRMD == 'Y' & DateReadyforMedicalDischarge > 
         #                             OriginalDateReadyforDischarge 
         #                           ~ DateReadyforMedicalDischarge),
         
         # NOTE: HAVE NOT UPDATED Readyfordischargedate TO LATEST RDD (RDD2) 
         # BECAUSE Readyfordischargedate IS STILL REQUIRED TO CALCULATE OBDs WHICH
         # NEED TO COUNT FULL LENGTH OF DELAY - NOT JUST FROM LATEST RDD.   DC 241016
         
         # NOW CALCULATE DELAY AT CENSUS BASED ON LATEST READY FOR DISCHARGE DATE.  DC 201016
         # Second compute clause updated to calculate length of delay at census from ORIGINAL RDD where the RDD2 (latest) is AFTER the census date
         # DelayatCensus <- case_when(
         #   CENSUSFLAG == 'Y' & ORDDneDRMD == 'Y' & RDD2 < Census_Date 
         #   ~ lubridate::interval(RDD2, Census_Date),
         #   CENSUSFLAG == 'Y' & ORDDneDRMD == 'Y' & RDD2 > Census_Date 
         #   ~ lubridate::interval(OriginalDateReadyforDischarge, Census_Date),
         #   CENSUSFLAG == 'Y' 
         #   ~ lubridate::interval(Readyfordischargedate, Census_Date)),
         # LENGTH OF DELAY AT CENSUS POINT.
         DelayatCensus <- case_when(CENSUSFLAG == "Y" & 
                              date_declared_medically_fit < census_date 
                              ~ lubridate::interval(date_declared_medically_fit, 
                                                    census_date),
                              CENSUSFLAG == "" ~ 0),
         # DELAY LENGTH GROUP 
         # Values:
         # 1-3 days
         # 3-14 days
         # 2-4 weeks
         # 4-6 weeks
         # 7-12 weeks
         # 3-5 months
         # 6-11 months
         # 12 months or more
         # >2 weeks, 4 weeks, 6 weeks
         # 
         # DELAY VARIABLES TO BE CREATED & POPULATED WITH COUNTS:
         #   Delay1to3days
         # Delay3to14days
         # Delay2to4weeks
         # Delay4to6weeks
         # Delay6to12weeks
         # Delay3to5months
         # Delay6to11months
         # DelayOver12months
         # DelayOver3days
         # DelayUnder2wks
         # DelayOver2wks
         # DelayOver4wks
         # DelayOver6wks
         week <- 7,
         month <- 30.4375,
         DelayLengthGroup <- case_when(DelayatCensus >= 1 & DelayatCensus <= 3
                                       ~ "1-3 days",
                                      DelayatCensus > 3 & DelayatCensus <= 14
                                      ~ "3-14 days",
                                      DelayatCensus > 14 & DelayatCensus <= 28
                                      ~ "2-4 weeks",
                                      DelayatCensus > 28 & DelayatCensus <= 42
                                      ~ "4-6 weeks",
                                      DelayatCensus > 42 & DelayatCensus <= 84
                                      ~ "6-12 weeks",
                                      DelayatCensus > (6 * month) &
                                        DelayatCensus <= (12 * month) 
                                      ~ "6-12 months",
                                      DelayatCensus > 84 & DelayatCensus 
                                      <= (6*month) ~ "3-6 months",
                                      DelayatCensus > (12 * month) 
                                      ~ "12 months or more",
                                      CENSUSFLAG == "" ~ NA_character_),
         # Create counts for each delay period
         Delay1to3days <- dplyr::if_else(DelayatCensus >= 1 & 
                                           DelayatCensus <= 3, 1, 0),
         Delay3to14days <- dplyr::if_else(DelayatCensus > 3 &
                                            DelayatCensus <= 14, 1, 0),
         Delay2to4weeks <- dplyr::if_else(DelayatCensus > 14 &
                                            DelayatCensus <= 28, 1, 0),
         Delay4to6weeks <- dplyr::if_else(DelayatCensus > 28 &
                                            DelayatCensus <= 42, 1, 0),
         Delay6to12weeks <- dplyr::if_else(DelayatCensus > 42 &
                                             DelayatCensus <= 84, 1, 0),
         Delay3to6months <- dplyr::if_else(DelayatCensus > 84 &
                                             DelayatCensus <= (6*month), 1, 0),
         Delay6to12months <- dplyr::if_else(DelayatCensus > (6*month) &
                                              DelayatCensus <= (12*month), 1, 0),
         DelayOver12months <- dplyr::if_else(DelayatCensus > (12*month), 1, 0),
         DelayOver2wks <- dplyr::if_else(DelayatCensus > 14, 1, 0),
         DelayOver4wks <- dplyr::if_else(DelayatCensus > 28, 1, 0),
         DelayOver6wks <- dplyr::if_else(DelayatCensus > 42, 1, 0)) %>%

  # DUPLICATE RECORDS
  
  #Check for duplicate CHI numbers and flag.
  
  dplyr::arrange(chi_number) %>%
  dplyr::mutate(duplicate_CHI <- 
            dplyr::if_else(chi_number == dplyr::lag(chi_number), "Y", "N")) %>%
  # Need to capture paired records with errors to investigate reason for 
  # duplicate - so update Error code for both
  dplyr::arrange(chi_number, desc(duplicate_CHI)) %>%
  if(duplicate_CHI == "N" & chi_number == dplyr::lag(chi_number)) 
    mutate(duplicate_CHI == dplyr::lag(duplicate_CHI)) %>%
  # Check for duplicate CENSUS records and flag
  dplyr::arrange(chi_number, desc(CENSUSFLAG)) %>%
  dplyr::mutate (error_Duplicate_CHI_Census == 
                   dplyr::if_else(chi_number == lag(chi_number) & 
                                    CENSUSFLAG == 'Y' & 
                                    CENSUSFLAG == lag(CENSUSFLAG), "Y", "N")) %>%
# unique error_DuplicateCHI_Census
  # Need to capture paired records with errors to investigate reason for 
  # duplicate - so update Error code for both..
  dplyr::arrange(chi_number, desc(error_Duplicate_CHI_Census)) %>%
  if(error_DuplicateCHI_Census == "N" & chi_number == dplyr::lag(chi_number) & 
     CENSUSFLAG == "Y") mutate(error_DuplicateCHI_Census == "Y") %>%
  
  # DELAY LOCATION (acute, gpled, notgpled) while spec still in data.
  # Note: lookup below is Acute hospitals ONLY.
  # get file = '/conf/delayed_discharges/Acute hospital lookup2.sav'.
  dplyr::arrange(location)
  
  # Create location name lookup.
################################################################################
# Use online version of location names? does this differ from local DD copy???
  hospital_lookup <- bind_rows(read_spss(paste0(
    plat_filepath, "lookups/Unicode/National Reference Files/",
    "location.sav")) %>%
      select(Location, Locname) %>%
      rename(location      = Location,
             location_name = Locname),
    read_spss(paste0(plat_filepath,
                     "lookups/Unicode/National Reference Files/",
                     "Health_Board_Identifiers.sav")) %>%
      select(description, HB_Area_2014) %>%
      rename(location      = HB_Area_2014,
             location_name = description),
    tibble(location = "Scot", location_name = "Scotland"),
    tibble(location = "S08000029", location_name = "NHS Fife"),
    tibble(location = "S08000030", location_name = "NHS Tayside"),
    tibble(location = "S08000031", location_name = "NHS Greater Glasgow & Clyde"),
    tibble(location = "S08000032", location_name = "NHS Lanarkshire"))
# Match on location names  
datafile <- datafile %>%
  tidylog::left_join(hospital_lookup, by = "location") %>%
  dplyr::mutate(acute <- dplyr::if_else(is.na(location_name), 1, 0),
                gpled <- dplyr::if_else(acute == 0 & 
                                          discharge_specialty_nat_code == "E12",
                                        1, 0),
                notgpled <- dplyr::if_else(acute == 0 & gpled == 0, 1, 0),
                # Check to ensure each row only has one of either acute, 
                # carehome, gpled or notgpled marked as one.
                total <- acute + gpled + notgpled,
                check2 <- dplyr::if_else(total == 1, 0, 1)) %>%
# unique freq vars check2.
  dplyr::select(-c(total, check2, hospname)) %>%
# DERIVATIONS END
  
  # Set NoofPatients variable to 1 for all records (incl Islands/d&g).
  mutate(NoofPatients <- 1) %>%
#unique NoofPatients

# save outfile = !Filepath + 'scotland_temp.sav'.


# PRODUCE PROVISIONAL CENSUS / OBD FIGURES FOR CHECKING AGAINST VERIFICATION FORMS.
# Check the number of census records match those in Verification form.
# Number should equal total cases where CENSUSFLAG = Y.
# get file = !Filepath + 'scotland_temp.sav'.


# Create a provisional HB census total - excl Code 100.
filter(dd_code_1 != "100" & (CENSUSFLAG == 'Y' | 
                               DischargeWithin3daysCensus == 1)) %>%
  tidylog::group_by(nhs_board, DischargeWithin3daysCensus, 
                    reasongrp_highlevel) %>%
  tidylog::summarise(census_total_hb <- n())
  dplyr::ungroup() %>%
#DATASET NAME Census_hb.


# Create a provisional HB/LA census total - excl Code 100.
  filter(dd_code_1 != "100" & (CENSUSFLAG == 'Y' | 
                                 DischargeWithin3daysCensus == 1)) %>%
    tidylog::group_by(nhs_board, local_authority_code, 
                      DischargeWithin3daysCensus, reasongrp_highlevel) %>%
    tidylog::summarise(census_total_la <- n())
  dplyr::ungroup() %>%
# DATASET NAME Census_la.


# Create a provisional HB OBD total- excl Code 100
  filter(dd_code_1 != "100") %>%
    tidylog::group_by(nhs_board, reasongrp_highlevel) %>%
    tidylog::summarise(OBDs_in_Month_hb = sum(OBDs_intheMonth))
  dplyr::ungroup() %>%
# DATASET NAME OBDs_hb.
  # Create a provisional LA OBD total- excl Code 100
  filter(dd_code_1 != "100") %>%
    tidylog::group_by(nhs_board, local_authority_code, reasongrp_highlevel) %>%
    tidylog::summarise(OBDs_in_Month_la = sum(OBDs_intheMonth))
  dplyr::ungroup() %>%
    if(reasongrp_highlevel != "Code 9") mutate(reasongrp_highlevel <- "HSC/PCF") %>%
    dplyr::rename(delay_category = reasongrp_highlevel) %>%
    tidyr::replace_na(list(census_total_hb = 0,
                           census_total_la = 0,
                           OBDs_in_Month_hb = 0,
                           OBDs_in_Month_la = 0)) %>%
    # Copy Census Figure to DischargeWithin3daysCensus column.
  mutate_if(DischargeWithin3daysCensus == 1, 
            DischargeWithin3daysCensus <- census_total) %>%
    # Now set Census value to 0 if there is a value in 
    # DischargeWithin3daysCensus column.
  mutate_if(DischargeWithin3daysCensus > 0, census_total <- 0) %>%
    # This is so that both columns can be aggregated then summed to get a Census 
    # figure including those discharged within 3 days of census.
    tidylog::group_by(nhs_board, local_authority_code, delay_category) %>%
################################################################################
#rename these!
  tidylog::summarise(census_DischargeWithin3daysCensus == 
                       sum(DischargeWithin3daysCensus), census = sum(census),
                       census_OBDs_in_Month = sum(OBDs_intheMonth))
  dplyr::ungroup() %>%
  arrange(nhs_board, delay_category) %>%
  mutate(census_total_2 <- census_DischargeWithin3daysCensus + census) %>%
    write_csv(here("Provisional Census and OBD totals.csv"))


  
datafile <- datafile %>%
#needed???
  mutate(RDD_day <- date_declared_medically_fit,
         disch_day <- discharge_date)
# ADD SPECIALTY DESCRIPTION.
# Original lookup file here
  arrange(dischare_specialty_nat_code)
  
  specialty_group <- read_spss(paste0(plat_filepath, "delayed_discharges/Data files/Single Submissions (July 2016 onwards)/Specialty.sav"))

    datafile <- datafile %>%
    tidylog::left_join(specialty_group, by = "discharge_specialty_nat_code" = "SpecialtyCode") %>%
# unique SpecialtyDesc
      
# ADD HOSPITAL NAME - for ALL hospitals, not just Acute..
      dplyr::arrange(location) %>%
      tidylog::left_join(select(hospital_lookup, c(Location, Locname), 
                                by = "location")) %>%
      rename(health_location_code = Location, hosp_name = Locname) %>%
      
      # MATCH FILE WITH LATEST NATIONAL POSTCODE DIRECTORY FILE - This will 
      # add DATAZONE2011.
      
      postcode_lookup <- read_spss(paste0(plat_filepath, 
              "lookups/Unicode/Unicode/Geography/Scottish Postcode Directory/",
              "Scottish_Postcode_Directory_2019_2.sav")) %>%
          select(pc7, DataZone2011Code) %>%
          rename(postcode = pc7,
                 dz_2011  = DataZone2011Code)
    
    dz_2011_lookup <- read_spss(paste0(plat_filepath, 
                                    "lookups/Unicode/Geography/HSCP Locality/",
                                    "HSCP Localities_DZ11_Lookup_20180903.sav")) %>%
      select(Datazone2011, HSCPLocality, HSCP2019Name) %>%
      rename(dz_2011 = Datazone2011,
             HSCP_Locality_derived = HSCPLocality,
             HSCP_2019_Name_derived  = HSCP2019Name)
        
      datafile <- datafile %>%
        dplyr::arrange(postcode) %>%
        tidylog::left_join(postcode_lookup, by = "postcode") %>%
      # NEXT, USE DATAZONE TO MATCH ON HSCP LOCALITY AND HSCP, USING LOOKUP
        dplyr::arrange(dz_2011) %>%
        tidylog::left_join(postcode_lookup, by = "dz_2011") %>%
          
        # Save as Scotland file.
        dplyr::arrange(Healthboard,CHINo,Readyfordischargedate)
        
        ### 3 - Save data ----
      write_csv(datafile, paste0(filepath, census_date, "_SCOTLAND.csv"))

# SAVE OUTFILE=!Filepath + 'SCOTLAND.sav'
# /KEEP MONTHFLAG
# Healthboard
# LocalAuthorityArea
# PatientPostcode
# DataZone2011
# HSCPLocality_derived
# HSCP2019_derived
# Outofareacaseindicator
# CHINo
# DuplicateCHI
# error_DuplicateCHI_Census
# CENSUSFLAG
# Census_Date
# HealthLocationCode
# hospname
# Gender
# PatientDOB
# AGEATRDD
# AgeGrouping
# SpecialtyCode
# SpecialtyDesc
# OriginalAdmissionDate
# DateReferralReceived
# Readyfordischargedate
# RDD_Day
# DateDischarge
# DischDay
# Dischargewithin3daysCensus
# DateDischargeInMonth
# DischargeReason
# OBDs_intheMonth
# DelayatCensus
# REASONFORDELAY
# REASONFORDELAYSECONDARY
# REASONGRP_HIGHLEVEL
# REASONGRP
# DELAY_DESCRIPTION
# NoofPatients
# DelayLengthGroup
# Delay1to3days
# Delay3to14days
# Delay2to4weeks
# Delay4to6weeks
# Delay6to12weeks
# Delay3to6months
# Delay6to12months
# DelayOver12months
# DelayOver3days
# DelayUnder2wks
# DelayOver2wks
# DelayOver4wks
# DelayOver6wks
# acute
# gpled
# notgpled
# FirstDoM
# LastDoM
# dob2
# rmddate
# CensusDate_Plus3WorkingDays
# CURRENT_MONTH_START
# CURRENT_MONTH_END
# DRMDInMonth
# week
# month
# /COMPRESSED.
# EXECUTE.
      
# Save as Scotland_validated (same file with some unnecessary variables removed)
      datafile <- datafile %>%
        select(monthflag, nhs_board, local_authority_code, postcode, dz_2011,
               HSCP_locality_derived, HSCP_2019_derived, 
               out_of_area_case_indicator, chi_number, duplicat_chi, CENSUSFLAG,
               census_date, location, hosp_name, gender, date_of_birth, 
               age_at_rdd, age_grp, discharge_specialty_nat_code, 
               specialty_group, admission_date, date_referred_for_sw_assessment,
               Readyfordischargedate, RDD_day, discharge_date, 
               dischargewithin3daysCensus, discharge_to_code, OBDs_in_Month, 
               delay_at_census, dd_code_1, dd_code_2, delay_description, 
               no_of_pats, delay_length_group, Delay1to3days, Delay3to14days,
               Delay2to4weeks, Delay4to6weeks, Delay6to12weeks, Delay3to6months,
               Delay6to12months, DelayOver12months, DelayOver2wks, 
               DelayOver4wks, DelayOver6wks, acute, gpled, notgpled) %>%
        rename(reas1 = reasongrp_highlevel,
               reas2 = reasongrp,
               OBDs = OBDs_intheMonth) %>%
        write_csv(paste0(filepath, census_date,
                        "_SCOTLAND_validated.csv")) %>%

# Remove any records which have the RDD and Discharge Date the same as they will
# have zero delays.
        mutate_if(Readyfordischargedate=DateDischarge, removed = "Yes") %>%
        filter(removed != "Yes") %>%
        write_csv(paste0(filepath, census_date,
                         "_removed.csv")) %>%
        select(-removed) %>%
        write_csv(paste0(filepath, census_date,
                         "SCOTLAND_validated.csv"))

# Need an excel file in addition to csv?
# SAVE TRANSLATE OUTFILE= !Filepath + 'SCOTLAND_validated.xlsx'
# /TYPE=XLS
# /VERSION=12
# /MAP
# /REPLACE
# /FIELDNAMES
# /COMPRESSED
# /CELLS=VALUES.


### END OF SCRIPT ###