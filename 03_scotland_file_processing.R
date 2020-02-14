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
datafile <- readr::read_csv(paste0(filepath,"Allboards_R.csv")) %>%
  janitor::clean_names()

# Create variable with first / last day of month value (defined above).
# datafile <- datafile %>%
#   mutate(first_dom = first_dom,
#          last_dom = last_dom)


# Format variables and recode
datafile <- datafile %>%
  #CHI NUMBER - Remove leading / trailing spaces & add leading 0 if missing.
  dplyr::rename(location = discharge_hospital_nat_code) %>%
  mutate(chi_number = trimws(chi_number),
         chi_number = ifelse(nchar(chi_number) == 9, paste0("0", chi_number), 
                              chi_number),
         #Format dates
         # replace missing dates with NA 
         discharge_date = dplyr::na_if(discharge_date, ""),
                            date_declared_medically_fit = 
                            dplyr::na_if(date_declared_medically_fit, ""),
         date_of_birth = lubridate::dmy(date_of_birth),
         date_referred_for_sw_assessment = 
           lubridate::dmy(date_referred_for_sw_assessment),
         date_declared_medically_fit = 
           lubridate::dmy(date_declared_medically_fit),
# SPSS syntax used to use 2 variables to calculate OBDs in the month based on 
# full length of delay, not from latest RDD (RDD2)- no longer needed?
         ready_for_discharge_date = date_declared_medically_fit,
         admission_date = lubridate::dmy(admission_date),
         discharge_date = lubridate::dmy(discharge_date),
         # "All formats failed to parse. No formats found." error - special character 
         # in data causing to fail?
         #   date_declared_medically_fit = lubridate::dmy(date_declared_medically_fit),
         date_declared_medically_fit = as.Date(date_declared_medically_fit, "%d/%m/%y"),
         # Recode discharge reason:
         discharge_to_code <- case_when(discharge_to_code == "01" ~ "1",
                                        discharge_to_code == "02" ~ "2",
                                        discharge_to_code == "03" ~ "3",
                                        discharge_to_code == "04" ~ "4",
                                        discharge_to_code == "05" ~ "5",
                                        discharge_to_code == "06" ~ "6",
                                       discharge_to_code == "07" ~ "7"),
         # Recode out of area:
         out_of_area_case_indicator = case_when(out_of_area_case_indicator == "Yes" ~ "Y",
                     out_of_area_case_indicator == "No" ~ "N",
                     out_of_area_case_indicator == "no" ~ "N",
                     out_of_area_case_indicator == "" ~ "N"),
         # Recode sex 
         sex_code = case_when(sex_code == "1" ~ "Male",
                              sex_code == "M" ~ "Male",
                              sex_code == "2" ~ "Female",
                              sex_code == "F" ~ "Female",
                              # If Gender is blank check CHINo 9th charcter 
                              # (1, 3, 5, 7, 9 = Male, 0, 2, 4, 6 , 8 = female)
                              sex_code != "" ~ sex_code,
                              substr(chi_number,9,9) %in% c("1","3","5","7","9")
                              ~ "Male",
                              substr(chi_number,9,9) %in% c("0","2","4","6","8")
                              ~ "Female",
                              TRUE ~ "Unknown"),

         # Recode delay reason
         # Set blank codes to 11A
         dd_code_1 <- case_when(!is.na(dd_code_1) & dd_code_1 == " " & 
                                  dd_code_2 == " " ~ "11A",
                               # recode code 9's
                               !is.na(dd_code_1) & dd_code_1 == "09" ~ "9"),
         # reason code variables to upper case
         dd_code_1 <- case_when(is.na(dd_code_1) ~ NA_character_,
                               TRUE ~ toupper(dd_code_1)),
         dd_code_2 <- case_when(is.na(dd_code_2) ~ NA_character_,
                                TRUE ~ toupper(dd_code_2)),
         # recode old delay reasons
         dd_code_1 <- case_when(is.na(dd_code_1) ~ NA_character_,
                                dd_code_1 == "41" ~ "25E",
                               dd_code_1 == "41A" ~ "23C",
                               dd_code_1 == "41B" ~ "23D",
                               dd_code_1 == "43" ~ "24A",
################################################################################
# Is the "." a typo??????????????
                               dd_code_1 == "27A." ~ "27",
                               dd_code_1 == "62" ~ "67",
                               dd_code_1 == "63" ~ "67"),
         
  # Format postcode variable into pc7
  postcode = phimethods::postcode(postcode, format = "pc7"),
  
  # Recode LA
  local_authority_code = case_when(
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
# Create interval() using Date of Birth and Ready for Discharge Date.
# Then time_length() gives the exact length in "year"
# Note that floor() rounds down to nearest integer to prevent 46.5 years giving
# age as 47
datafile <- datafile %>%
  mutate(age_at_rdd = lubridate::interval(date_of_birth, date_declared_medically_fit),
         age_at_rdd = floor(time_length(age_at_rdd, unit = "year")),
         # Check no missing values for age_at_rdd - all should be false.
         age_rdd_missing = is.na(age_at_rdd))

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
  mutate(RDD_same_as_DD = ifelse(!is.na(discharge_date) & 
                                 !is.na(date_declared_medically_fit),
                               difftime(discharge_date, 
                                        date_declared_medically_fit, 
                                        units="days") == 0, FALSE)) %>%
  # Remove zero delays & delete variable
  filter(RDD_same_as_DD == FALSE) %>%
  select(-RDD_same_as_DD) %>%

  #2. Remove any records where Ready for Discharge Date = Last Day of Month
  # (last_dom) - these do not become delays until the 1st of the following month.
  mutate(RDD_same_as_last_dom = ifelse(!is.na(date_declared_medically_fit),
                                       date_declared_medically_fit == last_dom, 
                                       FALSE)) %>%
  filter(RDD_same_as_last_dom == FALSE) %>%
  select(-RDD_same_as_last_dom)

### 4.Derivations ----

# Derive age groups
datafile <- datafile %>%
  mutate(age_grp <- case_when(age_at_rdd <75 ~ "18-74",
                             age_at_rdd >=75 ~ "75+"),
# Create Reason Code Groupings
# 1. High level reason code grouping - post July 2016
# Transport grouped under H&SC reasons as per BO report
  reason_grp_high_level = case_when(is.na(dd_code_1) ~ NA_character_,
                                    dd_code_1 == "100" ~ "Code 100", 
                                    dd_code_1 == "9" ~ "Code 9",
                                    dd_code_1 %in% c("11A","11B","23C","23D","24A","24B","24C","24D",
                                                     "24E","24F","27A","25A","25D","25E","25F","44")
                                    ~ "Health and Social Care Reasons",
                                    dd_code_1 %in% c("51","52","61","67","71","72","73","74")
                                    ~ "Patient/Carer/Family-related reasons"), 
  reason_grp = case_when(dd_code_1 %in% c("11A","11B") ~ "H&SC - Community Care Assessment",
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
  # Reason Codes INDIVIDUAL - Post July 2016.
  delay_description = case_when(dd_code_1 == "11A"
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
################################################################################
# Is this handling missing dates correctly? Code to "No"???
  census_flag = case_when(is.na(date_declared_medically_fit) ~ "N",
                          is.na(discharge_date) & date_declared_medically_fit < 
                          census_date & ((is.na(dd_code_2) | dd_code_2 != "26X" 
                                          | dd_code_2 != "46X")) ~ "Y",
                        !is.na(discharge_date) & discharge_date >= census_date & 
                          date_declared_medically_fit < census_date & 
                          ((is.na(dd_code_2) | dd_code_2 != "26X" | 
                              dd_code_2 != "46X")) ~ "Y",
                        !is.na(discharge_date) & discharge_date <= census_date & 
                          ((is.na(dd_code_2) | dd_code_2 != "26X" |
                              dd_code_2 != "46X")) ~ "N",
                        !is.na(discharge_date) & discharge_date == 
                          date_declared_medically_fit & ((is.na(dd_code_2) | 
                                dd_code_2 != "26X" | dd_code_2 != "46X")) ~ "N",
                        !is.na(discharge_date) & discharge_date >= census_date & 
                          ((is.na(dd_code_2) | dd_code_2 != "26X" | 
                              dd_code_2 != "46X")) 
                        ~ "N",
                        TRUE ~ "N"),
         
         # Flag those discharged up to 3 working days after census 
         # (NOTE census is always last THURS of month).
         
         # 1 Identify Census Date + 3 working days:
         census_date_plus_3_working_days = census_date + days(5),
         
################################################################################
#Is this handling NAs correctly?
# 2 Flag those with a discharge date le census_date_plus_3_working_days 
         # and check against BO variable CENSUSDISCHARGEWITHIN3WORKINGDAYS.
         discharge_within_3_days_census =
           case_when(is.na(discharge_date) ~ 0,
                     census_flag == "Y" & 
                       discharge_date < census_date_plus_3_working_days & 
                       dd_code_1 != "100" &
                       (is.na(dd_code_2) | (dd_code_2 != "26X" & 
                                            dd_code_2 != "46X")) ~ 1, TRUE ~ 0),

         # Check - should be zero cases returned.
         # census_flagcheck <- filter(datafile, discharge_within_3_days_census = 1 & census_flag == "")
         
         # count(census_flagcheck, chi_number, census_flag) %>%
         #   spread(census_flag, n)
         
         # Flag whether date_declared_medically_fit in current month
         drmd_in_month = case_when(
           date_declared_medically_fit >= first_dom & 
             date_declared_medically_fit <= last_dom ~ "Y",
           TRUE ~ "N"),
         
         # Flag whether discharge date in current month
         date_discharge_in_month = case_when(
           discharge_date >= first_dom & 
             discharge_date <= last_dom ~ "Y",
           TRUE ~ "N"),
         
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
         obds_in_month = case_when(
           drmd_in_month == "Y" & date_discharge_in_month == "Y" ~
             lubridate::time_length(
               lubridate::interval(date_declared_medically_fit, discharge_date),
               "days"),
           drmd_in_month == "N" & date_discharge_in_month == "Y" ~
             lubridate::time_length(lubridate::interval(first_dom, 
                                                      discharge_date), "days"), # + days(1),
           drmd_in_month == "Y" & date_discharge_in_month == "N" ~
             lubridate::time_length(
               lubridate::interval(date_declared_medically_fit, last_dom), 
               "days"), # + days(1)
           drmd_in_month == "N" & date_discharge_in_month == "N" ~ 
             lubridate::time_length(lubridate::interval(first_dom, last_dom), "days")
           ),
         
         # LENGTH OF DELAY AT CENSUS POINT.
         delay_at_census = case_when(census_flag == "Y" & 
                                       date_declared_medically_fit < census_date 
                  ~ lubridate::time_length(interval(date_declared_medically_fit, 
                                                    census_date), "days"),
                                       census_flag == "N" ~ 0),
         # Create delay length group & counts
         week <- 7,
         month <- 30.4375,
         delay_length_group = case_when(delay_at_census >= 1 & delay_at_census <= 3
                                       ~ "1-3 days",
                                      delay_at_census > 3 & delay_at_census <= 14
                                      ~ "3-14 days",
                                      delay_at_census > 14 & delay_at_census <= 28
                                      ~ "2-4 weeks",
                                      delay_at_census > 28 & delay_at_census <= 42
                                      ~ "4-6 weeks",
                                      delay_at_census > 42 & delay_at_census <= 84
                                      ~ "6-12 weeks",
                                      delay_at_census > (6 * month) &
                                        delay_at_census <= (12 * month) 
                                      ~ "6-12 months",
                                      delay_at_census > 84 & delay_at_census 
                                      <= (6*month) ~ "3-6 months",
                                      delay_at_census > (12 * month) 
                                      ~ "12 months or more",
                                      census_flag == "" ~ NA_character_),
         # Create counts for each delay period
         delay_1_to_3_days = dplyr::if_else(delay_at_census >= 1 & 
                                           delay_at_census <= 3, 1, 0),
         delay_3_to_14_days = dplyr::if_else(delay_at_census > 3 &
                                            delay_at_census <= 14, 1, 0),
         delay_2_to_4_weeks = dplyr::if_else(delay_at_census > 14 &
                                            delay_at_census <= 28, 1, 0),
         delay_4_to_6_weeks = dplyr::if_else(delay_at_census > 28 &
                                            delay_at_census <= 42, 1, 0),
         delay_6_to_12_weeks = dplyr::if_else(delay_at_census > 42 &
                                             delay_at_census <= 84, 1, 0),
         delay_3_to_6_months = dplyr::if_else(delay_at_census > 84 &
                                             delay_at_census <= (6*month), 1, 0),
         delay_6_to_12_months = dplyr::if_else(delay_at_census > (6*month) &
                                              delay_at_census <= (12*month), 1, 0),
         delay_over_12_months = dplyr::if_else(delay_at_census > (12*month), 1, 0),
         delay_over_2_wks = dplyr::if_else(delay_at_census > 14, 1, 0),
         delay_over_4_wks = dplyr::if_else(delay_at_census > 28, 1, 0),
         delay_over_6_wks = dplyr::if_else(delay_at_census > 42, 1, 0)) %>%

  # DUPLICATE RECORDS
  
  #Check for duplicate CHI numbers and flag.
  dplyr::arrange(chi_number) %>%
  tidylog::group_by(chi_number) %>%
    mutate(duplicate_CHI = if_else(max(dplyr::row_number()) > 1, "Y","N")) %>%
    dplyr::ungroup() %>%

  # Need to capture paired records with errors to investigate reason for 
  # duplicate - so update Error code for both
  dplyr::arrange(chi_number, desc(duplicate_CHI)) %>%
  tidylog::group_by(chi_number) %>%
    mutate(duplicate_CHI = if_else(max(row_number()) > 1, "Y", "N")) %>%
  dplyr::ungroup() %>%

  # Check for duplicate CENSUS records and flag
  dplyr::arrange(chi_number, desc(census_flag)) %>%
  tidylog::group_by(chi_number, census_flag) %>%
    mutate(error_Duplicate_CHI_Census = if_else(max(row_number()) > 1 & 
                                                census_flag == "Y", "Y", "N")) %>%
  dplyr::ungroup() %>%

  # Need to capture paired records with errors to investigate reason for 
  # duplicate - so update Error code for both.
  dplyr::arrange(chi_number, desc(error_Duplicate_CHI_Census)) %>%
  tidylog::group_by(chi_number, census_flag) %>%
    mutate(error_Duplicate_CHI_Census = if_else(max(row_number()) > 1 & 
                                                  census_flag == "Y", 
                  lag(error_Duplicate_CHI_Census), error_Duplicate_CHI_Census)) %>%
  dplyr::ungroup() %>%
    # DELAY LOCATION (acute, gpled, notgpled) while spec still in data.
  dplyr::arrange(location)

  # Create location name lookup.
  hospital_lookup <- bind_rows(readr::read_rds(paste0(
    plat_filepath, "linkage/output/lookups/Unicode/National Reference Files/",
    "location.rds")) %>%
      select(Location, Locname) %>%
      rename(location      = Location,
             location_name = Locname),
    readr::read_rds(paste0(plat_filepath,
                     "linkage/output/lookups/Unicode/National Reference Files/",
                     "Health_Board_Identifiers.rds")) %>%
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
  dplyr::mutate(acute = dplyr::if_else(is.na(location_name), 1, 0),
                gpled = dplyr::if_else(acute == 0 & 
                                          discharge_specialty_nat_code == "E12",
                                        1, 0),
                notgpled = dplyr::if_else(acute == 0 & gpled == 0, 1, 0),
                # Check to ensure each row only has one of either acute, 
                # carehome, gpled or notgpled marked as one.
                total = acute + gpled + notgpled,
                check2 = dplyr::if_else(total == 1, 0, 1)) %>%
# unique freq vars check2.
  dplyr::select(-c(total, check2)) %>%
# DERIVATIONS END
  
  # Set num_pats variable to 1 for all records (incl Islands/d&g).
  mutate(num_pats = 1)
#unique num_pats

# 'scotland_temp.sav' saved out here


# PRODUCE PROVISIONAL CENSUS / OBD FIGURES FOR CHECKING AGAINST VERIFICATION FORMS.
# Check the number of census records match those in Verification form.
# Number should equal total cases where census_flag = Y.
# get file = !Filepath + 'scotland_temp.sav'.


# Create a provisional HB census total - excl Code 100.
census_hb <- datafile %>%
  filter(dd_code_1 != "100" & 
           (census_flag == 'Y' | discharge_within_3_days_census == 1)) %>%
  group_by(nhs_board, discharge_within_3_days_census, reason_grp_high_level) %>%
    summarise(census_total = n()) %>%
    dplyr::ungroup()


# Create a provisional HB/LA census total - excl Code 100.
census_la <- datafile %>%
  filter(dd_code_1 != "100" & 
           (census_flag == 'Y' | discharge_within_3_days_census == 1)) %>%
  group_by(nhs_board, local_authority_code, discharge_within_3_days_census, 
           reason_grp_high_level) %>%
  summarise(census_total = n()) %>%
  dplyr::ungroup()

# Create a provisional HB OBD total- excl Code 100
obd_hb <- datafile %>%
  filter(dd_code_1 != "100") %>%
    tidylog::group_by(nhs_board, reason_grp_high_level) %>%
    tidylog::summarise(obds_in_month = sum(obds_in_month)) %>%
  dplyr::ungroup()

  # Create a provisional LA OBD total- excl Code 100
obd_la <- datafile %>%
  filter(dd_code_1 != "100") %>%
    tidylog::group_by(nhs_board, local_authority_code, reason_grp_high_level) %>%
    tidylog::summarise(obds_in_month = sum(obds_in_month)) %>%
  dplyr::ungroup()

prov_census_la <- merge(census_la, obd_la, all = TRUE)
prov_census_hb <- merge(census_hb, obd_hb, all = TRUE)
prov_census <- dplyr::bind_rows(prov_census_hb, prov_census_la) %>%
  mutate(reason_grp_high_level = ifelse(reason_grp_high_level != "Code 9", 
                                        "HSC/PCF", reason_grp_high_level)) %>%
    dplyr::rename(delay_category = reason_grp_high_level) %>%
    tidyr::replace_na(list(census_total = 0,
                           obds_in_month = 0)) %>%
    # Copy Census Figure to discharge_within_3_days_census column.
  mutate(discharge_within_3_days_census = ifelse(discharge_within_3_days_census 
                          == 1, census_total, discharge_within_3_days_census),
    # Now set Census value to 0 if there is a value in 
    # discharge_within_3_days_census column.
  census_total = ifelse(discharge_within_3_days_census > 0, 0, census_total)) %>%
    # This is so that both columns can be aggregated then summed to get a Census 
    # figure including those discharged within 3 days of census.
  tidylog::group_by(nhs_board, local_authority_code, delay_category) %>%
  tidylog::summarise(discharge_within_3_days_census = 
                       sum(discharge_within_3_days_census), 
                       census_total = sum(census_total),
                       obs_in_Month = sum(obds_in_month)) %>%
  dplyr::ungroup() %>%
  arrange(nhs_board, local_authority_code, delay_category) %>%
  mutate(overall_total = discharge_within_3_days_census + census_total) %>%
    readr::write_csv(readr::write_csv(paste0(filepath, census_date,
                                    "_Provisional Census and OBD totals.csv")))


  
# ADD SPECIALTY DESCRIPTION.
# Original lookup file here
specialty_group <- readr::read_rds(paste0(plat_filepath,
                    "linkage/output/lookups/Unicode/National Reference Files/",
                                          "specialt.rds")) %>%
  select(speccode, description) %>%
  rename(spec_code = speccode,
         spec_desc = description)

datafile <- datafile %>%
  arrange(discharge_specialty_nat_code) %>%
  left_join(specialty_group, by = c("discharge_specialty_nat_code" = "spec_code"))
  
# MATCH FILE WITH LATEST NATIONAL POSTCODE DIRECTORY FILE - This will add 
#DATAZONE2011.
  
postcode_lookup <- readr::read_rds(paste0(plat_filepath,
        "linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/",
                                  "Scottish_Postcode_Directory_2019_2.rds")) %>%
  select(pc7, DataZone2011) %>%
  rename(postcode = pc7,
         dz_2011  = DataZone2011)

dz_2011_lookup <- readr::read_rds(paste0(plat_filepath,
                       "linkage/output/lookups/Unicode/Geography/HSCP Locality/",
                       "HSCP Localities_DZ11_Lookup_20191216.rds")) %>%
  select(data_zone2011, hscp_locality, hscp2019name) %>%
  rename(dz_2011 = data_zone2011,
         hscp_locality_derived = hscp_locality,
         hscp_2019_name_derived  = hscp2019name)

datafile <- datafile %>%
  dplyr::arrange(postcode) %>%
  tidylog::left_join(postcode_lookup, by = "postcode") %>%
  # NEXT, USE DATAZONE TO MATCH ON HSCP LOCALITY AND HSCP, USING LOOKUP
  dplyr::arrange(dz_2011) %>%
  tidylog::left_join(dz_2011_lookup, by = "dz_2011") %>%
  
  # Save as Scotland file.
  dplyr::arrange(nhs_board, chi_number, ready_for_discharge_date)

### 3 - Save data ----
write_csv(datafile, paste0(filepath, census_date, "_SCOTLAND.csv"))

# Save as Scotland_validated (same file with some unnecessary variables removed)
# age_grp, specialty_group?
      datafile <- datafile %>%
        select(monthflag, nhs_board, local_authority_code, postcode, dz_2011,
               hscp_locality_derived, hscp_2019_name_derived, 
               out_of_area_case_indicator, chi_number, census_flag,
               location, location_name, sex_code, date_of_birth, 
               age_at_rdd, discharge_specialty_nat_code, 
               admission_date, date_referred_for_sw_assessment,
               ready_for_discharge_date, discharge_date, 
               discharge_within_3_days_census, discharge_to_code, obds_in_month, 
               delay_at_census, reason_grp_high_level, reason_grp, dd_code_1, 
               dd_code_2, delay_description, 
               num_pats, delay_length_group, delay_1_to_3_days, delay_3_to_14_days,
               delay_2_to_4_weeks, delay_4_to_6_weeks, delay_6_to_12_weeks, 
               delay_3_to_6_months, delay_6_to_12_months, delay_over_12_months, 
               delay_over_2_wks, delay_over_4_wks, delay_over_6_wks, acute, 
               gpled, notgpled)

# Remove any records which have the RDD and Discharge Date the same as they will
# have zero delays.
        removed_records <- datafile %>%
        filter(ready_for_discharge_date == discharge_date) %>%
        readr::write_csv(paste0(filepath, census_date,
                         "_removed.csv"))
      
        datafile <- datafile %>%
          filter(ready_for_discharge_date != discharge_date) %>%
          readr::write_csv(paste0(filepath, census_date,
                         "_SCOTLAND_validated.csv"))

### END OF SCRIPT ###