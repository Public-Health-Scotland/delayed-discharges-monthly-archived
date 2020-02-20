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
# Version of R that the script was most recently run on: 3.5.1
# Description of content: Updates trendfile with DD records from most recent publication month.
# Approximate run time: TBC
##########################################################


#filename for latest month needs to be manually updated - can we automate this somehow?


### 1. Housekeeping ----
source("00_setup_environment.R")


### 2. Get Scotland_validated file for latest month ----

# Read in file
datafile <- data.frame(readr::read_csv(paste0(filepath, census_date, 
                                   "_SCOTLAND_validated.csv")))

# Add variable 'cen_num' (census number for latest month)
# First census is 
datafile <- datafile %>%
  mutate(cen_num = lubridate::time_length(
    lubridate::interval(first_census_month, first_dom), "months"),
    cal_yr = lubridate::year(census_date),
    fin_yr = phsmethods::fin_year(census_date),
    ready_for_discharge_day = toupper(wday(ready_for_discharge_date, 
                                           label = TRUE, abbr = TRUE)),
    discharge_day = toupper(wday(ready_for_discharge_date, label = TRUE, 
                                 abbr = TRUE)))

### 2.Add latest monthly file to current trend file ----

# Add files together to update trend file
trend_file <- readr::read_csv(paste0(filepath, prev_census_date, "_trend_file_", 
                              initial_month, "_", previous_month, ".csv")) %>%
  mutate(chi_number = toString(chi_number),
         census_date = lubridate::parse_date_time(census_date, c('dmy')),
         date_of_birth = lubridate::parse_date_time(date_of_birth, c('dmy')),
         admission_date = lubridate::parse_date_time(admission_date, c('dmy')),
         date_referred_for_sw_assessment = lubridate::parse_date_time(date_referred_for_sw_assessment, c('dmy')),
         ready_for_discharge_date = lubridate::parse_date_time(ready_for_discharge_date, c('dmy')),
         discharge_date = lubridate::parse_date_time(discharge_date, c('dmy')))

trend_file <- trend_file %>%
  bind_rows(datafile, trend_file) %>%
  # Change discharge reason from text to code
  mutate(discharge_reason =
           case_when(discharge_reason == "Placement" ~ "1",
           discharge_reason == "Continuing Care NHS (MEL)" ~ "1",
           discharge_reason == "Discharge Home with Home Care" ~ "2",
           discharge_reason == "Discharge Home" ~ "3",
           discharge_reason == "Death" ~ "4",
           discharge_reason == "Not Fit For Discharge" ~ "5")) %>%
  arrange(cen_num, chi_number) #%>%
  readr::write_csv(paste0(filepath, census_date, "trend_file_", 
                          initial_month, "_", current_month))
### 3.Checks ----

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


### END OF SCRIPT ###