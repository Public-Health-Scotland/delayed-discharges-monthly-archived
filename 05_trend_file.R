##########################################################
# Name of file: 05_trend_file.R
# Data Release: Delayed Discharges monthly publication
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
datafile <- readr::read_csv(paste0(filepath, census_date, "_SCOTLAND_validated.csv"))

# Add variable 'cen_num' (census number for latest month)
# First census is 
datafile <- datafile %>%
  mutate(cen_num = lubridate::time_length(
    lubridate::interval(first_census_month, first_dom), "months"),
    cal_yr = lubridate::year(census_date),
    fin_yr = phsmethods::fin_year(census_date),
    ready_for_discharge_day = toupper(wday(ready_for_discharge_date, 
                                           label = TRUE, abbr = TRUE)),
    discharge_day = toupper(wday(discharge_date, label = TRUE, abbr = TRUE)),
    census_date = census_date) %>%
  dplyr::select(-x1, -date_declared_medically_fit, -age_rdd_missing, -census_date_plus_3_working_days, -drmd_in_month, -date_discharge_in_month, -week, -month, -duplicate_CHI, -error_Duplicate_CHI_Census)

### 3.Add latest monthly file to current trend file ----

# Add files together to update trend file
trend_file <- readr::read_csv(paste0(filepath, prev_census_date, "_trend_file_", 
                                initial_month, "_", previous_month, ".csv")) %>%
  janitor::clean_names() %>%
  mutate(chi_number = toString(chi_no)) %>%
         mutate_at(vars(contains("date")), dmy)

trend_file <- bind_rows(trend_file, datafile)

readr::write_csv(trend_file, paste0(filepath, census_date, "_trend_file_", 
                          initial_month, "_", current_month, ".csv"))
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


### END OF SCRIPT ###