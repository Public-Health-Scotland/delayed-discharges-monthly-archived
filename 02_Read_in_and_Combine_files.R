##########################################################
# Name of file: 02_Read_in_and_Combine_files.R
# Data Release: Delayed Discharges monthly publication
# Original author(s): Peter McClurg (spss version: James Mc Nally)
# Original date: 24/09/19 (spss version: 30/11/2017)
#
# Type of script: Preparation
# Written/run on: R Studio Server
# Version of R that the script was most recently run on: v3.5.1
# Description: Reads in Health board submitted files and creates Allboards file
##########################################################


### 0 - Housekeeping ----

source("00_setup_environment.R")

# Source function
source(here("functions", "read_clean_data.R"))


### 1 - Read in csv files ----

boards <-
  c("a&a", "borders", "d&g", "fife", "fv", "glasgow", "grampian", 
    "highland", "lanark", "lothian", "orkney", "shetland", "tayside", "wi")

all_boards <-
  paste0(filepath_data, boards, "/", boards, ".csv") %>%
  map(read_clean_data) %>%
  reduce(bind_rows)


# Check number of columns and column names correct before proceeding
if(ncol(all_boards) != 17){
  stop("Incorrect number of columns.")
}

if(!all(names(all_boards) %in% 
   c("healthboard", "monthflag", "health_location_code", "chi_no", 
     "patient_postcode", "local_authority_area", "patient_dob", 
     "specialty_code", "date_referral_received", "readyfordischargedate", 
     "reasonfordelay", "reasonfordelaysecondary", "outofareacaseindicator", 
     "original_admission_date", "gender", 
     "date_discharge", "discharge_reason"))){
  stop("At least one variable name incorrect.")
}


### 2 - Change Local Authority desc to code  ----

all_boards %<>%
  mutate(local_authority_area = case_when(
    local_authority_area == "Aberdeen City" ~ "1",
    local_authority_area == "Aberdeenshire" ~ "2",
    local_authority_area == "Angus" ~ "3",
    local_authority_area == "Argyll & Bute" ~ "4",
    local_authority_area == "Scottish Borders" ~ "5",
    local_authority_area == "Clackmannanshire" ~ "6",
    local_authority_area == "West Dunbartonshire" ~ "7",
    local_authority_area == "Dumfries & Galloway" ~ "8",
    local_authority_area == "Dundee City" ~ "9",
    local_authority_area == "East Ayrshire" ~ "10",
    local_authority_area == "East Dunbartonshire" ~ "11",
    local_authority_area == "East Lothian" ~ "12",
    local_authority_area == "East Renfrewshire" ~ "13",
    local_authority_area == "City of Edinburgh" ~ "14",
    local_authority_area == "Falkirk" ~ "15",
    local_authority_area == "Fife" ~ "16",
    local_authority_area == "Glasgow City" ~ "17",
    local_authority_area == "Highland" ~ "18",
    local_authority_area == "Inverclyde" ~ "19",
    local_authority_area == "Midlothian" ~ "20",
    local_authority_area == "Moray" ~ "21",
    local_authority_area == "North Ayrshire" ~ "22",
    local_authority_area == "North Lanarkshire" ~ "23",
    local_authority_area == "Orkney" ~ "24",
    local_authority_area == "Perth & Kinross" ~ "25",
    local_authority_area == "Renfrewshire" ~ "26",
    local_authority_area == "Shetland" ~ "27",
    local_authority_area == "South Ayrshire" ~ "28",
    local_authority_area == "South Ayrshire " ~ "28",
    local_authority_area == "South Lanarkshire" ~ "29",
    local_authority_area == "Stirling" ~ "30",
    local_authority_area == "West Lothian" ~ "31",
    local_authority_area == "Comhairle nan Eilean Siar" ~ "32",
    local_authority_area %in% c("Other", "Unknown") ~ "90",
    TRUE ~ local_authority_area
  ))


### 3 - Remove records where Ready for Discharge equals Discharge Date ----

all_boards %<>%
  filter(readyfordischargedate != date_discharge | is.na(date_discharge))
  

### 4 - Save file as csv and rds file  ----

write_csv(
  all_boards,
  paste0(filepath, "Allboards_R_.csv")
)

write_rds(
  all_boards,
  paste0(filepath, "Allboards_R_.rds"),
  compress = "gz"
)


### END OF SCRIPT ###