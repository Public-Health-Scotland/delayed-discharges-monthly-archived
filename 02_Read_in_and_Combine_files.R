##########################################################
# Name of file: Allboards
# Data Release: Delayed Discharges monthly publication
# Original author(s): Peter McClurg (spss version: James Mc Nally)
# Original date: 24/09/19 (spss version: 30/11/2017)
# Latest update author (if not using version control)
# Latest update date (if not using version control)
# Latest update description (if not using version control)
# Type of script: Preparation
# Written/run on: R Studio Server
# Version of R that the script was most recently run on: R Studio
# Description of content: Reads in Healthboard submitted files and creates Allboards.sav file.
# Approximate run time: TBC
##########################################################


### 1 - Housekeeping ----
# This section should be the only section of the script which requires manual changes 
# for future updates and includes:
#   loading packages
#   setting filepaths and extract dates
#   functions (defined here or sourced from another file)
#   setting plot parameter
#   specifying codes (e.g. ICD-10 codes)
#bring in environment 
source("00_setup_environment.R")




### 1. Read in csv files ----
a_a <- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/a&a/a&a.csv")
borders <- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/borders/borders.csv")
d_g<-read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/d&g/d&g.csv")
fife <- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/fife/fife.csv")
fv<- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/fv/fv.csv")
glasgow<- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/glasgow/glasgow.csv")
grampian<- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/grampian/grampian.csv")
highland<- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/highland/highland.csv")
lanark<- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/lanark/lanark.csv")
lothian<- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/lothian/lothian.csv")
orkney<- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/orkney/orkney.csv")
shetland <- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/shetland/shetland.csv")
tayside <- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/tayside/tayside.csv")
wi <- read.csv("/conf/delayed_discharges/RAP development/2020_04/Data/wi/wi.csv")

# Description - Define function to format data file to ensure consistent
# variable names, variable types, etc.
#########################################################################


format_data <- function(data){
  
  data(a_a,borders,d_g,fife,fv,glasgow,grampian,highland,lanark,lothian,orkney,shetland,tayside,wi) %>%
    
    # Standardise variable names
    rename_all(~ str_replace(., "\n", " ")) %>%
    clean_names() %>%
    rename_all(~ str_replace(., "chi$", "CHINo")) %>%
    rename_all(~ str_replace(., "HB", "Healthboard")) %>%
    rename_all(~ str_replace(., "pc7", "PatientPostcode")) %>%
    rename_all(~ str_replace(., "sex$", "Gender")) %>%
    
    # Remove blank rows that contain no data 
    filter_all(any_vars(!is.na(.))) %>%
    
    # Recode all NULLs with NA
    mutate_all(~ if_else(. == "NULL", NA_character_, .)) %>%
    
    # Convert all dates to date format
    mutate_at(vars(contains("date")),
              ~ case_when(
                # Excel format
                nchar(.) == 5 ~ janitor::excel_numeric_to_date(as.numeric(.)),
                # Invalid format
                !is.na(.) & suppressWarnings(is.na(lubridate::dmy(.))) ~ dmy("09099999"),
                TRUE ~ dmy(.)
              )) %>%
    
    # Trim white space from all character variables
    mutate_if(is.character, ~ str_trim(., side = "both")) %>%
    
    # Remove special characters from character variables
    # Previous issues with file encoding different apostrophe types
    mutate_if(is.character, ~ map_chr(str_extract_all(., "[0-9A-Za-z\\s/-]"),
                                      ~ reduce(.x, ~ paste0(.x, .y)))) %>%
    
    # Pad CHI Number
    mutate(CHINo = str_pad(CHINo, 
                                width = 10, side = "left", pad = "0"))
  
}

### 2. Ensure that Local Authority Code is saved as a character ( this causes issues later if not done!) ----

# JN - wont need this if use read_csv is used as variables already read in as characters.

#a_a$Local.Authority.Code <- as.character(a_a$Local.Authority.Code)
#borders$Local.Authority.Code <- as.character(borders$Local.Authority.Code)
#d_g$Local.Authority.Code <- as.character(d_g$Local.Authority.Code)
#fife$Local.Authority.Code <- as.character(fife$Local.Authority.Code)
#fv$Local.Authority.Code <- as.character(fv$Local.Authority.Code)
#glasgow$Local.Authority.Code <- as.character(glasgow$Local.Authority.Code)
#grampian$Local.Authority.Code <- as.character(grampian$Local.Authority.Code)
#highland$Local.Authority.Code <- as.character(highland$Local.Authority.Code)
#lanark$Local.Authority.Code <- as.character(lanark$Local.Authority.Code)
#lothian$Local.Authority.Code <- as.character(lothian$Local.Authority.Code)
#orkney$Local.Authority.Code <- as.character(orkney$Local.Authority.Code)
#shetland$Local.Authority.Code <- as.character(shetland$Local.Authority.Code)
#tayside$Local.Authority.Code <- as.character(tayside$Local.Authority.Code)
#wi$Local.Authority.Code <- as.character(wi$Local.Authority.Code)


### 3. Add boards Together  ----

Scot_boards<-rbind(a_a,borders,d_g,fife,fv,glasgow,grampian,highland,lanark,lothian,orkney,shetland,tayside,wi)
View(Scot_boards)

Scot_boards <-
  Scot_boards %>% clean_names()

count (Scot_boards,discharge_to_code)

### 4. Change discharge reason from text to code  ----
#Scot_boards$discharge_to_code[Scot_boards$discharge_to_code=="Placement"] <- "1"
#Scot_boards$discharge_to_code[Scot_boards$discharge_to_code=="Continuing Care NHS (MEL)"] <- "1"
#Scot_boards$discharge_to_code[Scot_boards$discharge_to_code=="Discharge Home with Home Care"] <- "2"
#Scot_boards$discharge_to_code[Scot_boards$discharge_to_code=="Discharge Home"] <- "3"
#Scot_boards$discharge_to_code[Scot_boards$discharge_to_code=="Death"] <- "4"
#Scot_boards$discharge_to_code[Scot_boards$discharge_to_code=="Not Fit For Discharge"] <- "5"


### 5.Change Local Authority Code to characters  ----`

#Scot_boards$local_authority_code<- as.character(Scot_boards$local_authority_code)

### 6.Trim Local Authority Code to ensure no leading or trailing spaces  ----
Scot_boards$local_authority_code<-trimws(Scot_boards$local_authority_code)
unique(Scot_boards$local_authority_code)

Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Aberdeen City"] <- "1"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Aberdeenshire"] <- "2"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Angus"] <- "3"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Argyll & Bute"] <- "4"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Scottish Borders"] <- "5"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Clackmannanshire"] <- "6"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="West Dunbartonshire"] <- "7"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Dumfries & Galloway"] <- "8"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Dundee City"] <- "9"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="East Ayrshire"] <- "10"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="East Dunbartonshire"] <- "11"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="East Lothian"] <- "12"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="East Renfrewshire"] <- "13"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="City of Edinburgh"] <- "14"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Falkirk"] <- "15"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Fife"] <- "16"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Glasgow City"] <- "17"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Highland"] <- "18"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Inverclyde"] <- "19"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Midlothian"] <- "20"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Moray"] <- "21"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="North Ayrshire"] <- "22"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="North Lanarkshire"] <- "23"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Orkney"] <- "24"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Perth & Kinross"] <- "25"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Renfrewshire"] <- "26"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Shetland"] <- "27"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="South Ayrshire"] <- "28"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="South Ayrshire "] <- "28"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="South Lanarkshire"] <- "29"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Stirling"] <- "30"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="West Lothian"] <- "31"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="West Lothian "] <- "31"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Comhairle nan Eilean Siar"] <- "32"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Unknown"] <- "90"
Scot_boards$local_authority_code[Scot_boards$local_authority_code=="Other"] <- "90"

# Check Local Authority Code  ----
unique(Scot_boards$local_authority_code)

#Checking types  ----
class(Scot_boards$discharge_date)
class(Scot_boards$date_declared_medically_fit)
 
### 7. Change dates to strings to ensure matching types  ----
#Scot_boards$Discharge.Date <- as.character(Scot_boards$Discharge.Date)
#Scot_boards$Date.Declared.Medically.Fit <- as.character(Scot_boards$Date.Declared.Medically.Fit)
#View(Scot_boards$Date.Declared.Medically.Fit)
#View(Scot_boards$Discharge.Date)

### 8. Check if any RDD=DD ( select if RDD<>DD)  ----
Scotland_allboards <- filter(Scot_boards, is.na(discharge_date) | date_declared_medically_fit!=discharge_date)





#Check that each board has output 
View(Scotland_allboards)
#Check the number of entries for each 'NHS Board'
table(Scotland_allboards$nhs_board)


### 9. Save out file as csv and also as a sav file  ----

write.csv(Scotland_allboards,"//conf/delayed_discharges/RAP development/2020_04/Outputs/Allboards_R_.csv")
write_sav(Scotland_allboards,"//conf/delayed_discharges/RAP development/2020_04/Outputs/Allboards_R.sav")

###############################################################################################################

