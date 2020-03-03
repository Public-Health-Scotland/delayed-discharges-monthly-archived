##########################################################
# Name of file: delay_overlap_queries
# Data Release: Delayed Discharges monthly publication
# Original author(s): Peter McClurg (spss version: Peter McClurg )
# Original date: 02/03/20 (spss version: 07/01/2019)
# Latest update author (if not using version control)
# Latest update date (if not using version control)
# Latest update description (if not using version control)
# Type of script: Preparation
# Written/run on: R Studio Server
# Version of R that the script was most recently run on: R Studio
# Description of content: Checks that delays not discharged in previous month have reappeared in current month 
# Approximate run time: 5 minutes
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

Monthflag<-("Jul 2019")
nhs_board<-("glasgow")

filepath1<-("/conf/delayed_discharges/RAP development/2019_06/Data/")
filepath2<-("/conf/delayed_discharges/RAP development/2019_07/Data/")

### 2. Get previous month data file ( csv )

datafile<-read.csv(paste0(filepath1,nhs_board,"/",nhs_board,".csv"))

datafile <- datafile %>% clean_names() # clean names of variables to common RAP terminology

#Add a leading 0 to chino

datafile$chi<-str_pad(datafile$chi, 10, pad = "0")

#compute month
datafile<-datafile %>% mutate(month=substring(delay_end_date, 4,5))
table(datafile$month)

#select if month = blank or next month number ( i.e. is not equal to current month)

datafile<-filter(datafile,month!="06")

arrange(datafile,chi,date_declared_medically_fit)

# save out previous month file with four variables
datafile_prev_month<-select(datafile,chi,date_declared_medically_fit,delay_end_date,discharge_reason)

###3. Get current month's data file and arrange for matching

datafile2<-read.csv(paste0(filepath2,nhs_board,"/",nhs_board,"_2.csv"))

datafile2 <- datafile2 %>% clean_names() # clean names of variables to common RAP terminology

#rename chi_number as chi

datafile2 <- datafile2 %>% rename(chi = chi_number)
         

#Add a leading 0 to chino

datafile2$chi<-str_pad(datafile2$chi, 10, pad = "0")

#aggregate to get one row per chi

chi_check<- datafile2 %>% 
  group_by(chi) %>% 
  summarise(Total=n()) %>% 
  ungroup()

#record current_month 
chi_check$current_month<-1

#match with previous month's file by chi

Query_overlap <- left_join(datafile_prev_month, chi_check,
                        by = c(("chi" = "chi")))

#select any delays with no record in current_month
delay_overlap_query<-filter(Query_overlap,is.na(current_month))

#write out file ( keeping only chi and date_declared_medically_fit)

write.xlsx(datafile,paste0(filepath,nhs_board,"delay_overlap_queries.xlsx")) # save out file

#end of script






