#########################################################################
# Name of file - setup_environment.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Simon Quirk
# Orginal Date - January 2020
#
# Type - Reproducible Analytical Pipeline
# Written/run on - RStudio server
# Version of R - 3.5.1
#
# Description - Sets up environment required for running reproducible analytical
# pipeline process. This is the only file which should require updating every
# time the RAP process is run
#
# Approximate run time - xx minutes
#########################################################################

### 1 - Load packages ----
library(magrittr)     # For the %<>% functionality
library(here)         # For the here() function
library(dplyr)        # For data manipulation in the "tidy" way
library(tidyr)        # For data manipulation in the "tidy" way
library(haven)        # For reading in SPSS files
library(lubridate)    # For dates
library(stringr)      # For string manipulation and matching
library(janitor)      # For 'cleaning' variable names
library(phsmethods)   # For postcode tidying


### 2 - Define Whether Running on Server or Locally ----
if (sessionInfo()$platform == "x86_64-redhat-linux-gnu (64-bit)") {
  platform <- "server"
} else {
  platform <- "locally"
}


# Define root directory for cl-out based on whether script is running locally or
# on server
plat_filepath <- dplyr::if_else(platform == "server",
                                '/conf/',
                                '//stats/')

filepath <- paste0(plat_filepath, "delayed_discharges/RAP development/2019_07/Outputs/")

### 3 - Census dates ----

# Monthly census snapshot taken on the last Thursday of the month
census_date <- lubridate::dmy(25072019)


# First month census was run in current form. Used to calculate census number
first_census_month <- lubridate::dmy(28072016)

# Create a sequence of all census dates.
# Census is the last Thursday of the month. First census was 28th July 2016
# Create a sequence of all Thursdays from first to current census date
census_date_seq <- tibble(thursdays = seq(lubridate::dmy(28072016), census_date, 
                                          by="1 week"),
                          census_month = lubridate::month(thursdays)) %>%
  # Flag if Thursday is the last in the month by comparing to the 
  # previous Thursday
  mutate(last_thurs_in_month = ifelse((wday(thursdays) == 5) & month(thursdays)
                                    != month(lead(thursdays)), TRUE, FALSE)) %>%
  filter(last_thurs_in_month == TRUE)

# First date of census month
first_dom <- lubridate::floor_date(census_date, "month")

# Last date of the census month
last_dom <- lubridate::ceiling_date(census_date, "month") - 1

previous_census_month <- first_dom - months(1)

# Using the modulus operator (%%) to return a 2 digit year by returning the 
# fraction of dividing by 100
initial_month <- paste0(tolower(month(first_census_month, label = TRUE, 
                                      abbr = TRUE)), 
                        "_", (year(first_census_month) %% 100))

previous_month <-paste0(tolower(month(previous_census_month, label = TRUE, 
                                      abbr = TRUE)), 
                        "_", (year(previous_census_month) %% 100))

current_month <- paste0(tolower(month(census_date, label = TRUE, abbr = TRUE)), 
                        "_", (year(census_date) %% 100))

# Previous census date in "yyyy-mm-dd" format
prev_census_date <- census_date_seq[nrow(census_date_seq),]$thursdays

### END OF SCRIPT ###