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
library(readxl)
library(here)         # For the here() function
library(dplyr)        # For data manipulation in the "tidy" way
library(tidyr)        # For data manipulation in the "tidy" way
library(haven)        # For reading in SPSS files
library(lubridate)    # For dates
library(stringr)      # For string manipulation and matching
library(openxlsx)     # For manipulating Excel files
library(janitor)      # For 'cleaning' variable names
library(devtools)     # Used to install phimethods from GitHub

# Check the following  are needed
library(stringi)      # Where is this package used?
library(xlsx)         # used for write.xlsx. Use openxlsx::saveWorkbook once 
                      # templates are created?

devtools::install_github("Health-SocialCare-Scotland/phsmethods")

options(stringsAsFactors = FALSE)

### 2 - Define Whether Running on Server or Locally ----
# Covers both the old server and the pro one
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)",
                                  "x86_64-pc-linux-gnu (64-bit)")) {
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

# First date of census month
first_dom <- lubridate::floor_date(census_date, "month")

# Last date of the census month
last_dom <- lubridate::ceiling_date(census_date, "month") - 1

# First month census was run in current form. Used to calculate census number
first_census_month <- lubridate::dmy(01112006)

### END OF SCRIPT ###