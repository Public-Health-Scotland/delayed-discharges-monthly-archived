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
library(readxl)
library(here)         # For the here() function
library(dplyr)        # For data manipulation in the "tidy" way
library(tidyr)        # For data manipulation in the "tidy" way
library(haven)        # For reading in SPSS files
library(lubridate)    # For dates
library(stringr)      # For string manipulation and matching
library(openxlsx)     # For manipulating Excel files
library(tidyverse)#Need all packeges?
library(janitor)      # For 'cleaning' variable names
library(stringi) # Where is this package used?
library(stats)#Used by PMcC in Prov & Var - where?
library(xlsx)#used for write.xlsx. Use openxlsx::saveWorkbook once templates are
             #created?
library(devtools)     # Used to install phimethods from GitHub

devtools::install_github("Health-SocialCare-Scotland/phimethods")


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


### 3 - Census dates ----


# Monthly census snapshot taken on the last Thursday of the month
census_date <- lubridate::dmy(25072019)

# First date of month
first_dom <- lubridate::dmy(01072019)

# Last date of the month
last_dom <- lubridate::dmy(31072019)

### END OF SCRIPT ###