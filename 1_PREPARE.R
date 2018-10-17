# IMPLICIT ASSOCIATIONS RT TASK: PREPARE DATA
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018
# OCTOBER 2018

# 1 Clean and add variables
# 2 filter (accuracy, bad participants)
# 3 Inspect overall RT
# 4 set absolute thresholds
# 5 write CSV (robust)
# 6 set individual thresholds
# 7 write CSV

# -------------------------------------------
####### LOAD PACKAGES AND DATA ##############

library(tidyverse)
library(magrittr)
library(janitor)
library(gridExtra)
library(stellenbosch)
library(data.table)

files <- list.files("RT_logs_stellenbosch",pattern="*.csv",full.names = T)

df    <- files   %>%
  map(read_csv2) %>%
  reduce(rbind)

theme_set(theme_bw())