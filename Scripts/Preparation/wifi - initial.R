# -------------------------------------------------------------------------
# GOAL: Organize Wifi Data 
# DESCRIPTION: This script 
# DEVELOPER:
# Tue Dec 17 11:16:04 2019 ------------------------------
# -------------------------------------------------------------------------

# load library -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse")

# load data -------------------------------------------------------------------------
setwd("C:/Users/Gebruiker/Desktop/wifi")
wifi <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                  sep = ",")

# process -------------------------------------------------------------------------



# output -------------------------------------------------------------------------

saveRDS()


