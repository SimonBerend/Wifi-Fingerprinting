# -------------------------------------------------------------------------
# GOAL: Organize Wifi Data 
# DESCRIPTION: This script is a prediction model for the building, 
# with minimal and intuitive transformations to the wifi data. 
# The outcomes serve as a benchmark that can be used to test 
# hypotheses about feature selection, pre-pocessing, and model selection.
# DEVELOPER: Berend
# Tue Dec 17 11:16:04 2019 ------------------------------
# -------------------------------------------------------------------------

# load library -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse", "caret",
               "data.table", "lubridate")

# load data -------------------------------------------------------------------------
setwd("C:/Users/Gebruiker/Desktop/wifi")
wifi_main <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                  sep = ",")

wifi_valid <- read.csv("Data/Raw/UJIndoorLoc/validationData.csv",
                 sep = ",")


# Create train & test set -------------------------------------------------
set.seed(123)

# sample wifi data: 30 %
wifi_sample <- sample_frac(wifi_main, size = 0.3, replace = FALSE)

# select features
# all WAPS and BUILDINGID
wifi_sample <- wifi_sample %>% select(1:520,524) 

# Determine , y = dependent variable, 
# p = percentage of data in trainingset,don't list result 
inTrain <- createDataPartition(y = wifi_sample$BUILDINGID, p = .75, list = FALSE)
# Output is a string of values that represent row numbers
# create training/test set based on those values
wifi_train <- wifi_sample[inTrain, ]
wifi_test <- wifi_sample[-inTrain, ]

# process -------------------------------------------------------------------------


# output -------------------------------------------------------------------------

saveRDS()


