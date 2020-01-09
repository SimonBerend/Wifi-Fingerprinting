# -------------------------------------------------------------------------
# GOAL: Data Preparation
# DESCRIPTION: In this script, I collect pieces of code that
# together prepare the wifi data for the different models.
# (NOTE: The linear and categorical models might demand 
# different data preperation)
# 
# DEVELOPER: Berend
# Sun Jan 05 17:06:02 2020 ------------------------------
# -------------------------------------------------------------------------

# Packs -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse", "caret",
               "data.table", "lubridate", "pls")

# load data ---------------------------------------------------------------
wifi <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                 sep = ",",
                 na.strings = "100"
)


wifi_val <- read.csv("Data/Raw/UJIndoorLoc/validationData.csv",
                       sep = ",",
                       na.strings = "100")


# summarise WAP value per unique observation ------------------------------
# vector with WAP names
wap_names <- wifi %>% select(starts_with("WAP")) %>% colnames()


# Summarise at
# note that time stamp is lost
# summarise: mean or median?
wifi <- wifi %>% 
  group_by(LONGITUDE, LATITUDE, FLOOR, BUILDINGID,
           SPACEID, RELATIVEPOSITION, USERID, PHONEID) %>%
  summarise_at(wap_names, mean, na.rm = TRUE) %>% 
  ungroup()

# Throw out PHONEID 7 & 19 , those phones crazy
# PHONEID != 7 & 
wifi <- wifi %>% filter(PHONEID !=  19)

# Remove redundant WAPS ---------------------------------------------------
# select from wifi columns that have non-NA observations
wifi <- wifi[colSums(!is.na(wifi)) > 0]
wifi_val <- wifi_val[colSums(!is.na(wifi_val)) > 0]

# Make a vector of useful WAP names
wifi_waps <- wifi %>% select(starts_with("WAP")) %>% colnames()
wifi_val_waps <- wifi_val %>% select(starts_with("WAP")) %>% colnames()

useful_waps <- intersect(wifi_waps, wifi_val_waps)

# compare that with all WAP names to determine redundant waps
redundant_waps <- setdiff(wap_names, useful_waps)

# remove redundant waps from test & validation set
wifi <- wifi[ , !names(wifi) %in% redundant_waps]
wifi_val <- wifi_val[ , !names(wifi_val) %in% redundant_waps]


# replace NaN with -120 value ---------------------------------------------
wifi[is.na(wifi)] <- -120
wifi_val[is.na(wifi_val)] <- -120


# as.factor to BUILDINGID and FLOOR ---------------------------------------
wifi[,c("BUILDINGID", "FLOOR")] <- apply(wifi %>% select(BUILDINGID, FLOOR), 2 , as.factor)
wifi_val[,c("BUILDINGID", "FLOOR")] <- apply(wifi_val %>% select(BUILDINGID, FLOOR), 2 , as.factor)


# make a copy -------------------------------------------------------------

wifi2 <- wifi

# normlize WAP values per row ---------------------------------------
normalize_it <- function(x){
  (x-min(x))/(max(x)-min(x))
}

zscore <- function(x) {
  (x-mean(x))/sd(x)
}

wifi[, useful_waps] <- t(apply(wifi %>% select(starts_with("WAP")), 1, normalize_it))
wifi_val[, useful_waps] <- t(apply(wifi_val %>% select(starts_with("WAP")), 1, normalize_it))
