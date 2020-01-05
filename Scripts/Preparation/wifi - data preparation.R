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

# Some Packs? -------------------------------------------------------------



# load data ---------------------------------------------------------------
setwd("C:/Users/Gebruiker/Desktop/wifi")
wifi <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                 sep = ",",
                 na.strings = "100"
)

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


# Remove redundant WAPS ---------------------------------------------------
# select from wifi columns that have non-NA observations
wifi2 <- wifi[colSums(!is.na(wifi)) > 0]

# Make a vector of useful WAP names
useful_waps <- wifi2 %>% select(starts_with("WAP")) %>% colnames()

# compare that with all WAP names
redundant_waps <- setdiff(wap_names, useful_waps)

# remove redundant waps from vaidation set
wifi_valid <- wifi_valid[ , !names(wifi_valid) %in% redundant_waps]


# replace NaN with -150 value ---------------------------------------------
wifi[is.na(wifi)] <- -150

