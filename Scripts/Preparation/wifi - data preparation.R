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


# Remove redundant WAPS ---------------------------------------------------
# select from wifi columns that have non-NA observations
wifi <- wifi[colSums(!is.na(wifi)) > 0]

# Make a vector of useful WAP names
useful_waps <- wifi %>% select(starts_with("WAP")) %>% colnames()

# compare that with all WAP names
redundant_waps <- setdiff(wap_names, useful_waps)

# remove redundant waps from vaidation set
wifi_val <- wifi_val[ , !names(wifi_val) %in% redundant_waps]


# replace NaN with -150 value ---------------------------------------------
wifi[is.na(wifi)] <- -150
wifi_val[is.na(wifi_val)] <- -150

# # function below is not working
# # why?
# replace.na.with.value <- function(x, y){
#   # x = data.frame
#   # y = value that takes place of NA's
#   x[is.na(x)] <- y
# }

# as.factor to BUILDINGID and FLOOR ---------------------------------------
wifi[,c("BUILDINGID", "FLOOR")] <- apply(wifi %>% select(BUILDINGID, FLOOR), 2 , as.factor)
wifi_val[,c("BUILDINGID", "FLOOR")] <- apply(wifi_val %>% select(BUILDINGID, FLOOR), 2 , as.factor)
