# -------------------------------------------------------------------------
# GOAL: Adress multiple measurements
# DESCRIPTION: This script serves to adress the multiple 
# observations at a single place/time in the wifi data. 
# I aim to reduce them to a single value. 
# 
# DEVELOPER: Berend
# Fri Dec 20 09:20:00 2019 ------------------------------
# -------------------------------------------------------------------------

# load data -------------------------------------------------------------------------
setwd("C:/Users/Gebruiker/Desktop/wifi")
wifi_try2 <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                     sep = ",",
                     na.strings = "100"
                     )



# check how many observations per unique ID -------------------------------
wifi_check2 <- wifi_try2 %>% group_by(LONGITUDE, LATITUDE, FLOOR, BUILDINGID,
                       SPACEID, RELATIVEPOSITION, USERID, PHONEID) %>% 
  summarise(number_of_obs_per_unique_ID = n())


wifi_check_count <- wifi_check2 %>% group_by(number_of_obs_per_unique_ID) %>% summarise(occurance = n())
tail(wifi_check_count)

# summarise WAP value per unique observation ------------------------------
# vector with WAP names
wap_names <-wifi_main %>% select(starts_with("WAP")) %>% colnames()

# summarise at
# note that time stamp is lost
wifi_try <- wifi_try %>% group_by(LONGITUDE, LATITUDE, FLOOR, BUILDINGID,
                       SPACEID, RELATIVEPOSITION, USERID, PHONEID) %>%
  summarise_at(wap_names, mean, na.rm = TRUE) %>% 
  ungroup()


# check result ------------------------------------------------------------
wifi_try2 %>% filter(LONGITUDE == -7691.338 & LATITUDE == 4864928) %>% select(WAP029)


  wifi_try2 %>% filter  (FLOOR == 1 & BUILDINGID == 0 &
                        SPACEID == 222 & RELATIVEPOSITION == 2 & USERID == 1 & PHONEID == 14) %>% 
    select(WAP029)

  

#  Replace NaN ------------------------------------------------------------
# replace NaN with -150 value ---------------------------------------------

wifi_try[is.na(wifi_try)] <- -150
  

