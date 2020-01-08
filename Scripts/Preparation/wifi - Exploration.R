# -------------------------------------------------------------------------
# GOAL: Explore Wifi Data 
# DESCRIPTION: EDA
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


# some stats  ---------------------------------------------------------------

stats1 <- data.frame(
  Min = apply(wifi_main, 2, min), # minimum
  Q1 = apply(wifi_main, 2, quantile, 1/4), # First quartile
  Med = apply(wifi_main, 2, median), # median
  Mean = apply(wifi_main, 2, mean), # mean
  Q3 = apply(wifi_main, 2, quantile, 3/4), # Third quartile
  Max = apply(wifi_main, 2, max) # Maximum
)
stats2 <- data.frame(
  Min = apply(wifi_valid, 2, min), # minimum
  Q1 = apply(wifi_valid, 2, quantile, 1/4), # First quartile
  Med = apply(wifi_valid, 2, median), # median
  Mean = apply(wifi_valid, 2, mean), # mean
  Q3 = apply(wifi_valid, 2, quantile, 3/4), # Third quartile
  Max = apply(wifi_valid, 2, max) # Maximum
)


# date --------------------------------------------------------------------

wifi_main$TIMESTAMP <- as_datetime(wifi_main$TIMESTAMP)

# building image ----------------------------------------------------------
wifi_main$PHONEID <- as.factor(wifi_main$PHONEID) 
building_phone <- 
  ggplot(wifi_main,aes(x= LONGITUDE, y = LATITUDE)) +
  geom_jitter(aes(color = PHONEID))

savePlot

wifi_valid$PHONEID <- as.factor(wifi_valid$PHONEID) 
# building <- 
ggplot(wifi_valid,aes(x= LONGITUDE, y = LATITUDE)) +
    geom_(aes(color = PHONEID))
  

# visualize ---------------------------------------------------------------
wifi$PHONEID <- as.factor(wifi$PHONEID)

# errors on floor 0
ggplot() +
  geom_jitter(data = wifi %>% filter(FLOOR == 0, BUILDINGID == c(0, 1)),
              aes(x= LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter(data = errors %>% filter(FLOOR == c(0,1), BUILDINGID == c(0, 1)),
              aes(x= LONGITUDE, y = LATITUDE), color = "darkred") +
  ggtitle(label = paste("SUP")) +
  labs(color = "YO")

# errors on floor 1
ggplot() +
  geom_jitter(data = wifi %>% filter(FLOOR == 1, BUILDINGID == c(0, 1)),
              aes(x= LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter(data = errors %>% filter(FLOOR == c(0,1), BUILDINGID == c(0, 1)),
              aes(x= LONGITUDE, y = LATITUDE), color = "darkred") +
  ggtitle(label = paste("SUP")) +
  labs(color = "YO")

