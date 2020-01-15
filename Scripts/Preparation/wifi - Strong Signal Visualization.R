# load data ---------------------------------------------------------------
wifi <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                 sep = ",",
                 na.strings = "100"
)


# Summarise at
# note that time stamp is lost
# summarise: mean or median?
wifi <- wifi %>% 
  group_by(LONGITUDE, LATITUDE, FLOOR, BUILDINGID,
           SPACEID, RELATIVEPOSITION, USERID, PHONEID) %>%
  summarise_at(wap_names, mean, na.rm = TRUE) %>% 
  ungroup()


wifi[,wap_names][wifi[,wap_names] > -30] <- NaN
wifi[,wap_names][wifi[,wap_names] < -70] <- NaN
wifi[is.na(wifi)] <- 0

# Droping 0 variance rows
aux_WAPs <- wifi %>% select(starts_with("WAP"))
aux_variance <- apply(aux_WAPs, 1, var)!=0
wifi  <- wifi[aux_variance,]


wifi %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE))+
  geom_jitter() +
  theme_bw()+
  labs(title = "Strong Wifi Signals (-30 - -70 dBm)")
