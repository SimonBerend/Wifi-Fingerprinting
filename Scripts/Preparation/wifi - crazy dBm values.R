# -------------------------------------------------------------------------
# GOAL: FIND THE ZERO FUCK 
# DESCRIPTION: Some observations give 0 dBm. Here, I aims to find out whats up
# DEVELOPER: Berend
# Wed Jan 08 15:34:34 2020 ------------------------------
# -------------------------------------------------------------------------

# Be sure to use the wifi data with sufficient info

# dunno
# zero_dbm_obs <- wifi[apply(wifi[, useful_waps ], 1, max) == 0.1,]

# find observations with 0 dBm
zero_dbm <- wifi[apply(wifi[, useful_waps ], 1, max) >= -25,]

# throw away zeroVar WAPS for clarity
zero_dbm <- zero_dbm[, apply(zero_dbm, 2, mean) == -120]

# find their PHONEID and other info
xtra_info <- wifi[rownames(zero_dbm), ] %>% select(PHONEID, BUILDINGID, FLOOR, LONGITUDE, LATITUDE)
# and bind that
zero_dbm <- cbind(xtra_info, zero_dbm)


# visualize ---------------------------------------------------------------
zero_dbm$PHONEID <- as.factor(zero_dbm$PHONEID)

ggplot(zero_dbm, aes(x= LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter() +
  geom_jitter(data = errors, aes(x= LONGITUDE, y = LATITUDE), color = "darkred") +
  ggtitle(label = paste("SUP")) +
  labs(color = "YO")


