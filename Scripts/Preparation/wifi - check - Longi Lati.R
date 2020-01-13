# run to get test with coordinates
wifi_test <- wifi_val %>% filter(BUILDINGID == 1) %>% select(starts_with("WAP"), FLOOR, LONGITUDE, LATITUDE)
# Set FLOOR as ordered factor
wifi_test$FLOOR <- factor(wifi_test$FLOOR, ordered = TRUE)
# remove redundant waps
wifi_test <- wifi_test[ , !names(wifi_test) %in% building_redundant_waps]
# Set threshold : Use only WAPs with value above ... ----------------------------
wifi_test[,building_useful_waps][wifi_test[,building_useful_waps] < 0.65] <- 0
# remove suspect waps ...
# wifi_test <- wifi_test[ , !names(wifi_test) %in% c("WAPxxx", "WAPxxx")]

# waps involved with f2 > f0 error -------------------------------------------------------------
check <- cbind(wifi_test, predict_B1_lon)
check <- cbind(check, predict_B1_lon)
# check <- check %>% filter(FLOOR == 4, predict_floor %in% c(0, 4))
check <- check %>% mutate (lat_error = abs(LATITUDE - predict_B1_lat))
check <- check %>% mutate (lon_error = abs(LONGITUDE - predict_B1_lon))

check <- check %>% filter(lat_error > 8 | lon_error > 8)
# info <- check %>% select(-starts_with("WAP"))

lon_error_distribution_rf <- check$LONGITUDE - check$predict_B1_lon
hist(abs(lon_error_distribution_rf))

# Nice density plot -------------------------------------------------------

check <- check %>% 
  add_residuals(model = model_B1_longitude_knn, var = "knn_resid") %>%
  add_residuals(model = model_B1_longitude_rf, var = "rf_resid")

check %>%
  select(knn_resid, rf_resid) %>%
  pivot_longer(cols = c(knn_resid, rf_resid)) %>%
  # filter(between(value, 0, 10)) %>%
  ggplot(aes(x = value, color = name)) +
  geom_density(alpha = 0.3) +
  annotate("text", x = 35, y = 0.043, label = "      RMSE        Rsquared    MAE") +
  annotate("label", x = 35, y = 0.04, label = "knn : 8.7777022   0.9639885   6.2959340", color = "#F8766D") +
  annotate("label", x = 36, y = 0.037, label = "rf  : 10.3734383  0.9497405   7.2915868", color = "#00BFC4")


# Joan's density plot -----------------------------------------------------

check %>%
  add_residuals(model = model_B1_longitude_knn) %>%
  mutate(resid = abs(resid)) %>%
  select(lon_error, resid) %>%
  pivot_longer(cols = c(lon_error, resid)) %>%
  filter(between(value, 0, 10)) %>%
  ggplot(aes(x = value, color = name)) +
  geom_density(alpha = 0.3)

# -------------------------------------------------------------------------


# lon_error_distribution_knn2 <- check$LONGITUDE - check$predict_B1_lon
hist(lon_error_distribution_knn2)


#lon_error_distribution_knn <- check$LATITUDE - check$predict_B1_lat
#hist(lon_error_distribution_knn)

# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(check,function(x) length(unique(x)))
check <- subset(check, select=uniquelength>1)
# check <- cbind(info, check)


# check <- check %>% filter(LONGITUDE > -7640 & LONGITUDE < -7620 & LATITUDE > 4864935 & LATITUDE < 4864980)
check <- check %>% filter(LATITUDE < 4864775)
check <- check %>% filter(LATITUDE > 4864812)


ggplot(check)+
  geom_jitter(aes(x= LONGITUDE, y = LATITUDE), color = "gray38", alpha = 0.5) +
  geom_jitter(aes(x= predict_B1_lon, y = predict_B1_lat), color = "red4") +
  ggtitle(label = paste("Please work")) 
# +
  # labs(color = "Interaction")
