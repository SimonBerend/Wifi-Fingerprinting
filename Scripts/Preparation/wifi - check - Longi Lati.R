# run to get test with coordinates
wifi_test <- wifi_val %>% filter(BUILDINGID == 1) %>% select(starts_with("WAP"), FLOOR, LONGITUDE, LATITUDE)
# Set FLOOR as ordered factor
wifi_test$FLOOR <- factor(wifi_test$FLOOR, ordered = TRUE)
# remove redundant waps
wifi_test <- wifi_test[ , !names(wifi_test) %in% building_redundant_waps]
# Set threshold : Use only WAPs with value above ... ----------------------------
wifi_test[,building_useful_waps][wifi_test[,building_useful_waps] < 0.8] <- 0
# remove suspect waps ...
# wifi_test <- wifi_test[ , !names(wifi_test) %in% c("WAPxxx", "WAPxxx")]


# Nice density plot -------------------------------------------------------

check <- wifi_test %>% 
  add_residuals(model = model_B1_longitude_knn, var = "knn_resid")



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
# hist(lon_error_distribution_knn2)


#lon_error_distribution_knn <- check$LATITUDE - check$predict_B1_lat
#hist(lon_error_distribution_knn)

# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(df,function(x) length(unique(x)))
df <- subset(df, select=uniquelength>1)
# check <- cbind(info, check)


check <- check %>% filter(LONGITUDE > -7490 & LONGITUDE < -7425 & LATITUDE > 4864820 & LATITUDE < 4864880)
# check <- check %>% filter(LATITUDE < 4864775)

check1 <- check %>% filter(LONGITUDE < -7450 & LATITUDE < 4864840)
check2 <- check %>% filter(LONGITUDE > -7440 & LATITUDE > 4864850)
check3 <- check %>% filter(LONGITUDE < -7465 & LATITUDE > 4864860)
check1 <- check1 %>% filter(FLOOR == 1)


uniquelength <- sapply(check1,function(x) length(unique(x)))
check1 <- subset(check1, select=uniquelength>1)


check <- check %>% mutate(outlier = knn_resid > )


ggplot(check)+
  geom_jitter(aes(x= LONGITUDE, y = LATITUDE, color = outlier)) +
#  geom_jitter(aes(x= predict_B1_lon, y = predict_B1_lat), color = "red4") +
  ggtitle(label = paste("Outlier : Residual > +25")) 
# +
  # labs(color = "Interaction")



# crzay plot --------------------------------------------------------------


check %>%
  select(knn_resid, rf_resid) %>%
  pivot_longer(cols = c(knn_resid, rf_resid)) %>%
  # filter(between(value, 0, 10)) %>%
  ggplot(aes(x = value, color = name)) +
  geom_density(alpha = 0.3) +
  annotate("text", x = 35, y = 0.043, label = "      RMSE        Rsquared    MAE") +
  annotate("label", x = 35, y = 0.04, label = "knn : 8.7777022   0.9639885   6.2959340", color = "#F8766D") +
  annotate("label", x = 36, y = 0.037, label = " rf : 10.3734383  0.9497405   7.2915868", color = "#00BFC4")


