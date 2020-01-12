# run to get test with coordinates
wifi_test <- wifi_val %>% filter(BUILDINGID == 2) %>% select(starts_with("WAP"), FLOOR, LONGITUDE, LATITUDE)
# Set FLOOR as ordered factor
wifi_test$FLOOR <- factor(wifi_test$FLOOR, ordered = TRUE)
# remove redundant waps
wifi_test <- wifi_test[ , !names(wifi_test) %in% building_redundant_waps]
# Set threshold : Use only WAPs with value above ... ----------------------------
wifi_test[,building_useful_waps][wifi_test[,building_useful_waps] < 0.7] <- 0
# remove suspect waps 71 & 72
# wifi_test <- wifi_test[ , !names(wifi_test) %in% c("WAP071", "WAP072")]

# waps involved with f2 > f0 error -------------------------------------------------------------
check <- cbind(wifi_test, predict_floor)
check <- check %>% filter(FLOOR == 4, predict_floor %in% c(0, 4))
check <- check %>% mutate (interaction = interaction(FLOOR, predict_floor))
check <- check %>% mutate (correct = interaction == 4.4)


# info <- check %>% select(-starts_with("WAP"))


# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(check,function(x) length(unique(x)))
check <- subset(check, select=uniquelength>1)
# check <- cbind(info, check)


# check <- check %>% filter(LONGITUDE > -7640 & LONGITUDE < -7620 & LATITUDE > 4864935 & LATITUDE < 4864980)
check <- check %>% filter(LATITUDE < 4864775)
check <- check %>% filter(LATITUDE > 4864812)


ggplot(check,aes(x= LONGITUDE, y = LATITUDE)) +
  geom_jitter(aes(color = interaction)) +
  ggtitle(label = paste("Predicted/actual floor 2")) +
  labs(color = "Interaction")

this <- check %>% filter(predict_floor == 2)
uniquelength <- sapply(this,function(x) length(unique(x)))
this <- subset(this, select=uniquelength>1)
more_waps <- this %>% select(starts_with("WAP")) %>% colnames()

# waps involved with f1 > f3 error -------------------------------------------------------------
check_f1f3 <- cbind(wifi_test, predict_floor)
check_f1f3 <- check_f1f3 %>% filter(FLOOR == 1, predict_floor == 3)
# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(check_f1f3,function(x) length(unique(x)))
check_f1f3 <- subset(check_f1f3, select=uniquelength>1)
names_f1f3 <- names(check_f1f3)



bla_suspects <- bla[, names(bla) %in% suspects_b1]


# een aantal obs op verdieping 0 worden op verdieping 2 voorspelt
# het kan zijn dat er waps van verdieping 2 naar verdieping 0 zijn verschoven

# welke waps zijn er bij de training op verdieping 2 in het spel ----------
train_2 <- wifi_train %>% filter(FLOOR == 2)
uniquelength <- sapply(train_2,function(x) length(unique(x)))
train_2 <- subset(train_2, select=uniquelength>1)

names_t2 <- names(train_2)
# welke waps zijn er tijdens de test op verdieping 0 in het spel ----------

test_0 <- wifi_test %>% filter(FLOOR == 0)
uniquelength <- sapply(test_0,function(x) length(unique(x)))
test_0 <- subset(test_0, select=uniquelength>1)

names_t0 <- names(test_0)

# wat is de overeenkomst daartussen ---------------------------------------
suspects_b1 <- intersect(names_t0, names_t2)

