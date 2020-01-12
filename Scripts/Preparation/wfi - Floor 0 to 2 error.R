
# waps involved with f0 > f2 error -------------------------------------------------------------
check_f0f2 <- cbind(wifi_test, predict_floor)
check_f0f2 <- check_f0f2 %>% filter(FLOOR %in% c(0,2), predict_floor == 2)
check_f0f2 <- check_f0f2 %>% mutate (interaction = interaction(FLOOR, predict_floor))
check_f0f2 <- check_f0f2 %>% mutate (o.2 = interaction == 0.2)


ggplot(check_f0f2,aes(x= LONGITUDE, y = LATITUDE)) +
  geom_jitter(aes(color = o.2.1)) +
  ggtitle(label = paste("Predicted/actual floor 2")) +
  labs(color = "Interaction")

info <- check_f0f2[, 1:6]
# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(check_f0f2,function(x) length(unique(x)))
check_f0f2 <- subset(check_f0f2, select=uniquelength>1)
check_f0f2 <- cbind(info, check_f0f2)

check_f0f2 <- check_f0f2[, 4:49]
check_f0f2[, 1:3] <- check_f0f2[, 1]
check_f0f2 <- check_f0f2 %>% filter(LONGITUDE > -7525 & LONGITUDE < -7475 & LATITUDE > 4864870 & LATITUDE < 4864890)


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

