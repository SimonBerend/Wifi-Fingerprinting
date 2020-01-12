# -------------------------------------------------------------------------
# GOAL: Train and Test FLOOR Models for Building 1
# DEVELOPER: BEREND
# Fri Jan 10 10:46:06 2020 ------------------------------
# -------------------------------------------------------------------------

# Packs -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse", "caret",
               "data.table", "pls")

# Source ------------------------------------------------------------------
source("Scripts/Preparation/wifi - data preparation.R")

# Set seed -------------------------------------------------
set.seed(123)

# train & test set ------------------------------------------------------------
# subset and feature selection: WAPS and FLOOR
wifi_train <- wifi %>% filter(BUILDINGID == 2) %>% select(starts_with("WAP"), FLOOR) 
wifi_test <- wifi_val %>% filter(BUILDINGID == 2) %>% select(starts_with("WAP"), FLOOR)

# Set FLOOR as ordered factor (doesn't improve model)
wifi_train$FLOOR <- factor(wifi_train$FLOOR, ordered = TRUE)
wifi_test$FLOOR <- factor(wifi_test$FLOOR, ordered = TRUE)


# remove WAPs that are not used in this building --------------------------
# vector 'useful_waps' gives all the wap names in use
# which columns have a mean of -120?

# get wap means in a vector
train_wap_means <- wifi_train[, useful_waps] %>% colMeans()
test_wap_means <- wifi_test[, useful_waps] %>% colMeans()

# make it into a data frame and get colnames of useful waps
train_building_waps <- data.frame(WAP = names(train_wap_means), mean = train_wap_means) %>% 
  filter(mean != 0) %>% select(WAP)
test_building_waps <- data.frame(WAP = names(test_wap_means), mean = test_wap_means) %>% 
  filter(mean != 0) %>% select(WAP)

# Make a vector of useful WAP names
building_useful_waps <- intersect(train_building_waps$WAP, test_building_waps$WAP)

# compare that with all WAP names to determine redundant waps
building_redundant_waps <- setdiff(wap_names, building_useful_waps)

# remove redundant waps from test & validation set
wifi_train <- wifi_train[ , !names(wifi_train) %in% building_redundant_waps]
wifi_test <- wifi_test[ , !names(wifi_test) %in% building_redundant_waps]

# Set threshold : Use only WAPs with value above ... ----------------------------
wifi_train[,building_useful_waps][wifi_train[,building_useful_waps] < 0.8] <- 0
wifi_test[,building_useful_waps][wifi_test[,building_useful_waps] < 0.8] <- 0


# remove suspect waps 
# wifi_train <- wifi_train[ , !names(wifi_train) %in% c("WAP", "WAP", "WAP", "WAP050")]
# wifi_test <- wifi_test[ , !names(wifi_test) %in% c("WAP071", "WAP072", "WAP049", "WAP050")]


# Train Model -------------------------------------------------------------------------
# modify resampling method : repeatedcv = K-fold Cross Validation
# ctrl <- trainControl(method = "repeatedcv",
 #                   verboseIter = TRUE,
  #                  repeats = 3)

# train Model
# try "pca" in preProcess
start_time <- Sys.time()

floor_model <- train(FLOOR ~ .,
                          wifi_train,
                          method = "knn",
                           tuneGrid = expand.grid(k = c(1:5)),
                          # tuneLength = 10,
                          trControl = ctrl
                          #,
                          #preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

# run predictions
predict_floor <-predict(floor_model, wifi_test)

# Create confusion matrix
floor_cm <- confusionMatrix(predict_floor, wifi_test$FLOOR)
print(floor_cm)

# 0.91 radialSigma model (< 0.7 <- 0) is saved as follows
# floor_B1_knn <- floor_cm
# saveRDS(floor_B1_knn, file = "Data/Clean/floor_model_B1_knn.rds")
