# -------------------------------------------------------------------------
# GOAL: A model to predict Building ID
# DEVELOPER: BEREND
# Mon Jan 06 14:35:03 2020 ------------------------------
# -------------------------------------------------------------------------

# Packs -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("e1071", "dplyr", "dbplyr", "tidyverse",
               "caret", "data.table", "pls", "magrittr")

# Source ------------------------------------------------------------------
source("Scripts/Preparation/wifi - data preparation.R")

# Create train & test set -------------------------------------------------
set.seed(123)

# Feature Selection: WAPS and BUILDINGID
wifi_train <- wifi %>% select(starts_with("WAP"), BUILDINGID) 
wifi_test <- wifi_val %>% select(starts_with("WAP"), BUILDINGID)

# Set FLOOR as ordered factor
wifi_train$BUILDINGID %<>% as.factor()
wifi_test$BUILDINGID %<>% as.factor()

# Train Model -------------------------------------------------------------------------
# modify resampling method : repeatedcv = K-fold Cross Validation
ctrl <- trainControl(method = "repeatedcv",
                     verboseIter = TRUE,
                     repeats = 3)

# train Model
model_building_knn <- train(BUILDINGID ~ .,
                            wifi_train,
                            method = "knn",
                            tuneGrid = expand.grid(k = c(1:5)),
                            trControl = ctrl
)

# run predictions
building_predictions <-predict(model_building_knn, wifi_test)

# Create confusion matrix
building_confusion_matrix <- confusionMatrix(building_predictions, wifi_test$BUILDINGID)
print(building_confusion_matrix)

# save model
saveRDS(model_building_knn, file = "Data/Clean/building_model_knn.rds")
