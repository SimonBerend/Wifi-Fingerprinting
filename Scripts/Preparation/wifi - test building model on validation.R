# -------------------------------------------------------------------------
# GOAL: TEST BUILDING MODEL ON VALIDATION DATA
# DESCRIPTION: Test vsm model based on all data (after prep)
# on the validation data.
# DEVELOPER: BEREND
# Mon Jan 06 14:35:03 2020 ------------------------------
# -------------------------------------------------------------------------

# Packs -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse", "caret",
               "data.table", "lubridate", "pls",
               "scatterplot3d", "plotly")

# Source ------------------------------------------------------------------
source("Scripts/Preparation/wifi - data preparation.R")

# Create train & test set -------------------------------------------------
set.seed(123)


# Feature Selection: WAPS and BUILDINGID
wifi_train <- wifi %>% select(starts_with("WAP"), FLOOR) 
wifi_test <- wifi_val %>% select(starts_with("WAP"), FLOOR)

# Set FLOOR as ordered factor
wifi_train$FLOOR <- factor(wifi_train$FLOOR, ordered = TRUE)
wifi_test$FLOOR <- factor(wifi_test$FLOOR, ordered = TRUE)

# Train Model -------------------------------------------------------------------------
# modify resampling method : repeatedcv = K-fold Cross Validation
ctrl <- trainControl(method = "repeatedcv",
                     verboseIter = TRUE,
                     repeats = 3)

# train Model
# try "pca" in preProcess
start_time <- Sys.time()

KNN_floor_model <- train(FLOOR ~ .,
                          wifi_train,
                          method = "knn",
                         tuneGrid = expand.grid(k = c(1:5)),
                          #tuneLength = 10,
                          trControl = ctrl
                          #,
                          #preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

# run predictions
KNN_floor_predictions <-predict(KNN_floor_model, wifi_test)

# Create confusion matrix
# wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)
KNN_floor_cm <- confusionMatrix(KNN_floor_predictions, wifi_test$FLOOR)
print(KNN_floor_cm)
