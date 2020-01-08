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

# Train Model -------------------------------------------------------------------------
# modify resampling method : repeatedcv = K-fold Cross Validation
ctrl <- trainControl(method = "repeatedcv",
                     verboseIter = TRUE,
                     repeats = 3)

# train Model
# try "pca" in preProcess
start_time <- Sys.time()

SVM_floor_model <- train(FLOOR ~ .,
                          wifi_train,
                          method = "knn",
                          tuneLength = 10,
                          trControl = ctrl
                          #,
                          #preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

# run predictions
SVM_floor_predictions <-predict(SVM_floor_model, wifi_test)

# Create confusion matrix
wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)
SVM_floor_cm <- confusionMatrix(SVM_floor_predictions, wifi_test$FLOOR)
print(SVM_floor_cm)
