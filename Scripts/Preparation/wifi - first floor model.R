# -------------------------------------------------------------------------
# GOAL: FLOOR MODEL
# DESCRIPTION: Try a model to predict FLOOR
# DEVELOPER: BEREND
# Mon Jan 06 11:54:13 2020 ------------------------------
# -------------------------------------------------------------------------

# Packs -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse", "caret",
               "data.table", "lubridate", "pls")

# Source ------------------------------------------------------------------
source("Scripts/Preparation/wifi - data preparation.R")

# Create train & test set -------------------------------------------------
set.seed(123)

# sample wifi data: 50 %
wifi_sample <- sample_frac(wifi, size = 0.5, replace = FALSE)

# Feature Selection: WAPS and FLOOR
wifi_sample <- wifi_sample %>% select(starts_with("WAP"), FLOOR) 

# Determine obs to go in Train set, y = dependent variable, 
# p = percentage of data in trainingset,don't list result 
inTrain <- createDataPartition(y = wifi_sample$FLOOR, p = .75, list = FALSE)
# Output is a string of values that represent row numbers
# create training/test set based on those values
wifi_train <- wifi_sample[inTrain, ]
wifi_test <- wifi_sample[-inTrain, ]


# SVM Model -------------------------------------------------------------------------
# modify resampling method : repeatedcv = K-fold Cross Validation
ctrl <- trainControl(method = "repeatedcv",
                     verboseIter = TRUE,
                     repeats = 3)

# train Model
# try "pca" in preProcess
start_time <- Sys.time()

floor_svm_model <- train(FLOOR ~ .,
                          wifi_train,
                          method = "svmLinear2",
                          tuneLength = 10,
                          trControl = ctrl,
                          preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

# run predictions
predictions_floor_svm <-predict(floor_svm_model, wifi_test)

# Create confusion matrix
cm_floor_svm <- confusionMatrix(predictions_floor_svm, wifi_test$FLOOR)
print(cm_floor_svm)



