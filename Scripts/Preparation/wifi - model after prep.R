# -------------------------------------------------------------------------
# GOAL: BUILDING MODEL
# DESCRIPTION: Try a model to predict building ID, after the
# data preparation.
# DEVELOPER: BEREND
# Mon Jan 06 10:10:14 2020 ------------------------------
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

# Feature Selection: WAPS and BUILDINGID
wifi_sample <- wifi_sample %>% select(starts_with("WAP"), BUILDINGID) 

# Determine obs to go in Train set, y = dependent variable, 
# p = percentage of data in trainingset,don't list result 
inTrain <- createDataPartition(y = wifi_sample$BUILDINGID, p = .75, list = FALSE)
# Output is a string of values that represent row numbers
# create training/test set based on those values
wifi_train <- wifi_sample[inTrain, ]
wifi_test <- wifi_sample[-inTrain, ]


# Train Model -------------------------------------------------------------------------

# modify resampling method : repeatedcv = K-fold Cross Validation
ctrl <- trainControl(method = "repeatedcv",
                     verboseIter = TRUE,
                     repeats = 3)

# train Model
# try "pca" in preProcess
start_time <- Sys.time()

sample_mod_knn <- train(BUILDINGID ~ .,
                          wifi_train,
                          method = "knn",
                          tuneGrid = expand.grid(k = c(1:5)),
                          #tuneLength = 10,
                          trControl = ctrl,
                          #preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

# run predictions
sample_predict_knn <-predict(sample_mod_knn, wifi_test)

# Create confusion matrix
wifi_test$BUILDINGID <- as.factor(wifi_test$BUILDINGID)
cm_bid <- confusionMatrix(sample_predict_knn, wifi_test$BUILDINGID)
print(cm_bid)

# perfect score! let's try floor





