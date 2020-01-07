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

THE_floor_model <- train(FLOOR ~ .,
                          wifi_train,
                          method = "svmLinear2",
                          tuneLength = 10,
                          trControl = ctrl,
                          preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

# run predictions
THE_floor_predictions <-predict(THE_floor_model, wifi_test)

# Create confusion matrix
wifi_test$FLOOR <- as.factor(wifi_test$FLOOR)
THE_floor_cm <- confusionMatrix(THE_floor_predictions, wifi_test$FLOOR)
print(THE_floor_cm)


# visualize errors --------------------------------------------------------
error_check <- cbind(wifi_val, THE_floor_predictions)

visualise_pred <- function(data, floor){
  
  ggplot(data %>% filter(FLOOR == floor),aes(x= LONGITUDE, y = LATITUDE)) +
    geom_jitter(aes(color = interaction(BUILDINGID, pred_svm_bid, sep = " - "))) +
    ggtitle(label = paste("Predicted/actual building on floor", floor)) +
    labs(color = "Interaction")
  
}
  
visualise_pred(val_error_check, 1)

# -------------------------------------------------------------------------

# extra columns for actual/predicted value interaction
error_check <- error_check %>% mutate(correct = FLOOR == THE_floor_predictions)

ggplot(error_check ,aes(x= LONGITUDE, y = LATITUDE)) +
  geom_jitter(aes(color = correct)) +
  ggtitle(label = "Predicted/actual floors")

# analyse errors ----------------------------------------------------------

errors <- val_error_check %>% filter(BUILDINGID != pred_svm_bid)





