# -------------------------------------------------------------------------
# GOAL: Organize Wifi Data 
# DESCRIPTION: This script is a prediction model for the building, 
# with minimal and intuitive transformations to the wifi data. 
# The outcomes serve as a benchmark that can be used to test 
# hypotheses about feature selection, pre-pocessing, and model selection.
# DEVELOPER: Berend
# Tue Dec 17 11:16:04 2019 ------------------------------
# -------------------------------------------------------------------------

# load library -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse", "caret",
               "data.table", "lubridate", "pls")

# load data -------------------------------------------------------------------------
setwd("C:/Users/Gebruiker/Desktop/wifi")
wifi_main <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                  sep = ",")

wifi_valid <- read.csv("Data/Raw/UJIndoorLoc/validationData.csv",
                 sep = ",")


# Create train & test set -------------------------------------------------
set.seed(123)

# sample wifi data: 30 %
wifi_sample <- sample_frac(wifi_main, size = 0.3, replace = FALSE)

# select features
# all WAPS and BUILDINGID
wifi_sample <- wifi_sample %>% select(starts_with("WAP"), matches("BUILDINGID")) 

# Determine , y = dependent variable, 
# p = percentage of data in trainingset,don't list result 
inTrain <- createDataPartition(y = wifi_sample$BUILDINGID, p = .75, list = FALSE)
# Output is a string of values that represent row numbers
# create training/test set based on those values
wifi_train <- wifi_sample[inTrain, ]
wifi_test <- wifi_sample[-inTrain, ]

# process -------------------------------------------------------------------------


##modify resampling method : repeatedcv = K-fold Cross Val
ctrl <- trainControl(method = "repeatedcv",
                     verboseIter = TRUE,
                     repeats = 3)

#mtry <- 2
tunegrid <- expand.grid(.mtry = 5)
# training model with SVM

# train Model
# try "pca" in preProcess
start_time <- Sys.time()

wifi_class_mod_SVM <- train(BUILDINGID ~ .,wifi_main,
                 method = "svmLinear2",
                 tuneLength = 10,
                 preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

SVMPredictions <-predict(SVModel, testData)
# Create confusion matrix
cmSVM <-confusionMatrix(SVMPredictions, testData$Class)
print(cmSVM)

# output -------------------------------------------------------------------------

saveRDS()


