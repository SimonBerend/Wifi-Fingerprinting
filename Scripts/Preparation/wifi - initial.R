# -------------------------------------------------------------------------
# GOAL: run first (benchmark) prediction model
# DESCRIPTION: In this script, I run an initial (benchmark) model 
# to predict the BUILDINGID based on only WAP values. It can be used 
# to test future hypotheses about feature selection,
# pre-pocessing, and model selection.
# 
# DEVELOPER: Berend
# Tue Dec 17 11:16:04 2019 ------------------------------
# -------------------------------------------------------------------------

# load library -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "tidyverse", "caret",
               "data.table", "lubridate")

# load data -------------------------------------------------------------------------
setwd("C:/Users/Gebruiker/Desktop/wifi")
wifi_main <- read.csv("Data/Raw/UJIndoorLoc/trainingData.csv",
                  sep = ",")

# wifi_valid <- read.csv("Data/Raw/UJIndoorLoc/validationData.csv",
#                 sep = ",")

# Sample and some Feat selection -------------------------------------------------
set.seed(123)

# sample data
wifi_sample <- sample_frac(wifi_main, size = 0.1, replace = FALSE)


# drop WAPs with only '100' value
# get names of unused WAPS
useless_wap_names <- colnames(wifi_main[,sapply(wifi_main, function(x) mean(x))==100])
# remove WAPS that with avg value 100
wifi_sample = wifi_sample[,!sapply(wifi_sample, function(x) mean(x))==100]

# BUILDINGID as factor
wifi_sample$BUILDINGID <- as.factor(wifi_sample$BUILDINGID)

# Split data > train/test
# Determine , y = dependent variable, 
# p = percentage of data in trainingset,don't list result 
inTrain <- createDataPartition(y = wifi_sample$BUILDINGID, p = .75, list = FALSE)
# Output is a string of values that represent row numbers
# create training/test set based on those values
wifi_train <- wifi_sample[inTrain, ]
wifi_test2 <- wifi_sample[-inTrain, ]

# Only WAPS and BUILDINGID in train/test sets
wifi_train <- wifi_train %>% select(starts_with("WAP"),matches("BUILDINGID"))
wifi_test <- wifi_test2 %>% select(starts_with("WAP"),matches("BUILDINGID"))

# # train Model -------------------------------------------------------------------------
# try "pca" in preProcess
# check tuneGrid/control (in cust behav script)
start_time <- Sys.time()

wifi_fit_knn <- train(BUILDINGID ~ .,wifi_train,
                            method = "knn",
                            preProcess = c("scale","center")
)

end_time <- Sys.time()
end_time - start_time

# do predictions on wifi_test
wifi_predict_knn <-predict(wifi_fit_knn, wifi_test)

# Create confusion matrix
cm_knn <-confusionMatrix(wifi_predict_knn, wifi_test$BUILDINGID)
print(cm_knn)



# visualize predictions ---------------------------------------------------
which(wifi_predict_knn!=wifi_test$BUILDINGID)
wifi_test[242,]

# get coordinations back
wifi_test_and_predicted <-cbind(wifi_test2, wifi_predict_knn)

# visulize wrong predictions
ggplot(wifi_test_and_predicted, aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "grey",
             alpha = 0.5) +
  geom_point(data = wifi_test_and_predicted[252,],
             aes(LONGITUDE,LATITUDE),
                 color = "red") +
  geom_point(data = wifi_test_and_predicted[495,],
             aes(LONGITUDE, LATITUDE),
                 color = "red")


# output -------------------------------------------------------------------------

saveRDS()


