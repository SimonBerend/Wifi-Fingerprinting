# -------------------------------------------------------------------------
# GOAL : Wi-Fi Indoor Locationing System
# DESCRIPTION : I develop a pipeline to determine the building,
# floor and coordinates of a random sample from the validation set.
# DEVELOPER : BEREND
# Mon Mar 09 17:39:01 2020 ------------------------------
# -------------------------------------------------------------------------

# Packs -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "e1071", "dplyr", "dbplyr", 
               "tidyverse", "caret", "data.table", "pls")

# Source ------------------------------------------------------------------
source("Scripts/Preparation/wifi - data preparation.R")


# Take a sample from val set to feed the pipeline ---------------------------------------------------------
guy <- sample_n(wifi_val, size = 1, replace = F)



# Check Building ID -------------------------------------------------------

 



# app creation ------------------------------------------------------------

guy <- validation[42:56,]

# preprocess 

# call the model
mod <- read_rds("mod.rds")

# predict position based on a guy
predict(object = mod, newdata = guy, type = "class")

pred_b <- function(guy, model, type, name) {
  library(readr)
  if (type == "B") {
    # guy data preprocess
    
    # predictions
    pred_building <- predict(object = model, newdata = guy, type = "class")
    print(paste("You are in the building", pred_building))
    
    # store results
    write_rds(pred_building, paste0("data/clean/model_results_",name,".rds"))
  }
  
  if (type == "F") {
    # predictions
    # pred_building <- predict(object = model, newdata = guy, type = "class")
    print(paste("You are in the floor", "unkown"))
    
  }
  
}

results <- pred_b(guy = guy, model = mod, type = "B", name = "dt_new")