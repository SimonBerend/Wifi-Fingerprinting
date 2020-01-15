# Packs -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "e1071", "plotly", "dplyr", "dbplyr",
                "tidyverse", "caret", "data.table", "pls")

# Source ------------------------------------------------------------------
source("Scripts/Preparation/wifi - data preparation.R")

# Plotly Phones -------------------------------------------------------------------
wifi$PHONEID <- as.factor(wifi$PHONEID)
plot_ly(wifi, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~PHONEID)