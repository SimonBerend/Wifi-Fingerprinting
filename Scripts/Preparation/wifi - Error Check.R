# -------------------------------------------------------------------------
# GOAL: ERROR CHECK ON FLOOR PREDICTIONS
# DESCRIPTION: Here, I develop methods to check error. I start with the floor model,
# but methods may be used for other predictions as well.
# DEVELOPER: BEREND
# Wed Jan 08 10:27:42 2020 ------------------------------



THE_floor_cm["overall"]

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

ggplot(error_check %>%  filter(FLOOR == 4),aes(x= LONGITUDE, y = LATITUDE)) +
  geom_jitter(aes(color = correct)) +
  ggtitle(label = "Predicted/actual floors")

# analyse errors ----------------------------------------------------------

# one way to filter out errors
errors <- val_error_check %>% filter(BUILDINGID != pred_svm_bid)
# and another way
errors <- error_check %>% filter(correct == FALSE)

errors_per_floor <- errors %>% group_by(FLOOR) %>% summarise(n = n())


false_per_floor <- errors %>% group_by(THE_floor_predictions, FLOOR) %>% 
  summarise(times = n()) 
#%>% filter(times > 5)

false_ground <- errors %>%  filter(THE_floor_predictions == 0 & FLOOR == 1)
false_ground %>% group_by(BUILDINGID) %>% summarise(n=n())

