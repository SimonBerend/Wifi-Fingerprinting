# -------------------------------------------------------------------------
# GOAL: ERROR CHECK ON FLOOR PREDICTIONS
# DESCRIPTION: Here, I develop methods to check error. I start with the floor model,
# but methods may be used for other predictions as well.
# DEVELOPER: BEREND
# Wed Jan 08 10:27:42 2020 ------------------------------

# bind predictions to validation set with all info
error_check <- cbind(wifi_val, KNN_floor_predictions)

# visualize errors --------------------------------------------------------
visualise_pred <- function(data, floor){
  
  ggplot(data %>% filter(FLOOR == floor),aes(x= LONGITUDE, y = LATITUDE)) +
    geom_jitter(aes(color = interaction(BUILDINGID, pred_svm_bid, sep = " - "))) +
    ggtitle(label = paste("Predicted/actual building on floor", floor)) +
    labs(color = "Interaction")
  
}

visualise_pred(val_error_check, 1)

# -------------------------------------------------------------------------

# extra columns for actual/predicted value interaction
error_check <- error_check %>% mutate(correct = FLOOR == KNN_floor_predictions)

# visualize
# %>%  filter(FLOOR == 4)
ggplot(error_check %>% filter(correct == FALSE),aes(x= LONGITUDE, y = LATITUDE)) +
  geom_jitter(aes(color = correct)) +
  ggtitle(label = "Predicted/actual floors")

# analyse errors ----------------------------------------------------------

# one way to filter out errors
# errors <- val_error_check %>% filter(BUILDINGID != pred_svm_bid)
# and another way
errors <- error_check %>% filter(correct == FALSE)

# where are the errors
errors_per_floor <- errors %>% group_by(BUILDINGID, FLOOR) %>% summarise(n = n())
# B1F1 : 68
# B0F2 : 39
# B0F1 : 38

# BUILDING 1 FLOOR 1
b1f1 <- errors %>% filter(BUILDINGID == 1, FLOOR == 1)
# b1f1_info <- b1f1 %>% select(-strats_with("WAP"))

# get rid of zero variance waps
bfwaps <- b1f1 %>% select(starts_with("WAP")) %>% colnames()
nearzerowaps <- b1f1[,bfwaps] %>% nearZeroVar()
b1f1 <- b1f1[,-nearzerowaps]

# where do the false predictions go?
histogram(b1f1$KNN_floor_predictions, )

# relevant columns
b1_colnames <- colnames(b1f1)

false_per_floor <- errors %>% group_by(THE_floor_predictions, FLOOR) %>% 
  summarise(times = n()) 
#%>% filter(times > 5)

false_ground <- errors %>%  filter(THE_floor_predictions == 0 & FLOOR == 1)
false_ground %>% group_by(BUILDINGID) %>% summarise(n=n())

