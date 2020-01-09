# -------------------------------------------------------------------------
# GOAL: B1F1
# DESCRIPTION: What happens between B1F1 and B1F2
# DEVELOPER: BEREND
# Thu Jan 09 12:14:38 2020 ------------------------------
install.packages("plotly")
library(plotly)

# plot B1 F1,2
wifi_b1f1 <- wifi %>% filter(BUILDINGID == 1, FLOOR %in% c(1, 2))

wifi_b1f1 <- wifi_b1f1[,names(wifi_b1f1) %in% b1_colnames]
wifi_b1f1$PHONEID <- as.factor(wifi_b1f1$PHONEID)


plot_ly(wifi_b1f1, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~PHONEID)


b1f1 %>% group_by(USERID) %>% summarise(n=n())

wifi_b1f1 %>% group_by(PHONEID) %>% summarise(n=n())

# wapnames
wapwap <- b1_colnames[1:32]

# summarise wap values per phone
phone14 <- wifi_b1f1 %>% filter(PHONEID == 14) %>%  
    group_by(FLOOR) %>%
  summarise_at(wapwap, max)



# visualize plot B1
wifi_b1 <- wifi %>% filter(BUILDINGID == 1)

wifi_b1 <- wifi_b1[,names(wifi_b1) %in% b1_colnames]
wifi_b1$PHONEID <- as.factor(wifi_b1$PHONEID)

plot_ly(wifi_b1, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~PHONEID)



# hypo: WAPs used to localize floor 1 in B1 are gone in validation set 
# therefore, WAPs of floor 2 took over

# waps in full for b1f1
full_b1f1 <- wifi %>% filter(BUILDINGID == 1, FLOOR == 1)

#get rid of zeroVar waps
full_b1f1_waps <- full_b1f1 %>% select(starts_with("WAP")) %>% colnames()
nearzerowaps_b1f1 <- full_b1f1[,full_b1f1_waps] %>% nearZeroVar()
full_b1f1 <- full_b1f1[,-nearzerowaps_b1f1]

# get the names of the waps involved and compare with the waps involved with the false predictions
full_wapwap <- full_b1f1 %>% select(starts_with("WAP")) %>% colnames()
who_is_it <- setdiff(wapwap, full_wapwap)

# waps in val for b1f1

# which wap in full is not in val
