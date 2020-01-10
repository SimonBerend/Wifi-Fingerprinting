wifi %>%
  gather(starts_with("WAP"), key = "wap", value = "value") %>%
  filter(value != 0) %>%
  # group_by(BUILDINGID, FLOOR) %>%
  # count() %>%
  ggplot(aes(x = value)) +
  geom_density(alpha = 0.3) +
  facet_grid(FLOOR~BUILDINGID)


wifi2 %>%
  gather(starts_with("WAP"), key = "wap", value = "value") %>%
  filter(value != 100) %>%
  # group_by(BUILDINGID, FLOOR) %>%
  # count() %>%
  ggplot(aes(x = value)) +
  geom_density(alpha = 0.3) +
  facet_grid(FLOOR~BUILDINGID)

wifi %>% 
  filter(BUILDINGID == 1, FLOOR == 1) %>% 
  filter_at(vars(useful_waps), any_vars(-75 < .)) %>% 
  group_by(PHONEID) %>% 
  count()
