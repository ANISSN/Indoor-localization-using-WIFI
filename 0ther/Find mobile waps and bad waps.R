
# Get the mean of RSSi per building
wifi_training_nodup %>% 
  filter(BUILDINGID == 0) %>%
  mutate(ID = seq.int(n())) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  left_join(wap_vs_b) %>% 
  group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION,USERID,PHONEID,TIMESTAMP,ID,Building) %>% 
  summarise(value = mean(value), n = n()) %>%
  filter(duplicated(ID) | duplicated(ID, fromLast = TRUE)) %>%
  filter(Building == 0) %>% 
  ungroup() %>% 
  summarize(mean = mean(value))


# Find the mobile waps ----------------------------------------------------

# Identify the ones that have high number of obs in different buildings
wifi_training_nodup %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  select(BUILDINGID, WAP, value) %>% 
  filter(value != -110) %>% 
  group_by(WAP, BUILDINGID) %>% 
  summarise(count = n(), value = mean(value)) %>% 
  filter(duplicated(WAP) | duplicated(WAP, fromLast = T)) %>% 
  pivot_wider(names_from = BUILDINGID, values_from = c(count,value)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(ratio_01 = if_else(count_0 == 0 | count_1 == 0, 0,
                            abs(min(c(count_0,count_1))/max(c(count_0,count_1)))*100)) %>% 
  mutate(ratio_02 = if_else(count_0 == 0 | count_2 == 0, 0,
                            abs(min(c(count_0,count_2))/max(c(count_0,count_2)))*100)) %>% 
  mutate(ratio_12 = if_else(count_1 == 0 | count_2 == 0, 0,
                            abs(min(c(count_1,count_2))/max(c(count_1,count_2)))*100)) %>%
  filter(ratio_01 > 50 | ratio_02 > 50 | ratio_12 > 50)
  
  
# Identify waps that have high value of RSSI in different buildings
wifi_training_nodup %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  select(BUILDINGID, WAP, value) %>% 
  filter(value != -110) %>% 
  group_by(WAP, BUILDINGID) %>% 
  summarise(count = n(), value = max(value)) %>% 
  filter(duplicated(WAP) | duplicated(WAP, fromLast = T)) %>% 
  pivot_wider(names_from = BUILDINGID, values_from = c(count,value)) %>% 
  mutate_all(~replace(., is.na(.), -110)) %>% 
  filter((value_0 > -50 & value_1 > -50) |
           (value_0 > -50 & value_2 > -50) |
           (value_1 > -50 & value_2 > -50))
  

# # Identify the worst WAPS in building 1
# wifi_training_nodup %>% 
#   pivot_longer(cols = starts_with("WAP"),
#                names_to = "WAP") %>% 
#   select(BUILDINGID, WAP, value) %>% 
#   mutate(count = if_else(value == -110, 0, 1)) %>% 
#   group_by(WAP, BUILDINGID) %>% 
#   summarise(count = sum(count)) %>%
#   filter(count != 0) %>% 
#   filter(duplicated(WAP) | duplicated(WAP,fromLast = T)) %>% 
#   group_by(WAP) %>% 
#   summarise(count2 = min(count), BUILDINGID = BUILDINGID[which.min(count)]) %>% 
#   filter(BUILDINGID == 1) %>% 
#   arrange(desc(count2)) -> bad_waps_1
# 
# wifi_training_nodup %>% 
#   pivot_longer(cols = starts_with("WAP"),
#                names_to = "WAP") %>% 
#   select(BUILDINGID, WAP, value) %>% 
#   mutate(count = if_else(value == -110, 0, 1)) %>% 
#   group_by(WAP, BUILDINGID) %>% 
#   summarise(count = sum(count)) %>% 
#   group_by(WAP) %>% 
#   summarise(count2 = max(count), BUILDINGID = BUILDINGID[which.max(count)]) %>% 
#   filter(WAP %in% bad_waps_1$WAP)
