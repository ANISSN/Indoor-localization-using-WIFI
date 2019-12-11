# ###########################################################################-
# GOAL: Wifi Locationing
# DESCRIPTION: First analysis
# AUTHOR: Aniss N
# ###########################################################################-

setwd("C:\\Users\\nisso\\Desktop\\Ubiqum\\Projects")

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(tibble)
library(lubridate)
library(caret)
library(plotly)
library(tidyr)
library(foreign)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(reshape2)

# Importing data ----------------------------------------------------------

wifi_training <- read_csv("./Module 3 - Task 3/Raw Data/trainingData.csv")

# Checking data
#summary(wifi_training[521:529])
#sum(is.na(wifi_training))

wifi_training$FLOOR <- factor(wifi_training$FLOOR)
wifi_training$BUILDINGID <- factor(wifi_training$BUILDINGID)
wifi_training$SPACEID <- factor(wifi_training$SPACEID)
wifi_training$RELATIVEPOSITION <- factor(wifi_training$RELATIVEPOSITION)
wifi_training$USERID <- factor(wifi_training$USERID)
wifi_training$PHONEID <- factor(wifi_training$PHONEID)
wifi_training$TIMESTAMP <- as_datetime(wifi_training$TIMESTAMP)

#sum(duplicated(wifi_training) | duplicated(wifi_training, fromLast = TRUE))
#wifi_training %>% 
#  filter(duplicated(wifi_training) | duplicated(wifi_training, fromLast = TRUE)) %>% 
#  arrange(TIMESTAMP)
wifi_training <- unique(wifi_training)

# Replacing values that are 100 to -110
wifi_training[1:520] <- mutate_all(wifi_training[1:520],~replace(., . == 100, -110))
wifi_training[1:520] <- mutate_all(wifi_training[1:520],~replace(., . < -90, -110))

# Removing zero variance columns

# wifi_training[1:520] %>%
#   t() %>% as_tibble() %>% 
#   mutate(mean = rowMeans(.)) %>%
#   filter(mean == -110)

# Take off the columns with no signals
wifi_training[1:520] %>% 
  select(as.numeric(which(apply(wifi_training[1:520], 2, var) != 0))) %>% 
  cbind(wifi_training[521:529]) %>% as_tibble() -> wifi_training

wifi_training %>% 
  select(-WAP248,-WAP113,-WAP114,-WAP186,-WAP187,-WAP180,-WAP181,-WAP189,-WAP046) -> wifi_training

# Optionnal Converting dBm to mW ----------------------------------------------------------

# wifi_training[1:404] <- lapply(wifi_training[1:404], function(x) {
#   ifelse(x!=-110, 10^(x/10), -110)
# })
# 
# saveRDS(wifi_training,"./Module 3 - Task 3/New Data/wifi_training_pp_nolog.rds")

###########################################################################################
# Validation data
###########################################################################################

wifi_validation <- read_csv("./Module 3 - Task 3/Raw Data/validationData.csv")

wifi_validation$FLOOR <- factor(wifi_validation$FLOOR)
wifi_validation$BUILDINGID <- factor(wifi_validation$BUILDINGID)
wifi_validation$SPACEID <- factor(wifi_validation$SPACEID)
wifi_validation$RELATIVEPOSITION <- factor(wifi_validation$RELATIVEPOSITION)
wifi_validation$USERID <- factor(wifi_validation$USERID)
wifi_validation$PHONEID <- factor(wifi_validation$PHONEID)
wifi_validation$TIMESTAMP <- as_datetime(wifi_validation$TIMESTAMP)

# Removing duplicates
wifi_validation <- unique(wifi_validation)

# Selecting the same columns as training ---------------------------------------------------

# Take off the columns with no signals
wifi_validation[1:520] %>%
  select(as.numeric(which(apply(wifi_validation[1:520], 2, var) != 0))) %>%
  cbind(wifi_validation[521:529]) %>% as_tibble() -> wifi_validation

wifi_training %>%
  select(intersect(colnames(wifi_training), colnames(wifi_validation))) -> wifi_training

wifi_validation %>% 
  select(colnames(wifi_training)) -> wifi_validation


# Replacing values that are 100 to -110
wifi_validation[1:(length(wifi_validation)-9)] <- mutate_all(wifi_validation[1:(length(wifi_validation)-9)],
                                                             ~replace(., . == 100, -110))

# Identify the observations that we can not change the values under -85
wifi_validation %>% 
  mutate(ID = seq.int(n())) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  filter(value != -110) %>% 
  mutate(iflow = if_else(value < -85,1,0)) %>% 
  group_by(ID,iflow) %>% 
  summarise(n = n()) %>% 
  filter(!(duplicated(ID) | duplicated(ID,fromLast = T))) %>% 
  filter(iflow == 1) %>% pull(ID) -> ID_val_not_change

# Replace values less than -85 only if there are other WAP we can rely on
wifi_validation[1:(length(wifi_validation)-9)] %>%
  mutate(ID = seq.int(n())) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  mutate(value = if_else(value < -85 & !(ID %in% ID_val_not_change), -110, value)) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% 
  select(-ID) -> wifi_validation[1:(length(wifi_validation)-9)]

# Replace the Building 1 values in there are more than 10 waps working (potential noise)
# wifi_validation[1:292] %>%
#   mutate(ID = seq.int(n())) %>% 
#   pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
#   left_join(wap_vs_b) %>% 
#   mutate(Building2 = if_else(value == -110,0,1)) %>% 
#   mutate(id2 = if_else(Building2 == 0, "NA", paste0(ID,Building2))) %>% 
#   add_count(id2) %>%
#   mutate(value = if_else(Building == 1 & n > 10 & value < -84, -110, value)) %>%
#   select(ID,WAP,value) %>% 
#   pivot_wider(names_from = WAP, values_from = value) %>% 
#   select(-ID) -> wifi_validation[1:292]


###########################################################################################
# Select the best record
###########################################################################################

#wifi_training %>% mutate(ID = seq.int(nrow(wifi_training))) -> wifi_training

wifi_training %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION,USERID,PHONEID,WAP,TIMESTAMP) %>% 
  summarise(value = if_else(min(value) == -110, -110, max(value))) %>% 
  ungroup() -> wifi_training_long

# wifi_training_long %>% 
#   pivot_wider(names_from = WAP, values_from = value) -> wifi_training_nodup

as_tibble(dcast(wifi_training_long,
                LONGITUDE+LATITUDE+FLOOR+BUILDINGID+SPACEID+RELATIVEPOSITION+USERID+PHONEID+TIMESTAMP~WAP,
                value.var = "value")) -> wifi_training_nodup

wifi_training_nodup <- wifi_training_nodup[c(10:length(wifi_training_nodup),1:9)]

wifi_training_nodup %>%
  mutate(mean = rowMeans(.[1:(length(.)-9)])) %>%
  filter(mean != -110) %>%
  select(-mean) -> wifi_training_nodup

saveRDS(wifi_training_nodup,"./Module 3 - Task 3/New Data/wifi_training_pp_nodup.rds")


# Identify the rows with no signal : Training ----------------------------------------

wifi_training %>% 
  mutate(mean = rowMeans(.[1:(length(wifi_training)-9)])) %>% 
  filter(mean != -110) %>%
  select(-mean) -> wifi_training

# Removing the rows with no signal : Validation  ----------------------------------------
wifi_validation %>% 
  mutate(mean = rowMeans(.[1:(length(wifi_validation)-9)])) %>% 
  filter(mean != -110) %>%
  select(-mean)

saveRDS(wifi_training,"./Module 3 - Task 3/New Data/wifi_training_pp.rds")
saveRDS(wifi_validation,"./Module 3 - Task 3/New Data/wifi_validation_pp.rds")
