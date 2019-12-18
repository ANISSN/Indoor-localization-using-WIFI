# ###########################################################################-
# GOAL: Wifi Locationing
# DESCRIPTION: Floor detection
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

# Import data -------------------------------------------------------------

wifi_training <- readRDS("./Module 3 - Task 3/New Data/wifi_training_pp.rds")
wifi_validation <- readRDS("./Module 3 - Task 3/New Data/wifi_validation_pp.rds")

wifi_training_nodup <- readRDS("./Module 3 - Task 3/New Data/wifi_training_pp_nodup.rds")

# Selecting only building 0 and preprocessing -----------------------------------------------

wifi_training_nodup_b0 <- wifi_training_nodup %>% 
  filter(BUILDINGID == 1) %>% 
  mutate(FLOOR = factor(FLOOR, levels = c(0,1,2,3)))

# Remove all waps that are not used
wifi_training_nodup_b0[1:(length(wifi_training_nodup_b0)-9)] %>%
  select(as.numeric(which(apply(wifi_training_nodup_b0[1:(length(wifi_training_nodup_b0)-9)],
                                2, var) != 0))) %>% 
  cbind(wifi_training_nodup_b0[(length(wifi_training_nodup_b0)-8):length(wifi_training_nodup_b0)]) %>%
  as_tibble() -> wifi_training_nodup_b0


# Removing bad observation
wifi_training_nodup_b0 %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  group_by(WAP,FLOOR) %>%
  summarise(n= n(), value = mean(value)) %>% 
  mutate(freq = n / sum(n)*100) %>% #-> waps_vs_f
  group_by(WAP) %>% 
  summarise(FLOOR_W = FLOOR[which.max(freq)]) -> waps_vs_f

wifi_training_nodup_b0 %>%
  mutate(ID = seq.int(n())) %>%
  filter(FLOOR == 1) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>%
  left_join(waps_vs_f) %>% 
  group_by(ID,FLOOR_W) %>% 
  summarize(n = n()) %>% 
  mutate(freq = n / sum(n)*100) %>%
  group_by(ID) %>% 
  summarize(freq2 = max(freq)) %>% 
  filter(freq2 < 40) %>% pull(ID) -> id0_noisy

wifi_training_nodup_b0 %>%
  mutate(ID = seq.int(n())) %>% 
  filter(!(ID %in% id0_noisy)) %>% 
  select(-ID) -> wifi_training_nodup_b0


# Scaling the obs ---------------------------------------------------------

wifi_training_nodup_b0 %>%
  mutate(ID = seq.int(n())) %>%
  select(-LONGITUDE,-LATITUDE,-FLOOR,-BUILDINGID,-SPACEID,-RELATIVEPOSITION,-USERID,-PHONEID,-TIMESTAMP) %>%
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  mutate(ID = paste0("ID_",ID)) %>% 
  pivot_wider(names_from = ID, values_from = value) -> wifi_training_nodup_b0_scaled

wifi_training_nodup_b0_scaled[2:length(wifi_training_nodup_b0_scaled)] <- 
  lapply(wifi_training_nodup_b0_scaled[2:length(wifi_training_nodup_b0_scaled)],
         function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100))

wifi_training_nodup_b0_scaled %>% 
  pivot_longer(cols = starts_with("ID"), names_to = "ID") %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% 
  select(-ID) -> wifi_training_nodup_b0_scaled

wifi_training_nodup_b0_scaled %>% 
  cbind(wifi_training_nodup_b0[(length(wifi_training_nodup_b0)-8):length(wifi_training_nodup_b0)]) -> wifi_training_nodup_b0

#-----------------------

testing_data %>%
  mutate(ID = seq.int(n())) %>%
  select(-LONGITUDE,-LATITUDE,-FLOOR,-BUILDINGID,-SPACEID,-RELATIVEPOSITION,-USERID,-PHONEID,-TIMESTAMP) %>%
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  mutate(ID = paste0("ID_",ID)) %>% 
  pivot_wider(names_from = ID, values_from = value) -> testing_data_scaled

testing_data_scaled[2:length(testing_data_scaled)] <- 
  lapply(testing_data_scaled[2:length(testing_data_scaled)], 
         function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100))

testing_data_scaled %>% 
  pivot_longer(cols = starts_with("ID"), names_to = "ID") %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% 
  select(-ID) -> testing_data_scaled

testing_data_scaled %>% 
  cbind(testing_data[(length(testing_data)-8):length(testing_data)]) -> testing_data

# First analysis KNN using building ---------------------------------------

# PATH 1: Data used for training and testing
training_data <- wifi_training_nodup_b0
testing_data <- wifi_validation %>% filter(BUILDINGID == 1) %>% mutate(FLOOR = factor(FLOOR, levels = c(0,1,2,3))) %>% 
  select(colnames(training_data))

# PATH 2: Creating sub sets for training 75% and testing 25%
data_to_split <- wifi_training_nodup_b0
set.seed(123)
indTrain <- createDataPartition(y=data_to_split$FLOOR, p=0.75, list=FALSE)
training_data <- data_to_split[indTrain,]
testing_data <- data_to_split[-indTrain,]


# Training models ---------------------------------------------------------

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

#KNN ------------------
set.seed(123)
mod_floor <- train(factor(FLOOR)~.,
                         data = add_column(training_data[1:(length(training_data)-9)],
                                           FLOOR = factor(training_data$FLOOR)),
                         method = "rf",
                         trControl = fitControl, tuneLength = 1, preProc = c("center","scale"),
                         na.action = na.omit)


# Testing the models ------------------------------------------------------

# Choose the model
test_model <- mod_floor
# Apply the model
test_results <- predict(test_model, newdata = testing_data)
test_results_prob <- predict(test_model, newdata = testing_data,type = "prob") %>% mutate(ID = seq.int(n()))
# Show confusion matrix
confusionMatrix(data = factor(test_results, levels = levels(testing_data$FLOOR)), testing_data$FLOOR)


# Checking errors ---------------------------------------------------------

testing_data %>%
  mutate(ID = seq.int(n())) %>% 
  add_column(FLOOR_P = factor(test_results, levels = levels(testing_data$FLOOR))) %>% 
  filter(FLOOR==1 & FLOOR_P==2) %>%
  #filter(FLOOR == 1) %>% 
  arrange(FLOOR) %>% 
  #slice(2L) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% view()


testing_data %>%
  mutate(ID = seq.int(n())) %>% 
  add_column(FLOOR_P = factor(test_results, levels = levels(testing_data$FLOOR))) %>%
  left_join(test_results_prob) %>% 
  filter(FLOOR != FLOOR_P) %>%
  arrange(FLOOR) %>%
  select(ID,`0`,`1`,`2`,`3`)
  
#     ID   `0`   `1`   `2`   `3`
# <int> <dbl> <dbl> <dbl> <dbl>
# 1   249 0.126 0.74  0.13  0.004
# 2   657 0.192 0.8   0.008 0    
# 3   258 0     0.326 0.674 0    
# 4   697 0.04  0.454 0.484 0.022
# 5   713 0.036 0.246 0.718 0    

ggplotly(
testing_data %>%
  add_column(FLOOR_P = factor(test_results, levels = levels(testing_data$FLOOR))) %>% 
  filter(FLOOR != FLOOR_P) %>%
  #filter(FLOOR == 2) %>% 
  arrange(FLOOR_P) %>% 
  #slice(2L) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>%
  ggplot() +
    geom_point(data = testing_data, aes(x = LONGITUDE, y = LATITUDE), alpha = 0.01) +
    geom_point(aes(x = LONGITUDE, y = LATITUDE), color = "darkred", alpha = 1) +
    facet_wrap( ~ FLOOR)
)







