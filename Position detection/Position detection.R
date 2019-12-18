# ###########################################################################-
# GOAL: Wifi Locationing
# DESCRIPTION: Position detection
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

# Selecting BUILDING and FLOOR --------------------------------------------

BUILDINGG <- 0
FLOORR <- 1

wifi_training_nodup %>% 
  filter(BUILDINGID == BUILDINGG) %>% 
  mutate(FLOOR = factor(FLOOR, levels = c(0,1,2,3))) -> wifi_training_per_bf

wifi_training_per_bf[1:(length(wifi_training_per_bf)-9)] %>%
  select(as.numeric(which(apply(wifi_training_per_bf[1:(length(wifi_training_per_bf)-9)],
                                2, var) != 0))) %>% 
  cbind(wifi_training_per_bf[(length(wifi_training_per_bf)-8):length(wifi_training_per_bf)]) %>%
  as_tibble() -> wifi_training_per_bf

wifi_validation %>%
  filter(BUILDINGID == BUILDINGG) %>%
  mutate(FLOOR = factor(FLOOR, levels =levels(wifi_training_per_bf$FLOOR))) %>% 
  select(colnames(wifi_training_per_bf)) -> wifi_validation_per_bf


# First analysis KNN using building ---------------------------------------

# PATH 1: Data used for training and testing
training_data <- wifi_training_per_bf
testing_data <- wifi_validation_per_bf
# PATH 2: Creating sub sets for training 75% and testing 25%
data_to_split <- wifi_training_per_bf
set.seed(123)
indTrain <- createDataPartition(y=data_to_split$FLOOR, p=0.75, list=FALSE)
training_data <- data_to_split[indTrain,]
testing_data <- data_to_split[-indTrain,]


# Scaling -----------------------------------------------------------------

# Training scaling
training_data %>%
  mutate(ID = seq.int(n())) %>%
  select(-LONGITUDE,-LATITUDE,-FLOOR,-BUILDINGID,-SPACEID,-RELATIVEPOSITION,-USERID,-PHONEID,-TIMESTAMP) %>%
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  mutate(ID = paste0("ID_",ID)) %>% 
  pivot_wider(names_from = ID, values_from = value) -> training_data_temp

training_data_temp[2:length(training_data_temp)] <- 
  sapply(training_data_temp[2:length(training_data_temp)],
         function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100))

training_data_temp %>% 
  pivot_longer(cols = starts_with("ID"), names_to = "ID") %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% 
  select(-ID) -> training_data_temp

training_data_temp %>% 
  cbind(training_data[(length(training_data)-8):length(training_data)]) -> training_data

# Validation scaling
testing_data %>%
  mutate(ID = seq.int(n())) %>%
  select(-LONGITUDE,-LATITUDE,-FLOOR,-BUILDINGID,-SPACEID,-RELATIVEPOSITION,-USERID,-PHONEID,-TIMESTAMP) %>%
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  mutate(ID = paste0("ID_",ID)) %>% 
  pivot_wider(names_from = ID, values_from = value) -> testing_data_scaled

testing_data_scaled[2:length(testing_data_scaled)] <- 
  sapply(testing_data_scaled[2:length(testing_data_scaled)], 
         function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100))

testing_data_scaled %>% 
  pivot_longer(cols = starts_with("ID"), names_to = "ID") %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% 
  select(-ID) -> testing_data_scaled

testing_data_scaled %>% 
  cbind(testing_data[(length(testing_data)-8):length(testing_data)]) -> testing_data

# Training models ---------------------------------------------------------

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

#LONGITUDE ------------------
set.seed(123)
mod_longitude <- train(LONGITUDE~.,
                         data = add_column(training_data[1:(length(training_data)-9)],
                                           LONGITUDE = training_data$LONGITUDE,
                                           FLOOR = as.numeric(training_data$FLOOR)),
                         method = "knn",
                         trControl = fitControl, tuneLength = 1, preProc = c("center","scale"),
                         na.action = na.omit)


#LATITUDE ------------------
set.seed(123)
mod_latitude <- train(LATITUDE~.,
                       data = add_column(training_data[1:(length(training_data)-9)],
                                         LATITUDE = training_data$LATITUDE,
                                         FLOOR = as.numeric(training_data$FLOOR)),
                       method = "knn",
                       trControl = fitControl, tuneLength = 1, preProc = c("center","scale"),
                       na.action = na.omit)

# Testing the models ------------------------------------------------------

testing_data$FLOOR <- as.numeric(testing_data$FLOOR)

test_longitude <- predict(mod_longitude, newdata = testing_data)
test_latitude <- predict(mod_latitude, newdata = testing_data)

wifi_mod_tst_results <- testing_data %>% 
  add_column(LONGITUDE_P = test_longitude, LATITUDE_P = test_latitude) %>% 
  mutate(Error_d = sqrt((LONGITUDE_P-LONGITUDE)^2 + (LATITUDE_P-LATITUDE)^2))
  #%>% view()


# Checking errors ---------------------------------------------------------

wifi_mod_tst_results %>%
  filter(Error_d > 15) %>% view()

ggplotly(
  wifi_mod_tst_results %>%
    filter(abs(LONGITUDE - LONGITUDE_P) > 20 | abs(LATITUDE - LATITUDE_P) > 20) %>% 
    filter(row_number() == 5L) %>% 
    ggplot() +
    geom_point(data = wifi_mod_tst_results, aes(x = LONGITUDE, y = LATITUDE), alpha = 0.02) +
    geom_point(aes(x = LONGITUDE, y = LATITUDE), color = "gold3", alpha = 1) +
    geom_point(aes(x = LONGITUDE_P, y = LATITUDE_P), color = "darkred", alpha = 1)
)








