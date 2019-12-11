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


# Import data -------------------------------------------------------------

wifi_training <- readRDS("./Module 3 - Task 3/New Data/wifi_training_pp.rds")
wifi_validation <- readRDS("./Module 3 - Task 3/New Data/wifi_validation_pp.rds")

wifi_training_nodup <- readRDS("./Module 3 - Task 3/New Data/wifi_training_pp_nodup.rds")

# Choose the data ---------------------------------------------------------

# PATH 1: Data used for training and testing
training_data <- wifi_training
testing_data <- wifi_validation

# PATH 2: Creating sub sets for training 75% and testing 25%
data_to_split <- wifi_training
set.seed(123)
indTrain <- createDataPartition(y=data_to_split$BUILDINGID, p=0.75, list=FALSE)
training_data <- data_to_split[indTrain,]
testing_data <- data_to_split[-indTrain,]

# PATH 3: PCA approach
pca_res_train$eig <- PCA(training_data[1:297],ncp = 130, graph = FALSE)
training_data <- add_column(as_tibble(pca_res_train$ind$contrib[,1:100]),
                              BUILDINGID = training_data$BUILDINGID,
                              FLOOR = training_data$FLOOR,
                              LONGITUDE = training_data$LONGITUDE,
                              LATITUDE = training_data$LATITUDE)

pca_res_test <- PCA(testing_data[1:297],ncp = 130, graph = FALSE)
testing_data <- add_column(as_tibble(pca_res_test$ind$contrib[,1:100]),
                            BUILDINGID = testing_data$BUILDINGID,
                            FLOOR = testing_data$FLOOR,
                            LONGITUDE = testing_data$LONGITUDE,
                            LATITUDE = testing_data$LATITUDE)

# Training models ---------------------------------------------------------

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

#KNN ------------------
set.seed(123)
KNNmod_building <- train(factor(BUILDINGID)~.,
                         data = add_column(training_data[1:(length(training_data)-9)],
                                           BUILDINGID = factor(training_data$BUILDINGID)),
                         method = "kknn",
                         trControl = fitControl, tuneLength = 1, preProc = c("center","scale"),
                         na.action = na.omit)

saveRDS(KNNmod_building,"./Module 3 - Task 3/New Data/Models/KNNmod_building_nodup.rds")

KNNmod_building <- readRDS("./Module 3 - Task 3/New Data/Models/KNNmod_building_nodup.rds")

#RF -------------------
set.seed(123)
RFmod_building <- train(factor(BUILDINGID)~.,
                         data = add_column(training_data[1:(length(training_data)-9)],
                                           BUILDINGID = factor(training_data$BUILDINGID)),
                         method = "rf",
                         trControl = fitControl, tuneLength = 1, preProc = c("center","scale"),
                         na.action = na.omit)

saveRDS(RFmod_building,"./Module 3 - Task 3/New Data/Models/RFmod_building_nodup.rds")

RFmod_building <- readRDS("./Module 3 - Task 3/New Data/Models/RFmod_building_nodup.rds")

# Testing the models ------------------------------------------------------

# Choose the model
test_model <- RFmod_building
# Apply the model
test_results <- predict(test_model, newdata = testing_data)
# Show confusion matrix
confusionMatrix(data = test_results, testing_data$BUILDINGID)

ggplotly(
  testing_data %>%
    add_column(BUILDINGID_P = test_results) %>% 
    filter(BUILDINGID != BUILDINGID_P) %>%
    ggplot() +
    geom_point(data = testing_data, aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID), alpha = 0.01) +
    geom_point(aes(x = LONGITUDE, y = LATITUDE), color = "darkred", alpha = 1)
)


testing_data %>%
  add_column(BUILDINGID_P = test_results) %>% 
  filter(BUILDINGID == 0 & BUILDINGID_P == 1) %>% 
  #slice(2L) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% view()


testing_data %>%
  add_column(BUILDINGID_P = test_results) %>% 
  filter(BUILDINGID == 0 & BUILDINGID_P == 1) %>% 
  #slice(2L) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% 
  colnames(.) %>% tibble(varr = .) %>% 
  left_join(wap_vs_b, by = c("varr" = "WAP")) %>%
  slice(11:n()) %>% 
  #group_by(Building) %>% summarize(n = n())
  pivot_wider(names_from = varr, values_from = Building) -> temp_waps_b





