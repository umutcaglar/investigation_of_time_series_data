#using xgboost

# Run XGBoost inside caret wrapper using 5-fold CV
library(readr)
library(caret)
library(dplyr)
library(xgboost)

# Read train and test data
power_plant = as.data.frame(read_excel("Folds5x2_pp.xlsx"))

# Log transform loss and convert character to factor variables
train$logloss <- log(train$loss + 1)
train[, 2:117] <- lapply(train[, 2:117], as.factor)

trainX <- data.matrix(select(train, -loss, -logloss, -id))

ctrl <- trainControl(method = "cv", 
                     number = 5,                        
                     allowParallel = TRUE)

set.seed(56)
xgb_model <- train(x = trainX, y = train$logloss,
                   method = "xgbTree",
                   nrounds = 50,
                   max_depth = 6,
                   subsample=0.85,
                   colsample_bytree=0.7,
                   eta = 0.1,
                   trControl = ctrl,
                   verbose=TRUE)