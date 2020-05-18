# First Keras investigation with original data (without adding time lags)


# investigation with Keras

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
library(keras)
library(caret)

source(file = "replace_funf.R")


# 2. Control switches and additional parameters ------------------------------------------------------
generate_correlation_figures = 1


# 3. Load Data ------------------------------------------------------
main_data <- read.csv(file = "../data/QR_test_x_ins.csv")

# 4. Fix column names ------------------------------------------------------
colnames(main_data) <- c("time", "r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10")

# 5a. add some time associated features ------------------------------------------------------
main_data %>% 
    dplyr::mutate(time = as.Date(time)) %>%
    dplyr::mutate(time_numerical = as.numeric(time)) %>%
    dplyr::mutate(day_of_week = weekdays(time)) %>%
    dplyr::arrange(time_numerical) -> main_data


# 8. Divide the data as train and test ------------------------------------------------------
main_data %>%
    dplyr::arrange(time_numerical) %>%
    tail(300) -> test_data

dplyr::anti_join(main_data, test_data) -> train_data


# 9. Train data Keras ------------------------------------------------------

# just do for R02
train_data %>%
    dplyr::select(- time, - time_numerical, -day_of_week, -r02) %>%
    as.matrix(.) -> xtrain

train_data %>%
    dplyr::select(r02) %>%
    as.matrix(.) -> ytrain

test_data %>%
    dplyr::select(- time, - time_numerical, -day_of_week, -r02) %>%
    as.matrix(.) -> xtest

test_data %>%
    dplyr::select(r02) %>%
    as.matrix(.) -> ytest


in_dim = dim(xtrain)[2]
out_dim = dim(ytrain)[2] 

# Generate Keras Model
model = keras_model_sequential() %>%
    layer_dense(units = 8, activation="relu", input_shape=in_dim) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 2, activation = "relu") %>%
    layer_dense(units = out_dim, activation = "linear")

model %>% compile(
    loss = "mse",
    optimizer = "adam")

model %>% summary()



# Run the model
model %>% fit(xtrain, ytrain, epochs = 1000, verbose = 1)
scores = model %>% evaluate(xtrain, ytrain, verbose = 1)
print(scores)

# Make Predictions
ypred = model %>% predict(xtest)

# See the prediction results
data.frame(true_value = as.vector(ytest), pred = ypred) -> error_data
error_data %>%
    dplyr::mutate(err_sqr = (true_value - pred)^2) %>%
    .$err_sqr %>% mean(.) %>% sqrt(.) 