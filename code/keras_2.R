# First Keras investigation with original data (with adding time lags)


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


# 6. extend data, add time lag data features ------------------------------------------------------
column_name_vector = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10")
main_data_ext <- main_data
max_delay_amount = 10

data_extend_function = function(df, delay_amount_, column_name_)
{
    new_column_name = paste(column_name_, sprintf("lag%02d", delay_amount_),sep = "_")
    df %>% 
        dplyr::mutate(new_column = dplyr::lag(x = get(column_name_), order_by = time_numerical, n = delay_amount_)) %>%
        dplyr::rename(!!quo_name(new_column_name) := new_column) -> df
    
    return(df)
}

for(counter01 in 1: length(column_name_vector))
{
    column_name = column_name_vector[counter01]
    for(counter02 in 1: max_delay_amount)
    {
        delay_amount = counter02
        main_data_ext = data_extend_function(df = main_data_ext, delay_amount_ = delay_amount, column_name_ = column_name)
    } 
}

# 7. remove messing up initial lines because of time lag ------------------------------------------------------
main_data_ext %>%
    tail(x = ., -1 * max_delay_amount) -> main_data_ext # remove NA rows
main_data_ext -> main_data

# 8. Divide the data as train and test ------------------------------------------------------
main_data %>%
    dplyr::arrange(time_numerical) %>%
    tail(300) -> test_data

dplyr::anti_join(main_data, test_data) -> train_data


# 9. Train data Keras ------------------------------------------------------

# just do for R02
train_data %>%
    dplyr::select(- time, - time_numerical, -day_of_week, -r10) %>%
    as.matrix(.) -> xtrain

train_data %>%
    dplyr::select(r10) %>%
    as.matrix(.) -> ytrain

test_data %>%
    dplyr::select(- time, - time_numerical, -day_of_week, -r10) %>%
    as.matrix(.) -> xtest

test_data %>%
    dplyr::select(r10) %>%
    as.matrix(.) -> ytest


in_dim = dim(xtrain)[2]
out_dim = dim(ytrain)[2] 

# Generate Keras Model
model = keras_model_sequential() %>%
    layer_dense(units = 20, activation = "softmax", input_shape=in_dim, activity_regularizer = regularizer_l1(0.01)) %>%
    layer_dense(units = 8, activation = "softmax", activity_regularizer = regularizer_l1(0.01)) %>%
    layer_dense(units = 6, activation = "softmax", activity_regularizer = regularizer_l1(0.01)) %>%
    layer_dense(units = 2, activation = "softmax", activity_regularizer = regularizer_l1(0.01)) %>%
    layer_dense(units = out_dim, activation = "linear")

model %>% compile(
    loss = "mse",
    optimizer = "adam")

history <- model %>% fit(
    xtrain, ytrain, 
    epochs = 50, batch_size = 128, 
    validation_split = 0.2
)

model %>% summary()



# Run the model
#model %>% fit(xtrain, ytrain, epochs = 15, verbose = 1)
scores = model %>% evaluate(xtrain, ytrain, verbose = 1)
print(scores)

# Make Predictions
ypred = model %>% predict(xtest)

# See the prediction results
data.frame(true_value = as.vector(ytest), pred = ypred) -> error_data
error_data %>%
    dplyr::mutate(err_sqr = (true_value - pred)^2) %>%
    .$err_sqr %>% mean(.) %>% sqrt(.) 



# history <- model %>% fit(
#     xtrain, ytrain, 
#     epochs = 10, batch_size = 128, 
#     validation_split = 0.2
# )

