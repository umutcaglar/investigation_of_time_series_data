# investigation with XGBoost

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
require(lmtest)
require(mgcv)
require(xgboost)
require(caret)

library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

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
column_name_vector = setdiff(colnames(main_data), c("day_of_week", "time_numerical", "time"))

# define the structure for lags
main_data_ext <- main_data
max_delay_amount = 10
delay_step = 1
delay_vector = seq(delay_step, max_delay_amount, delay_step) # this vector gives the structute for time delays

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
    for(counter02 in 1: length(delay_vector))
    {
        delay_amount = delay_vector[counter02]
        main_data_ext = data_extend_function(df = main_data_ext, delay_amount_ = delay_amount, column_name_ = column_name)
    } 
}


# 7. remove messing up initial lines because of time lag ------------------------------------------------------
main_data_ext %>%
    tail(x = ., -1 * max_delay_amount) -> main_data_ext # remove NA rows

# 8. Divide the data as train and test ------------------------------------------------------
main_data_ext %>%
    dplyr::arrange(time_numerical) %>%
    tail(300) -> test_data_ext

dplyr::anti_join(main_data_ext, test_data_ext) -> train_data_ext


# 9. Train data XGB ------------------------------------------------------
xgb_trcontrol = trainControl(
    method = "cv",
    number = 5,  
    allowParallel = TRUE,
    verboseIter = FALSE,
    returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100),  # this is n_estimators in the python code above
                       max_depth = c(10, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = c(1),
                       gamma=c(0),
                       min_child_weight = 1,
                       subsample = 1
)


#************************************************
xgboost_function <- function(y_value_ = "r01", df_tr = train_data_ext, df_te = test_data_ext)
{
    set.seed(0)
    df_tr %>%
        dplyr::rename(y_value = !!quo_name(y_value_)) -> df_train
    
    #df_train %>%
    #    select(setdiff(colnames(df_train), sprintf("r%02d",1:10))) -> df_train

    xgb_model <- caret::train(y_value~.,
                              data = df_train %>% 
                                  select(-time, -time_numerical, -day_of_week), 
                              method = "xgbTree", 
                              trControl = xgb_trcontrol,
                              tuneGrid = xgbGrid,
                              importance = "impurity")
    
    
    df_te %>%
        dplyr::rename(y_value = !!quo_name(y_value_)) -> df_test
    
    #df_test %>%
    #    select(setdiff(colnames(df_train), sprintf("r%02d",1:10))) -> df_test
    
    pred <- predict(xgb_model, df_test %>% 
                        select(-time, -time_numerical, -day_of_week, -y_value))
    
    results <- data.frame(true_values = df_test[["y_value"]], predicted_values = pred)
    ggplot(results, aes(x= true_values, y = predicted_values)) + geom_point() -> fig
    
    varImp(xgb_model, scale = TRUE)$importance %>% 
        rownames_to_column() %>% 
        arrange(-Overall) -> var_importance
    
    return(list(results = results, var_importance = var_importance)) #, fig = fig, model = xgb_model))
}


result01 <- xgboost_function(y_value_ = "r01")
result02 <- xgboost_function(y_value_ = "r02")
result03 <- xgboost_function(y_value_ = "r03")
result04 <- xgboost_function(y_value_ = "r04")
result05 <- xgboost_function(y_value_ = "r05")
result06 <- xgboost_function(y_value_ = "r06")
result07 <- xgboost_function(y_value_ = "r07")
result08 <- xgboost_function(y_value_ = "r08")
result09 <- xgboost_function(y_value_ = "r09")
result10 <- xgboost_function(y_value_ = "r10")

result01$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result02$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result03$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result04$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result05$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result06$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result07$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result08$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result09$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))

result10$results %>%
    dplyr::mutate(sqre_diff = (true_values - predicted_values)^2) %>%
    .$sqre_diff %>% mean(.) %>% `^`(.,(1/2))


save(result01,
     result02,
     result03,
     result04,
     result05,
     result06,
     result07,
     result08,
     result09,
     result10,
     file = "../results/xgb_results.RData")
