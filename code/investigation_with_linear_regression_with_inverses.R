# investigation with linear regression

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
require(lmtest)
source(file = "replace_funf.R")


# 2. Control switches and additional parameters ------------------------------------------------------
generate_correlation_figures = 1


# 3. Load Data ------------------------------------------------------
main_data <- read.csv(file = "../data/QR_test_x_ins.csv")

# 4. Fix column names ------------------------------------------------------
colnames(main_data) <- c("time", "r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10")

# 5. add some time associated features ------------------------------------------------------
main_data %>% 
    dplyr::mutate(time = as.Date(time)) %>%
    dplyr::mutate(time_numerical = as.numeric(time)) %>%
    dplyr::mutate(day_of_week = weekdays(time)) %>%
    dplyr::arrange(time_numerical) -> main_data


column_name_vector = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10")
data_inverse_function = function(df, column_name_)
{
    new_column_name = paste(column_name_, "i", sep = "")
    df %>%
        dplyr::mutate(new_column = 1/get(column_name_)) %>%
        dplyr::rename(!!quo_name(new_column_name) := new_column) -> df
    
    return(df)
}

main_data_i = main_data

for(counter01 in 1: length(column_name_vector))
{
    column_name = column_name_vector[counter01]
    main_data_i = data_inverse_function(df = main_data_i, column_name_ = column_name)
}

main_data_i <- do.call(data.frame, lapply(main_data_i, function(x) replace(x, is.infinite(x), NA)))

# 6. extend data, add time lag data features ------------------------------------------------------
column_name_vector = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10",
                       "r01i", "r02i", "r03i", "r04i", "r05i", "r06i", "r07i", "r08i", "r09i", "r10i")
main_data_i_ext <- main_data_i
max_delay_amount = 30

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
        main_data_i_ext = data_extend_function(df = main_data_i_ext, delay_amount_ = delay_amount, column_name_ = column_name)
    } 
}


# predict parameter without using itself directly
# elemination_vars = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10", "time" ,"time_numerical", "day_of_week") # predict today with only yesterday
elemination_vars = c("time" ,"time_numerical", "day_of_week") # predict today with today


linear_fit_ = function(df, y_value_, elemination_vars_)
{
    elemination_vars_cleaned = elemination_vars
    elemination_vars_cleaned <- setdiff(elemination_vars_cleaned, y_value_)
    
    df %>%
        dplyr::select_at(vars(-one_of(elemination_vars_cleaned))) %>%
        dplyr::rename(y_value = !!quo_name(y_value_)) -> selected_data_ext
    
    
    model <- lm(y_value ~. , data = selected_data_ext)
    lmtest::coeftest(model) -> df
    data.frame(df[,]) -> df
    
    return(df)
}

df_r01 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r01", 
                      elemination_vars_ = elemination_vars)

df_r02 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r02", 
                      elemination_vars_ = elemination_vars)

df_r03 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r03", 
                      elemination_vars_ = elemination_vars)

df_r04 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r04", 
                      elemination_vars_ = elemination_vars)

df_r05 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r05", 
                      elemination_vars_ = elemination_vars)

df_r06 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r06", 
                      elemination_vars_ = elemination_vars)

df_r07 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r07", 
                      elemination_vars_ = elemination_vars)

df_r08 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r08", 
                      elemination_vars_ = elemination_vars)

df_r09 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r09", 
                      elemination_vars_ = elemination_vars)

df_r10 <- linear_fit_(df = main_data_i_ext, 
                      y_value_ = "r10", 
                      elemination_vars_ = elemination_vars)














