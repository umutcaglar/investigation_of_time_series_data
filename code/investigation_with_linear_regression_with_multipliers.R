# investigation with linear regression

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
require(lmtest)
require(glmnet)
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


column_name_vector = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10")
data_inverse_function = function(df, column_name_)
{
    new_column_name = paste(column_name_, "i", sep = "")
    df %>%
        dplyr::mutate(new_column = 1/get(column_name_)) %>%
        dplyr::rename(!!quo_name(new_column_name) := new_column) -> df
    
    return(df)
}

# 5***. modify r10 with a known function and lets see if the algorithm works !!! NEED REMOVAL ------------------------------------------------------
#main_data %>%
#    dplyr::mutate(r10 = 0.4 * dplyr::lag(x = r07, n = 3, default = 0)+
#                      0.6 * dplyr::lag(x = r03, n = 4, default = 0) * dplyr::lag(x = r04, n = 4, default = 0) +
#                      0.06) -> main_data

# 5b. add inverse associated features ------------------------------------------------------
main_data_i = main_data

for(counter01 in 1: length(column_name_vector))
{
    column_name = column_name_vector[counter01]
    main_data_i = data_inverse_function(df = main_data_i, column_name_ = column_name)
}

main_data_i <- do.call(data.frame, lapply(main_data_i, function(x) replace(x, is.infinite(x), NA)))

# 5c. add multipliers associated features ------------------------------------------------------
main_data_m = main_data_i
column_name_vector = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10",
                       "r01i", "r02i", "r03i", "r04i", "r05i", "r06i", "r07i", "r08i", "r09i", "r10i")


data_multiply_function = function(df, column_name_1_, column_name_2_)
{
    new_column_name = paste(column_name_1_, column_name_2_,sep = "_")
    df %>% 
        dplyr::mutate(new_column = get(column_name_1_) * get(column_name_2_)) -> df
    
    if(sd(df$new_column, na.rm = T) < 10^-10){df %>% dplyr::select(-new_column) -> df}else{
        df%>%
            dplyr::rename(!!quo_name(new_column_name) := new_column) -> df
    }
    
    return(df)
}

for(counter01 in 1: length(column_name_vector))
{
    for(counter02 in counter01: length(column_name_vector))
    {
        column_name_1 = column_name_vector[counter01]
        column_name_2 = column_name_vector[counter02]
        main_data_m = data_multiply_function(df = main_data_m, column_name_1_ = column_name_1, column_name_2_ = column_name_2)
    }
}


# 6. extend data, add time lag data features ------------------------------------------------------
#column_name_vector = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10",
#                       "r01i", "r02i", "r03i", "r04i", "r05i", "r06i", "r07i", "r08i", "r09i", "r10i")
column_name_vector = setdiff(colnames(main_data_m), c("day_of_week", "time_numerical", "time"))

main_data_m_ext <- main_data_m
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
        main_data_m_ext = data_extend_function(df = main_data_m_ext, delay_amount_ = delay_amount, column_name_ = column_name)
    } 
}

# 7. remove messing up initial lines because of time lag ------------------------------------------------------
main_data_m_ext %>%
    tail(x = ., -1 * max_delay_amount) -> main_data_m_ext # remove NA rows

# predict r01 without using itself directly
# elemination_vars = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10", "time" ,"time_numerical", "day_of_week") # predict today with only yesterday
elemination_vars = c("time" ,"time_numerical", "day_of_week") # predict today with today


linear_fit_ = function(df, y_value_, elemination_vars_)
{
    elemination_vars_cleaned = elemination_vars
    setdiff(colnames(df), grep(pattern = "lag", x = colnames(df), value = T)) -> not_lagged_features
    grep(pattern = y_value_, x = not_lagged_features, value = T) -> not_lagged_features_includes_y
    elemination_vars_cleaned = c(elemination_vars, not_lagged_features_includes_y)
    elemination_vars_cleaned <- setdiff(elemination_vars_cleaned, y_value_)
    
    df %>%
        #dplyr::select_at(vars(-one_of(elemination_vars_cleaned))) %>%
        dplyr::rename(y_value = !!quo_name(y_value_)) -> selected_data_ext
    
    
    model <- lm(y_value ~. , data = selected_data_ext)
    data.frame(coeff = model$coefficients) -> temp_df
    temp_df %>% 
        dplyr::mutate(feature_names = rownames(temp_df)) -> temp_df
    
    
    lmtest::coeftest(model) -> dfi
    data.frame(dfi[,]) -> df
    
    df %>% 
        dplyr::mutate(feature_names = rownames(dfi)) %>%
        dplyr::arrange(Pr...t..) %>%
        dplyr::mutate(Pr_adj = p.adjust(Pr...t.., method = "fdr")) -> df
    
    left_join(df, temp_df) -> df
    
    return(df)
}

df_r01 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r01", 
                      elemination_vars_ = elemination_vars)

df_r02 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r02", 
                      elemination_vars_ = elemination_vars)

df_r03 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r03", 
                      elemination_vars_ = elemination_vars)

df_r04 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r04", 
                      elemination_vars_ = elemination_vars)

df_r05 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r05", 
                      elemination_vars_ = elemination_vars)

df_r06 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r06", 
                      elemination_vars_ = elemination_vars)

df_r07 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r07", 
                      elemination_vars_ = elemination_vars)

df_r08 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r08", 
                      elemination_vars_ = elemination_vars)

df_r09 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r09", 
                      elemination_vars_ = elemination_vars)

df_r10 <- linear_fit_(df = main_data_m_ext, 
                      y_value_ = "r10", 
                      elemination_vars_ = elemination_vars)

df_r09 %>%
    dplyr::filter(abs(coeff)>0.05) %>%
    dplyr::filter(Pr...t.. < 0.05)-> df_r09_


linear_fit_wo_lag_ = function(df, y_value_, elemination_vars_)
{
    elemination_vars_cleaned = elemination_vars
    setdiff(colnames(df), grep(pattern = "lag", x = colnames(df), value = T)) -> not_lagged_features
    grep(pattern = y_value_, x = not_lagged_features, value = T) -> not_lagged_features_includes_y
    elemination_vars_cleaned = c(elemination_vars, not_lagged_features_includes_y)
    elemination_vars_cleaned <- setdiff(elemination_vars_cleaned, y_value_)
    
    df %>%
        dplyr::select_at(vars(-one_of(elemination_vars_cleaned))) %>%
        dplyr::rename(y_value = !!quo_name(y_value_)) -> selected_data_ext
    
    setdiff(colnames(selected_data_ext), grep(pattern = "lag", x = colnames(selected_data_ext), value = T)) -> not_lagged_features
    selected_data_ext %>%
        dplyr::select_at(vars(not_lagged_features)) -> selected_data_ext
    
    grep(pattern = "(r06|r09|y_value)", x = colnames(selected_data_ext), value = T) -> r06_09_subset
    
    selected_data_ext %>%
        dplyr::select_at(vars(r06_09_subset)) -> selected_data_ext
    
    model <- lm(y_value ~. , data = selected_data_ext)
    data.frame(coeff = model$coefficients) -> temp_df
    temp_df %>% 
        dplyr::mutate(feature_names = rownames(temp_df)) -> temp_df
    
    
    lmtest::coeftest(model) -> dfi
    data.frame(dfi[,]) -> df
    
    df %>% 
        dplyr::mutate(feature_names = rownames(dfi)) %>%
        dplyr::arrange(Pr...t..) -> df
    
    left_join(df, temp_df) -> df
    
    return(df)
}




df_r10_wo_lag <- linear_fit_wo_lag_(df = main_data_m_ext, 
                                    y_value_ = "r10", 
                                    elemination_vars_ = elemination_vars)








