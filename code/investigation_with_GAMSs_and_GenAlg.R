# investigation with GAMs

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
require(lmtest)
require(mgcv)

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



# 5***. syntetically change r10 !!! NEED REMOVAL ------------------------------------------------------
# main_data %>%
#     dplyr::mutate(r10 = 0.4 * dplyr::lag(x = r07, n = 3, default = 0)+
#                       0.6 * dplyr::lag(x = r03, n = 4, default = 0) * dplyr::lag(x = r04, n = 5, default = 0) +
#                       0.06) -> main_data


# 6. extend data, add time lag data features ------------------------------------------------------
#column_name_vector = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10",
#                       "r01i", "r02i", "r03i", "r04i", "r05i", "r06i", "r07i", "r08i", "r09i", "r10i")
column_name_vector = setdiff(colnames(main_data), c("day_of_week", "time_numerical", "time"))

main_data_ext <- main_data
max_delay_amount = 5

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

# 8. Divide the data as train and test ------------------------------------------------------
main_data_ext %>%
    dplyr::arrange(time_numerical) %>%
    tail(300) -> test_data_ext

dplyr::anti_join(main_data_ext, test_data_ext) %>%
    dplyr::sample_frac(0.3) -> train_data_ext


# 9. predict parameter without using itself directly ------------------------------------------------------
# elemination_vars = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10", "time" ,"time_numerical", "day_of_week") # predict today with only yesterday
elemination_vars = c("time" ,"time_numerical", "day_of_week") # predict today with today

GAM_fit_ = function(df, y_value_, elemination_vars_)
{
    elemination_vars_cleaned = elemination_vars
    elemination_vars_cleaned <- setdiff(elemination_vars_cleaned, y_value_)
    
    df %>%
        dplyr::select_at(vars(-one_of(elemination_vars_cleaned))) %>%
        dplyr::rename(y_value = !!quo_name(y_value_)) -> selected_data_ext
    
    setdiff(colnames(selected_data_ext), "y_value") %>% 
        paste0("s(",.,")") %>% paste(., collapse = " + ") %>% 
        paste0("y_value ~ ", .) %>% as.formula(.) -> formula_
    
    model_ <- mgcv::gam(formula = formula_,
                        data = selected_data_ext)
    
    return(model_)
}


browser()
model_r01 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r01", 
                      elemination_vars_ = elemination_vars)

model_r02 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r02", 
                      elemination_vars_ = elemination_vars)

model_r03 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r03", 
                      elemination_vars_ = elemination_vars)

model_r04 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r04", 
                      elemination_vars_ = elemination_vars)

model_r05 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r05", 
                      elemination_vars_ = elemination_vars)

model_r06 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r06", 
                      elemination_vars_ = elemination_vars)

model_r07 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r07", 
                      elemination_vars_ = elemination_vars)

model_r08 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r08", 
                      elemination_vars_ = elemination_vars)

model_r09 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r09", 
                      elemination_vars_ = elemination_vars)

model_r10 <- GAM_fit_(df = train_data_ext, 
                      y_value_ = "r10", 
                      elemination_vars_ = elemination_vars)


summary(model_r01)
mgcv::gam.check(model_r01)

summary(model_r02)
mgcv::gam.check(model_r02)

summary(model_r03)
mgcv::gam.check(model_r03)

summary(model_r08)
mgcv::gam.check(model_r08)

ggplot(main_data_ext, aes(x = r01_lag05, y = r01)) + geom_point()
