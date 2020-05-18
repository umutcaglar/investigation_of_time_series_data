# investigation with neural networks

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
library(neuralnet)
library(GGally)
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

# 5***. modify r10 with a known function and lets see if the algorithm works !!! NEED REMOVAL ------------------------------------------------------
main_data %>%
   dplyr::mutate(r10 = 0.4 * dplyr::lag(x = r07, n = 3, default = 0)+
                     0.6 * dplyr::lag(x = r03, n = 4, default = 0) * dplyr::lag(x = r04, n = 4, default = 0) +
                     0.06) -> main_data


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

# predict r01 without using itself directly
# elemination_vars = c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10", "time" ,"time_numerical", "day_of_week") # predict today with only yesterday
elemination_vars = c("time" ,"time_numerical", "day_of_week") # predict today with today

df = main_data_ext
elemination_vars_ = elemination_vars
y_value_ = "r10"

elemination_vars_cleaned = elemination_vars_
elemination_vars_cleaned <- setdiff(elemination_vars_cleaned, y_value_)

df %>%
    dplyr::select_at(vars(-one_of(elemination_vars_cleaned))) %>%
    dplyr::rename(y_value = !!quo_name(y_value_)) -> selected_data_ext

scale01 <- function(x){(x - min(x)) / (max(x) - min(x))} # min 0 max 1 scaling

selected_data_ext %>%
    tail(x = ., -1 * max_delay_amount) %>% # select NA rows
    mutate_all(scale01) -> selected_data_ext_scaled   # rescale the data

selected_data_ext_scaled %>% tail(300) -> test_data

set.seed(1415)
anti_join(selected_data_ext_scaled, test_data) %>%
    dplyr::sample_frac(0.3) -> train_data


set.seed(1415)
R01_NN1 <- neuralnet::neuralnet(formula = y_value ~ ., 
                                data = train_data,
                                hidden = c(2, 2, 1),
                                stepmax = 1e7,
                                rep = 3)

# 6. Visualize the NN ------------------------------------------------------
#plot(R01_NN1, rep = "best")

# 7. Calculation of error ------------------------------------------------------
data.frame(a = as.vector(R01_NN1$net.result[[1]]), b = train_data$y_value)
NN1_Train_SSE <- sum((as.vector(R01_NN1$net.result[[1]]) - train_data$y_value)^2)/2
paste("SSE: ", round(NN1_Train_SSE, 4))

temp <- test_data
temp[["y_value"]] <- NULL
Test_NN2_Output <- neuralnet::compute(x = R01_NN1, covariate = temp)$net.result
NN2_Test_SSE <- sum((Test_NN2_Output - test_data$y_value)^2)/2
paste("SSE: ", round(NN2_Test_SSE, 4))

