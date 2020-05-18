# investigation with visuals

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
require(lmtest)
require(mgcv)
require(xgboost)
require(caret)

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

# 8. Divide the data as train and test ------------------------------------------------------
main_data_ext %>%
    dplyr::arrange(time_numerical) %>%
    tail(300) -> test_data_ext

dplyr::anti_join(main_data_ext, test_data_ext) -> train_data_ext


# 9. Train data figure signals ------------------------------------------------------

train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r01, names_to = "x_axis", values_to = "x_values") -> r01_fig_data

ggplot(r01_fig_data, aes(x = x_values, y = r01)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r01")-> fig_r01


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r02, names_to = "x_axis", values_to = "x_values") -> r02_fig_data

ggplot(r02_fig_data, aes(x = x_values, y = r02)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r02") -> fig_r02


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r03, names_to = "x_axis", values_to = "x_values") -> r03_fig_data

ggplot(r03_fig_data, aes(x = x_values, y = r03)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r03") -> fig_r03


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r04, names_to = "x_axis", values_to = "x_values") -> r04_fig_data

ggplot(r04_fig_data, aes(x = x_values, y = r04)) +
    facet_wrap(vars(x_axis), ncol = 10) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r04") -> fig_r04


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r05, names_to = "x_axis", values_to = "x_values") -> r05_fig_data

ggplot(r05_fig_data, aes(x = x_values, y = r05)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r05") -> fig_r05


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r06, names_to = "x_axis", values_to = "x_values") -> r06_fig_data

ggplot(r06_fig_data, aes(x = x_values, y = r06)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r06") -> fig_r06


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r07, names_to = "x_axis", values_to = "x_values") -> r07_fig_data

ggplot(r07_fig_data, aes(x = x_values, y = r07)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r07") -> fig_r07


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r08, names_to = "x_axis", values_to = "x_values") -> r08_fig_data

ggplot(r08_fig_data, aes(x = x_values, y = r08)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r08") -> fig_r08


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r09, names_to = "x_axis", values_to = "x_values") -> r09_fig_data

ggplot(r09_fig_data, aes(x = x_values, y = r09)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() +
    ggtitle("r09") -> fig_r09


train_data_ext %>%
    dplyr::select(-time, -time_numerical, -day_of_week) %>%
    tidyr::pivot_longer(-r10, names_to = "x_axis", values_to = "x_values") -> r10_fig_data

ggplot(r10_fig_data, aes(x = x_values, y = r10)) +
    facet_wrap(vars(x_axis)) +
    geom_density_2d() +
    theme_bw() + 
    ggtitle("r10")-> fig_r10


 cowplot::save_plot(filename = "../figures/r01_relations.pdf", plot = fig_r01, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r02_relations.pdf", plot = fig_r02, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r03_relations.pdf", plot = fig_r03, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r04_relations.pdf", plot = fig_r04, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r05_relations.pdf", plot = fig_r05, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r06_relations.pdf", plot = fig_r06, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r07_relations.pdf", plot = fig_r07, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r08_relations.pdf", plot = fig_r08, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r09_relations.pdf", plot = fig_r09, ncol = 5, nrow = 5)
 cowplot::save_plot(filename = "../figures/r10_relations.pdf", plot = fig_r10, ncol = 5, nrow = 5)


main_data %>%
    dplyr::select(-time, -day_of_week) %>%
    tidyr::pivot_longer(-time_numerical, names_to = "y_axis", values_to = "y_values") -> time_vs_features

ggplot(time_vs_features, aes(x = time_numerical, y = y_values)) +
    facet_wrap(vars(y_axis), ncol = 1) +
    geom_point() +
    theme_bw() + 
    ggtitle("time_series")-> time_series_fig

time_series_fig

cowplot::save_plot(filename = "../figures/time_series.pdf", plot = time_series_fig, ncol = 10, nrow = 10, limitsize = FALSE)


main_data[1:500,] %>%
    dplyr::select(-time, -day_of_week) %>%
    tidyr::pivot_longer(-time_numerical, names_to = "y_axis", values_to = "y_values") -> time_vs_features_s

ggplot(time_vs_features_s, aes(x = time_numerical, y = y_values)) +
    facet_wrap(vars(y_axis), ncol = 1) +
    geom_point(size = 0.1) +
    theme_bw() + 
    ggtitle("time_series")-> time_series_fig_s

time_series_fig_s

cowplot::save_plot(filename = "../figures/time_series_fig_short.pdf", plot = time_series_fig_s, ncol = 4, nrow = 4, limitsize = FALSE)