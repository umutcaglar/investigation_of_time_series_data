# Investigation of data

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
require(corrplot)
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

colnames(main_data_ext) %>% grep(pattern = "(r03|r04|time|day)", x = ., value = T)-> columns_r03_04
main_data_ext %>%
    dplyr::select_at(vars(columns_r03_04)) -> main_data_ext_r03_04

colnames(main_data_ext) %>% grep(pattern = "(r05|r07|time|day)", x = ., value = T)-> columns_r05_07
main_data_ext %>%
    dplyr::select_at(vars(columns_r05_07)) -> main_data_ext_r05_07


colnames(main_data_ext) %>% grep(pattern = "(r09|r10|time|day)", x = ., value = T)-> columns_r09_10
main_data_ext %>%
    dplyr::select_at(vars(columns_r09_10)) -> main_data_ext_r09_10


# 7. generate correlation figures ------------------------------------------------------
if(generate_correlation_figures == 1)
{
    # fig01: direct column comparison
    

    main_data %>%
        dplyr::select(-time, -time_numerical, -day_of_week) %>%
        cor(., use = "pairwise.complete.obs") -> cor_table
    
    corrplot::corrplot.mixed(corr = cor_table, lower = "number", lower.col = "black", upper = "circle", order = "hclust") -> fig01
    fig01
    

    # fig02: column comparison with 10 day delay
    main_data_ext %>%
        dplyr::select(-time, -time_numerical, -day_of_week) %>%
        cor(., use = "pairwise.complete.obs") -> cor_table
    
    scale_factor = 7
    pdf(file = "../figures/large_correlation.pdf", 
        width = 14 * scale_factor, height = 14 * scale_factor)
    corrplot::corrplot.mixed(corr = cor_table, lower = "number", lower.col = "black", upper = "circle", order = "hclust") -> fig02
    dev.off()
    fig02
    
    # fig02b: column comparison with 10 day delay (alphabetical)
    main_data_ext %>%
        dplyr::select(-time, -time_numerical, -day_of_week) %>%
        cor(., use = "pairwise.complete.obs") -> cor_table
    
    scale_factor = 7
    pdf(file = "../figures/large_correlation_alphabetical.pdf", 
        width = 14 * scale_factor, height = 14 * scale_factor)
    corrplot::corrplot.mixed(corr = cor_table, lower = "number", lower.col = "black", upper = "circle", order = "alphabet") -> fig02b
    dev.off()
    fig02b
    
    
    # fig03: column comparison with 10 day delay for r03 & r04
    main_data_ext_r03_04 %>%
        dplyr::select(-time, -time_numerical, -day_of_week) %>%
        cor(., use = "pairwise.complete.obs") -> cor_table
    
    scale_factor = 7/5
    pdf(file = "../figures/large_correlation_r03_04.pdf", 
        width = 14 * scale_factor, height = 14 * scale_factor)
    corrplot::corrplot.mixed(corr = cor_table, lower = "number", lower.col = "black", upper = "circle", order = "alphabet") -> fig03
    dev.off()
    fig03
    
    # fig04: column comparison with 10 day delay for r05 & r07
    main_data_ext_r05_07 %>%
        dplyr::select(-time, -time_numerical, -day_of_week) %>%
        cor(., use = "pairwise.complete.obs") -> cor_table
    
    scale_factor = 7/5
    pdf(file = "../figures/large_correlation_r05_07.pdf", 
        width = 14 * scale_factor, height = 14 * scale_factor)
    corrplot::corrplot.mixed(corr = cor_table, lower = "number", lower.col = "black", upper = "circle", order = "alphabet") -> fig04
    dev.off()
    fig04
    
    # fig05: column comparison with 10 day delay for r09 & r10
    main_data_ext_r09_10 %>%
        dplyr::select(-time, -time_numerical, -day_of_week) %>%
        cor(., use = "pairwise.complete.obs") -> cor_table
    
    scale_factor = 7/5
    pdf(file = "../figures/large_correlation_r09_10.pdf", 
        width = 14 * scale_factor, height = 14 * scale_factor)
    corrplot::corrplot.mixed(corr = cor_table, lower = "number", lower.col = "black", upper = "circle", order = "alphabet") -> fig05
    dev.off()
    fig05
    
}