# investigation with neural networks
# http://uc-r.github.io/ann_regression

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
library(neuralnet)
library(GGally)
source(file = "replace_funf.R")

# 2. Load_data ------------------------------------------------------
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data'

Yacht_Data <- read_table(file = url,
                         col_names = c('LongPos_COB', 'Prismatic_Coeff',
                                       'Len_Disp_Ratio', 'Beam_Draut_Ratio', 
                                       'Length_Beam_Ratio','Froude_Num', 
                                       'Residuary_Resist')) %>%
    na.omit()

# 3. Investigate data ------------------------------------------------------
ggpairs(Yacht_Data, title = "Scatterplot Matrix of the Features of the Yacht Data Set")

# 4. Scale the data ------------------------------------------------------
scale01 <- function(x){
    (x - min(x)) / (max(x) - min(x))
}

Yacht_Data <- Yacht_Data %>%
    mutate_all(scale01)

# 5. Split into train & test ------------------------------------------------------
set.seed(12345)
Yacht_Data_Train <- dplyr::sample_frac(tbl = Yacht_Data, replace = FALSE, size = 0.80)
Yacht_Data_Test <- dplyr::anti_join(Yacht_Data, Yacht_Data_Train)

# 6. Train single layer NN ------------------------------------------------------
set.seed(12321)
Yacht_NN1 <- neuralnet::neuralnet(formula = Residuary_Resist ~ LongPos_COB + Prismatic_Coeff + 
                                      Len_Disp_Ratio + Beam_Draut_Ratio + Length_Beam_Ratio +
                                      Froude_Num, 
                                  data = Yacht_Data_Train)

set.seed(12321)
Yacht_NN1b <- neuralnet::neuralnet(formula = Residuary_Resist ~ ., 
                                  data = Yacht_Data_Train)

# 6. Visualize the NN ------------------------------------------------------
plot(Yacht_NN1, rep = "best")
plot(Yacht_NN1b, rep = "best")

# 7. Calculation of error ------------------------------------------------------
NN1_Train_SSE <- sum((Yacht_NN1$net.result - Yacht_Data_Train[, 7])^2)/2
paste("SSE: ", round(NN1_Train_SSE, 4))

# 8. Predict ------------------------------------------------------
Test_NN1_Output <- neuralnet::compute(Yacht_NN1, Yacht_Data_Test[, 1:6])$net.result
NN1_Test_SSE <- sum((Test_NN1_Output - Yacht_Data_Test[, 7])^2)/2
NN1_Test_SSE


# Add more layers ------------------------------------------------------
set.seed(12321)
Yacht_NN2 <- neuralnet::neuralnet(formula = Residuary_Resist ~ LongPos_COB + Prismatic_Coeff + Len_Disp_Ratio + Beam_Draut_Ratio + Length_Beam_Ratio + Froude_Num, 
                                  data = Yacht_Data_Train, 
                                  hidden = c(4, 4, 4, 1), 
                                  act.fct = "logistic")

## Training Error
NN2_Train_SSE <- sum((Yacht_NN2$net.result - Yacht_Data_Train[, 7])^2)/2

## Test Error
Test_NN2_Output <- neuralnet::compute(x = Yacht_NN2, covariate = Yacht_Data_Test[, 1:6])$net.result
NN2_Test_SSE <- sum((Test_NN2_Output - Yacht_Data_Test[, 7])^2)/2

plot(Yacht_NN2, rep = "best")










