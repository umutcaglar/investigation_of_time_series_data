# Check the output of xgboost feature importance

# 1. Required Libraries ------------------------------------------------------
require(tidyverse)
require(cowplot)
require(lmtest)
require(mgcv)
require(xgboost)
require(caret)

# 2. Load Results ------------------------------------------------------
load(file = "../results/xgb_results.RData")

# 3. Adjust parameters ------------------------------------------------------
thr = 50

# 3. Combine Results ------------------------------------------------------
result01$var_importance %>%
    dplyr::mutate(effect = "r01") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r01 = Overall) -> r01_
r01_[which(nchar(r01_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r01_) -> r1__

result02$var_importance %>%
    dplyr::mutate(effect = "r02") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r02 = Overall) -> r02_
r02_[which(nchar(r02_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r02_) -> r2__

result03$var_importance %>%
    dplyr::mutate(effect = "r03") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r03 = Overall) -> r03_
r03_[which(nchar(r03_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r03_) -> r3__
    

result04$var_importance %>%
    dplyr::mutate(effect = "r04") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r04 = Overall) -> r04_
r04_[which(nchar(r04_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r04_) -> r4__

result05$var_importance %>%
    dplyr::mutate(effect = "r05") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r05 = Overall) -> r05_
r05_[which(nchar(r05_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r05_) -> r5__

result06$var_importance %>%
    dplyr::mutate(effect = "r06") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r06 = Overall) -> r06_
r06_[which(nchar(r06_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r06_) -> r6__

result07$var_importance %>%
    dplyr::mutate(effect = "r07") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r07 = Overall) -> r07_
r07_[which(nchar(r07_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r07_) -> r7__

result08$var_importance %>%
    dplyr::mutate(effect = "r08") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r08 = Overall) -> r08_
r08_[which(nchar(r08_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r08_) -> r8__

result09$var_importance %>%
    dplyr::mutate(effect = "r09") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r09 = Overall) -> r09_
r09_[which(nchar(r09_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r09_) -> r9__

result10$var_importance %>%
    dplyr::mutate(effect = "r10") %>%
    dplyr::filter(Overall > thr) %>%
    dplyr::select(effect = effect, cause = rowname, Overall_r10 = Overall) -> r10_
r10_[which(nchar(r10_$cause)==3),] %>% 
    dplyr::rename(cause = effect, effect = cause) %>% 
    rbind(.,r10_) -> r10__


full_join(r1__, r2__) %>%
    full_join(., r3__) %>%
    full_join(., r4__) %>%
    full_join(., r5__) %>%
    full_join(., r6__) %>%
    full_join(., r7__) %>%
    full_join(., r8__) %>%
    full_join(., r9__) %>%
    full_join(., r10__) -> q

View(q)