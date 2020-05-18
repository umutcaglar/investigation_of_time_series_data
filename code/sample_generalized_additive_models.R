

# Sample Generalized Additive Models
# https://noamross.github.io/gams-in-r-course/chapter1

# Chapter 1 (Intro)
# How to understand complex, multifaceted, non-linear relationships in data
# They are in between lm and deep learning in terms of explainibility
# Draw relationships between samples with delays

# Chapter 2 (Motorcycle Crash Data )
# Data contains two wariables acceleration and time

require("MASS") # needed for the data set
mcycle <- MASS::mcycle

# Examine the mcycle data frame
head(mcycle)
plot(mcycle)

# Fit a linear model
lm_mod <- lm(formula = accel ~ times, data = mcycle)

# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)





# Chapter 3 (Motorcycle Crash Data Nonlinear Approach)
# We will do the same with non-linear GAMs approach

# Fit a nonlinear model by using gans
require("mgcv") # needed for building GAMs

# Fit the model
mcycle %>% 
    mgcv::gam(accel ~ s(times), data = .) -> gam_mod

# Look at the coefficients 
coef(gam_mod)

# Plot the results
plot(gam_mod, residuals = TRUE, pch = 1)




# Chapter 5 (Basis Functions and Smoothing)
# we want to fit the trends we do not want to fit the noise
# Fit = Likelihood - lambda * wiggliness; lambda is smoothing parameter

# do not run:    mgcv::gam(y ~ s(x), data = dat, sp = 0.1);    set smoothing parameter for all eqn
# do not run:    mgcv::gam(y ~ s(x, sp = 0.1), data = dat);    set smoothing parameter for 1 vrbl

# There are also models to pick the correct smoothness with the help of mgcv package
# Higher smoothing parameter makes the fit smoother 
# Typical smoothing parameters are 0.1, 0.0001

# do not run:    gam(y ~ s(x), data = dat, method = "REML")

# most GAM experts, strongly recommend that you fit models with the REML, 
# or "Restricted Maximum Likelihood" method. 

# Setting number of basis functions

# do not run:    gam(y ~ s(x, k = 3), data = dat, method = "REML")  # run with 3 functions for x
# do not run:    gam(y ~ s(x, k = 10), data = dat, method = "REML")  # rund with 10 functions for x
# do not run:    gam(y ~ s(x), data = dat, method = "REML")  # rund with default number of functions for x

# Setting k value (i.e. number of basis functions that will be used)
#    * too low will prevent the model from being sufficiently wiggly. 
#    * If it's high, the automatic smoothing parameter selection will prevent it from being too wiggly. 
#       * We just don't want to set it very high, 
#         which can result in a model with more parameters than data, 
#         or one that is slow to fit.






# Chapter 6 (Basis Functions and Smoothing)

# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# Visualize the GAMs
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)




# Chapter 7 (Using Smoothing Parameters to Avoid Overfitting)

# Will fit using different Smoothing Parameters

# How to extract; auto-picked the smoothing parameter
gam_mod <- mgcv::gam(accel ~ s(times), data = mcycle, method = "REML")
gam_mod$sp


# Fix the smoothing parameter at 0.1
gam_mod_s1 <- mgcv::gam(accel ~ s(times), data = mcycle, sp = 0.1)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- mgcv::gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)




# Chapter 8 (Complexity and Smoothing Together)
# Fit the GAM
gam_mod_sk <- mgcv::gam(accel ~ s(times, k = 50), data = mcycle, sp = 0.0001)

# Visualize the model
plot(gam_mod_sk, residuals = TRUE, pch = 1)




# Chapter 9 (Complexity and Smoothing Together)

# We can use multiple input 
# * can contain mixture of smoothing parameters
# * can contain linear effects
# * can contain continious or categorical variables

# load data
library(gamair)
data("mpg", package="gamair")

# Examine the data
head(mpg)
str(mpg)

# Fit the model
mod_city <- mgcv::gam(formula = city.mpg ~ s(weight) + s(length) + s(price), 
                      data = mpg, method = "REML")

# Plot the model
plot(mod_city, pages = 1, pch = 1)


# Import formula
mod_city1 <- mgcv::gam(formula = city.mpg ~ s(weight) + s(length) + s(price), 
                      data = mpg, method = "REML")
gam.check(mod_city1)

mod_city2 <- mgcv::gam(formula = "city.mpg ~ s(weight) + s(length) + s(price)", 
                       data = mpg, method = "REML")
gam.check(mod_city2)

mod_city3 <- mgcv::gam(formula = as.formula("city.mpg ~ s(weight) + s(length) + s(price)"), 
                       data = mpg, method = "REML")
gam.check(mod_city3)


formula_ = as.formula("city.mpg ~ s(weight) + s(length) + s(price)")
mod_city4 <- mgcv::gam(formula = formula_, 
                       data = mpg, method = "REML")
gam.check(mod_city4)



mod_city$coefficients
data.frame(true_value = mod_city$y, predicted = mod_city$fitted.values)

coef(mod_city)

gam.check(mod_city)
summary(mod_city)

as.formula("city.mpg ~ s(weight) + s(length) + s(price)")










