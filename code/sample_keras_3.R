## Remove existing packages
remove.packages("keras")
remove.packages("tensorflow")
remove.packages("reticulate")

# Install github versions
devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/reticulate")