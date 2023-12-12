cran_package_list <- 
  c("doParallel", 
    "foreach", 
    "parallelly", 
    "tictoc", 
    "RColorBrewer", 
    "devtools", 
    "dplyr", 
    "fixest", 
    "ggplot2", 
    "glue", 
    "gt", 
    "haven", 
    "hdm", 
    "here", 
    "modelsummary", 
    "msm", 
    "purrr", 
    "readr", 
    "readxl", 
    "tidyr")

install.packages(cran_package_list,repos = "https://CRAN.R-project.org")

#Install MedBounds package from github (using SHA as of time of publication)
devtools::install_github("jonathandroth/MedBounds@6085a4c2b4bac93f7d312d0661c5be7a8bd07e84") 

