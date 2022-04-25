if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, lubridate, PerformanceAnalytics, parallel, furrr, readxl, writexl, xlsx)#*xlsx

# * Troubleshooting Error : .onLoad failed in loadNamespace() for 'rJava'
## https://stackoverflow.com/questions/37735108/r-error-onload-failed-in-loadnamespace-for-rjava
