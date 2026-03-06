# scripts/update_data.R
library(dplyr)
library(data.table)
library(tidyr)

source("App/utilities_SID_data.R")
source("App/Update_SID_cache.R")
# source("App/update_SAG_data.R")  # if needed

# rebuild caches (ensure it writes to App/Data/...)
save_SID_cache_many(2018:as.integer(format(Sys.Date(), "%Y")), force = TRUE)