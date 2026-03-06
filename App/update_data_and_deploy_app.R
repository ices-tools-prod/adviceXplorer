#### This code will update both SAG and SID data at regular time intervals and re-deploy the app at the end

# Load libraries listed in ui.R

library(rsconnect)



# setwd("./App")

## Once a year, please update SID_cache using
source("./App/utilities_SID_data.R")
source("./App/Update_SID_cache.R")

source("deploy.R")

