#### This code will update both SAG and SID data at regular time intervals and re-deploy the app at the end

# Libraries required for the code are loaded

library(rsconnect)



setwd("./App")

# Optional
#source("Update_SID_cache.R")


source("deploy.r")

