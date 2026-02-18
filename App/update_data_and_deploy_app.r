#### This code will update both SAG and SID data at regular time intervals and re-deploy the app at the end

# Libraries required for the code are loaded

library(rsconnect)



setwd("./App")

# This script updates data and deploys the application by sourcing necessary utility and update scripts.
# It includes:
# - utilities_SID_data.r: Contains utility functions for SID data processing.
# - utilities_sag_data.r: Contains utility functions for SAG data processing.
# - update_SAG_data.r: Script to update SAG data.
# - update_SID_data.r: Script to update SID data.
# source("utilities_SID_data.r")
# source("utilities_sag_data.r")
# source("update_SAG_data.r")
# source("update_SID_data.r")

# Function:
# - UpdateDataApp(mode = "LatestYear"): Updates the application data to the latest year.
#
# Parameters:
# - mode: A string parameter that specifies the mode of the update. 
#         In this case, it is set to "LatestYear" to update the data to the most recent year.
#
# Usage:
# - Run this script to ensure the application is using the latest available data.
#
# UpdateDataApp(mode = "AllYears")


source("deploy.r")

