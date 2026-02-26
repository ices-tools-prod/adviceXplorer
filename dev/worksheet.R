
library(icesTAF)
library(icesSAG)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(icesFO)
library(stringr)
library(purrr)
library(htmltools)

setwd("App")

source("utilities_sag_data.r")
source("utilities_SID_data.r")
source("update_SAG_data.r")
source("update_SID_data.r")

year <- 2021

update_SAG(year)
update_SID(year)

stock <- c("her.27.28", "cap.27.1-2", "ghl.27.1-2", "cod.27.2.coastS", "reg.27.561214")[5]
access_sag_data_local <- function(stock_code, year) {
#   
    # Dowload the data
    df_summary <- fread(sprintf("App/Data/SAG_%s/SAG_summary.csv", year)) ####there is a space after SAG_ fix this below
    SAGsummary <- df_summary %>% filter(fishstock == stock_code)

    df_refpts <- fread(sprintf("App/Data/SAG_%s/SAG_refpts.csv", year)) ####there is a space after SAG_ fix this below
    SAGrefpts <- df_refpts %>% filter(StockKeyLabel == stock_code)

    data_sag <- merge(SAGsummary, SAGrefpts)
    data_sag <- data_sag %>% select(-fishstock) %>% filter(StockPublishNote == "Stock published")
    
    return(data_sag)
}
df <- access_sag_data_local(stock, year)
df$recruitment
shadeYears <- c(2018,2019)



recruitment_shaded <- df %>% filter(Year %in% shadeYears)
# view sag page
key <- df %>% head(1) %>% pull(AssessmentKey)
browseURL(paste0("https://standardgraphs.ices.dk/ViewCharts.aspx?key=", key))

source("utilities_plotting.r")

#ICES_plot_1(df)
#ICES_plot_2(df)
#ICES_plot_3(df)
ICES_plot_4(df)

# test in app


library(shiny)
runApp("App")


getSAGSettings <- function(assessmentkey) {
    sagSettings <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    )
}
assessmentkey <- 17615
settings <- getSAGSettings(assessmentkey)
df<- settings[!(settings$settingValue == ""), ]




catch_scenario_list <- get_Advice_View_info("hom.27.2a4a5b6a7a-ce-k8", 2019)
table <- get_catch_scenario_table(catch_scenario_list)
table_stand <- standardize_catch_scenario_table(table)

stock_name <- "ane.27.9a"
year <- 2022
catches_data<- access_sag_data_local("nep.fu.17", 2022)
catches_data <- catches_data %>%
    filter(Purpose == "Advice") %>%
    select(Year, catches, landings, discards) #%>% na.omit()

# Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, ..col_name])))
    }

    if (is_na_column(catches_data,"catches")){
      catches_data$catches <- rowSums(catches_data[,c("landings", "discards")], na.rm=TRUE)
      catches_data <- catches_data %>% select(-c("landings", "discards"))
    }



  catches_data <- catches_data %>% add_column(cat = "Historical Catches")
  catch_scenario_table <- table_stand$table %>% select(Year, TotCatch, cat)


  catch_scenario_list_previous_year <- get_Advice_View_info(stock_name, year - 1)

    catches_data <- catches_data %>% mutate(catches = ifelse(Year == year,  as.numeric(catch_scenario_list_previous_year$adviceValue), catches)) %>% na.omit()
#   catches_data <- catches_data %>% mutate(catches = c(catches[-n()], as.numeric(catch_scenario_list_previous_year$adviceValue))) 

  catches_data_year_before <- catch_scenario_table
  catches_data_year_before$Year <- catch_scenario_table$Year - 1 ## assessmnet year

  catches_data_year_before$TotCatch <- tail(catches_data$catches, 1)

  catches_data <- setNames(catches_data, names(catch_scenario_table))
  final_df <- rbind(catches_data, catches_data_year_before, catch_scenario_table)

  final_df <- na.omit(final_df)
wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel, query$year), catch_scenario_table()$table, query$stockkeylabel, query$year)


  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", "tur.27.4", 2018)
    )
  )


######################################################################################

get_Advice_View_info <- function(stock_name, year) {
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", stock_name, year)
    )
  )
  
  if (!is_empty(catch_scenario_list)){
  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice")
  } else {
     catch_scenario_list <- list()
  }
  
  return(catch_scenario_list)
}

get_catch_scenario_table <- function(catch_scenario_list) {
  catch_scenario_table <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getCatchScenariosTable/%s", catch_scenario_list$adviceKey)
    )
  )

  if (length(catch_scenario_table) != 0) {
  catch_scenario_table <- catch_scenario_table %>%
    pivot_wider(
      names_from = c(aK_ID, aK_Label, yearLabel, unit, stockDataType),
      names_glue = "{aK_Label} ({yearLabel}) _{stockDataType}_",
      values_from = value
    ) %>%
    select(-assessmentKey, -adviceKey, -cS_Basis, -aR_ID)


  catch_scenario_table <- catch_scenario_table %>% add_column(Year = catch_scenario_list$assessmentYear + 1, .before = "cS_Label")
  } else {
    catch_scenario_table <- character(0) 
  }

  return(catch_scenario_table)
}
standardize_catch_scenario_table <- function(tmp) {
  if (!is_empty(tmp)) {
  
  tmp_unified <- data.frame()

  # Year
  pattern <- c("Year")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp[, c(subset)]

  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])

  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])

  # Total catch"
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # Ftotal"
  pattern <- c("_FTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # Fwanted"
  pattern <- c("_Fwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F_wanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # HR
  pattern <- c("_HR_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(HR = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # SSB"
  pattern <- c("_StockSize_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }
  # dead discards"
  pattern <- c("_CatchUnwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # surviving discards"
  pattern <- c( "surviving")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwantedSurviving = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # % TAC change"
  pattern <- c("_TACchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # % Advice change"
  pattern <- c("_Advchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # % SSB change "
  pattern <- c("_StockSizechange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # Save the names to re-use them later when diplaying the table
  colnames(tmp_unified) <- sub(" _.*_", "", colnames(tmp_unified))
  col_names_for_display <- colnames(tmp_unified)
  
  # rename columns to standard names
  colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "TotCatch", "F", "F_wanted", "HR", "SSB","CatchUnwanted","CatchUnwantedSurviving", "TAC change", "ADVICE change", "SSB change")

  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  }
  else {
    tmp_unified <- character(0)
    col_names_for_display <- character(0)
  }
  
  return(list(table = tmp_unified, cols = col_names_for_display))
}








is_na_column <- function(list) {
        return(all(is.na(list)))
    }
if (is_na_column(data.frame(table_stand$table$F))){
  print("yes")
}

data.frame(table_stand$table)
Basis <- table_stand$table$cS_Purpose == "Basis Of Advice"

data.frame(table_stand$table[c(Basis),])


tmp <- data.frame(table_stand$table)
    
    labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)

    
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice",]

# Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, col_name])))
    }
if (is_na_column(tmp, "F")){
  print("yes")
}
names(tmp)

catch_scenario_list_1 <- get_Advice_View_info("nep.fu.17", 2022)
catch_scenario_list_2 <- get_Advice_View_info("cod.27.7e-k", 2022-1)
table_1 <- get_catch_scenario_table(catch_scenario_list_1)
table_2 <- get_catch_scenario_table(catch_scenario_list_2)
table_stand_1 <- standardize_catch_scenario_table(table_1)
table_stand_2 <- standardize_catch_scenario_table(table_2)





scale_catch_scenarios_for_radialPlot <- function(old_catch_scen_table, new_catch_scen_table){
  if (!is_empty(new_catch_scen_table)) {
  Basis <- old_catch_scen_table[old_catch_scen_table$cS_Purpose == "Basis Of Advice",]
  catch_scen_table_perc <- new_catch_scen_table[, c("Year", "cat", "cS_Purpose")]
  
  catch_scen_table_perc$F <- (new_catch_scen_table$F - Basis$F) / Basis$F *100
  catch_scen_table_perc$F_wanted <- (new_catch_scen_table$F_wanted - Basis$F_wanted) / Basis$F_wanted *100
  catch_scen_table_perc$HR <- (new_catch_scen_table$HR - Basis$HR) / Basis$HR *100
  catch_scen_table_perc$TotCatch <- (new_catch_scen_table$TotCatch - Basis$TotCatch) / Basis$TotCatch *100
  catch_scen_table_perc$`TAC change` <- new_catch_scen_table$`TAC change`
  catch_scen_table_perc$`ADVICE change` <- new_catch_scen_table$`ADVICE change`
  catch_scen_table_perc$SSB <- (new_catch_scen_table$SSB - Basis$SSB) / Basis$SSB *100
  catch_scen_table_perc$`SSB change` <- new_catch_scen_table$`SSB change`
  } else {
    catch_scen_table_perc <- character(0)
  }
  
  return(catch_scen_table_perc)
}


old_catch_scen_table <- table_stand_2$table
new_catch_scen_table <- table_stand_1$table
scale_catch_scenarios_for_radialPlot(table_stand_2$table, table_stand_1$table)

# calculate_perc_change <- function(new_catch_scen_table_column, Basis, catch_scen_table_perc_column){
#   catch_scen_table_perc$F <- (new_catch_scen_table$F - Basis$F[1]) / Basis$F[1] *100
#   ifelse(new_catch_scen_table$F == Basis$F[1] & Basis$F[1] == 0, 1, (new_catch_scen_table$F - Basis$F[1]) / Basis$F[1])
# }



# calculate_perc_change <- function(new_catch_scen_table, Basis, catch_scen_table_perc) {
  
#   for (m in 4:ncol(new_catch_scen_table)) {  
#   for (i in seq_len(nrow(new_catch_scen_table))) {
#     if (Basis[1,m] == 0) {
#       catch_scen_table_perc[i,m] <- 1 * 100
#       if (new_catch_scen_table[i,m] == Basis[1,m]) {
#         catch_scen_table_perc[i,m] <- 0
#       }
#     } else {
#       catch_scen_table_perc[i,m] <- 100 * ((new_catch_scen_table[i,m] - Basis[1,m]) / Basis[1,m])
#     }
#   }
# }
# }
catch_scenario_list_1 <- get_advice_view_info("nep.fu.17", 2022)
catch_scenario_list_2 <- get_Advice_View_info("her.27.irls", 2022-1)
table_1 <- get_catch_scenario_table(catch_scenario_list_1$adviceKey, 2022)
table_2 <- get_catch_scenario_table(catch_scenario_list_2)
table_stand_1 <- standardize_catch_scenario_table(table_1)
table_stand_2 <- standardize_catch_scenario_table(table_2)

changes_columns <- table_stand_1$table %>% select("cat","TAC change","ADVICE change", "SSB change")


keep.cols <- c("Year", "cat" , "cS_Purpose","F", "F_wanted","HR","TotCatch","SSB")
df_old <- table_stand_2$table %>% select(keep.cols)
df_new <- table_stand_1$table %>% select(keep.cols)

df_old <- df_old[,colSums(is.na(df_old))<nrow(df_old)]
df_new <- df_new[,colSums(is.na(df_new))<nrow(df_new)]
Basis <- df_old[df_old$cS_Purpose == "Basis Of Advice",]
df_new <- df_new %>% na.omit()
catch_scen_table_perc <- df_new[, c("Year", "cat", "cS_Purpose")]

which(is.na(df_new), arr.ind=TRUE)

df_new <- df_new[c(-9,-10),]
catch_scen_table_perc <- catch_scen_table_perc[c(-9,-10),]
catch_scen_table_perc <- calculate_perc_change(df_new, Basis, catch_scen_table_perc)
catch_scen_table_perc <- catch_scen_table_perc %>% left_join(., changes_columns, by = c("cat"))

calculate_perc_change <- function(df_new, Basis, catch_scen_table_perc) {

  for (m in 4:ncol(df_new)) {
    for (i in seq_len(nrow(df_new))) {
      if (Basis[1, m] == 0) {
        catch_scen_table_perc[i, m] <- 1 * 100
        if (df_new[i, m] == Basis[1, m]) {
          catch_scen_table_perc[i, m] <- 0
        }
      } else {
        catch_scen_table_perc[i, m] <- 100 * ((df_new[i, m] - Basis[1, m]) / Basis[1, m])
      }
    }
  }
  return(catch_scen_table_perc)
}
catch_scen_table_perc <- catch_scen_table_perc %>% left_join(., changes_columns, by = c("cat"))

a <- 0
b<- 0
# 100*((b-a)/a)

if (a == 0) {
  c <- 1 * 100
  if (b == a) {
    c <- 0
  }
} else {
  c <- 100 * ((b - a) / a)
}
print(c)


scale_catch_scenarios_for_radialPlot <- function(old_catch_scen_table, new_catch_scen_table){
  if (!is_empty(new_catch_scen_table) & !is_empty(old_catch_scen_table)) {



  Basis <- old_catch_scen_table[old_catch_scen_table$cS_Purpose == "Basis Of Advice",]
  catch_scen_table_perc <- new_catch_scen_table[, c("Year", "cat", "cS_Purpose", "F")]
  
  catch_scen_table_perc$F <- calculate_perc_change(new_catch_scen_table$F, Basis$F, catch_scen_table_perc$F)
  catch_scen_table_perc$F <- (new_catch_scen_table$F - Basis$F) / Basis$F *100
  catch_scen_table_perc$F_wanted <- (new_catch_scen_table$F_wanted - Basis$F_wanted) / Basis$F_wanted *100
  catch_scen_table_perc$HR <- (new_catch_scen_table$HR - Basis$HR) / Basis$HR *100
  catch_scen_table_perc$TotCatch <- (new_catch_scen_table$TotCatch - Basis$TotCatch) / Basis$TotCatch *100
  catch_scen_table_perc$`TAC change` <- new_catch_scen_table$`TAC change`
  catch_scen_table_perc$`ADVICE change` <- new_catch_scen_table$`ADVICE change`
  catch_scen_table_perc$SSB <- (new_catch_scen_table$SSB - Basis$SSB) / Basis$SSB *100
  catch_scen_table_perc$`SSB change` <- new_catch_scen_table$`SSB change`
  } else {
    catch_scen_table_perc <- character(0)
  }
  
  return(catch_scen_table_perc)
}



# Use which() to find the row indices where second_column is not NA
not_na_indices <- which(!is.na(catches_data$SSB))

# Use the result to index the first_column vector and find the minimum value
min_value <- min(catches_data$Year[not_na_indices])



########################################################################5
library(icesASD)
library(httr)
library(jsonlite)

year <- 2019
stock <- "aru.27.5a14"
test <- get_advice_view_info(stock, year)

if (is_empty(test)) {
  test <- get_advice_view_info(stock, year - 1)
  if (!is_empty(test)) {
    test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
  } else {
    test <- list()
  }
} else {
  if (nrow(test > 1)) {
    test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
  } else {
    test <- test
  }
}
test

if (nrow(test) > 1) {
  test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
} else if (is_empty(test)) {
  test <- get_advice_view_info("bli.27.5b67", year - 1)
  test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
}


get_additional_landing_data <- function(assessmentKey) {
  out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentKey)
        )
    )  
  df <- data.frame(Year = out$lines$year, ibc = out$lines$ibc, unallocated_Removals = out$lines$unallocated_Removals)
  return(df)
}

out <- jsonlite::fromJSON(
            URLencode(
                sprintf("https://sag.ices.dk/SAG_API/api/S
    ummaryTable?assessmentKey=%s", 17689)))






api <- function(stock_name = NULL, year = NULL, adviceKey = NULL, api = c("record", "table", "notes")) {
  
  record_api_url <- "https://sg.ices.dk/adviceview/API/getAdviceViewRecord"
  table_api_url <- "https://sg.ices.dk/adviceview/API/getCatchScenariosTable"
  notes_api_url <- "https://sg.ices.dk/adviceview/API/getCatchScenariosNotes"


  api <- match.arg(api)
  if (api == "record") {
    api_url <- get(paste0(api, "_api_url"))
    url <- paste0(api_url, "?", "stockcode=", stock_name, "&year=", year)
    url <- parse_url(url)
    url <- build_url(url)
    url
  } else {
    api_url <- get(paste0(api, "_api_url"))
    url <- paste0(api_url, "/", adviceKey)
    url <- parse_url(url)
    url <- build_url(url)
    url
  }
}
get_advice_view_info <- function(stock_name, year) {
 
  catch_scenario_list <-
        read_json(
            api(stock_name = stock_name, year = year, api = "record"),
            simplifyVector = TRUE
        )
  
  if (!is_empty(catch_scenario_list)){
  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice")
  } else {
     catch_scenario_list <- list()
  }
  
  return(catch_scenario_list)
}


dates <- as.POSIXct(test$adviceApplicableFrom)
format(as.POSIXct(test$adviceApplicableFrom), format="%Y")
test$adviceApplicableFrom


shorten_labels <- function(catch_scenarios_array) {
        
        for (i in 1:length(catch_scenarios_array)) {
            if (nchar(catch_scenarios_array[i]) > 20) {
                catch_scenarios_array[i] <- paste0(substr(catch_scenarios_array[i], 1, 20), "...")
            } else {
                catch_scenarios_array[i] <- catch_scenarios_array[i]
            }
        }
        return(catch_scenarios_array)
    }
shorten_labels(table_1$cS_Label)

library(icesASD)
library(icesASD)
library(httr)
library(jsonlite)

year <- 2022
stock <- "hom.27.2a4a5b6a7a-ce-k8"
test <- icesASD::get_advice_view_info(stock,year)
notes <- icesASD::get_catch_scenario_notes(test$adviceKey)



SAGrefpts <- getSAG("cod.27.47d20", 2022,
        data = "refpts", combine = TRUE, purpose = "Advice"
    )
SAGsummary <- getSAG("cod.27.47d20", 2022,
        data = "summary", combine = TRUE, purpose = "Advice"
    )



xx<- icesASD::get_catch_scenario_table(3102, 2022)
tmp <- xx
test <- standardize_catch_scenario_table(tmp)
 
standardize_catch_scenario_table <- function(tmp) {
  if (!is_empty(tmp)) {
  
  tmp_unified <- data.frame()

  # Year
  pattern <- c("Year")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <-  data.frame(tmp[subset])

  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset]) ####this works!

  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset])

  # Total catch"
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # Ftotal"
  pattern <- c("_FTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # Fwanted"
  pattern <- c("_Fwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F_wanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # HR
  pattern <- c("_HR_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(HR = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # SSB"
  pattern <- c("_StockSize_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # dead discards"
  pattern <- c("_CatchUnwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # surviving discards"
  pattern <- c( "surviving")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwantedSurviving = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # % TAC change"
  pattern <- c("_TACchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # % Advice change"
  pattern <- c("_Advchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # % SSB change "
  pattern <- c("_StockSizechange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # Save the names to re-use them later when diplaying the table
  colnames(tmp_unified) <- sub(" _.*_", "", colnames(tmp_unified))
  col_names_for_display <- colnames(tmp_unified)
  
  # rename columns to standard names
  # colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "TotCatch", "F", "F_wanted", "HR", "SSB", "TAC change", "ADVICE change", "SSB change")
  colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "TotCatch", "F", "F_wanted", "HR", "SSB","CatchUnwanted","CatchUnwantedSurviving", "TAC change", "ADVICE change", "SSB change")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  }
  else {
    tmp_unified <- character(0)
    col_names_for_display <- character(0)
  }
  
  return(list(table = tmp_unified, cols = col_names_for_display))
}


library(icesSAG)
SAGsummary <- getSAG("cod.27.47d20", 2022,
         purpose = "Replaced"
    )
codKeys <- findAssessmentKey("cod.27.47d20", year = 2022)


out <- jsonlite::fromJSON(
            URLencode(
                sprintf("https://sag.ices.dk/SAG_API/api/StockList?year=%s", 2022))) %>% filter(stockKeyLabel == "cod.27.47d20") %>% filter(purpose == "Replaced") %>% pull(linkToAdvice)
                
out <- out %>% filter(stockKeyLabel == "cod.27.47d20") %>% filter(purpose == "Replaced") %>% pull(linkToAdvice)



test <- getListStocks(2022) %>% filter(StockKeyLabel == "cod.27.47d20") %>% filter(Purpose == "Replaced") %>% pull(LinkToAdvice)
test <- filter(stockKeyLabel == "cod.27.47d20") %>% filter(purpose == "Replaced") %>% pull(linkToAdvice)


out <- jsonlite::fromJSON(
            URLencode(
                sprintf("https://sag.ices.dk/SAG_API/api/FishStockReferencePoints?assessmentKey=%s", 17652)))



https://sag.ices.dk/SAG_API/api/FishStockReferencePoints?assessmentKey=17652

out2 <- icesSAG::getSAG(
    stock = NULL, 2022, purpose = "Advice",
    data = "refpts", combine = TRUE
  )

df1 <- data.frame(assessmentKey = c(1200,1250),
                  stockkeylabel = c(120,125)
                  )
  


df2 <- data.frame(AssessmentKey = c(1200,1250),
                  StockKeyLabel= c(120,125),
                  confidenceIntervalDefinition = c(0.1,0.2))
print(df2)
colnames(df2)[which(names(df2) == "AssessmentKey")] <- "assessmentKey"
colnames(df2)[which(names(df2) == "StockKeyLabel")] <- "stockkeylabel"

# Merging dataframes
merged_df <- merge(df1, df2, by = c("assessmentKey", "stockkeylabel"))


out <- data.frame()
for (AssessmentKey in out2$AssessmentKey) {
  out_temp <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sag.ices.dk/SAG_API/api/FishStockReferencePoints?assessmentKey=%s", AssessmentKey)
    )
  )
  out <- rbind(out, out_temp)
}

names(out)
names(out2)

# Convert elements to lowercase
arr1_lower <- tolower(names(out))
arr2_lower <- tolower(names(out2))

# Find common elements
common_elements <- intersect(arr1_lower, arr2_lower)


colnames(out)[which(names(out) == "assessmentKey")] <- "AssessmentKey"
colnames(out)[which(names(out) == "stockKeyLabel")] <- "StockKeyLabel"
colnames(out)[which(names(out) == "stockDatabaseID")] <- "StockDatabaseID"
colnames(out)[which(names(out) == "stockKey")] <- "StockKey"
colnames(out)[which(names(out) == "assessmentYear")] <- "AssessmentYear"
colnames(out)[which(names(out) == "fLim")] <- "FLim"
colnames(out)[which(names(out) == "fpa")] <- "Fpa"
colnames(out)[which(names(out) == "fAge")] <- "FAge"
colnames(out)[which(names(out) == "bpa")] <- "Bpa"
colnames(out)[which(names(out) == "blim")] <- "Blim"
colnames(out)[which(names(out) == "recruitmentAge")] <- "RecruitmentAge"
colnames(out)[which(names(out) == "fmsy")] <- "FMSY"
colnames(out)[which(names(out) == "msyBtrigger")] <- "MSYBtrigger"
colnames(out)[which(names(out) == "fmanagement")] <- "Fmanagement"
colnames(out)[which(names(out) == "bmanagement")] <- "Bmanagement"
colnames(out)[which(names(out) == "recruitmentLength")] <- "RecruitmentLength"
colnames(out)[which(names(out) == "fLength")] <- "FLength"
colnames(out)[which(names(out) == "fCap")] <- "Fcap"

merged_df <- merge(out, out2, by = c("AssessmentKey"
, "StockKeyLabel",
"StockDatabaseID","StockKey","AssessmentYear","FLim","Fpa","FAge","Bpa","Blim","RecruitmentAge","FMSY","MSYBtrigger",
"Fmanagement","Bmanagement","RecruitmentLength","FLength","Fcap"))





names(merged_df)

final <- merged_df %>% select(c("AssessmentKey", 
"StockKeyLabel",
     "StockDatabaseID",   
     "StockKey" ,        
      "AssessmentYear",
"Blim"   ,           "RecruitmentAge" ,   "FAge"  ,            "Bpa"   ,            "Fpa",
"FMSY"    ,          "MSYBtrigger"    ,   "Fmanagement"  ,     "FLim"     ,         "Bmanagement",
[16] "RecruitmentLength", "FLength"  ,         "Fcap",))

library(dplyr)


get_CI <- function(df) {
  out <- data.frame()
  for (AssessmentKey in df$AssessmentKey) {
    out_temp <- jsonlite::fromJSON(
      URLencode(
        sprintf("https://sag.ices.dk/SAG_API/api/FishStockReferencePoints?assessmentKey=%s", AssessmentKey) 
      )
    )
    out_temp <- out_temp %>% select(assessmentKey,confidenceIntervalDefinition)
    out <- rbind(out, out_temp)
    
  }

  colnames(out)[which(names(out) == "assessmentKey")] <- "AssessmentKey"
  # colnames(out)[which(names(out) == "stockKeyLabel")] <- "StockKeyLabel"
  # colnames(out)[which(names(out) == "stockDatabaseID")] <- "StockDatabaseID"
  # colnames(out)[which(names(out) == "stockKey")] <- "StockKey"
  # colnames(out)[which(names(out) == "assessmentYear")] <- "AssessmentYear"
  # colnames(out)[which(names(out) == "fLim")] <- "FLim"
  # colnames(out)[which(names(out) == "fpa")] <- "Fpa"
  # colnames(out)[which(names(out) == "fAge")] <- "FAge"
  # colnames(out)[which(names(out) == "bpa")] <- "Bpa"
  # colnames(out)[which(names(out) == "blim")] <- "Blim"
  # colnames(out)[which(names(out) == "recruitmentAge")] <- "RecruitmentAge"
  # colnames(out)[which(names(out) == "fmsy")] <- "FMSY"
  # colnames(out)[which(names(out) == "msyBtrigger")] <- "MSYBtrigger"
  # colnames(out)[which(names(out) == "fmanagement")] <- "Fmanagement"
  # colnames(out)[which(names(out) == "bmanagement")] <- "Bmanagement"
  # colnames(out)[which(names(out) == "recruitmentLength")] <- "RecruitmentLength"
  # colnames(out)[which(names(out) == "fLength")] <- "FLength"
  # colnames(out)[which(names(out) == "fCap")] <- "Fcap"

  # merged_df <- merge(out, df, by = c(
  #   "AssessmentKey",
  #   "StockKeyLabel",
  #   "StockDatabaseID",
  #   "StockKey",
  #   "AssessmentYear",
  #   "FLim",
  #   "Fpa",
  #   "FAge",
  #   "Bpa",
  #   "Blim",
  #   "RecruitmentAge",
  #   "FMSY",
  #   "MSYBtrigger",
  #   "Fmanagement",
  #   "Bmanagement",
  #   "RecruitmentLength",
  #   "FLength",
  #   "Fcap"
  # ))
  return(out)
}

xx <-get_CI(out2)





 SAGsummary <- getSAG("spr.27.3a4", 2023,
        data = "summary", combine = TRUE, purpose = "Advice"
    )


library(shiny)

ui <- fluidPage(
  titlePanel("Download Example"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("downloadBtn", "Download")
    ),
    
    mainPanel(
      h4("Download Link"),
      verbatimTextOutput("downloadLink")
    )
  )
)

library(shiny)
library(dplyr)
library(zip)

server<-function(input, output) {
  # Create a sample dataframe
  df <- data.frame(
    Name = c("John", "Jane", "Mike"),
    Age = c(25, 30, 35),
    StringsAsFactors = FALSE
  )
  
  # Function to create the zip file
  createZipFile <- function() {
    # Convert dataframe to CSV
    df_csv <- df %>% write_csv(path = "data.csv")
    
    # Create the disclaimer text file
    disclaimer <- "This is a disclaimer."
    writeLines(disclaimer, "disclaimer.txt")
    
    # Create the zip file
    zip_file <- "data.zip"
    zip(zip_file, files = c("data.csv", "disclaimer.txt"))
    
    # Remove the temporary files
    file.remove(c("data.csv", "disclaimer.txt"))
    
    # Return the zip file name
    return(zip_file)
  }
  
  # Event handler for the download button
  observeEvent(input$downloadBtn, {
    output$downloadLink <- renderText({
      zip_file <- createZipFile()
      downloadLink <- sprintf('<a href="%s" download>Download</a>', zip_file)
      return(downloadLink)
    })
  })
}
shinyApp(ui, server)
server <- function(input, output) {

  datasetInput <- reactive({
    return(list(rock=rock, pressure=pressure, cars=cars))
  })

  output$downloadData <- downloadHandler(
    filename = 'pdfs.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())

      fs <- c("rock.csv", "pressure.csv", "cars.csv")
      write.csv(datasetInput()$rock, file = "rock.csv", sep =",")
      write.csv(datasetInput()$pressure, file = "pressure.csv", sep =",")
      write.csv(datasetInput()$cars, file = "cars.csv", sep =",")
      print (fs)

      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )

}

# ui.R
ui <- shinyUI(fluidPage(
  titlePanel('Downloading Data'),
  sidebarLayout(
    sidebarPanel(
      downloadButton('downloadData', 'Download')
    ),
    mainPanel()
    )
  )
  )

shinyApp(ui = ui, server = server)


###### this one works 
ui <- fluidPage(
  downloadLink("downloadData", HTML("<font size= 15>HERE</font>"))
)

server <- function(input, output) {
  # Our dataset
  data <- mtcars

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
}

library(icesASD)
icesASD::adviceDownload(adviceKeys = 3427)
icesASD::getCatchScenariosTable(adviceKey = 3427)
icesASD::get_catch_scenario_table("cod.27.47d20",2022)
icesASD::getAdviceViewRecord(stockcode = "cod.27.46a7d20", 2023)


sag <-
      getListStocks(year = 2023) %>%
      select(AssessmentKey, StockKeyLabel, AssessmentYear, Purpose, StockDescription, ModifiedDate, SAGStamp, LinkToAdvice) %>%
      rename(fishstock = StockKeyLabel)

summary <- load_sag_summary(2023) 

%>%
      left_join(sag, by = c("fishstock", "AssessmentKey","AssessmentYear", "Purpose"))


stock_list_long <- fread(sprintf("App/Data/SID_%s/SID.csv", 2023))


data <- stock_list_long
# Row to duplicate (e.g., row 2)
row_to_duplicate <- data %>% filter(StockKeyLabel == "cod.27.46a7d20")
# row_to_duplicate <- data[2, ]

# List of values to change
new_values <- c(18282,18283,18284)

# Number of times to duplicate the row
# Sample dataframe
# Sample dataframe
# Sample dataframe
df <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David"),
  Age = c(25, 30, 22, 27),
  Score = c(90, 85, 88, 92)
)

# Rows to duplicate (e.g., rows 2 and 3)
# rows_to_duplicate <- df[c(2, 3), ]
row_to_duplicate <- data %>% filter(StockKeyLabel == "cod.27.46a7d20")
# List of new values for the specified column (e.g., "Score")

new_values <- c(18282,18283,18284)

# Number of times to duplicate each row
n_duplications <- 2

# Create an empty dataframe for the result
result_df <- data.frame()

# Loop through the rows to duplicate
for (i in 1:nrow(rows_to_duplicate)) {
  row <- rows_to_duplicate[i, ]
  
  # Duplicate the row and modify the specified column
  duplicated_rows <- data.frame(
    # AssessmentKey = rep(row$AssessmentKey, n_duplications)
    # Age = rep(row$Age, n_duplications),
    AssessmentKey = rep(new_values[i], n_duplications)
  )
  
  # Add the duplicated rows to the result dataframe
  result_df <- rbind(result_df, duplicated_rows)
}

# Combine the result with the original dataframe
result_df <- rbind(df, result_df)

# Reset row names and row indices
rownames(result_df) <- NULL

# Print the result
print(result_df)

##############################################################################################################################
# Sample dataframe
# data <- data.frame(
#   AssessmentKey = c(18396, 18396),
#   icon = c("<img src='cod-7e-k_pic.png' height=40>", "<img src='cod-7e-k_pic.png' height=40>"),
#   doi = c("<a href='not_available' target='_blank'><img src='pdf-file.png' height='30px'/></a>", "<a href='not_available' target='_blank'><img src='pdf-file.png' height='30px'/></a>"),
#   FO_doi = c("<a href='' target='_blank'><img src='seafood.png' height='30px'/></a>", "<a href='' target='_blank'><img src='seafood.png' height='30px'/></a>"),
#   group_url = c("<a href='https://www.ices.dk/community/groups/Pages/WGNSSK.aspx' target='_blank'>WGNSSK</a>", "<a href='https://www.ices.dk/community/groups/Pages/WGNSSK.aspx' target='_blank'>WGNSSK</a>"),
#   SAG_url = c("<a href='https://standardgraphs.ices.dk/ViewCharts.aspx?key=18396' target='_blank'><img src='database.png' height='30px'/></a>", "<a href='https://standardgraphs.ices.dk/ViewCharts.aspx?key=18396' target='_blank'><img src='database.png' height='30px'/></a>"),
#   visa_url = c("<a href='https://gis.ices.dk/sf/index.html?widget=visa&assessmentKey=18396' target='_blank'><img src='map.png' height='30px'/></a>", "<a href='https://gis.ices.dk/sf/index.html?widget=visa&assessmentKey=18396' target='_blank'><img src='map.png' height='30px'/></a>")
# )
data <- stock_list_long
# Rows to duplicate
# rows_to_duplicate <- data[c(1, 2), ]
rows_to_duplicate <- data %>% filter(StockKeyLabel == "cod.27.46a7d20")

# List of new values for specific columns
new_values <- list(
  AssessmentKey = c(18282,18283,18284)
)

# Number of times to duplicate each row
n_duplications <- 1

# Create an empty dataframe for the result
result_df <- data.frame()

# Loop through the rows to duplicate
for (i in 1:nrow(rows_to_duplicate)) {
  for(j in 1:3) {
  row <- rows_to_duplicate[i, ]
  
  # Duplicate the row n_duplications times
  duplicated_rows <- data.frame(
    StockKeyLabel = rep(row$StockKeyLabel, n_duplications),
    EcoRegion = rep(row$EcoRegion, n_duplications),
    SpeciesCommonName = rep(row$SpeciesCommonName, n_duplications),
    ExpertGroup = rep(row$ExpertGroup, n_duplications),
    DataCategory = rep(row$DataCategory, n_duplications),
    YearOfLastAssessment = rep(row$YearOfLastAssessment, n_duplications),
    AssessmentFrequency = rep(row$AssessmentFrequency, n_duplications),
    AdviceCategory = rep(row$AdviceCategory, n_duplications),
    AssessmentKey = new_values$AssessmentKey[j],
    icon = rep(row$icon, n_duplications),
    doi = rep(row$icon, n_duplications),
    FO_doi = rep(row$doi, n_duplications),
    group_url = rep(row$group_url, n_duplications),
    SAG_url = rep(row$SAG_url, n_duplications),
    visa_url = rep(row$visa_url, n_duplications)
  )
  
  # Add the duplicated rows to the result dataframe
  result_df <- rbind(result_df, duplicated_rows)
}
}

# Combine the result with the original dataframe
result_df <- rbind(data, result_df)

# Reset row names and row indices
rownames(result_df) <- NULL

# Print the result
print(result_df)
write.csv(result_df, "SID.csv")

test <- result_df %>% filter(StockKeyLabel == "cod.27.46a7d20")

test

stock_code <- "cod.27.46a7d20"
year <- 2023
assessmentkey <- 18282
access_sag_data_local <- function(stock_code, year, assessmentkey) {
#   
    # Dowload the data
    df_summary <- fread(sprintf("App/Data/SAG_%s/SAG_summary.csv", 2023)) ####there is a space after SAG_ fix this below
    SAGsummary <- df_summary %>% filter(fishstock == stock_code & AssessmentKey == assessmentkey)

    names(SAGsummary)
    unique(SAGsummary$AssessmentKey)
    unique(SAGsummary$Purpose)
    SAGsummary$Year

    df_refpts <- fread(sprintf("App/Data/SAG_%s/SAG_refpts.csv", year)) ####there is a space after SAG_ fix this below
    SAGrefpts <- df_refpts %>% filter(StockKeyLabel == stock_code)

    data_sag <- merge(SAGsummary, SAGrefpts)

    # data_sag <- data_sag %>% filter(AssessmentKey == AssessmentKey)

    data_sag <- data_sag %>% select(-fishstock) %>% filter(StockPublishNote == "Stock published")
    # print(data_sag) %>% 
    return(data_sag)
    
}
names(SAGsummary)
SAGsummary <- getSAG("cod.27.46a7d20", 2023,
        data = "summary", combine = TRUE, purpose = "Advice"
    )

write.csv(SAGsummary, "test_2023.csv")
SAGsummary <- SAGsummary %>% filter()


out_temp <- jsonlite::fromJSON(
      URLencode(
        sprintf("https://sag.ices.dk/SAG_API/api/StockDownload?assessmentKey=%s", 18282) 
      )
    )
https://sag.ices.dk/SAG_API/api/StockDownload?assessmentKey=18282


getAdviceViewRecord(assessmentkey = 18396)


df <- data.frame(
  ID = c(1, 2, 3),
  Name = c("John", "Alice", "Bob"),
  Age = c(25, 30, 22)
)

# Choose the row you want to duplicate (let's say the first row, index 1)
row_to_duplicate <- df[1, ]

# Duplicate the chosen row
duplicated_row <- rbind(df, row_to_duplicate)

# Change one value in the duplicated row (let's say change the ID to 4)
duplicated_row[nrow(duplicated_row), "ID"] <- 4

year <- 2023
update_SID <- function(year){
    mkdir(paste0("App/Data/SID_", year))
    
    ### download SID
    stock_list_all <- download_SID(year)
    ### modifify SID table, 1 row == 1 Ecoregion
    stock_list_long <- separate_ecoregions(stock_list_all)
    names(stock_list_long)
    ### add hyperlinks to table
    # stock_list_long <- sid_table_links(stock_list_long)

    
    if (stock_list_long$YearOfLastAssessment == 2023){
      row_to_duplicate <- stock_list_long %>% filter(StockKeyLabel == "cod.27.46a7d20" & stock_list_long$EcoRegion %in% c("Greater North Sea Ecoregion", "Celtic Seas Ecoregion"))
      # row_to_duplicate <- stock_list_long[stock_list_long$StockKeyLabel == "cod.27.46a7d20" & stock_list_long$EcoRegion == "Greater North Sea Ecoregion" , ] 
      duplicated_df <- row_to_duplicate[rep(row.names(row_to_duplicate), each = 3), ]
      row.names(duplicated_df) <- NULL  # Reset row names
      
      Cod_Keys <- c(18282,18283,18284,18282,18283,18284)
      for (i in 1:nrow(duplicated_df)) {
        duplicated_df$AssessmentKey[i] <- Cod_Keys[i]
      }
    }

    stock_list_long <- rbind(stock_list_long, duplicated_df)
    row.names(stock_list_long) <- NULL  # Reset row names
  
    write.taf(stock_list_long, file = "SID.csv", dir = paste0("App/Data/SID_", year))

}

year <- 2023
update_SID(2022)

library(icesSAG)

testSAG <- icesSAG::StockList(2023)
names(testSAG)


Sys.setenv("LOADTEST_JMETER_PATH"="[D:/apache-jmeter-5.6.3/bin]")
remotes::install_github("tmobile/loadtest")

library(loadtest)
results <- loadtest(url = "https://ices-taf.shinyapps.io/advicexplorer",
                    method = "GET",
                    # headers = c("version"="v1.0"),
                    # body = list(sentences = list("I love this band")),
                    # encode="json",
                    threads = 10,
                    loops = 20,
                    delay_per_request=100)
loadtest_report(results,"D:/report.html")
plot_elapsed_times(results)
plot_elapsed_times_histogram(results)
plot_requests_by_thread(results)
plot_requests_per_second(results)


SAGsummary <- getSAG("whb.27.1-91214", 2023,
        data = "summary", combine = TRUE, purpose = "Advice"
    )
head(SAGsummary)
names(SAGsummary)
test <- icesSAG::SummaryTable(13429)
names(test)


getSAGSettings <- function(assessmentkey) {
    sagSettings <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    )
}
assessmentkey <- 17615

settings <- icesSAG::StockSettings(18515)

sag <- icesSAG::SummaryTable(18515)
refP <- icesSAG::FishStockReferencePoints(18515)
head(sag)
settings <- getSAGSettings(assessmentkey)
df<- settings[!(settings$settingValue == ""), ]


library(reactablefmtr)
table <- reactable(iris[10:29, ])

table %>%
  add_title("This is a title")

## Use options to adjust the style and position of the title
table %>%
  add_title("This is a title", align = "center", font_color = "red")
}

library(reactable)
library(shiny)
ui <- fluidPage(
  h2("Top CRAN Packages of 2019"),
  reactableOutput("table_1")
)
 
server <- function(input, output) {
  output$table_1 <- renderReactable({
    
    example<- reactable(data.frame(country=c("argentina","brazil"),value=c(1,2)))

  })
}

shinyApp(ui = ui, server = server)


reactable(
  iris[1:5, ],
  defaultColDef = colDef(
    header = function(value) gsub(".", " ", value, fixed = TRUE),
    # cell = function(value) format(value, nsmall = 1),
    align = "center",
    minWidth = 70,
    headerStyle = list(background = "#99AABF")
  ),
  columns = list(
    Species = colDef(minWidth = 140)  # overrides the default
  ),
  bordered = TRUE,
  highlight = TRUE
)





reactable(
  iris[1:30, ],
  searchable = TRUE,
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  theme = reactableTheme(
    # borderColor = "#dfe2e5",
    stripedColor = "#eff2f5",
    highlightColor = "#f9b99f",
    cellPadding = "18px 22px",
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
    ),
    searchInputStyle = list(width = "100%")
  )
)



library(MASS)
library(dplyr)
library(reactable)

test <- read.csv("D:/GitHub_2023/online-advice/App/Data/SID_2023/SID.csv")
test$Component[test$AssessmentKey == 18396] <- "All"
test <- test %>%
  dplyr::select("StockKeyLabel", "EcoRegion", "SpeciesCommonName", "icon", "Component", "stock_location") %>%
  dplyr::filter(., EcoRegion == "Greater North Sea ")
  

# Mock data, 3 levels of grouping, 2 numerical non-grouping Variables per row
group_vars <- c("StockKeyLabel")
# data <- test[, c(group_vars,"EcoRegion", "SpeciesCommonName", "icon", "Component", "stock_location")]
data <- test
# custom `rowClass` function -
# Rows without subrows get class `block-expandable` which we later use to
# hide the expander-button (the little triangle) and disable the event listener
# Expandable rows get the class `allow-expandable` instead. We usi it for
# styling.
row_class_fun <- JS("function(rowInfo) { return( rowInfo.subRows.length <= 1 ? 'block-expandable' : 'allow-expandable') }")

# Custom `aggregate` funtion for group columns -
# For groups with only one row, show that row's value in the parent row
aggregate_group_col <- JS("function(x) { return(x.length == 1 ? x : '') }")

# Optional: Custom `grouped` function for group columns -
# Suppress the `(n)` after the group name.
# Without it, it will not be uniform across the expandable and not-expandable
# rows and across group cols, i.e. some will have `(n)` and  others wont,
# so you probbly want some version of this.
grouped_group_col <- JS("function(cellInfo) { return cellInfo.value }")

# Completely optional: Trigger group expansion regardless of which column the
# user clicks (...or filter "click-to-expand"-columns via`column`)
on_click_fun <- JS("function(rowInfo, column) { if (rowInfo.subRows.length > 1) rowInfo.toggleRowExpanded() }")

# ColDef for the 3 group cols -
# use custom group and aggregate functions, add class for styling.
group_col_def <- colDef(
  grouped   = grouped_group_col,
  aggregate = aggregate_group_col,
  class     = "group-col"
)

# The table
rt <- reactable(
  data,
  highlight = TRUE,
  groupBy   = group_vars,
  rowClass  = row_class_fun,
  onClick   = on_click_fun,
  columns   = list(
    StockKeyLabel = group_col_def,
    # Type         = group_col_def,
    # DriveTrain   = group_col_def,
    # per-row variables.
    # Don't forget to define aggregate functions for value when grouped
    # Price        = colDef("Max. Price", aggregate = "max"),
    # MPG.city     = colDef("Avg. MPG", aggregate = "mean", format = colFormat(digits = 1))
    
    EcoRegion = colDef("EcoRegion", aggregate = "unique"),
    SpeciesCommonName = colDef("SpeciesCommonName", aggregate = "unique"),
    icon = colDef("icon", aggregate = "unique"),
    Component = colDef("Component", aggregate = "unique"),
    stock_location = colDef("stock_location", aggregate = "unique" )
  ),
)

# Custom CSS:
# A lot of this is pure styling.
# - hide the expand button in non-expandable rows (this is the only non-optional change)
# - change the cursor on non-expandable rows from pointer to 'regular'
# - color the expandable rows blue and make text bold
# - color the expander Button red
# - add some left padding to the table header and group cols to align all text,
#   regardless of whether there is an expander-button
custom_css <- "
  .rt-tr.block-expandable button.rt-expander-button {
    display: none;
    pointer-events: none;
  }

  .rt-tr.block-expandable .rt-td-expandable {
    cursor: auto;
  }

  .rt-tr.allow-expandable .rt-td-inner {
      color: steelblue;
      font-weight: bold;
  }

  .rt-td-expandable .rt-expander::after {
      border-top-color: tomato;
  }

  .rt-th-inner .rt-text-content {
    padding-left: 20px;
  }

  .rt-tr.block-expandable .group-col {
      padding-left: 20px
  }
"
# Custom JS:
# click events on the table are first registered by the `rt-td-inner` elements
# We add an event listener that intercepts these events and checks if any
# ancestor element (this would be the row it is in) has the class 'block-expandable'.
# (<https://developer.mozilla.org/en-US/docs/Web/API/Element/closest>)
# If so, we stop propagation of the click event.
# One could also check just the parent and grandparent element, as this should
# cover all "candidates", i.e.
# ```
# e.target.parentElement.classList.contains('block-expandable')  ||
# e.target.parentElement.parentElement.classList.contains('block-expandable')
# ````
# (also note the ID `my-table` that is used to select the wrapping DOM element.)
custom_js <- "
  document.addEventListener('DOMContentLoaded', function() {
    let table = document.getElementById('my-table');

    table.addEventListener('click', function(e) {
        if ( e.target.closest('div.rt-tr.block-expandable') !== null) {
          e.stopImmediatePropagation()
        }
    },
    useCapture = true  // ensures that listener fires before the regular reactable listeners
  )
  });
"

# Put it all together in a "browsable()"
# In a Shiny app the custom CSS and JS code would be added to the head
# somewhere else.
htmltools::tagList(
  htmltools::tags$head(
    htmltools::tags$style(custom_css),
    htmltools::tags$script(custom_js)
  ),
  htmltools::tags$div(id = "my-table", rt)
) |>
htmltools::browsable()





##################
library(MASS)
library(dplyr)
library(reactable)

# Mock data, 3 levels of grouping, 2 numerical non-grouping Variables per row
group_vars <- c("Manufacturer", "Type", "DriveTrain")
data <- MASS::Cars93[, c(group_vars, "Price", "MPG.city")]

# custom `rowClass` function -
# Rows without subrows get class `block-expandable` which we later use to
# hide the expander-button (the little triangle) and disable the event listener
# Expandable rows get the class `allow-expandable` instead. We usi it for
# styling.
row_class_fun <- JS("function(rowInfo) { return( rowInfo.subRows.length <= 1 ? 'block-expandable' : 'allow-expandable') }")

# Custom `aggregate` funtion for group columns -
# For groups with only one row, show that row's value in the parent row
aggregate_group_col <- JS("function(x) { return(x.length == 1 ? x : '') }")

# Optional: Custom `grouped` function for group columns -
# Suppress the `(n)` after the group name.
# Without it, it will not be uniform across the expandable and not-expandable
# rows and across group cols, i.e. some will have `(n)` and  others wont,
# so you probbly want some version of this.
grouped_group_col <- JS("function(cellInfo) { return cellInfo.value }")

# Completely optional: Trigger group expansion regardless of which column the
# user clicks (...or filter "click-to-expand"-columns via`column`)
on_click_fun <- JS("function(rowInfo, column) { if (rowInfo.subRows.length > 1) rowInfo.toggleRowExpanded() }")

# ColDef for the 3 group cols -
# use custom group and aggregate functions, add class for styling.
group_col_def <- colDef(
  grouped   = grouped_group_col,
  aggregate = aggregate_group_col,
  class     = "group-col"
)

# The table
rt <- reactable(
  data,
  highlight = TRUE,
  groupBy   = group_vars,
  rowClass  = row_class_fun,
  onClick   = on_click_fun,
  columns   = list(
    Manufacturer = group_col_def,
    Type         = group_col_def,
    DriveTrain   = group_col_def,
    # per-row variables.
    # Don't forget to define aggregate functions for value when grouped
    Price        = colDef("Max. Price", aggregate = "max"),
    MPG.city     = colDef("Avg. MPG", aggregate = "mean", format = colFormat(digits = 1))
  ),
)

# Custom CSS:
# A lot of this is pure styling.
# - hide the expand button in non-expandable rows (this is the only non-optional change)
# - change the cursor on non-expandable rows from pointer to 'regular'
# - color the expandable rows blue and make text bold
# - color the expander Button red
# - add some left padding to the table header and group cols to align all text,
#   regardless of whether there is an expander-button
custom_css <- "
  .rt-tr.block-expandable button.rt-expander-button {
    display: none;
    pointer-events: none;
  }

  .rt-tr.block-expandable .rt-td-expandable {
    cursor: auto;
  }

  .rt-tr.allow-expandable .rt-td-inner {
      color: steelblue;
      font-weight: bold;
  }

  .rt-td-expandable .rt-expander::after {
      border-top-color: tomato;
  }

  .rt-th-inner .rt-text-content {
    padding-left: 20px;
  }

  .rt-tr.block-expandable .group-col {
      padding-left: 20px
  }
"
# Custom JS:
# click events on the table are first registered by the `rt-td-inner` elements
# We add an event listener that intercepts these events and checks if any
# ancestor element (this would be the row it is in) has the class 'block-expandable'.
# (<https://developer.mozilla.org/en-US/docs/Web/API/Element/closest>)
# If so, we stop propagation of the click event.
# One could also check just the parent and grandparent element, as this should
# cover all "candidates", i.e.
# ```
# e.target.parentElement.classList.contains('block-expandable')  ||
# e.target.parentElement.parentElement.classList.contains('block-expandable')
# ````
# (also note the ID `my-table` that is used to select the wrapping DOM element.)
custom_js <- "
  document.addEventListener('DOMContentLoaded', function() {
    let table = document.getElementById('my-table');

    table.addEventListener('click', function(e) {
        if ( e.target.closest('div.rt-tr.block-expandable') !== null) {
          e.stopImmediatePropagation()
        }
    },
    useCapture = true  // ensures that listener fires before the regular reactable listeners
  )
  });
"

# Put it all together in a "browsable()"
# In a Shiny app the custom CSS and JS code would be added to the head
# somewhere else.
htmltools::tagList(
  htmltools::tags$head(
    htmltools::tags$style(custom_css),
    htmltools::tags$script(custom_js)
  ),
  htmltools::tags$div(id = "my-table", rt)
) |>
htmltools::browsable()



install.packages("gfonts")
library(gfonts)
setwd("D:/GitHub_2023/online-advice/App")
setup_font(
  id = "gothic-a1",
  output_dir = "www",
  variants = "500"
)
 all_fonts <- get_all_fonts()
 all_fonts %>% filter(id == "gothic-a1")
names(all_fonts)
unique(all_fonts$id)

library(showtext)
font_add_google("Gothic A1")



access_sag_data_local <- function(stock_code, year) {
  
    out1 <-
    lapply(
      year,
      function(i) {        
          fread(sprintf("App/Data/SAG_%s/SAG_summary.csv", i))
      }
    )
    SAGsummary <- do.call(rbind, out1)
    
    out2 <-
    lapply(
      year,
      function(j) {        
          fread(sprintf("App/Data/SAG_%s/SAG_refpts.csv", j))
      }
    )
    SAGrefpts <- do.call(rbind, out2)

    data_sag <- merge(SAGsummary, SAGrefpts) %>% filter(FishStock == stock_code)

    data_sag <- data_sag %>% select(-FishStock) %>% filter(StockPublishNote == "Stock published")
    
    return(data_sag)
    
}
test <- access_sag_data_local("ane.27.9a", 2024)
names(test)
str(test$High_Recruitment)
list <- icesSAG::StockList(2024)




getwd()
setwd("D:/GitHub_2023/online-advice/App")
df <- access_sag_data_local("sbr.27.10", 2022)

ICES_plot_4 <- function(df, sagSettings) {

  sagSettings4 <- sagSettings %>% filter(SAGChartKey == 4)

df4 <- df %>%
  filter(Purpose == "Advice") %>%
  select(Year, Low_SSB, SSB, High_SSB, Blim, Bpa, MSYBtrigger, StockSizeDescription, StockSizeUnits, SAGStamp, ConfidenceIntervalDefinition) %>%
  mutate(segment = cumsum(is.na(SSB)))

p4 <- df4 %>%
    ggplot(., aes(x = Year, y = SSB))

if (any(!is.na(df4$Low_SSB))) {
  df_segments <- df4 %>%
    filter(!is.na(High_SSB) & !is.na(Low_SSB)) %>%
    group_by(segment) %>%
    mutate(start = first(Year), end = last(Year))

  p4 <- p4 +
    geom_ribbon(data =  df_segments, aes(
      ymin = Low_SSB,
      ymax = High_SSB,
      fill = ConfidenceIntervalDefinition,
      group = segment,
      text = map(
        paste0(
          "<b>Year: </b>", Year,
          "<br>",
          "<b>SSB: </b>", SSB,
          "<br>",
          "<b>High SSB: </b>", High_SSB,
          "<br>",
          "<b>Low SSB: </b>", Low_SSB
        ), HTML
      )
    ),
    linetype = "blank",
    size = 0
    )
}

p4 <- p4 +
    geom_line(data = df_segments, aes(
        x = Year,
        y = SSB,
        color = "SSB",
        group = segment,
        text = map(
            paste0(
                "<b>Year: </b>", Year,
                "<br>",
                "<b>SSB: </b>", SSB
            ), HTML
        )
    ))

if (any(!is.na(df4$MSYBtrigger))) {
    p4 <- p4 +
        geom_line(aes(
            x = Year,
            y = MSYBtrigger,
            linetype = "MSY B<sub>trigger</sub>",
            colour = "MSY B<sub>trigger</sub>",
            size = "MSY B<sub>trigger</sub>",
            text = map(
                paste0(
                    "<b>MSY B<sub>trigger</sub>: </b>", tail(MSYBtrigger, 1)
                ), HTML
            )
        ))
}

if (any(!is.na(df4$Blim))) {
    p4 <- p4 +
        geom_line(aes(
            x = Year,
            y = Blim,
            linetype = "B<sub>Lim</sub>",
            colour = "B<sub>Lim</sub>",
            size = "B<sub>Lim</sub>",
            text = map(
                paste0(
                    "<b>B<sub>Lim</sub>: </b>", tail(Blim, 1)
                ), HTML
            )
        ))
}

if (any(!is.na(df4$Bpa))) {
    p4 <- p4 +
        geom_line(aes(
            x = Year,
            y = Bpa,
            linetype = "B<sub>pa</sub>",
            colour = "B<sub>pa</sub>",
            size = "B<sub>pa</sub>",
            text = map(
                paste0(
                    "<b>B<sub>pa</sub>: </b>", tail(Bpa, 1)
                ), HTML
            )
        ))
}

diamondYears <-
    sagSettings4 %>%
    filter(settingKey == 14) %>%
    pull(settingValue) %>%
    str_split(pattern = ",", simplify = TRUE) %>%
    as.numeric()

if (any(!is.na(diamondYears))) {
        p4 <- p4 + geom_point( 
                            data = df4 %>% filter(Year %in% diamondYears), 
                            aes(x = Year, 
                            y = SSB,
                            text = map(
                                    paste0(
                                        "<b>Year: </b>", Year,
                                        "<br>",
                                        "<b>Forecast spawning-stock biomass (SSB): </b>", SSB
                                    ), HTML
                                )), 
                            shape = 23, 
                            fill = "#cfcfcf", 
                            color = "#3aa6ff", 
                            size = 2.5,                            
                            show.legend = FALSE, 
                            inherit.aes = FALSE)
    }


# add average lines
averageYears <-
    sagSettings4 %>%
    filter(settingKey == 46) %>%
    pull(settingValue) %>%
    str_split(",", simplify = TRUE) %>%
    as.numeric()

if (length(averageYears)) {
    id1 <- nrow(df4) - 1:averageYears[1] + 1
    id2 <- nrow(df4) - 1:averageYears[2] - averageYears[1] + 1
    avedf1 <- data.frame(
        Year = range(df4$Year[id1]) + c(-0.5, 0.5),
        SSB = mean(df4$SSB[id1], na.rm = TRUE)
    )
    avedf2 <- data.frame(
        Year = range(df4$Year[id2]) + c(-0.5, 0.5),
        SSB = mean(df4$SSB[id2], na.rm = TRUE)
    )

    p4 <-
        p4 + geom_line(data = avedf1,
                        aes(x = Year,
                            y = SSB,
                            linetype = "Average",
                            colour = "Average",
                            size = "Average",
                            text = map(
                                paste0(
                                    "<b>Average: </b>", SSB
                                ), HTML
            ))) + 
            geom_line(data = avedf2,
                        aes(x = Year,
                            y = SSB,
                            linetype = "Average",
                            colour = "Average",
                            size = "Average",
                            text = map(
                                paste0(
                                    "<b>Average: </b>", SSB
                                ), HTML
            )))
}

min_year <- min(df4$Year[which(!is.na(df4$SSB))])

nullifempty <- function(x) if (length(x) == 0) NULL else x

  p4 <-
    p4 + 
    # xlim(min_year, max(df4$Year+1)) +
    theme_ICES_plots(
      type = "SSB", df,
      title = sagSettings4 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
      ylegend = sagSettings4 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty(),
      ymax = sagSettings4 %>%
        filter(settingKey == 6) %>%
        pull(settingValue) %>%
        as.numeric() %>%
        nullifempty()
    )


#converting
fig4 <- ggplotly(p4, tooltip = "text") %>%
    layout(
        autosize = T,
        legend = list(
            itemsizing = "trace",
            orientation = "h",
            y = -.3,
            yanchor = "bottom",
            x = 0.5,
            xanchor = "center",
            itemwidth = 20,
            itemsizing= "trace",
            title = list(text = "")
        ),
        xaxis = list(zeroline = TRUE),
        annotations = list(
            showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
    )  #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))

for (i in 1:length(fig4$x$data)){
    if (!is.null(fig4$x$data[[i]]$name)){
        fig4$x$data[[i]]$name =  gsub("\\(","",str_split(fig4$x$data[[i]]$name,",")[[1]][1])
    }
}

fig4
}




library(ggplot2)
library(plotly)
library(dplyr)

# Example data with NA values
set.seed(123)
df <- data.frame(
  time = 1:100,
  high_value = sin(1:100 / 10) + rnorm(100, 0, 0.1) + 0.2,
  low_value = sin(1:100 / 10) + rnorm(100, 0, 0.1) - 0.2
)
df$high_value[c(20, 21, 22, 23, 50, 51, 52)] <- NA
df$low_value[c(20, 21, 22, 23, 50, 51, 52)] <- NA

# Identify segments by marking NAs and creating a cumulative sum
df <- df %>%
  mutate(segment = cumsum(is.na(high_value)))

# Filter out rows with NAs and create a segment identifier
df_segments <- df %>%
  filter(!is.na(high_value) & !is.na(low_value)) %>%
  group_by(segment) %>%
  mutate(start = first(time), end = last(time))

# Plot using ggplot2 and ggplotly
p <- ggplot() +
  geom_ribbon(data = df_segments, aes(x = time, ymin = low_value, ymax = high_value, group = segment), fill = "blue", alpha = 0.2) +
  geom_line(data = df_segments, aes(x = time, y = (high_value + low_value) / 2, group = segment))

ggplotly(p)




=======
df <- read.table("D:/GitHub_2023/online-advice/App/Data/SAG_2023/SAG_refpts.csv", header = TRUE, sep = ",")

names(df)
uniqueCustom1 <- unique(df$CustomRefPointName1)
uniqueCustom2 <- unique(df$CustomRefPointName2)
uniqueCustom3 <- unique(df$CustomRefPointName3)
uniqueCustom4 <- unique(df$CustomRefPointName4)
uniqueCustom5 <- unique(df$CustomRefPointName5)

customRefPoints <- c(uniqueCustom1, uniqueCustom2, uniqueCustom3, uniqueCustom4, uniqueCustom5)
standardRefPoints <- names(df)[!names(df) %in% c("CustomRefPointName1", 
"CustomRefPointName2", 
"CustomRefPointName3", 
"CustomRefPointName4", 
"CustomRefPointName5",
"CustomRefPointValue1",
"CustomRefPointValue2",
"CustomRefPointValue3",
"CustomRefPointValue4",
"CustomRefPointValue5",
"AssessmentKey",
"StockKeyLabel",
"StockDatabaseID",
"StockKey",
"AssessmentYear")]

totrefpoints <- c(standardRefPoints, customRefPoints)
write.csv(totrefpoints, "D:/GitHub_2023/online-advice/App/Data/SAG_2023/RefPoints.csv")




# Install and load necessary packages
install.packages("stringdist")

library(stringdist)
library(dplyr)

# Function to standardize similar strings to a target string
standardize_similar_strings <- function(strings, target_string, threshold = 0.2) {
  # Calculate string distances
  distances <- stringdist::stringdistmatrix(strings, target_string, method = "jw")
  
  # Create a logical vector to find strings within the threshold
  similar_strings <- distances <= threshold
  
  # Replace similar strings with the target string
  strings[similar_strings] <- target_string
  
  return(strings)
}

# Example array of strings
example_strings <- c("I_{trigger}", "I (trigger)", "Itrigger", "I _ trigger", "I_(_trigger_)", "anotherString")

# Define the target string
target_string <- "Itrigger"

# Apply the function to standardize similar strings
standardized_strings <- standardize_similar_strings(example_strings, target_string)

print(standardized_strings)

# Install and load necessary packages
install.packages("stringdist")
library(stringdist)

# Function to standardize similar strings to a target string
standardize_similar_strings <- function(strings, target_string, threshold = 0.1) {
  # Calculate string distances
  distances <- stringdist::stringdistmatrix(strings, target_string, method = "jw")
  
  # Create a logical vector to find strings within the threshold
  similar_strings <- distances <= threshold
  
  # Replace similar strings with the target string
  strings[similar_strings] <- target_string
  
  return(strings)
}

# Example array of strings
example_strings <- c("I_{trigger}", "I (trigger)", "Itrigger", "I _ trigger", "I_(_trigger_)", "anotherString")

# Define the target string
target_string <- "Itrigger"

# Apply the function to standardize similar strings
example_strings <- standardize_similar_strings(example_strings, target_string)

print(example_strings)

sort(totrefpoints)
target_string <- "FCap"
totrefpoints <- standardize_similar_strings(totrefpoints, target_string)
target_string <- "Itrigger"
totrefpoints <- standardize_similar_strings(totrefpoints, target_string)
target_string <- "FMSY proxy"
totrefpoints <- standardize_similar_strings(totrefpoints, target_string)

standardiseRefPoints <- function(totrefpoints) {
  if (any(totrefpoints %in% c(
    "FCap",
    "F_{cap}",
    "Fcap"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "FCap",
      "F_{cap}",
      "Fcap"
    )] <- "FCap"
  }
  if (any(totrefpoints %in% c(
    "F_(MSY proxy)",
    "FMSY proxy",
    "Fmsy proxy",
    "F_{MSY proxy}",
    "F_{MSYproxy}",
    "F_(MSY proxy)"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_(MSY proxy)",
      "FMSY proxy",
      "Fmsy proxy",
      "F_{MSY proxy}",
      "F_{MSYproxy}",
      "F_(MSY proxy)"
    )] <- "FMSY proxy"
  }

  if (any(totrefpoints %in% c(
    "I_{trigger}",
    "I (trigger)",
    "I_{trigger}",
    "Itrigger"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "I_{trigger}",
      "I (trigger)",
      "I_{trigger}",
      "Itrigger"
    )] <- "I<sub>trigger</sub>"
  }

  if (any(totrefpoints %in% c(
    "F_{eco}",
    "Feco"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{eco}",
      "Feco"
    )] <- "FEco"
  }

  if (any(totrefpoints %in% c(
    "F_{lim}",
    "FLim"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{lim}",
      "FLim"
    )] <- "FLim"
  }

  if (any(totrefpoints %in% c(
    "I_{loss}",
    "Iloss"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "I_{loss}",
      "Iloss"
    )] <- "Iloss"
  }

  if (any(totrefpoints %in% c(
    "F_{msy}",
    "FMSY",
    "Fmsy"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{msy}",
      "FMSY",
      "Fmsy"
    )] <- "FMSY"
  }

  if (any(totrefpoints %in% c(
    "F_{pa}",
    "Fpa",
    "FPa"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{pa}",
      "Fpa",
      "FPa"
    )] <- "Fpa"
  }

  if (any(totrefpoints %in% c(
    "HR_{mgt}",
    "HR_{mgt}",
    "HR (mgt)",
    "HRMGT"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HR_{mgt}",
      "HR_{mgt}",
      "HR (mgt)",
      "HRMGT"
    )] <- "HR MGT"
  }

  if (any(totrefpoints %in% c(
    "HR_{msy}",
    "HR_{MSY}",
    "HR_{MSY}"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HR_{msy}",
      "HR_{MSY}",
      "HR_{MSY}"
    )] <- "HR MSY"
  }

  if (any(totrefpoints %in% c(
    "HRmsy proxy",
    "HRMSY proxy",
    "HR_{MSY proxy}",
    "HR_{MSY proxy} (W)",
    "HR_{MSY proxy} (S)"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HRmsy proxy",
      "HRMSY proxy",
      "HR_{MSY proxy}",
      "HR_{MSY proxy} (W)",
      "HR_{MSY proxy} (S)"
    )] <- "HR MSY proxy"
  }

  if (any(totrefpoints %in% c(
    "MSY Btrigger",
    "MSYBtrigger"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "MSY Btrigger",
      "MSYBtrigger"
    )] <- "MSYBtrigger"
  }

  if (any(totrefpoints %in% c(
    "MGT B_{trigger}",
    "MGT B {trigger}",
    "MGT B (trigger)"
  ))) {
    totrefpoints[totrefpoints %in% c(
    "MGT B_{trigger}",
    "MGT B {trigger}",
    "MGT B (trigger)"
    )] <- "MGT Btrigger"
  }

  if (any(totrefpoints %in% c(
    "HR_{pa}",
    "HRpa",
    "HR {pa}"
  ))) {
    totrefpoints[totrefpoints %in% c(
     "HR_{pa}",
    "HRpa",
    "HR {pa}"
    )] <- "HRpa"
  }
  return(totrefpoints)
}


df <- read.table("D:/GitHub_2023/online-advice/App/Data/SAG_2023/SAG_refpts.csv", header = TRUE, sep = ",")






df <- df %>% mutate(across(c(
  CustomRefPointName1,
  CustomRefPointName2,
  CustomRefPointName3,
  CustomRefPointName4,
  CustomRefPointName5
), standardiseRefPoints))

uniqueCustom1 <- unique(df$CustomRefPointName1)
uniqueCustom2 <- unique(df$CustomRefPointName2)
uniqueCustom3 <- unique(df$CustomRefPointName3)
uniqueCustom4 <- unique(df$CustomRefPointName4)
uniqueCustom5 <- unique(df$CustomRefPointName5)

x <- icesSAG::StockList(2024)
t <-SummaryTable(18958)


summary<-read.table("D:/GitHub_2023/online-advice/App/Data/SAG_2024/SAG_summary.csv", header = TRUE, sep = ",")
library(stringr)
filtered_summary <- summary %>% filter(str_starts(LinkToAdvice, "NA"))

# Extract unique StockKeyLabel values
unique_stock_key_labels <- unique(filtered_summary$FishStock)

# Print the unique values
print(unique_stock_key_labels)

test1 <- icesSAG::StockDownload(assessmentKey = 18715)
names(test1)
test2 <- icesSAG::SummaryTable(18715)
names(test2)
test3 <- icesSAG::FishStockReferencePoints(18715)
names(test3)
# check the difference between headers names in test1 and test2
setdiff(names(test1), names(test2))
# do the opposite
setdiff(names(test2), names(test1))
# check the difference between headers names in test1 and test3
setdiff(names(test3), names(test1))


library(shiny)
library(plotly)
library(dplyr)

# Sample data
df <- data.frame(
    Year = rep(2000:2020, 3),
    Catches = rnorm(63, 100, 20),
    Scenario = rep(c("Scenario1", "Scenario2", "Scenario3"), each = 21),
    Color = rep(c("#1f77b4", "#ff7f0e", "#2ca02c"), each = 21),
    MarkerSize = rep(10, 63)
)

# Define the main Plotly plotting function
TAC_timeline <- function(final_df, catch_scenarios) {
    catch_time <- final_df %>%
        filter(Scenario %in% catch_scenarios) %>%
        plot_ly(
            x = ~Year,
            y = ~Catches,
            type = "scatter",
            mode = "lines+markers",
            color = ~Scenario,
            marker = list(size = ~MarkerSize)
        ) %>%
        layout(
            title = "Catch Scenarios Over Time",
            xaxis = list(title = "Year"),
            yaxis = list(title = "Catches")
        )
    
    return(catch_time)
}

# Define UI
ui <- fluidPage(
    titlePanel("Dynamic Scenario Plot"),
    
    sidebarLayout(
        sidebarPanel(
            # UI for selecting scenarios
            checkboxGroupInput("selected_scenarios", "Select Scenarios:",
                               choices = unique(df$Scenario),
                               selected = unique(df$Scenario))
        ),
        
        mainPanel(
            # Plotly output
            plotlyOutput("catch_plot")
        )
    )
)

# Define server
server <- function(input, output, session) {
    
    # Initial rendering of the plot
    output$catch_plot <- renderPlotly({
        TAC_timeline(df, input$selected_scenarios)
    })
    
    # Update plot using plotlyProxy when scenarios change
    observe({
        plotlyProxy("catch_plot", session) %>%
            plotlyProxyInvoke("restyle", list(
                x = list(df %>% filter(Scenario %in% input$selected_scenarios) %>% pull(Year)),
                y = list(df %>% filter(Scenario %in% input$selected_scenarios) %>% pull(Catches)),
                color = list(df %>% filter(Scenario %in% input$selected_scenarios) %>% pull(Scenario))
            ))
    })
}

# Run the app
shinyApp(ui = ui, server = server)



library(shiny)
library(plotly)
library(dplyr)

# Sample data with Color column
df <- data.frame(
    Year = rep(2000:2020, 5),
    Catches = rnorm(105, 100, 20),
    Scenario = rep(c("Historical Catches", "Previous Advice", "Scenario1", "Scenario2", "Scenario3"), each = 21),
    MarkerSize = rep(10, 105)
)

# Define colors for each scenario, including specific colors for Historical and Previous Advice
scenario_colors <- c(
    "Historical Catches" = "#000000",  # Black
    "Previous Advice" = "#808080",     # Gray
    "Scenario1" = "#1f77b4",           # Blue
    "Scenario2" = "#ff7f0e",           # Orange
    "Scenario3" = "#2ca02c"            # Green
)

# Add the Color column to df
df$Color <- scenario_colors[df$Scenario]

# Define the main Plotly plotting function
TAC_timeline <- function(final_df, catch_scenarios) {
    catch_time <- final_df %>%
        filter(Scenario %in% catch_scenarios) %>%
        plot_ly(
            x = ~Year,
            y = ~Catches,
            type = "scatter",
            mode = "lines+markers",
            color = ~Scenario,
            colors = scenario_colors,  # Apply fixed colors
            marker = list(size = ~MarkerSize)
        ) %>%
        layout(
            title = "Catch Scenarios Over Time",
            xaxis = list(title = "Year"),
            yaxis = list(title = "Catches")
        )
    
    return(catch_time)
}

# Define UI
ui <- fluidPage(
    titlePanel("Dynamic Scenario Plot"),
    
    sidebarLayout(
        sidebarPanel(
            # UI for selecting scenarios
            checkboxGroupInput("selected_scenarios", "Select Scenarios:",
                               choices = unique(df$Scenario),
                               selected = unique(df$Scenario))
        ),
        
        mainPanel(
            # Plotly output
            plotlyOutput("catch_plot")
        )
    )
)

# Define server
server <- function(input, output, session) {
    
    # Initial rendering of the plot
    output$catch_plot <- renderPlotly({
        TAC_timeline(df, input$selected_scenarios)
    })
    
    # Update plot using plotlyProxy when scenarios change
    observe({
        # Filter data based on selected scenarios
        filtered_data <- df %>% filter(Scenario %in% input$selected_scenarios)
        
        # Prepare lists for the `restyle` method, specifying color for each scenario
        x_values <- split(filtered_data$Year, filtered_data$Scenario)
        y_values <- split(filtered_data$Catches, filtered_data$Scenario)
        colors <- lapply(names(x_values), function(scenario) scenario_colors[scenario])
        
        # Use plotlyProxy to update the plot
        plotlyProxy("catch_plot", session) %>%
            plotlyProxyInvoke("restyle", list(
                x = x_values,
                y = y_values,
                "line.color" = colors  # Fix color for each scenario
            ))
    })
}

# Run the app
shinyApp(ui = ui, server = server)



library(shiny)
library(plotly)
library(dplyr)

# Sample data with Color column, where Scenarios 1, 2, and 3 have the same Year (2021)
df <- data.frame(
    Year = c(rep(2000:2020, 2), rep(2021, 3 * 21)),
    Catches = c(rnorm(42, 100, 20), rnorm(63, 50, 10)),
    Scenario = c(rep("Historical Catches", 21), rep("Previous Advice", 21), rep(c("Scenario1", "Scenario2", "Scenario3"), each = 21)),
    MarkerSize = rep(10, 105)
)

# Define colors for each scenario, including specific colors for Historical and Previous Advice
scenario_colors <- c(
    "Historical Catches" = "#000000",  # Black
    "Previous Advice" = "#808080",     # Gray
    "Scenario1" = "#1f77b4",           # Blue
    "Scenario2" = "#ff7f0e",           # Orange
    "Scenario3" = "#2ca02c"            # Green
)

# Add the Color column to df
df$Color <- scenario_colors[df$Scenario]

# Define the main Plotly plotting function
TAC_timeline <- function(final_df, catch_scenarios) {
    # Separate historical catches
    historical_df <- final_df %>% filter(Scenario == "Historical Catches")
    other_df <- final_df %>% filter(Scenario != "Historical Catches" & Scenario %in% catch_scenarios)
    
    # Create plot for historical catches (lines only)
    catch_time <- plot_ly(
        historical_df,
        x = ~Year,
        y = ~Catches,
        type = "scatter",
        mode = "lines",
        line = list(color = scenario_colors["Historical Catches"]),
        name = "Historical Catches"
    )
    
    # Add traces for other scenarios (lines + markers)
    for (scenario in unique(other_df$Scenario)) {
        scenario_data <- other_df %>% filter(Scenario == scenario)
        catch_time <- catch_time %>%
            add_trace(
                data = scenario_data,
                x = ~Year,
                y = ~Catches,
                type = "scatter",
                mode = "lines+markers",
                line = list(color = scenario_colors[scenario]),
                marker = list(size = ~MarkerSize),
                name = scenario  # Set legend name for each scenario
            )
    }
    
    catch_time <- catch_time %>%
        layout(
            title = "Catch Scenarios Over Time",
            xaxis = list(title = "Year"),
            yaxis = list(title = "Catches")
        )
    
    return(catch_time)
}

# Define UI
ui <- fluidPage(
    titlePanel("Dynamic Scenario Plot"),
    
    sidebarLayout(
        sidebarPanel(
            # UI for selecting scenarios
            checkboxGroupInput("selected_scenarios", "Select Scenarios:",
                               choices = unique(df$Scenario),
                               selected = unique(df$Scenario))
        ),
        
        mainPanel(
            # Plotly output
            plotlyOutput("catch_plot")
        )
    )
)

# Define server
server <- function(input, output, session) {
    
    # Initial rendering of the plot
    output$catch_plot <- renderPlotly({
        TAC_timeline(df, input$selected_scenarios)
    })
    
    # Update plot using plotlyProxy when scenarios change
    observe({
        # Filter data based on selected scenarios
        historical_data <- df %>% filter(Scenario == "Historical Catches")
        other_data <- df %>% filter(Scenario != "Historical Catches" & Scenario %in% input$selected_scenarios)
        
        # Prepare lists for the `restyle` method, specifying color and name for each scenario
        x_values <- split(other_data$Year, other_data$Scenario)
        y_values <- split(other_data$Catches, other_data$Scenario)
        colors <- split(other_data$Color, other_data$Scenario) %>% lapply(unique)
        names <- names(x_values)  # Scenario names
        
        # Use plotlyProxy to update the plot
        plotlyProxy("catch_plot", session) %>%
            plotlyProxyInvoke("deleteTraces", list(1)) %>%  # Removes previous "other" traces
            plotlyProxyInvoke("addTraces", lapply(seq_along(names), function(i) {
                list(
                    x = x_values[[i]],
                    y = y_values[[i]],
                    type = "scatter",
                    mode = "lines+markers",
                    line = list(color = colors[[i]]),
                    marker = list(size = 10),
                    name = names[i]  # Unique name for each scenario in legend
                )
            }))
    })
}

# Run the app
shinyApp(ui = ui, server = server)



library(shiny)
library(plotly)
library(dplyr)

# Sample data with Color column, where Scenarios 1, 2, and 3 have the same Year (2021)
df <- data.frame(
    Year = c(rep(2000:2020, 2), rep(2021, 3 * 21)),
    Catches = c(rnorm(42, 100, 20), rnorm(63, 50, 10)),
    Scenario = c(rep("Historical Catches", 21), rep("Previous Advice", 21), rep(c("Scenario1", "Scenario2", "Scenario3"), each = 21)),
    MarkerSize = rep(10, 105)
)

# Define colors for each scenario, including specific colors for Historical and Previous Advice
scenario_colors <- c(
    "Historical Catches" = "#000000",  # Black
    "Previous Advice" = "#808080",     # Gray
    "Scenario1" = "#1f77b4",           # Blue
    "Scenario2" = "#ff7f0e",           # Orange
    "Scenario3" = "#2ca02c"            # Green
)

# Add the Color column to df
df$Color <- scenario_colors[df$Scenario]

# Define the main Plotly plotting function
TAC_timeline <- function(final_df, catch_scenarios) {
    # Separate historical catches
    historical_df <- final_df %>% filter(Scenario == "Historical Catches")
    other_df <- final_df %>% filter(Scenario != "Historical Catches" & Scenario %in% catch_scenarios)
    
    # Create plot for historical catches (lines only)
    catch_time <- plot_ly(
        historical_df,
        x = ~Year,
        y = ~Catches,
        type = "scatter",
        mode = "lines",
        line = list(color = scenario_colors["Historical Catches"]),
        name = "Historical Catches"
    )
    
    # Add traces for other scenarios (lines + markers, with diamond shape for specific scenarios)
    for (scenario in unique(other_df$Scenario)) {
        scenario_data <- other_df %>% filter(Scenario == scenario)
        marker_symbol <- ifelse(scenario %in% c("Scenario1", "Scenario2", "Scenario3"), "diamond", "circle")
        
        catch_time <- catch_time %>%
            add_trace(
                data = scenario_data,
                x = ~Year,
                y = ~Catches,
                type = "scatter",
                mode = "lines+markers",
                line = list(color = scenario_colors[scenario]),
                marker = list(size = ~MarkerSize, symbol = marker_symbol),
                name = scenario  # Set legend name for each scenario
            )
    }
    
    catch_time <- catch_time %>%
        layout(
            title = "Catch Scenarios Over Time",
            xaxis = list(title = "Year"),
            yaxis = list(title = "Catches")
        )
    
    return(catch_time)
}

# Define UI
ui <- fluidPage(
    titlePanel("Dynamic Scenario Plot"),
    
    sidebarLayout(
        sidebarPanel(
            # UI for selecting scenarios
            checkboxGroupInput("selected_scenarios", "Select Scenarios:",
                               choices = unique(df$Scenario),
                               selected = unique(df$Scenario))
        ),
        
        mainPanel(
            # Plotly output
            plotlyOutput("catch_plot")
        )
    )
)

# Define server
server <- function(input, output, session) {
    
    # Initial rendering of the plot
    output$catch_plot <- renderPlotly({
        TAC_timeline(df, input$selected_scenarios)
    })
    
    # Update plot using plotlyProxy when scenarios change
    observe({
        # Filter data based on selected scenarios
        historical_data <- df %>% filter(Scenario == "Historical Catches")
        other_data <- df %>% filter(Scenario != "Historical Catches" & Scenario %in% input$selected_scenarios)
        
        # Prepare lists for the `restyle` method, specifying color, name, and marker shape for each scenario
        x_values <- split(other_data$Year, other_data$Scenario)
        y_values <- split(other_data$Catches, other_data$Scenario)
        colors <- split(other_data$Color, other_data$Scenario) %>% lapply(unique)
        names <- names(x_values)  # Scenario names
        marker_symbols <- ifelse(names %in% c("Scenario1", "Scenario2", "Scenario3"), "diamond-open", "circle")
        
        # Use plotlyProxy to update the plot
        plotlyProxy("catch_plot", session) %>%
            plotlyProxyInvoke("deleteTraces", list(1)) %>%  # Removes previous "other" traces
            plotlyProxyInvoke("addTraces", lapply(seq_along(names), function(i) {
                list(
                    x = x_values[[i]],
                    y = y_values[[i]],
                    type = "scatter",
                    mode = "lines+markers",
                    line = list(color = colors[[i]]),
                    marker = list(size = 10, symbol = marker_symbols[i]),
                    name = names[i]  # Unique name for each scenario in legend
                )
            }))
    })
}

# Run the app
shinyApp(ui = ui, server = server)


icesASD::get_catch_scenario_table(3288, 2023)



# Function to determine the division unit
get_division_unit <- function(value, relative = FALSE) {
  # If relative, return the average value
  if (relative) {
    return(mean(value, na.rm = TRUE)) 
  }
  
  # Calculate the number of significant digits
  abs_value <- abs(value)
  sig_digits <- ifelse(abs_value > 0, ceiling(log10(abs_value)), 1)
  
  # Determine the division unit based on significant digits
  if (sig_digits < 4) {
    return(value/1)
  } else if (sig_digits >= 4 & sig_digits <= 6) {
    return(value/1000)
  } else if (sig_digits >= 7 & sig_digits <= 9) {
    return(value/1000000)
  } else if (sig_digits >= 9) {
    return(value/1000000000)
  } else {
    return(value)
  }
}

# Example usage
get_division_unit(c(123, 456), relative = TRUE)    # Returns average
get_division_unit(123)                            # Returns 1
get_division_unit(12345)                          # Returns 1000
get_division_unit(12345678)                       # Returns 1000000
get_division_unit(14000000000)

# Function to calculate axis unit label
get_axis_unit_label <- function(value, unit = "tonnes") {
  # Calculate number of significant digits
  abs_value <- abs(value)
  sig_digits <- ifelse(abs_value > 0, ceiling(log10(abs_value)), 1)
  
  # Determine label based on unit and significant digits
  if (unit == "tonnes") {
    if (sig_digits < 4) {
      return("tonnes")
    } else if (sig_digits >= 4 & sig_digits <= 6) {
      return("1000 t")
    } else if (sig_digits >= 7 & sig_digits <= 9) {
      return("million t")
    } else {
      return("billion t")
    }
  } else if (unit == "thousands") {
    if (sig_digits < 4) {
      return("thousands")
    } else if (sig_digits >= 4 & sig_digits <= 6) {
      return("millions")
    } else {
      return("billions")
    }
  } else {  # Any other unit (individuals, etc.)
    if (sig_digits < 4) {
      return("unit")
    } else if (sig_digits >= 4 & sig_digits <= 6) {
      return("thousands")
    } else if (sig_digits >= 7 & sig_digits <= 9) {
      return("millions")
    } else {
      return("billions")
    }
  }
}

# Example usage
get_axis_unit_label(123, unit = "tonnes")       # Returns "tonnes"
get_axis_unit_label(12345, unit = "tonnes")     # Returns "1000 t"
get_axis_unit_label(12345678, unit = "tonnes")  # Returns "million t"
get_axis_unit_label(1234567890, unit = "tonnes")# Returns "billion t"

get_axis_unit_label(123, unit = "thousands")    # Returns "thousands"
get_axis_unit_label(123456, unit = "thousands") # Returns "millions"
get_axis_unit_label(123456789, unit = "thousands") # Returns "billions"

get_axis_unit_label(123, unit = "individuals")  # Returns "unit"
get_axis_unit_label(123456, unit = "individuals") # Returns "thousands"
get_axis_unit_label(123456789, unit = "individuals") # Returns "millions"


sag <- read.table("D:/GitHub_2023/online-advice/App/Data/SAG_2024/SAG.csv", header = TRUE, sep = ",")

units <- group_by(
  sag,
  StockSizeUnits
  
) %>%
  summarise(
    count = n()
  ) %>%
  arrange(desc(count))
units

write.csv(units, "D:/GitHub_2023/online-advice/StockSizeUnits_2024.csv")
#2024
#   UnitOfRecruitment      count
#   <chr>                  <int>
# 1 ""                      4532
# 2 "thousands"             3476
# 3 "Relative Recruitment"   197
# 4 "Thousands"               54
#2023
#   UnitOfRecruitment                   count        
#   <chr>                               <int>        
# 1 ""                                   4232
# 2 "thousands"                          3357        
# 3 "Number of individuals (fisheries)"   142        
# 4 "Relative Recruitment"                115        
# 5 "tonnes"                              112
# 6 "Thousands"                            53 
#2022
#   UnitOfRecruitment                   count        
#   <chr>                               <int>        
# 1 ""                                   4027
# 2 "thousands"                          3308        
# 3 "Number of individuals (fisheries)"   211        
# 4 "Relative Recruitment"                118        
# 5 "tonnes"                              110  

#2021
#   UnitOfRecruitment                   count        
#   <chr>                               <int>        
# 1 ""                                   3847
# 2 "thousands"                          2892        
# 3 "Relative Recruitment"                401        
# 4 "Number of individuals (fisheries)"   182        
# 5 "tonnes"                              108        
# 6 "N"                                    86
# 7 "Numbers per hour (fisheries)"         35 

#2020
#   UnitOfRecruitment                              count       
#   <chr>                                          <int>       
# 1 ""                                              3823       
# 2 "thousands"                                     2767       
# 3 "Relative Recruitment"                           358       
# 4 "tonnes"                                         166       
# 5 "Number of individuals in thousands (x1000)"      71       
# 6 "Number of individuals in millions (x1000000)"    37       
# 7 "Number of individuals (fisheries)"               36       
# 8 "Numbers per hour (fisheries)"                    34 

#2019
#   UnitOfRecruitment              count
#   <chr>                          <int>
# 1 ""                              4166
# 2 "thousands"                     3069
# 3 "Relative Recruitment"           598
# 4 "Number of individuals"           70
# 5 "Numbers per hour (fisheries)"    33
# 6 "ratio"                           15

#2018
#   UnitOfRecruitment                            count
#   <chr>                                        <int>
# 1 "thousands"                                   4272
# 2 ""                                            1420
# 3  NA                                            371
# 4 "Relative Recruitment"                         177
# 5 "Number of individuals in thousands (x1000)"    62
# 6 "No/hour"                                       54
# 7 "N/hour"                                        45
# 8 "tonnes"                                        33
# 9 "kg/hour"                                       32



scaling_factor <- switch(df$CatchesLandingsUnits[1],
            "thousands" = 1000,
            "Thousands" = 1000,
            "empty" = 1,
            "Number of individuals (fisheries)" = 1,
            "tonnes" = 1,
            "Kilograms per hour" = 1,
            "kilogram per square kilometer" = 1,
            "UWTV abundance (billions)" = 1000000000,
            stop("Invalid RecruitmentUnit: choose 'thousands', or 'relative'")
        )

scaling_factor <- switch(df$UnitOfRecruitment[1],
        "thousands" = 1000,
        "Thousands" = 1000,
        "empty" = 1000,
        "Relative Recruitment" = 1,
        "Number of individuals (fisheries)" = 1,
        "tonnes" = 1,
        stop("Invalid RecruitmentUnit: choose 'thousands', or 'relative'")
        )

# Determine scaling factor based on StockSizeUnits
        scaling_factor <- switch(df$StockSizeUnits[1],
            "thousands" = 1000,
            "Thousands" = 1000,
            "empty" = 1,
            # "Relative Recruitment" = 1,
            "Number of individuals (fisheries)" = 1,
            "tonnes" = 1,
            "tonnes/h" = 1,
            "Kilograms per hour" = 1,
            "kilogram per hour" = 1,
            "kilogram per square kilometer" = 1,
            "kilogram per km2" = 1,
            "Kilograms per trip" = 1,
            "Kilograms per trap" = 1,
            "Kilograms per hook" = 1,
            "UWTV abundance (billions)" = 1000000000,
            "Number of individuals (billions)" = 1000000000,
            "ratio" = 1,
            stop("Invalid RecruitmentUnit: choose 'thousands', or 'relative'")
        )

get_scaling_factor <- function(unit_type, unit_value) {
  scaling_factor <- switch(unit_value,
                           "thousands" = 1000,
                           "Thousands" = 1000,
                           "empty" = ifelse(unit_type == "UnitOfRecruitment", 1000, 1),
                           "Relative Recruitment" = 1,
                           "Number of individuals (fisheries)" = 1,
                           "tonnes" = 1,
                           "tonnes/h" = 1,
                           "Kilograms per hour" = 1,
                           "kilogram per hour" = 1,
                           "kilogram per square kilometer" = 1,
                           "kilogram per km2" = 1,
                           "Kilograms per trip" = 1,
                           "Kilograms per trap" = 1,
                           "Kilograms per hook" = 1,
                           "UWTV abundance (billions)" = 1000000000,
                           "Number of individuals (billions)" = 1000000000,
                           "ratio" = 1,
                           stop("Invalid unit value: choose 'thousands', 'relative', or other valid units"))
  return(scaling_factor)
}





# Required Libraries
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

# UI
ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"), # Use a modern theme
  fillable_row(
    # Dropdown for selecting which plots to display
    fillable_column(
      width = 4,
      selectInput(
        inputId = "selected_plots",
        label = "Select Plots to Display:",
        choices = c("Plot 1", "Plot 2", "Plot 3"),
        selected = "Plot 1",
        multiple = TRUE
      )
    )
  ),
  fillable_row(
    # Placeholder for dynamic cards
    uiOutput("plotCards")
  )
)

# Server
server <- function(input, output, session) {
  # Define the data and plots dynamically
  plotData <- reactive({
    list(
      "Plot 1" = ggplot(mtcars, aes(mpg, wt)) + 
        geom_point(color = "blue") + 
        ggtitle("Scatter Plot: MPG vs Weight"),
      "Plot 2" = ggplot(mtcars, aes(gear, fill = factor(cyl))) + 
        geom_bar() + 
        ggtitle("Bar Plot: Gear by Cylinders"),
      "Plot 3" = ggplot(mtcars, aes(mpg)) + 
        geom_histogram(binwidth = 5, fill = "orange") + 
        ggtitle("Histogram of MPG")
    )
  })
  
  # Dynamically generate cards for selected plots
  output$plotCards <- renderUI({
    req(input$selected_plots) # Ensure plots are selected
    selectedPlots <- input$selected_plots
    
    # Generate a card for each selected plot
    lapply(selectedPlots, function(plotName) {
      card(
        full_screen = TRUE, # Allow card to expand full screen
        card_header(plotName),
        card_body(
          plotlyOutput(outputId = paste0("plot_", plotName))
        ),
        card_footer("Footer text (optional)")
      )
    })
  })
  
  # Render each plot dynamically based on selection
  observe({
    req(input$selected_plots)
    for (plotName in input$selected_plots) {
      local({
        currentPlot <- plotName
        output[[paste0("plot_", currentPlot)]] <- renderPlotly({
          ggplotly(plotData()[[currentPlot]]) # Convert ggplot to Plotly
        })
      })
    }
  })
}

# Run the App
shinyApp(ui, server)


# Required Libraries
# Required Libraries
# Required Libraries
# Required Libraries
library(shiny)
library(bslib)
library(ggplot2)

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"), # Use a clean Bootstrap theme
  fluidRow(
    column(
      width = 4, # First plot in a responsive column
      card(
        card_header("Plot 1: Scatter Plot"),
        card_body(
          plotOutput("plot1") # Output for static ggplot
        )
      )
    ),
    column(
      width = 4, # Second plot in a responsive column
      card(
        card_header("Plot 2: Bar Plot"),
        card_body(
          plotOutput("plot2") # Output for static ggplot
        )
      )
    ),
    column(
      width = 4, # Third plot in a responsive column
      card(
        card_header("Plot 3: Histogram"),
        card_body(
          plotOutput("plot3") # Output for static ggplot
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Render Plot 1
  output$plot1 <- renderPlot({
    ggplot(mtcars, aes(mpg, wt)) +
      geom_point(color = "blue") +
      ggtitle("Scatter Plot: MPG vs Weight")
  })
  
  # Render Plot 2
  output$plot2 <- renderPlot({
    ggplot(mtcars, aes(gear, fill = factor(cyl))) +
      geom_bar() +
      ggtitle("Bar Plot: Gear by Cylinders")
  })
  
  # Render Plot 3
  output$plot3 <- renderPlot({
    ggplot(mtcars, aes(mpg)) +
      geom_histogram(binwidth = 5, fill = "orange") +
      ggtitle("Histogram of MPG")
  })
}

# Run the App
shinyApp(ui, server)



# Required Libraries
library(shiny)
library(bslib)
library(ggplot2)

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"), # Use a modern Bootstrap theme
  fluidRow(
    column(
      width = 12,
      # Buttons to dynamically add or remove plots
      actionButton("add_plot", "Add Plot", class = "btn-primary"),
      actionButton("remove_plot", "Remove Plot", class = "btn-danger"),
      br(), br()
    )
  ),
  fluidRow(
    # Dynamic UI for the plots
    uiOutput("plot_grid")
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store the number of plots
  num_plots <- reactiveVal(0)
  
  # Observe the "Add Plot" button
  observeEvent(input$add_plot, {
    num_plots(num_plots() + 1) # Increment the number of plots
  })
  
  # Observe the "Remove Plot" button
  observeEvent(input$remove_plot, {
    num_plots(max(0, num_plots() - 1)) # Decrement the number of plots, but not below 0
  })
  
  # Generate dynamic UI for plots
  output$plot_grid <- renderUI({
    # Create a responsive grid of plots
    plot_outputs <- lapply(seq_len(num_plots()), function(i) {
      column(
        width = 4, # Each plot gets a column width of 4 (3 per row)
        card(
          card_header(paste("Plot", i)),
          card_body(plotOutput(outputId = paste0("plot_", i))) # Dynamic plot output
        )
      )
    })
    do.call(fluidRow, plot_outputs) # Wrap all plots in a fluidRow
  })
  
  # Dynamically render each plot
  observe({
    lapply(seq_len(num_plots()), function(i) {
      local({
        plot_index <- i
        output[[paste0("plot_", plot_index)]] <- renderPlot({
          # Generate different plots based on the index
          ggplot(mtcars, aes(mpg, wt)) +
            geom_point(color = scales::hue_pal()(num_plots())[plot_index]) +
            ggtitle(paste("Scatter Plot", plot_index))
        })
      })
    })
  })
}

# Run the App
shinyApp(ui, server)





library(shiny)

ui <- fluidPage(
  titlePanel("Dynamic Plot Layout"),
  fluidRow(
    uiOutput("dynamicPlots")  # UI output for dynamic rendering
  )
)

server <- function(input, output, session) {
  # Sample data: Some datasets might be NULL (representing missing plots)
  dataset_list <- list(
    data1 = mtcars,   # Available
    data2 = NULL,     # Not available
    data3 = iris,     # Available
    data4 = mtcars,   # Available
    data5 = iris      # Available
  )

  output$dynamicPlots <- renderUI({
    available_plots <- names(dataset_list)[sapply(dataset_list, function(x) !is.null(x))]

    if (length(available_plots) == 0) {
      return(h3("No data available for plotting."))
    }

    # Create dynamic rows with max 2 plots per row
    plot_list <- lapply(seq(1, length(available_plots), by = 2), function(i) {
      row_content <- list()
      row_content[[1]] <- column(6, plotOutput(paste0("plot_", available_plots[i])))

      if (i + 1 <= length(available_plots)) {  # Check if there is a second plot for this row
        row_content[[2]] <- column(6, plotOutput(paste0("plot_", available_plots[i + 1])))
      }

      do.call(fluidRow, row_content)  # Wrap each row in fluidRow
    })

    do.call(tagList, plot_list)  # Return all rows
  })

  # Dynamically render plots based on available data
  observe({
    for (name in names(dataset_list)) {
      local({
        dataset <- dataset_list[[name]]
        plot_id <- paste0("plot_", name)

        output[[plot_id]] <- renderPlot({
          if (!is.null(dataset)) {
            plot(dataset[, 1], dataset[, 2], main = paste("Plot for", name))
          }
        })
      })
    }
  })
}

shinyApp(ui, server)

test <- icesSAG::getStockDownloadData(18715)
test <- icesSAG::getSAG("had.27.46a20", 2024)


download_SID <- function(Year) {
  stock_list_all <- jsonlite::fromJSON(
    URLencode(
      sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel, StockKeyDescription, SpeciesCommonName, EcoRegion, DataCategory, YearOfLastAssessment, AssessmentFrequency, YearOfNextAssessment, AssessmentKey", Year)
    )
  )$value
  # stock_list_all <- stock_list_all %>% filter(!StockKeyLabel %in% adviceXplorer_stocks_to_exclude_for_lunch)
  return(stock_list_all)
}

test2 <- download_SID(2025)
head(test2)
# find how many NA in AssessmentKey
sum(is.na(test2$AssessmentKey)) # 0 NA
library(dplyr)
df <- test2 %>% filter(is.na(AssessmentKey))
devtools::install_github("ices-tools-prod/icesSAG")

install.packages("icesSAG")
library(icesSAG)
getSAGGraphs(19287)


icesASD::getAdviceViewRecord(year = 2025, stock = "ank.27.78abd")
icesASD::get_catch_scenario_table(3876)
icesSAG::getSAGGraphs(19277)
icesSAG::getListStocks(assessmentKey = 19277)


## install github package
remotes::install_github("ices-tools-prod/icesSAG")

icesSAG::getListStocks(assessmentKey = 18808)
sagList <- jsonlite::fromJSON(
    utils::URLencode(
      sprintf("https://sag.ices.dk/SAG_API/api/StockList?year=0&assessmentKey=%s", 18808)
    )
  )


icesSAG::getSAGSettingsForAStock(20911)
icesSAG