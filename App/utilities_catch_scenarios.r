#' Returns an HTML string containing the summary of the advice view record to be displayed on top of the Advice page
#'
#' @param catch_scenario_list
#' @param StockDescription
#'
#' @return HTML string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' StockDescription(df, StockDescription)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
#' 
get_Advice_View_Summary <- function(catch_scenario_list, StockDescription) {
  advice_requester <- catch_scenario_list$adviceRequester
  advice_requester <- gsub("~", ", ", advice_requester)

  catch_scenario_advice_sentence <- HTML(paste0(
    "<font size=", 3, ">", "Stock description: ", "<b>", StockDescription, "</b><br/>",
    "<font size=", 3, ">", "Stock code: ", "<b>", catch_scenario_list$stockCode, "</b><br/>",
    "<font size=", 3, ">", "Assessment year: ", "<b>", catch_scenario_list$assessmentYear, "</b><br/>"
  ))

  return(catch_scenario_advice_sentence)
} 

#' Returns an HTML string containing the headline advice and a link to the advice sheet
#'
#' @param catch_scenario_list

#'
#' @return HTML string with link
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_Advice_View_Headline(df)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
get_Advice_View_Headline <- function(catch_scenario_list, replaced_advice_doi, tabset_id, catch_scenario_table, drop_plots) {
  
  catch_scenario_advice_sentence <- HTML(
    paste0(
      "<span class='hovertext' data-hover='Click here to access the pdf version of the Advice'>",
      # "<a href='", advice_doi, "' target='_blank'>",
      "<a href='", catch_scenario_list$adviceDOI, "' target='_blank'>",
      # "<a href='", catch_scenario_list$adviceLink, "' target='_blank'>",
      "<b><i><font size=4> Headline advice </b></i><i class='fa-solid fa-up-right-from-square'></i></font></a></span>",
      "<br/>",
      "<font size=3>", catch_scenario_list$adviceSentence, "</font>",
      if (!is.na(replaced_advice_doi)) {
        paste0(
          "<br/>",
          "<span class='hovertext' data-hover='Link to the replaced Advice'>",
          "<a href='", replaced_advice_doi, "' target='_blank'>",
          "<font size=3> Replaced advice <i class='fa-solid fa-up-right-from-square'></i></font></a></span>"
        )
      },
      if (tabset_id == "Development over time") {
        paste0(
          "<br/>",
          "<span class='hovertext' data-hover='Standard graphs data download'>",
          downloadLink("download_SAG_Data", HTML("<font size= 3>Download assessment data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
        )
      } else if (tabset_id == "Quality of assessment" && all(!10 %in% drop_plots)) {
        paste0(
          "<br/>",
          "<span class='hovertext' data-hover='Quality of assessment data download'>",
          downloadLink("download_QualAss_Data", HTML("<font size= 3>Download quality of assessment data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
        )
      } else if (tabset_id == "Catch scenarios") {
        paste0(
          "<br/>",
          "<span class='hovertext' data-hover='Link to ASD entry'>",
          "<a href='", "http://asd.ices.dk/viewAdvice/", catch_scenario_list$adviceKey, "' target='_blank'>",
          "<font size= 3>View ASD entry <i class='fa-solid fa-up-right-from-square'></i></font></a></span>",
          if (!is_empty(catch_scenario_table)) {
            paste0(
              " or ",
              "<span class='hovertext' data-hover='Download table (.csv)'>",
              downloadLink("download_catch_table", HTML("<font size= 3>download catch scenario table <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
            )
          }
        )
      }
    )
  )

  return(catch_scenario_advice_sentence)
}



#' Returns an HTML string containing some basic info on the selected stock and year
#'
#' @param CommonName
#' @param stockcode
#' @param assessmentYear
#' @param description
#' 
#'
#' @return HTML string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_Stock_info(stockcode, StockDescription, assessmentYear)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
get_Stock_info <- function(CommonName, stockcode, assessmentYear, AssessmentComponent, description) { # StockDescription,
  
  stock_info_sentence <- HTML(
    paste0(
      "<b><i><font size=", 4, ">", "Stock information:", "</font></b></i><br/>",
      "<font size=", 3, ">", "Common name: ", "<b>", CommonName, "</b><br/>",
      "<font size=", 3, ">", "Stock code: ", "<b>", stockcode, "</b><br/>",
      "<font size=", 3, ">", "Assessment year: ", "<b>", assessmentYear, "</b><br/>",
      if (all(AssessmentComponent != "NA") ) {
        paste0(     
          "<font size=", 3, ">", "Component: ", "<b>", AssessmentComponent, "</b><br/>",     
          "<font size=", 3, ">", "Location: ", "<b>", parse_location_from_stock_description(description), "</b>"
         )
      } else {
        paste0(          
          "<font size=", 3, ">", "Location: ", "<b>", parse_location_from_stock_description(description), "</b>"
        )
      }
    )
  )

  return(stock_info_sentence)
}

#' Returns an HTML string containing the catch scenario table's footnotes.
#'
#' @param ASDadviceKey

#' @return string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_catch_scenario_table(ASDadviceKey)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
#' 
format_catch_scenario_notes <- function(ASDadviceKey) {
  catch_scenario_table_notes <- icesASD::getCatchScenariosNotes(ASDadviceKey)

  if (length(catch_scenario_table_notes) != 0) {
    catch_scenario_table_notes <- catch_scenario_table_notes %>% select(-adviceKey)
    string_notes <- HTML(
      paste0("<ul>", paste0("<li><font size=2>", catch_scenario_table_notes$symbol, " "), paste0(catch_scenario_table_notes$notes, "</font></li>"), "</ul>")
    )
  } else {
    string_notes <- character(0)
  }

  return(string_notes)
}




#' Returns a version of the catch scenario table with standardised columns and columns headings
#'
#' @param tmp (catch scenario table)
#'
#' @return df
#'
#' @note
#' this function is usally run sequantially with get_catch_scenario_table()
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' standardize_catch_scenario_table(df)
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
standardize_catch_scenario_table <- function(tmp) {
  if (!is_empty(tmp)) {
  
  tmp_unified <- data.frame()

  # Year
  pattern <- c("Year")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <-  data.frame(tmp[subset][1])

  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset][1]) ####this works!

  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])

  # Total catch"
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # Ftotal"
  pattern <- c("_FTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # Fwanted"
  pattern <- c("_Fwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F_wanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # HR
  pattern <- c("_HR_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(HR = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
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
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # % Advice change"
  pattern <- c("_Advchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # % SSB change "
  pattern <- c("_StockSizechange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # Save the names to re-use them later when diplaying the table
  colnames(tmp_unified) <- sub(" _.*_", "", colnames(tmp_unified))
  col_names_for_display <- colnames(tmp_unified)
  
  # rename columns to standard names  
  colnames(tmp_unified) <- c("Year", "Scenario", "cS_Purpose", "TotCatch", "F", "F_wanted", "HR", "SSB","CatchUnwanted","CatchUnwantedSurviving", "TAC change", "ADVICE change", "SSB change")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  }
  else {
    tmp_unified <- character(0)
    col_names_for_display <- character(0)
  }
  
  return(list(table = tmp_unified, cols = col_names_for_display))
}


#' Returns a df that merges historical catches, previous year advice and current year catch scenarios
#'
#' @param catches_data (from SAG)
#' @param catch_scenario_table (from advice view)
#' @param stock_name
#' @param year
#'
#' @return df
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#'
wrangle_catches_with_scenarios <- function(catches_data, assessmentkey, catch_scenario_table, adviceValue, adviceApplicableUntil, year) {
  # Filter out the rows that are not advice
  catches_data <- catches_data %>%
    filter(Purpose == "Advice", AssessmentKey == assessmentkey) %>%
    select(Year, Catches, Landings, Discards, IBC, Unallocated_Removals) %>% arrange(Year) %>% 
    mutate(Year = as.numeric(Year),
           Catches = as.numeric(Catches),
           Landings = as.numeric(Landings),
           Discards = as.numeric(Discards),
           IBC = as.numeric(IBC),
           Unallocated_Removals = as.numeric(Unallocated_Removals))
  
  # Check if the last row is NA for both columns in columns_to_check
  if (all(is.na(catches_data[nrow(catches_data), c("Catches", "Landings")]))) {
    # Filter out the last row if it is NA for both columns
    catches_data <- catches_data[-nrow(catches_data), ]
  }
  
  # Check if the column "Landings" is NA
  if (is_na_column(catches_data, "Landings") | sum(!is.na(catches_data$Catches)) > sum(!is.na(catches_data$Landings))) {
    catches_data$Catches <- rowSums(catches_data[, c("Catches", "Discards", "IBC", "Unallocated_Removals")], na.rm = TRUE)
    catches_data <- catches_data %>% 
                    select(c("Year", "Catches")) %>% 
                    add_column(Scenario = "Historical Catches") %>%
                    add_column(Color = "#000000") %>%
                    add_column(MarkerSize = 5)
  } else {
    catches_data$Catches <- rowSums(catches_data[, c("Landings", "Discards", "IBC", "Unallocated_Removals")], na.rm = TRUE)
    catches_data <- catches_data %>% 
                    select(c("Year", "Catches")) %>% 
                    add_column(Scenario = "Historical Landings") %>%
                    add_column(Color = "#000000") %>%
                    add_column(MarkerSize = 5)
  }
  
  # Create a color palette for the catch scenario table
  palette <- tableau_color_pal("Tableau 20")(length(catch_scenario_table$Scenario))
  catch_scenario_table <- catch_scenario_table %>%
    select(Year, TotCatch, Scenario) %>%
    add_column(Color = palette) %>%
    add_column(MarkerSize = 15)

  # Set the names of the columns in the catch_scenario_table to match the names of the columns in the catches_data
  catch_scenario_table <- setNames(catch_scenario_table, names(catches_data))
  final_df <- rbind(catches_data, catch_scenario_table)
  final_df <- na.omit(final_df)

  # Extract the year from adviceApplicableUntil
  year_advice_until <- as.numeric(format(as.Date(adviceApplicableUntil), "%Y"))
  # year_advice_until <- 2024
  # Append the new row to the data frame
  final_df <- final_df %>%
    add_row(
      Year = year_advice_until,
      Catches = as.numeric(adviceValue),
      Scenario = "Previous advice",
      Color = "#ff7f0e",
      MarkerSize = 14
    ) %>%
    arrange(Year)


  return(final_df)
}

#' Returns a catch scenario plot with values in %. The values of the current catch scenario table are adjusted based on the previous year basis of advice.
#'
#' @param old_catch_scen_table (from advice view)
#' @param new_catch_scen_table (from advice view)

#'
#' @return df
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
scale_catch_scenarios_for_radialPlot <- function(old_catch_scen_table, new_catch_scen_table) {
  if (!is_empty(new_catch_scen_table) | !is_empty(new_catch_scen_table)) {
    changes_columns <- new_catch_scen_table %>% select("Scenario", "TAC change", "ADVICE change", "SSB change")

    keep.cols <- c("Year", "Scenario", "cS_Purpose", "F", "F_wanted", "HR")
    df_old <- old_catch_scen_table %>% 
      select(all_of(keep.cols)) %>% 
      drop_cols_with_all_nas()
    
    df_new <- new_catch_scen_table %>% 
      select(all_of(keep.cols)) %>% 
      drop_cols_with_all_nas() %>% 
      na.omit()

    Basis <- df_old[df_old$cS_Purpose == "Basis Of Advice", ]
    catch_scen_table_perc <- df_new[, c("Year", "Scenario", "cS_Purpose")]
    catch_scen_table_perc <- calculate_perc_change(df_new, Basis, catch_scen_table_perc)
    catch_scen_table_perc <- catch_scen_table_perc %>% left_join(., changes_columns, by = c("Scenario")) #%>% relocate("SSB change", .after = SSB)
    
  } else {
    catch_scen_table_perc <- character(0)
  }
  
  return(catch_scen_table_perc)
}


#' Drop columns from a data frame if they only contain NA values
#'
#' @param df a data frame
#'
#' @return a data frame where no columns contain only NA
#'
#' @examples
#' df <- data.frame(a = c(NA, NA, 1), b = rep(NA,3))
#' drop_all_na_cols(df)
#' 
drop_cols_with_all_nas <- function(df){
  df[,colSums(is.na(df)) < nrow(df)]
}

#' Returns a catch scenario plot with values in %. 
#' The values of the current catch scenario table are adjusted based on the previous year basis of advice.
#' If the previous year value == 0, the % of change is going to be 100%.
#' If the current year and previous year value are equal, then the percantage of change is 0.
#'
#' @param new_catch_scen_table (from advice view)
#' @param Basis
#' @param catch_scen_table_perc 

#'
#' @return catch_scen_table_perc (df)
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
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
  names(catch_scen_table_perc) <- names(df_new)
  return(catch_scen_table_perc)
}