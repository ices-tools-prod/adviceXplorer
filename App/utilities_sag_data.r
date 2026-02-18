options(icesSAG.use_token = FALSE)


#' Downloads SAG data using icesSAG library and web services
#'
#' @param stock_name
#' @param year
#'
#' @return an aggregated dataframe which includes SAGsummary and SAG reference points
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'access_sag_data("wit.27.3a47d", 2019)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
access_sag_data <- function(stock_code, year) {

    # Dowload the data
    SAGsummary <- getSAG(stock_code, year,
        data = "summary", combine = TRUE, purpose = "Advice"
    )
    SAGrefpts <- getSAG(stock_code, year,
        data = "refpts", combine = TRUE, purpose = "Advice"
    )

    data_sag <- cbind(SAGsummary, SAGrefpts)
    data_sag <- subset(data_sag, select = -fishstock)
    data_sag <- filter(data_sag, StockPublishNote == "Stock published")
    
}

#' Reads SAG data that is stored locally
#'
#' @param stock_name
#' @param year
#'
#' @return an aggregated dataframe which includes SAG summary and SAG reference points
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'access_sag_data_local("wit.27.3a47d", 2019)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
# stock_code <- "alf.27.nea"
# year <- 2024
# test <- access_sag_data_local(stock_code, year)
access_sag_data_local <- function(stock_code, year) {
  # out1 <-
  #   lapply(
  #     year,
  #     function(i) {
  #       fread(sprintf("Data/SAG_%s/SAG_summary.csv", i))
  #     }
  #   )
  # SAGsummary <- do.call(rbind, out1)

  # out2 <-
  #   lapply(
  #     year,
  #     function(j) {
  #       fread(sprintf("Data/SAG_%s/SAG_refpts.csv", j))
  #     }
  #   )
  # SAGrefpts <- do.call(rbind, out2)

  # data_sag <- merge(SAGsummary, SAGrefpts) %>% filter(FishStock == stock_code)
  out3 <-
    lapply(
      year,
      function(i) {
        fread(sprintf("Data/SAG_%s/SAG.csv", i))
      }
    )
  SAGsummary <- do.call(rbind, out3)

  data_sag <- SAGsummary %>%
  filter(StockKeyLabel == stock_code) %>% 
    # select(-FishStock) %>%
    # filter(StockPublishNote == "Stock published") %>%
    mutate(across(everything(), ~ if (class(.) == "integer64") as.integer(.) else .))

  return(data_sag)
}


#' Reads SAG data stored locally for multiple years prior to the year provided (ex year = 2019, years of data returned = c(2017,2018,2019))
#'
#' @param stock_name
#' @param year
#' 
#' @return an aggregated dataframe of SAG data from different years
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso access_sag_data_local()
#'
#' @examples
#' \dontrun{
#'quality_assessment_data_local("wit.27.3a47d", 2021)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'

quality_assessment_data_local <- function(stock_code, year, assessmentComponent) {
  years <- c(2024, 2023, 2022, 2021, 2020)
  years <- years[years <= year]
  datalist <- list()

  data_temp <- try(access_sag_data_local(stock_code, years))
  if (isTRUE(class(data_temp) == "try-error")) {
    next
  } else {
    data_temp <- filter(data_temp, between(Year, 2005, 2024))
    
    data_temp <- data_temp %>% select(
      Year,
      Recruitment, RecruitmentAge,UnitOfRecruitment,
      StockSize, Bpa, Blim, MSYBtrigger, StockSizeDescription, StockSizeUnits,
      FishingPressure, Flim, Fpa, FMSY, FAge, FishingPressureDescription,
      AssessmentYear, Purpose, SAGStamp, AssessmentComponent
    )
    data_temp$AssessmentComponent[data_temp$AssessmentComponent == "" | is.na(data_temp$AssessmentComponent) | data_temp$AssessmentComponent == 0 | data_temp$AssessmentComponent == "N.A."] <- "NA" # this probably needs to go when they update ASD from "N.A." to NA
    data_temp$RecruitmentAge <- as.character(data_temp$RecruitmentAge)
    data_temp$StockSizeDescription <- as.character(data_temp$StockSizeDescription)
    data_temp$StockSizeUnits <- as.character(data_temp$StockSizeUnits)
    data_temp$FAge <- as.character(data_temp$FAge)
    data_temp$FishingPressureDescription <- as.character(data_temp$FishingPressureDescription)
  }
  # take out non published data from before 2021 in big data
  SAG_data <- filter(data_temp, Purpose == "Advice" & AssessmentComponent == assessmentComponent) %>% distinct()
  # make assessmentYear as factor
  SAG_data$AssessmentYear <- as.factor(SAG_data$AssessmentYear)

  return(SAG_data)
}



#' Function for getting ices_areas for each stock
#'
#' @param stock_name
#'
#' @return the list of ICES areas for a particular stock
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
getStockAreas <- function(stockCode) {
  details <- getCodeDetail(code = stockCode, code_type = "ICES_StockCode")
  areas <- details$children$codes[details$children$code_types$Key == "ICES_Area", ]
  areas$Key
}

#' Downloads the SAG settings for each SAG plot using the SAG web-service.
#'
#' @param assessmentkey
#'
#' @return df of SAG settings
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
getSAGSettings <- function(assessmentkey) {
    sagSettings <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    )
}

#' Downloads the new version of the SAG summary. (output needs to be formatted)
#'
#' @param assessmentkey
#'
#' @return df of SAG summary
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
getSAGSummary <- function(assessmentkey, combine = TRUE) {
    out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentkey)
            # sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    ) 

# drop any null entries (happens when not published stocks creep in)
  out <- out[!sapply(out, is.null)]
    
  # combine tables
  if (length(out) > 1 && combine) {
    # form new column names for combined data frame
    outNames <- unique(unlist(lapply(out, names)))

    # rbind, adding in missing columns as characters
    out1 <-
      do.call(rbind,
        lapply(unname(out), function(x) {
          # are any columns missing?
          missing.cols <- !outNames %in% names(x)
          if (any(missing.cols)) {
            # add on missing columns as characters
            x[outNames[missing.cols]] <- NA
          }
          # reorder columns
          x[outNames]
        }))
        
        # take out rows where all columns are NA
        df2 <- subset(out1,!is.na(out1[,1]))

        # identify the columns that are not "lines" (ie. where values are stored)
        columns.to.add <- grep('lines', names(out), invert = TRUE, value = TRUE)

        # turn list to df and replicate values for the size of df2
        fixed_df <- bind_rows(out[columns.to.add]) %>% slice(rep(1:n(), each = nrow(df2)))

        # combine the 2 df to obtain final SAG data
        final_df <- cbind(fixed_df, df2)

        # renumber the rows
        rownames(final_df) = NULL
    # finally resimplify
    
  } else if (length(out) == 1) {
    final_df <- out[[1]]
  }
return(final_df)
}

#' Creates a download button to be displayed when SAG data can be downloaded.
#'
#' @param outputId
#'
#' @return HTML tag
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
myDownloadButton <- function(outputId){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, style = "width: 35px; height: 35px; background: url('downloading.png');  background-size: cover; background-position: center; border: 1px solid transparent;")
}



#' Returns a data.frame with ibc and unallocated removals (to be integrated with icesSAG)
#'
#' @param assessmentKey
#'
#' @return data.frame
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' }
#'
#' @references
#'
#' 
#'
#' @export
#'
get_additional_landing_data <- function(assessmentKey) {
  out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentKey)            
        )
    )  
  df <- data.frame(Year = out$Lines$Year, ibc = out$Lines$IBC, unallocated_Removals = out$Lines$Unallocated_Removals)
  return(df)
}

#' Returns a link to the replaced advice, if present
#'
#' @param year
#' @param stock_code
#'
#' @return char array
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' }
#'
#' @references
#'
#' 
#'
#' @export
#'
get_link_replaced_advice <- function(sagData) {
  # link <- access_sag_data_local(StockKeyLabel, year) %>% filter(Purpose == "Replaced")
  sagData <- sagData %>% filter(Purpose == "Replaced")
  link <- sagData$Report[1]
  return(link)
}


# Function to modify the assessment component to NA if it is "N.A." or "N.A"
modify_assessment_component <- function(assessment_component) {
  if (length(assessment_component) != 0 && assessment_component %in% c("N.A.", "N.A")) {
    return("NA")
  } else {
    return(assessment_component)
  }
}




# # Function to dynamically determine scaling factor and suffix
# get_scaling <- function(values, scaling_factor) {
#   max_val <- max(values, na.rm = TRUE) * scaling_factor # Find max value
#   order <- floor(log10(max_val)) # Find order of magnitude

#   if (order >= 9) { # Billions
#     return(list(divisor = 1e9, suffix = "billions"))
#   } else if (order >= 6 & order <= 9) { # Millions
#     return(list(divisor = 1e6, suffix = "millions"))
#   } else if (order >= 4 & order < 6) { # Thousands
#     return(list(divisor = 1e3, suffix = "1000"))
#   } else { # No scaling
#     return(list(divisor = 1, suffix = "relative"))
#   }
# }

# # Function to dynamically determine scaling factor and suffix
# get_scaling_ssb <- function(values, scaling_factor) {
#   max_val <- max(values, na.rm = TRUE) * scaling_factor # Find max value
#   order <- floor(log10(max_val)) # Find order of magnitude
  
#   if (order >= 9) { # Billions
#     return(list(divisor = 1e9, suffix = "billions t"))
#   } else if (order >= 6 & order <= 9) { # Millions
#     return(list(divisor = 1e6, suffix = "millions t"))
#   } else if (order >= 3 & order < 6) { # Thousands
#     return(list(divisor = 1e3, suffix = "1000 t"))
#   } else { # No scaling
#     return(list(divisor = 1, suffix = "relative")) #or tonnes
#   }
# }

# # Function to dynamically determine scaling factor and suffix
# get_scaling_catches <- function(values, scaling_factor) {
#   max_val <- max(values, na.rm = TRUE) * scaling_factor # Find max value
#   order <- floor(log10(max_val)) # Find order of magnitude
  
#   if (order >= 9) { # Billions
#     return(list(divisor = 1e9, suffix = "billions t"))
#   } else if (order >= 6 & order <= 9) { # Millions
#     return(list(divisor = 1e6, suffix = "millions t"))
#   } else if (order >= 3 & order < 6) { # Thousands
#     return(list(divisor = 1e3, suffix = "1000 t"))
#   } else { # No scaling
#     return(list(divisor = 1, suffix = "tonnes")) 
#   }
# }



get_scaling <- function(values, scaling_factor, type = "default") {
  max_val <- as.numeric(max(values, na.rm = TRUE)) * scaling_factor # Find max value
  order <- floor(log10(max_val)) # Find order of magnitude
  
  suffix <- switch(type,
                   "ssb" = c("billions t", "millions t", "1000 t", "relative"),
                   "catches" = c("billions t", "millions t", "1000 t", "tonnes"),
                   c("billions", "millions", "1000", "relative"))
  
  if (order >= 9) { # Billions
    return(list(divisor = 1e9, suffix = suffix[1]))
  } else if (order >= 6 & order <= 9) { # Millions
    return(list(divisor = 1e6, suffix = suffix[2]))
  } else if (order >= 3 & order < 6) { # Thousands
    return(list(divisor = 1e3, suffix = suffix[3]))
  } else { # No scaling
    return(list(divisor = 1, suffix = suffix[4]))
  }
}

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
                           "Kilograms per day" = 1,
                           "UWTV abundance (billions)" = 1000000000,
                           "Number of individuals (billions)" = 1000000000,
                           "ratio" = 1,
                           stop("Invalid unit value: choose 'thousands', 'relative', or other valid units"))
  return(scaling_factor)
}


# Define a function to process and refactor the dataframe
process_dataframe_catches <- function(df, additionalCustomeSeries, scaling_factor_catches) {
  # Filter for relevant rows and select initial columns
  
  df1 <- df %>%
    filter(Purpose == "Advice") %>%
    select(
      Year, Landings, Catches, Discards, CatchesLandingsUnits, #SAGStamp,
      IBC, Unallocated_Removals,
      # Dynamically include custom columns if present
      if (!is.null(additionalCustomeSeries) && !is.na(additionalCustomeSeries)) {
        c(paste0("Custom", additionalCustomeSeries), paste0("CustomName", additionalCustomeSeries))
      }
    ) %>%
    # Relocate standard columns
    relocate(c(IBC, Unallocated_Removals), .after = Discards) %>%
    # Rename Industrial Bycatch
    rename(
      "Industrial Bycatch" = IBC,
      "Unallocated Removals" = Unallocated_Removals
    ) %>%
    # Scale numerical columns
    mutate(
      Year = as.numeric(Year),
      Landings = as.numeric(Landings) * scaling_factor_catches,
      Catches = as.numeric(Catches) * scaling_factor_catches,
      Discards = as.numeric(Discards) * scaling_factor_catches,
      `Industrial Bycatch` = as.numeric(`Industrial Bycatch`) * scaling_factor_catches,
      `Unallocated Removals` = as.numeric(`Unallocated Removals`) * scaling_factor_catches
    )

  # Handle custom columns if additionalCustomeSeries is not empty
  if (!is.null(additionalCustomeSeries) && !is.na(additionalCustomeSeries)) {
    for (index in additionalCustomeSeries) {
      custom_col <- paste0("Custom", index)
      custom_name_col <- paste0("CustomName", index)

      # Ensure the custom name column exists and has a non-empty value
      if (custom_name_col %in% names(df1) && !is.na(df1[[custom_name_col]][1]) && df1[[custom_name_col]][1] != "") {
        # Rename the custom column using the first value in the corresponding CustomName column
        new_name <- df1[[custom_name_col]][1]
        names(df1)[names(df1) == custom_col] <- new_name

        # Relocate the renamed custom column before `Unallocated Removals`
        df1 <- df1 %>%
          relocate(all_of(new_name), .before = `Unallocated Removals`)

        # Multiply the custom column by scaling_factor_catches
        df1[[new_name]] <- as.numeric(df1[[new_name]]) * scaling_factor_catches

        # Drop the CustomName column
        df1[[custom_name_col]] <- NULL
      }
    }
  }

  return(df1)
}


process_dataframe_F <- function(df, sagSettings) {
  nullifempty <- function(x) if (length(x) == 0) NULL else x
  # Filter settings for SAGChartKey 3
  sagSettings3 <- sagSettings %>% filter(SAGChartKey == 3)

  # Extract custom reference points
  customRefPoint <-
        sagSettings3 %>%
        filter(settingKey == 51) %>%
        pull(settingValue) %>%
        standardiseRefPoints(.) %>%
        str_split(pattern = ",", simplify = TRUE)
        # as.numeric()

  # Extract additional custom series
  additionalCustomeSeries <- sagSettings3 %>%
    filter(settingKey == 45) %>%
    pull(settingValue) %>%
    as.numeric() %>%
    nullifempty()
  
  # Process the dataframe
  df3 <- df %>%
    filter(Purpose == "Advice") %>%
    arrange(Year) %>%
    select(
      c(
        Year, FishingPressure, Low_FishingPressure, High_FishingPressure, Flim, Fpa, FMSY, FAge,
        Fmanagement, HRMGT, FishingPressureDescription,  ConfidenceIntervalDefinition, #SAGStamp,
        FMGT_lower, FMGT_upper
      ),
      if (length(customRefPoint) != 0 && !all(customRefPoint %in% colnames(.))) {
        c(paste0("CustomRefPointValue", customRefPoint), paste0("CustomRefPointName", customRefPoint))
      },
      if (!is.null(additionalCustomeSeries) && !is.na(additionalCustomeSeries)) {
        c(paste0("Custom", additionalCustomeSeries), paste0("CustomName", additionalCustomeSeries))
      }
    ) %>%
    mutate(
      Year = as.numeric(Year),
      FishingPressure = as.numeric(FishingPressure),
      Low_FishingPressure = as.numeric(Low_FishingPressure),
      High_FishingPressure = as.numeric(High_FishingPressure),
      Flim = as.numeric(Flim),
      Fpa = as.numeric(Fpa),
      FMSY = as.numeric(FMSY),
      Fmanagement = as.numeric(Fmanagement),
      HRMGT = as.numeric(HRMGT),
      FMGT_lower = as.numeric(FMGT_lower),
      FMGT_upper = as.numeric(FMGT_upper),
      across(starts_with("CustomRefPointValue"), as.numeric)
    ) %>%
   mutate(segment = cumsum(is.na(FishingPressure)))
   
   new_name <- list()
  # Handle additional custom series
  if (!is.null(additionalCustomeSeries) && !is.na(additionalCustomeSeries)) {
    for (index in additionalCustomeSeries) {
      custom_col <- paste0("Custom", index)
      custom_name_col <- paste0("CustomName", index)

      # Ensure the custom name column exists and has a valid value
      if (custom_name_col %in% names(df3) && !is.na(df3[[custom_name_col]][1]) && nzchar(df3[[custom_name_col]][1])) {
        # Rename the custom column using the first value in the corresponding CustomName column
        new_name <- df3[[custom_name_col]][1]
        names(df3)[names(df3) == custom_col] <- new_name
        # Multiply the custom column by scaling_factor_catches
        df3[[new_name]] <- as.numeric(df3[[new_name]])
        # Drop the CustomName column
        df3[[custom_name_col]] <- NULL
      }
    }
  }

  return(list(
    df3 = df3,
    sagSettings3 = sagSettings3,
    customRefPoint = customRefPoint,
    additionalCustomeSeries = additionalCustomeSeries,
    new_name = new_name
  ))
}




process_dataframe_SSB <- function(df, sagSettings, scaling_factor_stockSize) {
  nullifempty <- function(x) if (length(x) == 0) NULL else x

  # Use total biomass only if requested AND any TBiomass data exist
  useTotBiomass <- {
    flag <- sagSettings %>%
      dplyr::filter(SAGChartKey == 0, settingKey == 37) %>%
      dplyr::pull(settingValue) %>%
      nullifempty()
    on <- !is.null(flag) && tolower(flag[1]) == "y"
    non_empty <- any(!is.na(df$TBiomass)) ||
                 any(!is.na(df$Low_TBiomass)) ||
                 any(!is.na(df$High_TBiomass))
    on && non_empty
  }

  # Helper to repurpose StockSizeDescription
  rewrite_metric_desc <- function(d, use_tot) {
    d <- as.character(d)
    if (use_tot) {
      out <- ifelse(
        !is.na(d) & nzchar(d),
        stringr::str_replace_all(
          d,
          stringr::regex("(SSB|spawning[- ]?stock biomass|stock size)", ignore_case = TRUE),
          "Total biomass"
        ),
        "Total biomass"
      )
    } else {
      out <- ifelse(!is.na(d) & nzchar(d), d, "SSB")
    }
    out
  }

  # Settings for this chart
  sagSettings4 <- sagSettings %>% dplyr::filter(SAGChartKey == 4)

  # Custom reference points
  customRefPoint <- sagSettings4 %>%
    dplyr::filter(settingKey == 51) %>%
    dplyr::pull(settingValue) %>%
    standardiseRefPoints(.) %>%
    stringr::str_split(pattern = ",", simplify = TRUE)

  # Additional custom series
  additionalCustomeSeries <- sagSettings4 %>%
    dplyr::filter(settingKey == 45) %>%
    dplyr::pull(settingValue) %>%
    as.numeric() %>%
    nullifempty()

  # Process and keep BOTH SSB and TBiomass (desc/units for TBiomass may not exist)
  df4 <- df %>%
    dplyr::filter(Purpose == "Advice") %>%
    dplyr::arrange(Year) %>%
    dplyr::select(
      c(
        Year, Low_StockSize, StockSize, High_StockSize, Blim, Bpa, MSYBtrigger,
        Bmanagement, StockSizeDescription, StockSizeUnits, ConfidenceIntervalDefinition,
        BMGT_lower, BMGT_upper,
        Low_TBiomass, TBiomass, High_TBiomass
      ),
      if (length(customRefPoint) != 0 && !all(customRefPoint %in% colnames(.))) {
        c(paste0("CustomRefPointValue", customRefPoint),
          paste0("CustomRefPointName",  customRefPoint))
      },
      if (!is.null(additionalCustomeSeries) && !is.na(additionalCustomeSeries)) {
        c(paste0("Custom",     additionalCustomeSeries),
          paste0("CustomName", additionalCustomeSeries))
      }
    ) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Blim = as.numeric(Blim),
      Bpa  = as.numeric(Bpa),
      MSYBtrigger = as.numeric(MSYBtrigger),
      Bmanagement = as.numeric(Bmanagement),
      BMGT_lower = as.numeric(BMGT_lower),
      BMGT_upper = as.numeric(BMGT_upper),

      # scale both sets consistently
      StockSize      = as.numeric(StockSize)      * scaling_factor_stockSize,
      Low_StockSize  = as.numeric(Low_StockSize)  * scaling_factor_stockSize,
      High_StockSize = as.numeric(High_StockSize) * scaling_factor_stockSize,

      TBiomass      = as.numeric(TBiomass)      * scaling_factor_stockSize,
      Low_TBiomass  = as.numeric(Low_TBiomass)  * scaling_factor_stockSize,
      High_TBiomass = as.numeric(High_TBiomass) * scaling_factor_stockSize
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("CustomRefPointValue"), as.numeric))

  # Unified metric columns
  if (useTotBiomass) {
    df4 <- df4 %>% dplyr::mutate(
      Metric      = TBiomass,
      Low_Metric  = Low_TBiomass,
      High_Metric = High_TBiomass
    )
    metric_key <- "TBiomass"
  } else {
    df4 <- df4 %>% dplyr::mutate(
      Metric      = StockSize,
      Low_Metric  = Low_StockSize,
      High_Metric = High_StockSize
    )
    metric_key <- "StockSize"
  }

  df4 <- df4 %>%
    dplyr::mutate(
      MetricDescription = rewrite_metric_desc(StockSizeDescription, useTotBiomass),
      segment = cumsum(is.na(Metric)),
      is_single_value_in_segment = ave(!is.na(Metric), segment, FUN = function(x) sum(x) == 1),
      show_error = !is.na(Metric) & is_single_value_in_segment
    )

  # Additional custom series renaming/scaling
  new_name <- list()
  if (!is.null(additionalCustomeSeries) && !is.na(additionalCustomeSeries)) {
    for (index in additionalCustomeSeries) {
      custom_col <- paste0("Custom", index)
      custom_name_col <- paste0("CustomName", index)
      if (custom_name_col %in% names(df4) && !is.na(df4[[custom_name_col]][1]) && nzchar(df4[[custom_name_col]][1])) {
        new_name <- df4[[custom_name_col]][1]
        names(df4)[names(df4) == custom_col] <- new_name
        df4[[new_name]] <- as.numeric(df4[[new_name]]) * scaling_factor_stockSize
        df4[[custom_name_col]] <- NULL
      }
    }
  }

  return(list(
    df4 = df4,
    sagSettings4 = sagSettings4,
    customRefPoint = customRefPoint,
    additionalCustomeSeries = additionalCustomeSeries,
    new_name = new_name,
    useTotBiomass = useTotBiomass,
    metric_key = metric_key
  ))
}



nullifempty <- function(x) if (length(x) == 0) NULL else x

convert_false_to_F <- function(x) {
  if (x == FALSE) {
    return("F")
  } else {
    return(x)
  }
}


getSAGData <- function(assessmentKey) {
  # Download the SAG data
  # assessmentKey <- findAssessmentKey(stock_code, year = YearOfLastAssessment)
  summary <- icesSAG::getStockDownloadData(assessmentKey) %>%
    # select(-AssessmentComponent, -Purpose, -AssessmentYear, -StockDescription) %>%
    # change AssessmentKey to integer
    mutate(AssessmentKey = as.integer(AssessmentKey)) %>%
    # left_join(sag, by = c("StockKeyLabel", "AssessmentKey")) %>%
    mutate(across(
      c(
        CustomRefPointName1,
        CustomRefPointName2,
        CustomRefPointName3,
        CustomRefPointName4,
        CustomRefPointName5
      ), standardiseRefPoints
    ))
 
  refpts <- icesSAG::getFishStockReferencePoints(assessmentKey)
  refpts <- refpts %>% mutate(across(
    c(
      CustomRefPointName1,
      CustomRefPointName2,
      CustomRefPointName3,
      CustomRefPointName4,
      CustomRefPointName5
    ), standardiseRefPoints
  ))

  # Perform the merge with suffixes to handle duplicate column names
  sagMerged <- merge(summary, refpts, by = "AssessmentKey", suffixes = c(".summary", ""))

  # Select only the columns from summary
  sagMerged <- sagMerged[, !grepl(".summary$", names(sagMerged))]

  return(sagMerged)  
}


is_na_column <- function(dataframe, col_name) {
  # Ensure col_name is treated as a string and extract column correctly
  if (!col_name %in% names(dataframe)) {
    stop("Column not found in dataframe")
  }
  
  return(all(is.na(dataframe[[col_name]])))
}


getSAGQualityAssessment <- function(stock_code, year, assessmentComponent, yearsToDisplay = NULL) {

  # years sequence
  year <- as.integer(year)
  if (!is.null(yearsToDisplay)) {
    years <- seq(year, year - (as.integer(yearsToDisplay) - 1))
  } else {
    years <- seq(year, year - 4)
  }

  # sequential: lapply (or for-loop)
  out_list <- lapply(years, function(y) {
    # 1) find assessmentKey for that year
    assessmentKey <- try(findAssessmentKey(stock_code, year = y), silent = TRUE)
    if (inherits(assessmentKey, "try-error") || is.null(assessmentKey) || length(assessmentKey) == 0) {
      return(NULL)
    }

    # 2) fetch SAG for that assessmentKey (your getSAGData does 2 SAG calls)
    dat <- try(getSAGData(assessmentKey), silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat) || !nrow(dat)) {
      return(NULL)
    }

    # 3) coerce + filter (same as your current function)
    dat$Year <- as.numeric(dat$Year)
    dat$Recruitment <- as.numeric(dat$Recruitment)
    dat$StockSize <- as.numeric(dat$StockSize)
    dat$Bpa <- as.numeric(dat$Bpa)
    dat$Blim <- as.numeric(dat$Blim)
    dat$MSYBtrigger <- as.numeric(dat$MSYBtrigger)
    dat$FishingPressure <- as.numeric(dat$FishingPressure)
    dat$Flim <- as.numeric(dat$Flim)
    dat$Fpa <- as.numeric(dat$Fpa)
    dat$FMSY <- as.numeric(dat$FMSY)

    dat <- dplyr::filter(dat, dplyr::between(.data$Year, 2005, year))

    dat <- dplyr::select(
      dat,
      Year,
      Recruitment, RecruitmentAge, UnitOfRecruitment,
      StockSize, Bpa, Blim, MSYBtrigger, StockSizeDescription, StockSizeUnits,
      FishingPressure, Flim, Fpa, FMSY, FAge, FishingPressureDescription,
      AssessmentYear, Purpose, AssessmentComponent
    )

    # standardise component
    dat$AssessmentComponent[dat$AssessmentComponent == "" |
                           is.na(dat$AssessmentComponent) |
                           dat$AssessmentComponent == 0 |
                           dat$AssessmentComponent == "N.A."] <- "NA"

    dat$RecruitmentAge <- as.character(dat$RecruitmentAge)
    dat$UnitOfRecruitment <- as.character(dat$UnitOfRecruitment)
    dat$StockSizeDescription <- as.character(dat$StockSizeDescription)
    dat$StockSizeUnits <- as.character(dat$StockSizeUnits)
    dat$FAge <- as.character(dat$FAge)
    dat$FishingPressureDescription <- as.character(dat$FishingPressureDescription)

    # keep only advice + component
    dat <- dplyr::filter(dat, Purpose == "Advice", AssessmentComponent == assessmentComponent) %>%
      dplyr::distinct()

    dat$AssessmentYear <- as.factor(dat$AssessmentYear)
    dat
  })

  dplyr::bind_rows(out_list)
}


getStockInfoFromSAG <- function(assessmentKey){
info <- jsonlite::fromJSON(
    utils::URLencode(
      sprintf("https://sag.ices.dk/SAG_API/api/StockList?year=0&assessmentKey=%s", assessmentKey)
    )
  )
}

#' In-memory cache for SAG mapping calls
#'
#' A `cachem::cache_mem()` backend used to memoise calls to [getSAG_latest_map()]
#' and [getSAG_year_map()]. The cache reduces repeated network/API calls during a
#' Shiny session and across short time windows.
#'
#' @details
#' The cache is time-limited (`max_age = 12 * 3600`, i.e., 12 hours) to balance
#' responsiveness with data freshness.
#'
#' @keywords internal
sag_cache <- cachem::cache_mem(max_age = 12 * 3600)

#' Get the latest SAG stock-to-assessment mapping
#'
#' Fetches the latest SAG "stock advice list" and returns a normalised mapping
#' containing the minimal columns needed by the app to connect a stock to its
#' assessment identifiers and component.
#'
#' @return
#' A `data.table` with columns:
#' - `StockKeyLabel` (character)
#' - `AssessmentKey` (integer)
#' - `AssessmentYear` (integer)
#' - `AssessmentComponent` (character; normalised so missing/blank/"N.A." variants become `"NA"`)
#'
#' If SAG returns an empty/non-data.frame object, returns an empty `data.table()`.
#'
#' @details
#' Data source: `icesSAG::getLatestStockAdviceList()`.
#'
#' The function normalises varying column names returned by icesSAG by creating
#' `StockKeyLabel`, `AssessmentKey`, `AssessmentYear`, and `AssessmentComponent`
#' when required. It also enforces stable types for ordering and joining, and
#' standardises component labels using the app convention (`"NA"` for missing).
#'
#' The output is deduplicated on
#' (`StockKeyLabel`, `AssessmentKey`, `AssessmentYear`, `AssessmentComponent`).
#'
#' This function is memoised using [sag_cache].
#'
#' @examples
#' \dontrun{
#' m <- getSAG_latest_map()
#' m[StockKeyLabel == "dgs.27.nea"]
#' }
#'
#' @export
getSAG_latest_map <- memoise(function() {
    dt <- icesSAG::getLatestStockAdviceList()
    if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) {
        return(data.table())
    }
    setDT(dt)

    # normalise names
    if ("stockCode" %in% names(dt)) dt[, StockKeyLabel := stockCode]
    if ("assessmentKey" %in% names(dt)) dt[, AssessmentKey := suppressWarnings(as.integer(assessmentKey))]
    if ("adviceComponent" %in% names(dt)) dt[, AssessmentComponent := adviceComponent]
    if ("assessmentYear" %in% names(dt)) dt[, AssessmentYear := suppressWarnings(as.integer(assessmentYear))]
    if ("AssessmentYear" %in% names(dt) && !"AssessmentYear" %in% names(dt)) dt[, AssessmentYear := suppressWarnings(as.integer(AssessmentYear))]

    out <- dt[, .(StockKeyLabel, AssessmentKey, AssessmentYear, AssessmentComponent)]
    out <- out[!is.na(StockKeyLabel) & nzchar(StockKeyLabel) &
        !is.na(AssessmentKey) & !is.na(AssessmentYear)]

    # Ensure it's character before fifelse (older years may return numeric/factor)
    out[, AssessmentComponent := as.character(AssessmentComponent)]

    out[, AssessmentComponent := data.table::fifelse(
        is.na(AssessmentComponent) | AssessmentComponent %in% c("", "N.A.", "N.A", "NA"),
        "NA",
        AssessmentComponent
    )]


    unique(out, by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear", "AssessmentComponent"))
}, cache = sag_cache)



#' Get the SAG stock-to-assessment mapping for a historical year
#'
#' Fetches the SAG list of stocks for a given year and returns a normalised mapping
#' containing the minimal columns needed by the app.
#'
#' @param y Integer-like. The year to query from SAG (passed to
#'   `icesSAG::getListStocks(year = ...)`).
#'
#' @return
#' A `data.table` with columns:
#' - `StockKeyLabel` (character)
#' - `AssessmentKey` (integer)
#' - `AssessmentYear` (integer)
#' - `AssessmentComponent` (character; normalised to the app convention)
#'
#' If SAG returns an empty/non-data.frame object, returns an empty `data.table()`.
#'
#' @details
#' Data source: `icesSAG::getListStocks(year = y)`.
#'
#' This is used when the active year is not the latest SID snapshot and the app
#' needs to build an historical map spanning multiple assessment years.
#'
#' The output is deduplicated on
#' (`StockKeyLabel`, `AssessmentKey`, `AssessmentYear`, `AssessmentComponent`).
#'
#' This function is memoised using [sag_cache].
#'
#' @examples
#' \dontrun{
#' m_2021 <- getSAG_year_map(2021)
#' }
#'

#' @export
getSAG_year_map <- memoise(function(y) {
    dt <- icesSAG::getListStocks(year = as.integer(y))
    if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) {
        return(data.table())
    }
    setDT(dt)

    # normalise (adjust if icesSAG returns different names)
    if ("stockCode" %in% names(dt)) dt[, StockKeyLabel := stockCode]
    if ("assessmentKey" %in% names(dt)) dt[, AssessmentKey := suppressWarnings(as.integer(assessmentKey))]
    if ("adviceComponent" %in% names(dt)) dt[, AssessmentComponent := adviceComponent]
    if ("assessmentYear" %in% names(dt)) dt[, AssessmentYear := suppressWarnings(as.integer(assessmentYear))]
    if ("AssessmentYear" %in% names(dt) && !"AssessmentYear" %in% names(dt)) dt[, AssessmentYear := suppressWarnings(as.integer(AssessmentYear))]

    out <- dt[, .(StockKeyLabel, AssessmentKey, AssessmentYear, AssessmentComponent)]
    out <- out[!is.na(StockKeyLabel) & nzchar(StockKeyLabel) &
        !is.na(AssessmentKey) & !is.na(AssessmentYear)]
    # Ensure it's character before fifelse (older years may return numeric/factor)
    out[, AssessmentComponent := as.character(AssessmentComponent)]

    out[, AssessmentComponent := data.table::fifelse(
        is.na(AssessmentComponent) | AssessmentComponent %in% c("", "N.A.", "N.A", "NA"),
        "NA",
        AssessmentComponent
    )]


    unique(out, by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear", "AssessmentComponent"))
}, cache = sag_cache)


#' Build a SAG mapping table for an active year
#'
#' Returns a SAG mapping table appropriate for the app's `active_year`.
#' For the latest configuration this can be the single-call "latest" map; for
#' historical configurations this function builds a pooled map across the set of
#' assessment years implied by the SID snapshot.
#'
#' @param active_year Integer-like. The year selected in the app.
#' @param sid_dt A SID snapshot `data.frame`/`data.table` containing a
#'   `YearOfLastAssessment` column. Used to infer which SAG years to fetch when
#'   building an historical map.
#' @param is_latest Logical. If `TRUE`, return [getSAG_latest_map()] directly.
#'   If `FALSE`, build a historical map based on `sid_dt`. Defaults to `TRUE`.
#'
#' @return
#' A `data.table` with columns `StockKeyLabel`, `AssessmentKey`, `AssessmentYear`,
#' `AssessmentComponent`, potentially containing multiple assessment years and
#' components per stock (to be deduplicated later as needed).
#'
#' @details
#' Historical mode (`is_latest = FALSE`):
#' 1. Extract unique `YearOfLastAssessment` values from `sid_dt`.
#' 2. Keep valid years (`!= 0`, non-NA) and restrict to `<= active_year`.
#' 3. Ensure `active_year` is included in the year set (so the pool covers the active view).
#' 4. Fetch each year via [getSAG_year_map()] and row-bind.
#' 5. Drop rows with `AssessmentYear > active_year` (safety guard).
#' 6. Deduplicate on (`StockKeyLabel`, `AssessmentKey`, `AssessmentYear`, `AssessmentComponent`).
#'
#' @examples
#' \dontrun{
#' sid <- getSID_meta(2022)
#' sag_hist <- build_SAG_map_for_active_year(2022, sid, is_latest = FALSE)
#' }
#'

#' @export
build_SAG_map_for_active_year <- function(active_year, sid_dt, is_latest = TRUE) {
    active_year <- as.integer(active_year)
    if (is_latest) {
        return(getSAG_latest_map())
    }

    # Historical: fetch only years implied by SID snapshot
    setDT(sid_dt)
    yrs <- sort(unique(as.integer(sid_dt$YearOfLastAssessment)))
    yrs <- yrs[!is.na(yrs) & yrs != 0 & yrs <= active_year]
    if (!(active_year %in% yrs)) yrs <- c(yrs, active_year)
    if (length(yrs) == 0) {
        return(data.table())
    }

    sag_all <- rbindlist(lapply(yrs, getSAG_year_map), fill = TRUE)
    sag_all <- sag_all[AssessmentYear <= active_year]
    unique(sag_all, by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear", "AssessmentComponent"))
}



#' Filter SAG mappings to stocks published in ASD (while keeping SAG components)
#'
#' Filters a SAG mapping table to retain only records that correspond to published
#' ASD advice entries. This is used to ensure the UI stock list only shows stocks
#' that have published advice in ASD, while still relying on SAG as the source of
#' truth for component values.
#'
#' @param sag_dt A `data.frame`/`data.table` produced by [getSAG_latest_map()],
#'   [getSAG_year_map()], or [build_SAG_map_for_active_year()]. Must contain at least
#'   `AssessmentKey`, `StockKeyLabel`, and `AssessmentYear`.
#' @param asd_dt A `data.frame`/`data.table` describing published ASD advice entries.
#'   Minimum expected columns:
#'   - `AssessmentKey`
#'   Optional (enables fallback matching):
#'   - `StockKeyLabel`
#'   - `AssessmentYear`
#'
#' @return
#' A `data.table` subset of `sag_dt` containing only rows that match the published
#' ASD set. If either input is empty, returns `sag_dt[0]` (empty table with SAG columns).
#'
#' @details
#' Matching strategy:
#' 1. Primary match by `AssessmentKey` for ASD rows where `AssessmentKey` is available.
#' 2. Fallback match by (`StockKeyLabel`, `AssessmentYear`) for ASD rows where
#'    `AssessmentKey` is missing or cannot be relied upon.
#'
#' The output is deduplicated on
#' (`StockKeyLabel`, `AssessmentKey`, `AssessmentYear`, `AssessmentComponent`).
#'
#' This function is deliberately "component-agnostic": it filters on publication
#' status but preserves the component values coming from SAG.
#'
#' @examples
#' \dontrun{
#' sag_map <- getSAG_latest_map()
#' asd_pub <- unique(asd_cache[, .(AssessmentKey, StockKeyLabel, AssessmentYear)])
#' sag_pub <- filter_SAG_to_ASD_published(sag_map, asd_pub)
#' }
#'

#' @export
filter_SAG_to_ASD_published <- function(sag_dt, asd_dt) {
    setDT(sag_dt)
    setDT(asd_dt)

    if (nrow(sag_dt) == 0 || nrow(asd_dt) == 0) {
        return(sag_dt[0])
    }

    # 1) match by AssessmentKey where possible
    a1 <- asd_dt[!is.na(AssessmentKey), .(AssessmentKey)]
    setkey(a1, AssessmentKey)
    sag1 <- sag_dt[a1, on = "AssessmentKey", nomatch = 0]

    # 2) fallback: stock+year match for ASD rows with missing AssessmentKey
    a2 <- asd_dt[is.na(AssessmentKey), .(StockKeyLabel, AssessmentYear)]
    if (nrow(a2) > 0) {
        setkey(a2, StockKeyLabel, AssessmentYear)
        setkey(sag_dt, StockKeyLabel, AssessmentYear)
        sag2 <- sag_dt[a2, nomatch = 0]
        sag_keep <- unique(rbindlist(list(sag1, sag2), fill = TRUE),
            by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear", "AssessmentComponent")
        )
    } else {
        sag_keep <- unique(sag1, by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear", "AssessmentComponent"))
    }

    sag_keep[]
}