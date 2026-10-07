#' Utility functions for testing and potentially integrating SAG data as SID complement
#'
#' These functions provide analysis and optional integration of SAG webservices
#' while maintaining compatibility with existing SID-based workflow

#' Test data availability from both SID and SAG sources
#'
#' @param year Assessment year to test
#' @return List with availability status and field comparison
#'
test_data_sources_availability <- function(year = 2024) {
  
  # Test SID
  sid_available <- tryCatch({
    test_data <- download_SID(year)
    !is.null(test_data) && nrow(test_data) > 0
  }, error = function(e) FALSE)
  
  # Test SAG (using existing icesSAG integration)
  sag_available <- tryCatch({
    if (requireNamespace("icesSAG", quietly = TRUE)) {
      test_data <- icesSAG::getListStocks(year = year)
      !is.null(test_data) && nrow(test_data) > 0
    } else {
      FALSE
    }
  }, error = function(e) FALSE)
  
  list(
    year = year,
    sid_available = sid_available,
    sag_available = sag_available,
    recommendation = if (sid_available && sag_available) {
      "Both available - hybrid approach possible"
    } else if (sid_available) {
      "SID only - continue current approach"
    } else if (sag_available) {
      "SAG only - limited functionality"
    } else {
      "No data sources available"
    }
  )
}

#' Enhanced SID data retrieval with optional SAG supplementation
#'
#' This function maintains backward compatibility while optionally enriching
#' data with SAG information when available
#'
#' @param year Assessment year
#' @param supplement_with_sag Logical, whether to add SAG data when available
#' @return Enhanced stock list data
#'
getSID_enhanced <- function(year, supplement_with_sag = TRUE) {
  
  # Get base SID data (existing functionality)
  message("Downloading SID data for year: ", year)
  stock_list_all <- download_SID(year)
  
  if (is.null(stock_list_all)) {
    stop("SID data unavailable for year ", year)
  }
  
  # Convert to long format (existing functionality)
  stock_list_long <- separate_ecoregions(stock_list_all)
  
  # Add icons (existing functionality) 
  setDT(stock_list_long)
  stock_list_long[, icon := paste0('<img src="', match_stockcode_to_illustration(StockKeyLabel, stock_list_long), '" height=40>')]
  
  # Optional SAG supplementation
  if (supplement_with_sag && requireNamespace("icesSAG", quietly = TRUE)) {
    
    message("Supplementing with SAG data...")
    sag_data <- tryCatch({
      icesSAG::getListStocks(year = year) %>%
        select(StockKeyLabel, AssessmentKey, AssessmentYear, Purpose, ModifiedDate, SAGStamp) %>%
        # Rename to avoid conflicts and add suffix for clarity
        rename(
          SAG_AssessmentKey = AssessmentKey,
          SAG_AssessmentYear = AssessmentYear,
          SAG_Purpose = Purpose,
          SAG_ModifiedDate = ModifiedDate
        )
    }, error = function(e) {
      message("SAG data unavailable: ", e$message)
      NULL
    })
    
    if (!is.null(sag_data)) {
      # Merge SAG data while preserving all SID records
      stock_list_long <- merge(stock_list_long, sag_data, 
                              by = "StockKeyLabel", 
                              all.x = TRUE)
      
      # Create a "best" AssessmentKey by preferring SAG when available
      stock_list_long[, AssessmentKey_Best := ifelse(
        !is.na(SAG_AssessmentKey), SAG_AssessmentKey, AssessmentKey
      )]
      
      message("Enhanced with SAG data for ", 
              sum(!is.na(stock_list_long$SAG_AssessmentKey)), 
              " stocks")
    }
  }
  
  # Continue with existing workflow (ASD integration, filtering, etc.)
  # Get unique valid years for assessment key lookup
  valid_years <- unique(stock_list_long$YearOfLastAssessment)
  valid_years <- valid_years[!is.na(valid_years) & valid_years != 0]
  
  # ASD integration (existing code pattern)
  if (requireNamespace("icesASD", quietly = TRUE)) {
    tryCatch({
      ASDList <- icesASD::getAdviceViewRecord(year = year) %>%
        mutate(adviceComponent = na_if(adviceComponent, "N.A.")) %>%
        rename(
          StockKeyLabel = stockCode,
          AssessmentKey = assessmentKey,
          AssessmentComponent = adviceComponent
        ) %>%
        filter(adviceStatus == "Advice")
      
      setDT(ASDList)
      stock_list_long <- ASDList[stock_list_long, on = "StockKeyLabel"]
      
      message("Merged with ASD records")
    }, error = function(e) {
      message("ASD integration failed: ", e$message)
    })
  }
  
  # Filter for valid assessment keys (existing logic)
  missing_keys <- which(!is.na(stock_list_long$AssessmentKey) &
                       !is.na(stock_list_long$YearOfLastAssessment) &
                       stock_list_long$YearOfLastAssessment != 0)
  
  stock_list_long <- stock_list_long[missing_keys, ]
  
  # Extract stock location (existing functionality)
  stock_list_long[, stock_location := parse_location_from_stock_description(StockKeyDescription)]
  
  message("Final dataset: ", nrow(stock_list_long), " records")
  
  return(stock_list_long)
}

#' Check if replacement would be feasible for specific functionality
#'
#' @param functionality Character vector of required functionality
#' @return Logical indicating feasibility
#'
check_replacement_feasibility <- function(functionality = c("ecoregion_mapping", "assessment_planning", "species_info")) {
  
  feasible <- list()
  
  # EcoRegion mapping is critical and not available in SAG
  feasible$ecoregion_mapping <- FALSE
  
  # Assessment planning fields not in SAG
  feasible$assessment_planning <- FALSE
  
  # Species info partially available
  feasible$species_info <- FALSE  # SpeciesCommonName not directly available
  
  overall_feasible <- all(sapply(feasible[functionality], isTRUE))
  
  list(
    individual_feasibility = feasible,
    overall_feasible = overall_feasible,
    missing_critical = names(feasible)[sapply(feasible, function(x) !x)],
    recommendation = if (overall_feasible) {
      "Replacement feasible for requested functionality"
    } else {
      paste("Replacement not feasible. Missing:", paste(names(feasible)[sapply(feasible, function(x) !x)], collapse = ", "))
    }
  )
}