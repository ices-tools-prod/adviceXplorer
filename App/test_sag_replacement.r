#' Experimental function to test SAG latestStocks as potential SID replacement
#'
#' @param year the year required
#' @return comparison of data availability
#'
#' @note This function tests the feasibility of replacing SID webservice with SAG latestStocks
#' Network access required to ICES webservices
#'
test_sag_replacement <- function(year = 2024) {
  
  cat("Testing SAG latestStocks as SID replacement for year:", year, "\n\n")
  
  # Required fields from current SID implementation
  required_fields <- c(
    "StockKeyLabel", "StockKeyDescription", "SpeciesCommonName", 
    "EcoRegion", "DataCategory", "YearOfLastAssessment", 
    "AssessmentFrequency", "YearOfNextAssessment", "AssessmentKey"
  )
  
  # Test current SID webservice
  cat("1. Testing current SID webservice...\n")
  sid_data <- tryCatch({
    download_SID(year)
  }, error = function(e) {
    cat("   ✗ SID webservice failed:", e$message, "\n")
    NULL
  })
  
  if (!is.null(sid_data)) {
    cat("   ✓ SID webservice accessible\n")
    cat("   Records:", nrow(sid_data), "\n")
    cat("   Fields:", paste(names(sid_data), collapse = ", "), "\n")
  }
  
  # Test SAG latestStocks
  cat("\n2. Testing SAG latestStocks webservice...\n")
  sag_latest_data <- tryCatch({
    jsonlite::fromJSON("https://sag.ices.dk/SAG_API/api/LatestStocks")
  }, error = function(e) {
    cat("   ✗ SAG latestStocks failed:", e$message, "\n")
    NULL
  })
  
  if (!is.null(sag_latest_data)) {
    cat("   ✓ SAG latestStocks accessible\n")
    if (is.data.frame(sag_latest_data)) {
      cat("   Records:", nrow(sag_latest_data), "\n")
      cat("   Fields:", paste(names(sag_latest_data), collapse = ", "), "\n")
    } else if (is.list(sag_latest_data)) {
      cat("   Records:", length(sag_latest_data), "\n")
      if (length(sag_latest_data) > 0) {
        cat("   Fields:", paste(names(sag_latest_data[[1]]), collapse = ", "), "\n")
      }
    }
  }
  
  # Test SAG StockList (which is used in current code)
  cat("\n3. Testing SAG StockList (current SAG usage)...\n")
  sag_stocklist_data <- tryCatch({
    # This uses the icesSAG package function already in use
    if (requireNamespace("icesSAG", quietly = TRUE)) {
      icesSAG::getListStocks(year = year)
    } else {
      cat("   icesSAG package not available\n")
      NULL
    }
  }, error = function(e) {
    cat("   ✗ SAG StockList failed:", e$message, "\n")
    NULL
  })
  
  if (!is.null(sag_stocklist_data)) {
    cat("   ✓ SAG StockList accessible\n")
    cat("   Records:", nrow(sag_stocklist_data), "\n")
    cat("   Fields:", paste(names(sag_stocklist_data), collapse = ", "), "\n")
  }
  
  # Field compatibility analysis
  cat("\n4. Field Compatibility Analysis:\n")
  
  comparison_results <- list(
    sid_available = !is.null(sid_data),
    sag_latest_available = !is.null(sag_latest_data),
    sag_stocklist_available = !is.null(sag_stocklist_data),
    field_mapping = list()
  )
  
  # Check each required field
  for (field in required_fields) {
    cat("   ", field, ":\n")
    
    # Check SID
    sid_has = if (!is.null(sid_data)) field %in% names(sid_data) else FALSE
    cat("     SID:", if (sid_has) "✓" else "✗", "\n")
    
    # Check SAG latestStocks
    sag_latest_has = FALSE
    if (!is.null(sag_latest_data)) {
      if (is.data.frame(sag_latest_data) && field %in% names(sag_latest_data)) {
        sag_latest_has = TRUE
      } else if (is.list(sag_latest_data) && length(sag_latest_data) > 0 && field %in% names(sag_latest_data[[1]])) {
        sag_latest_has = TRUE
      }
    }
    cat("     SAG LatestStocks:", if (sag_latest_has) "✓" else "✗", "\n")
    
    # Check SAG StockList  
    sag_stocklist_has = if (!is.null(sag_stocklist_data)) field %in% names(sag_stocklist_data) else FALSE
    cat("     SAG StockList:", if (sag_stocklist_has) "✓" else "✗", "\n")
    
    comparison_results$field_mapping[[field]] <- list(
      sid = sid_has,
      sag_latest = sag_latest_has,
      sag_stocklist = sag_stocklist_has
    )
  }
  
  # Recommendation
  cat("\n5. Replacement Feasibility:\n")
  
  critical_fields <- c("StockKeyLabel", "EcoRegion", "AssessmentKey")
  missing_critical <- c()
  
  for (field in critical_fields) {
    if (!is.null(comparison_results$field_mapping[[field]])) {
      if (!comparison_results$field_mapping[[field]]$sag_latest && 
          !comparison_results$field_mapping[[field]]$sag_stocklist) {
        missing_critical <- c(missing_critical, field)
      }
    }
  }
  
  if (length(missing_critical) > 0) {
    cat("   ✗ REPLACEMENT NOT FEASIBLE\n")
    cat("   Missing critical fields:", paste(missing_critical, collapse = ", "), "\n")
    cat("   Recommendation: Keep SID webservice for metadata, use SAG for assessment data\n")
  } else {
    cat("   ✓ REPLACEMENT POTENTIALLY FEASIBLE\n")
    cat("   Recommendation: Detailed field mapping and testing required\n")
  }
  
  return(comparison_results)
}

#' Alternative hybrid approach function
#'
#' This function demonstrates how to use both SID and SAG optimally
#'
get_combined_stock_data <- function(year = 2024) {
  
  cat("Implementing hybrid SID + SAG approach...\n")
  
  # Get SID data for metadata and ecoregions
  sid_data <- tryCatch({
    download_SID(year)
  }, error = function(e) {
    cat("Warning: SID data unavailable:", e$message, "\n")
    NULL
  })
  
  # Get SAG data for assessment information
  sag_data <- tryCatch({
    if (requireNamespace("icesSAG", quietly = TRUE)) {
      icesSAG::getListStocks(year = year)
    } else {
      NULL
    }
  }, error = function(e) {
    cat("Warning: SAG data unavailable:", e$message, "\n")
    NULL
  })
  
  if (!is.null(sid_data) && !is.null(sag_data)) {
    cat("✓ Both data sources available - merging optimal data\n")
    
    # Merge keeping the best fields from each source
    # SID provides: EcoRegion, DataCategory, AssessmentFrequency, YearOfNextAssessment
    # SAG provides: More recent AssessmentKey, detailed assessment info
    
    combined_data <- merge(
      sid_data %>% select(StockKeyLabel, EcoRegion, DataCategory, 
                         AssessmentFrequency, YearOfNextAssessment),
      sag_data %>% select(StockKeyLabel, AssessmentKey, AssessmentYear, 
                         Purpose, ModifiedDate),
      by = "StockKeyLabel",
      all.x = TRUE
    )
    
    cat("✓ Combined", nrow(combined_data), "records with optimal field selection\n")
    return(combined_data)
    
  } else if (!is.null(sid_data)) {
    cat("→ Using SID data only\n") 
    return(sid_data)
    
  } else if (!is.null(sag_data)) {
    cat("→ Using SAG data only (missing ecoregion info)\n")
    return(sag_data)
    
  } else {
    cat("✗ No data sources available\n")
    return(NULL)
  }
}