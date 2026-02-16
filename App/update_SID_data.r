#' Formats, updates and save the stock list table. It also adds the hyperlinks to the table.
#'
#' @param year the year required
#'
#' @return stock list table
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'update_SID(2021)
#' }
#'
#' @references
#'
#' https://sid.ices.dk/Default.aspx
#'
#' @export
#'

# getSID <- function(year) {
#     message("Downloading SID data for year: ", year)
#     stock_list_all <- download_SID(year)

#     # Convert 1 row per ecoregion
#     stock_list_long <- separate_ecoregions(stock_list_all)

#     # # Add icons using stock illustrations
#     # stock_list_long$icon <- paste0(
#     #     '<img src="', match_stockcode_to_illustration(stock_list_long$StockKeyLabel, stock_list_long), '" height=40>'
#     # )
#     # Add icons using stock illustrations
#     setDT(stock_list_long)
#     stock_list_long[, icon := paste0('<img src="', match_stockcode_to_illustration(StockKeyLabel, stock_list_long), '" height=40>')]

    
#     # Get unique valid years (excluding NA and 0)
#     valid_years <- unique(stock_list_long$YearOfLastAssessment)
#     valid_years <- valid_years[!is.na(valid_years) & valid_years != 0]

    
#     # Parallelized API calls for ASD records
#     ASDList <- rbindlist(future_lapply(valid_years, function(y) {
#         message("Fetching ASD advice records for year: ", y)
#         as.data.table(icesASD::getAdviceViewRecord(year = y))
#     }), fill = TRUE)
    
#     ASDList <- ASDList %>% group_by(stockCode) %>% filter(assessmentYear == max(assessmentYear, na.rm = TRUE, finite = TRUE)) %>% ungroup()
#     browser()
#     names(ASDList[,1])
#     # Ensure ASDList is a valid data frame
#     if (is.null(ASDList) || identical(ASDList, list()) || nrow(ASDList) == 0) {
#         ASDList <- data.frame(
#             StockKeyLabel = character(),
#             AssessmentKey = character(),
#             AssessmentComponent = character(),
#             stringsAsFactors = FALSE
#         )
#     } else {
#         ASDList <- ASDList %>%
#             mutate(adviceComponent = na_if(adviceComponent, "N.A.")) %>%
#             rename(
#                 StockKeyLabel = stockCode,
#                 AssessmentKey = assessmentKey,
#                 AssessmentComponent = adviceComponent
#             ) %>%
#             filter(adviceStatus == "Advice")
#     }
#     setDT(ASDList)

#     # Efficient merge using data.table
#     stock_list_long <- ASDList[stock_list_long, on = "StockKeyLabel"]
#     # Merge stock list with ASDList
#     message("Merging SID and ASD records...")
   
#     # Filter out rows where AssessmentKey is NA and YearOfLastAssessment is NA or 0
#     missing_keys <- which(!is.na(stock_list_long$AssessmentKey) &
#         !is.na(stock_list_long$YearOfLastAssessment) &
#         stock_list_long$YearOfLastAssessment != 0)

#     stock_list_long <- stock_list_long[missing_keys,]

    

#     stock_list_long[, stock_location := parse_location_from_stock_description(StockKeyDescription)]


#     message("Data processing complete.")
#     return(stock_list_long)
# }
#' Updates the data used to run the app
#'
#' @param mode the mode used to update the data
#'
#' @return downloads and save the data to the respective folders
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'UpdateDataApp(mode = "LatestYear")
#' }
#'
#' @references
#'
#' https://sid.ices.dk/Default.aspx
#'
#' @export
#'
# UpdateDataApp <- function(mode = c("AllYears", "LatestYear")) {
#     if (mode == "AllYears") {
#         years <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017)
#     } else if (mode == "LatestYear") {
#         years <- as.integer(format(Sys.Date(), "%Y"))
#     }

#     for (year in years) {
#         update_SAG(year)
#         update_SID(year)
#     }
# }


SID_CACHE_DIR <- "Data/SID_cache"

# -----------------------------------------------------------------------------
# SID: read local cache (as before)
# -----------------------------------------------------------------------------
read_SID_cache <- function(active_year, dir = SID_CACHE_DIR) {
  p <- file.path(dir, sprintf("SID_%d.rds", as.integer(active_year)))
  if (!file.exists(p)) stop("Missing SID cache file: ", p)
  sid <- readRDS(p)
  setDT(sid)
  sid <- unique(sid, by = c("StockKeyLabel", "EcoRegion"))
  sid[]
}

getSID_meta <- function(active_year) read_SID_cache(active_year)

# -----------------------------------------------------------------------------
# ASD: published index (do NOT trust components here)
# -----------------------------------------------------------------------------
# asd_year_cache <- cachem::cache_mem(max_age = 24 * 3600)

# getASD_published_year <- memoise(function(y) {
#   dt <- icesASD::getAdviceViewRecord(year = y)

#   # Handle [] / empty returns robustly
#   if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) {
#     return(data.table())
#   }

#   setDT(dt)
#   dt <- dt[adviceViewPublished == TRUE & adviceStatus == "Advice"]

#   if (nrow(dt) == 0) return(data.table())

#   # Keep only what we use for existence checks / later ASD fetch
#   out <- dt[, .(
#     StockKeyLabel  = stockCode,
#     AssessmentKey  = suppressWarnings(as.integer(assessmentKey)),
#     AssessmentYear = suppressWarnings(as.integer(assessmentYear)),
#     adviceKey      = suppressWarnings(as.integer(adviceKey))
#   )]

#   out <- out[!is.na(StockKeyLabel) & nzchar(StockKeyLabel) &
#              !is.na(AssessmentYear)]
#   # assessmentKey can be NA in edge cases; keep row anyway for fallback matching
#   out[]
# }, cache = asd_year_cache)

# For an active year, we need ASD rows possibly spanning multiple assessmentYears
# build_ASD_published_index_for_active_year <- function(active_year, sid_dt) {
#   active_year <- as.integer(active_year)
#   setDT(sid_dt)

#   yrs <- sort(unique(as.integer(sid_dt$YearOfLastAssessment)))
#   yrs <- yrs[!is.na(yrs) & yrs != 0 & yrs <= active_year]
#   if (!(active_year %in% yrs)) yrs <- c(yrs, active_year)
#   if (length(yrs) == 0) return(data.table())

#   asd_all <- rbindlist(lapply(yrs, getASD_published_year), fill = TRUE)
#   if (nrow(asd_all) == 0) return(asd_all)

#   # keep <= active_year
#   asd_all <- asd_all[AssessmentYear <= active_year]

#   # Dedupe: if multiple ASD rows exist, keep latest AssessmentYear then highest adviceKey
#   setorder(asd_all, StockKeyLabel, -AssessmentYear, -adviceKey)
#   asd_all <- unique(asd_all, by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear"))

#   asd_all[]
# }

# -----------------------------------------------------------------------------
# SAG: components + assessment year + assessmentKey (source of truth for component)
# -----------------------------------------------------------------------------
sag_cache <- cachem::cache_mem(max_age = 12 * 3600)

# Latest map (one call)
getSAG_latest_map <- memoise(function() {
  dt <- icesSAG::getLatestStockAdviceList()
  if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) return(data.table())
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


  unique(out, by = c("StockKeyLabel","AssessmentKey","AssessmentYear","AssessmentComponent"))
}, cache = sag_cache)

# Historical by year (one call per year; cached)
getSAG_year_map <- memoise(function(y) {
  dt <- icesSAG::getListStocks(year = as.integer(y))
  if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) return(data.table())
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


  unique(out, by = c("StockKeyLabel","AssessmentKey","AssessmentYear","AssessmentComponent"))
}, cache = sag_cache)

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
  if (length(yrs) == 0) return(data.table())

  sag_all <- rbindlist(lapply(yrs, getSAG_year_map), fill = TRUE)
  sag_all <- sag_all[AssessmentYear <= active_year]
  unique(sag_all, by = c("StockKeyLabel","AssessmentKey","AssessmentYear","AssessmentComponent"))
}

# -----------------------------------------------------------------------------
# Combine: keep only stocks published in ASD, but keep SAG components
# -----------------------------------------------------------------------------
# Match priority:
# 1) by AssessmentKey (best)
# 2) fallback by StockKeyLabel + AssessmentYear (when ASD assessmentKey missing/odd)
filter_SAG_to_ASD_published <- function(sag_dt, asd_dt) {
  setDT(sag_dt); setDT(asd_dt)

  if (nrow(sag_dt) == 0 || nrow(asd_dt) == 0) return(sag_dt[0])

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
                       by = c("StockKeyLabel","AssessmentKey","AssessmentYear","AssessmentComponent"))
  } else {
    sag_keep <- unique(sag1, by = c("StockKeyLabel","AssessmentKey","AssessmentYear","AssessmentComponent"))
  }

  sag_keep[]
}

# -----------------------------------------------------------------------------
# Final stock list for UI: SID rows (ecoregions) + SAG cols, only if ASD published
# -----------------------------------------------------------------------------
# getStockList_for_active_year <- function(active_year, asd_pub) {
#     sid <- getSID_meta(active_year)

#     # ASD is now PROVIDED, not built here
#     if (is.null(asd_pub) || nrow(asd_pub) == 0) {
#         return(sid[0])
#     }

#     latest <- latest_sid_cache_year()
#     sag_map <- if (!is.na(latest) && as.integer(active_year) == latest) {
#         getSAG_latest_map()
#     } else {
#         build_SAG_map_for_active_year(active_year, sid, is_latest = FALSE)
#     }

#     sag_pub <- filter_SAG_to_ASD_published(sag_map, asd_pub)
#     if (nrow(sag_pub) == 0) {
#         return(sid[0])
#     }

#     # IMPORTANT: collapse to one row per stock BEFORE joining to SID
#     sag_pub <- keep_latest_assessment(sag_pub, by_component = FALSE)
#     # (by_component=FALSE means "one per StockKeyLabel"; TRUE would keep one per component)

#     out <- sag_pub[sid, on = "StockKeyLabel", nomatch = 0, allow.cartesian = TRUE]
#     out[]
# }

getStockList_for_active_year <- function(active_year, asd_pub) {
  sid <- getSID_meta(active_year)
  if (is.null(asd_pub) || !nrow(asd_pub)) return(sid[0])

  latest <- latest_sid_cache_year()
  sag_map <- if (!is.na(latest) && as.integer(active_year) == latest) {
    getSAG_latest_map()
  } else {
    build_SAG_map_for_active_year(active_year, sid, is_latest = FALSE)
  }

  sag_pub <- filter_SAG_to_ASD_published(sag_map, asd_pub)
  if (!nrow(sag_pub)) return(sid[0])

  # KEY LINE: remove duplicates per Stock+Component
  sag_pub <- keep_latest_assessment_by_component(sag_pub)

  out <- sag_pub[sid, on = "StockKeyLabel", nomatch = 0, allow.cartesian = TRUE]
  data.table::setorder(out, StockKeyLabel, AssessmentComponent, EcoRegion)
  out[]
}




# # -----------------------------------------------------------------------------
# # SID local cache (build once locally, then deployed app reads only)
# # -----------------------------------------------------------------------------



# # --- 4) Read SID from local cache (this is what you use in the deployed app)
# read_SID_cache <- function(year, dir = SID_CACHE_DIR) {
#   year <- as.integer(year)
#   p <- sid_cache_path(year, dir = dir, ext = "rds")
#   if (!file.exists(p)) stop("Missing SID cache file: ", p)

#   sid <- readRDS(p)
#   data.table::setDT(sid)
#   sid[]
# }

# # --- 5) App-facing wrapper: "local first", but never tries to write on shinyapps
# # (So dev works even if you forget to build one year locally.)
# getSID_local_first <- function(year, dir = SID_CACHE_DIR) {
#   year <- as.integer(year)
#   p <- sid_cache_path(year, dir = dir, ext = "rds")

#   if (file.exists(p)) return(read_SID_cache(year, dir = dir))

#   # If missing: fail with a clear message (recommended for shinyapps)
#   stop(
#     "SID cache missing for year=", year, ".\n",
#     "Run save_SID_cache(", year, ") locally, commit Data/SID_cache/, and redeploy."
#   )
# }

# # Example: build 2017..current year
# years <- 2017:as.integer(format(Sys.Date(), "%Y"))
# save_SID_cache_many(years, force = TRUE)

latest_sid_cache_year <- function(dir = SID_CACHE_DIR) {
  if (!dir.exists(dir)) return(NA_integer_)
  files <- list.files(dir, pattern = "^SID_[0-9]{4}\\.rds$", full.names = FALSE)
  if (!length(files)) return(NA_integer_)
  yrs <- suppressWarnings(as.integer(sub("^SID_([0-9]{4})\\.rds$", "\\1", files)))
  yrs <- yrs[!is.na(yrs)]
  if (!length(yrs)) return(NA_integer_)
  max(yrs)
}

# # Keep only the most recent assessment per stock (optionally per component)
# keep_latest_assessment <- function(dt, by_component = TRUE) {
#   data.table::setDT(dt)
#   if (nrow(dt) == 0) return(dt)

#   # enforce types to make ordering stable
#   dt[, AssessmentYear := suppressWarnings(as.integer(AssessmentYear))]
#   dt[, AssessmentKey  := suppressWarnings(as.integer(AssessmentKey))]
#   dt[, AssessmentComponent := as.character(AssessmentComponent)]

#   # drop rows with missing year (cannot rank)
#   dt <- dt[!is.na(AssessmentYear)]

#   if (by_component) {
#     data.table::setorder(dt, StockKeyLabel, AssessmentComponent, -AssessmentYear, -AssessmentKey)
#     dt <- dt[, .SD[1], by = .(StockKeyLabel, AssessmentComponent)]
#   } else {
#     data.table::setorder(dt, StockKeyLabel, -AssessmentYear, -AssessmentKey)
#     dt <- dt[, .SD[1], by = .(StockKeyLabel)]
#   }

#   dt[]
# }


normalise_component <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x %in% c("", "N.A.", "N.A", "NA")] <- "NA"
  x
} 



keep_latest_assessment_by_component <- function(dt) {
  data.table::setDT(dt)
  if (!nrow(dt)) return(dt)

  dt[, StockKeyLabel := as.character(StockKeyLabel)]
  dt[, AssessmentKey := suppressWarnings(as.integer(AssessmentKey))]
  dt[, AssessmentYear := suppressWarnings(as.integer(AssessmentYear))]
  dt[, AssessmentComponent := normalise_component(AssessmentComponent)]

  dt <- dt[!is.na(StockKeyLabel) & nzchar(StockKeyLabel) & !is.na(AssessmentYear)]

  data.table::setorder(dt, StockKeyLabel, AssessmentComponent, -AssessmentYear, -AssessmentKey)
  dt <- dt[, .SD[1], by = .(StockKeyLabel, AssessmentComponent)]
  dt[]
}


# -----------------------------------------------------------------------------
# ASD: thin cache per year (published advice only), keyed by AssessmentKey
# -----------------------------------------------------------------------------
asd_thin_year_cache <- cachem::cache_mem(max_age = 24 * 3600)

getASD_thin_year <- memoise::memoise(function(y) {
  dt <- icesASD::getAdviceViewRecord(year = as.integer(y))

  # Robust empty handling (ASD can return [] for future years)
  if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) {
    return(data.table::data.table())
  }

  data.table::setDT(dt)

  # Keep only published advice rows
  dt <- dt[adviceViewPublished == TRUE & adviceStatus == "Advice"]
  if (nrow(dt) == 0) return(data.table::data.table())

  # Normalise types up-front
  dt[, assessmentKey  := suppressWarnings(as.integer(assessmentKey))]
  dt[, assessmentYear := suppressWarnings(as.integer(assessmentYear))]
  dt[, adviceKey      := suppressWarnings(as.integer(adviceKey))]

  # Keep only columns you want later (plus stockCode/assessmentYear for diagnostics)
  out <- dt[, .(
    AssessmentKey      = assessmentKey,
    AssessmentYear     = assessmentYear,
    StockKeyLabel      = stockCode,
    adviceSentence     = adviceSentence,
    adviceDOI          = adviceDOI,
    adviceStatus       = adviceStatus,
    adviceValue        = adviceValue,
    adviceComponent       = as.character(adviceComponent),
    adviceReleasedDate = adviceReleasedDate,
    adviceApplicableFrom = adviceApplicableFrom,
    adviceApplicableUntil = adviceApplicableUntil,
    adviceKey          = adviceKey,
    adviceViewPublished = adviceViewPublished
  )]

  out <- out[!is.na(AssessmentKey) & !is.na(AssessmentYear)]
  out[]
}, cache = asd_thin_year_cache)

# build_ASD_cache_for_active_year <- function(active_year, sid_dt) {
#   active_year <- as.integer(active_year)
#   data.table::setDT(sid_dt)

#   yrs <- sort(unique(as.integer(sid_dt$YearOfLastAssessment)))
#   yrs <- yrs[!is.na(yrs) & yrs != 0 & yrs <= active_year]
#   if (!(active_year %in% yrs)) yrs <- c(yrs, active_year)
#   if (!length(yrs)) return(data.table::data.table())

#   asd_all <- data.table::rbindlist(lapply(yrs, function(y) {
#     dt <- icesASD::getAdviceViewRecord(year = y)

#     if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) {
#       return(data.table::data.table())
#     }
#     data.table::setDT(dt)

#     # Keep only what your app cares about, but KEEP the later-needed fields
#     dt <- dt[adviceViewPublished == TRUE & adviceStatus == "Advice"]

#     if (!nrow(dt)) return(data.table::data.table())

#     dt[, .(
#       StockKeyLabel       = stockCode,
#       AssessmentKey       = suppressWarnings(as.integer(assessmentKey)),
#       AssessmentYear      = suppressWarnings(as.integer(assessmentYear)),
#       adviceKey           = suppressWarnings(as.integer(adviceKey)),
#       adviceSentence      = adviceSentence,
#       adviceDOI           = adviceDOI,
#       adviceStatus        = adviceStatus,
#       adviceValue         = adviceValue,
#       adviceReleasedDate  = adviceReleasedDate,
#       adviceApplicableFrom = adviceApplicableFrom,
#       adviceApplicableUntil = adviceApplicableUntil,
#       adviceComponent     = adviceComponent,
#       adviceViewPublished = adviceViewPublished
#     )]
#   }), fill = TRUE)

#   if (!nrow(asd_all)) return(asd_all)

#   # de-dupe: pick latest record per assessmentKey (or per stock/year if you prefer)
#   data.table::setorder(asd_all, AssessmentKey, -AssessmentYear, -adviceKey)
#   asd_all <- asd_all[!is.na(AssessmentKey)]
#   asd_all <- asd_all[, .SD[1], by = .(AssessmentKey)]

#   data.table::setkey(asd_all, AssessmentKey)
#   asd_all[]
# }
build_ASD_cache_for_active_year <- function(active_year, sid_dt) {
  active_year <- as.integer(active_year)
  data.table::setDT(sid_dt)

  yrs <- sort(unique(as.integer(sid_dt$YearOfLastAssessment)))
  yrs <- yrs[!is.na(yrs) & yrs != 0 & yrs <= active_year]
  if (!(active_year %in% yrs)) yrs <- c(yrs, active_year)
  if (!length(yrs)) return(data.table::data.table())

  asd_all <- data.table::rbindlist(lapply(yrs, getASD_thin_year), fill = TRUE)
  if (!nrow(asd_all)) return(asd_all)

  # Safety
  asd_all <- asd_all[AssessmentYear <= active_year]

  # Optional: remove true duplicates only
  data.table::setorder(asd_all, AssessmentKey, AssessmentYear, adviceKey)
  asd_all <- unique(
    asd_all,
    by = c("AssessmentKey", "AssessmentYear", "adviceKey", "adviceApplicableFrom", "adviceApplicableUntil")
  )

  # Fast lookup later
  data.table::setkey(asd_all, AssessmentKey)
  asd_all[]
}



pick_asd_record_for_year <- function(df, active_year, assessment_component = NULL) {
  if (is.null(df) || !inherits(df, "data.frame") || nrow(df) == 0) return(NULL)

  df <- tibble::as_tibble(df) %>%
    dplyr::filter(.data$adviceViewPublished == TRUE, .data$adviceStatus == "Advice")

  if (nrow(df) == 0) return(NULL)

  # Component filter (ASD uses "N.A." where your UI uses "NA")
  comp <- assessment_component
  if (is.null(comp) || is.na(comp) || comp == "NA") comp <- "N.A."

  if (nzchar(comp)) {
    df_comp <- df %>%
      dplyr::filter(.data$adviceComponent == comp | (is.na(.data$adviceComponent) & comp == "N.A."))
    if (nrow(df_comp) > 0) df <- df_comp
  }
  if (nrow(df) == 0) return(NULL)

  active_date <- as.Date(sprintf("%d-07-01", as.integer(active_year)))

  df <- df %>%
    dplyr::mutate(
      from  = as.Date(substr(.data$adviceApplicableFrom,  1, 10)),
      until = as.Date(substr(.data$adviceApplicableUntil, 1, 10)),

      contains_year = (is.na(.data$from)  | .data$from  <= active_date) &
                      (is.na(.data$until) | active_date <= .data$until),

      # signed distance: positive = starts after active_date; negative = starts before
      start_signed_dist = as.numeric(.data$from - active_date),

      # prefer future intervals when not contained
      is_future = dplyr::if_else(is.na(.data$from), FALSE, .data$start_signed_dist >= 0),

      # ranking metric:
      # - if future: smaller positive distance is better
      # - if past:  larger (closer to zero) negative distance is better
      start_rank = dplyr::case_when(
        .data$contains_year ~ 0,
        .data$is_future ~ .data$start_signed_dist,          # 10 days ahead beats 100 days ahead
        TRUE ~ abs(.data$start_signed_dist) + 1e6           # push past intervals behind future ones
      )
    )

  df %>%
    dplyr::arrange(
      dplyr::desc(.data$contains_year),
      .data$start_rank,
      dplyr::desc(.data$until),
      dplyr::desc(.data$adviceKey)
    ) %>%
    dplyr::slice(1)
}

