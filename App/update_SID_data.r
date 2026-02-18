#' SID cache directory
#'
#' Path to the directory containing pre-built SID cache files in RDS format.
#' Each file is expected to be named `SID_<YEAR>.rds` (e.g., `SID_2024.rds`).
#'
#' @details
#' The SID cache is built offline and committed/deployed with the app. The deployed
#' Shiny app should only read these files; it should not attempt to write them.
#'
#' @format A length-1 character string.
#' @keywords internal
SID_CACHE_DIR <- "Data/SID_cache"


#' Read a SID cache snapshot for a given year
#'
#' Loads a pre-built SID snapshot from disk and returns it as a `data.table`.
#' The returned table is deduplicated to one row per (`StockKeyLabel`, `EcoRegion`).
#'
#' @param active_year Integer-like. The year corresponding to the cache file name
#'   `SID_<active_year>.rds`.
#' @param dir Character. Cache directory containing `SID_<YEAR>.rds` files.
#'   Defaults to [SID_CACHE_DIR].
#'
#' @return
#' A `data.table` containing the SID snapshot for `active_year`, with duplicate
#' rows removed using `unique(..., by = c("StockKeyLabel","EcoRegion"))`.
#' The schema depends on how the SID cache was built, but typically includes:
#' `StockKeyLabel`, `EcoRegion`, `YearOfLastAssessment`, `StockKeyDescription`,
#' plus app-specific fields such as `icon` and `stock_location` if those were added
#' at build time.
#'
#' @details
#' This function is "read-only". It does not attempt to rebuild or refresh caches.
#' If the cache file is missing, it errors with a clear message so the deployment
#' can be fixed by rebuilding/committing the missing year.
#'
#' The function enforces a stable multiplicity before downstream joins by
#' deduplicating on (`StockKeyLabel`, `EcoRegion`), preventing accidental
#' cartesian joins when later joining with SAG-derived rows.
#'
#' @examples
#' \dontrun{
#' sid_2024 <- read_SID_cache(2024)
#' sid_2024[1:5]
#' }
#'
#' @export
read_SID_cache <- function(active_year, dir = SID_CACHE_DIR) {
    p <- file.path(dir, sprintf("SID_%d.rds", as.integer(active_year)))
    if (!file.exists(p)) stop("Missing SID cache file: ", p)
    sid <- readRDS(p)
    setDT(sid)
    sid <- unique(sid, by = c("StockKeyLabel", "EcoRegion"))
    sid[]
}

#' App-facing SID accessor (metadata snapshot)
#'
#' Thin wrapper around [read_SID_cache()] used to keep the server code readable.
#'
#' @param active_year Integer-like. The year to load from the SID cache.
#'
#' @return A `data.table` as returned by [read_SID_cache()].
#'
#' @details
#' This is intentionally minimal; it exists mainly as a semantic alias
#' ("get SID metadata for this active year") and a single place to adjust
#' SID-reading behaviour if needed later.
#'
#' @examples
#' \dontrun{
#' sid <- getSID_meta(2023)
#' }
#'
#' @export
getSID_meta <- function(active_year) read_SID_cache(active_year)


#' Build the stock list table for the UI for a given active year
#'
#' Returns the app's stock list table by combining:
#' 1) SID snapshot rows (one row per stock per ecoregion), and
#' 2) a SAG-derived mapping of stock -> (AssessmentKey, AssessmentYear, Component),
#' filtered to stocks that have published advice in ASD for the active-year context.
#'
#' @param active_year Integer-like. The active year selected in the app.
#' @param asd_pub A `data.table` (or data.frame convertible to one) describing which
#'   ASD records are considered "published" for filtering purposes.
#'   Minimum expected columns:
#'   - `AssessmentKey` (integer-ish)
#'   Optional (enables fallback matching inside `filter_SAG_to_ASD_published`):
#'   - `StockKeyLabel` (character)
#'   - `AssessmentYear` (integer-ish)
#'
#' @return
#' A `data.table` with one row per (`StockKeyLabel`, `AssessmentComponent`, `EcoRegion`)
#' for stocks that are published in ASD. Columns are those from SID plus these
#' SAG-derived columns:
#' - `AssessmentKey`
#' - `AssessmentYear`
#' - `AssessmentComponent`
#'
#' If there are no published ASD rows (empty `asd_pub`), returns an empty table
#' with the same columns as SID (`sid[0]`).
#'
#' @details
#' The function uses [latest_sid_cache_year()] to decide whether `active_year`
#' matches the most recent SID cache. If it matches, it uses `getSAG_latest_map()`
#' (one SAG call) to obtain the map. Otherwise, it calls
#' `build_SAG_map_for_active_year(active_year, sid, is_latest = FALSE)` which
#' builds a historical SAG map based on assessment years implied by the SID snapshot.
#'
#' After filtering the SAG map to published ASD via `filter_SAG_to_ASD_published()`,
#' it collapses duplicates to the single most recent assessment per
#' (`StockKeyLabel`, `AssessmentComponent`) using `keep_latest_assessment_by_component()`.
#' This prevents accidental cartesian expansion when joining to the SID ecoregion rows.
#'
#' @seealso
#' [getSID_meta()], [latest_sid_cache_year()]
#'
#' @examples
#' \dontrun{
#' sid <- getSID_meta(2024)
#' # asd_pub should typically come from a cached ASD build:
#' # asd_pub <- unique(asd_cache[, .(AssessmentKey, StockKeyLabel, AssessmentYear)])
#' stock_list <- getStockList_for_active_year(2024, asd_pub)
#' }
#'
#' @export
getStockList_for_active_year <- function(active_year, asd_pub) {
    sid <- getSID_meta(active_year)
    if (is.null(asd_pub) || !nrow(asd_pub)) {
        return(sid[0])
    }

    latest <- latest_sid_cache_year()
    sag_map <- if (!is.na(latest) && as.integer(active_year) == latest) {
        getSAG_latest_map()
    } else {
        build_SAG_map_for_active_year(active_year, sid, is_latest = FALSE)
    }

    sag_pub <- filter_SAG_to_ASD_published(sag_map, asd_pub)
    if (!nrow(sag_pub)) {
        return(sid[0])
    }

    # KEY LINE: remove duplicates per Stock+Component
    sag_pub <- keep_latest_assessment_by_component(sag_pub)

    out <- sag_pub[sid, on = "StockKeyLabel", nomatch = 0, allow.cartesian = TRUE]
    data.table::setorder(out, StockKeyLabel, AssessmentComponent, EcoRegion)
    out[]
}



#' Find the most recent year present in the SID cache directory
#'
#' Scans [SID_CACHE_DIR] (or `dir`) for files named `SID_<YEAR>.rds` and returns
#' the maximum year found.
#'
#' @param dir Character. Directory to scan for SID cache files.
#'   Defaults to [SID_CACHE_DIR].
#'
#' @return
#' An integer scalar: the latest cached year, or `NA_integer_` if the directory
#' does not exist or contains no valid `SID_<YEAR>.rds` files.
#'
#' @details
#' This is used to decide whether `active_year` is the latest available SID snapshot.
#' When `active_year` equals the latest cached year, the app can use the faster
#' `getSAG_latest_map()` route (single SAG call) rather than building a historical map.
#'
#' @examples
#' \dontrun{
#' latest_sid_cache_year()
#' latest_sid_cache_year("Data/SID_cache")
#' }
#'
#' @export
latest_sid_cache_year <- function(dir = SID_CACHE_DIR) {
    if (!dir.exists(dir)) {
        return(NA_integer_)
    }
    files <- list.files(dir, pattern = "^SID_[0-9]{4}\\.rds$", full.names = FALSE)
    if (!length(files)) {
        return(NA_integer_)
    }
    yrs <- suppressWarnings(as.integer(sub("^SID_([0-9]{4})\\.rds$", "\\1", files)))
    yrs <- yrs[!is.na(yrs)]
    if (!length(yrs)) {
        return(NA_integer_)
    }
    max(yrs)
}


