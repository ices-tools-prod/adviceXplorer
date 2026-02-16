
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


