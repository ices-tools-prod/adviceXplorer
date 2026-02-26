# -----------------------------------------------------------------------------
# ASD: thin cache per year (published advice only), keyed by AssessmentKey
# -----------------------------------------------------------------------------

#' In-memory cache for ASD "thin year" queries
#'
#' A `cachem::cache_mem()` object used as the backend for memoising calls to
#' [getASD_thin_year()]. The cache is time-limited to avoid stale results while
#' still reducing repeated network/API calls during a Shiny session.
#'
#' @details
#' - `max_age = 24 * 3600` caches results for 24 hours.
#' - Stored values are the *thinned* ASD tables returned by [getASD_thin_year()].
#'
#' @keywords internal
asd_thin_year_cache <- cachem::cache_mem(max_age = 24 * 3600)


#' Fetch and thin ASD advice records for a single year (published advice only)
#'
#' Downloads ASD advice view records for a given year via `icesASD::getAdviceViewRecord()`,
#' filters to published advice rows, normalises key fields to stable types, and returns a
#' reduced set of columns used later in the app.
#'
#' @param y Integer-like. The year to query from ASD (passed to
#'   `icesASD::getAdviceViewRecord(year = ...)`).
#'
#' @return
#' A `data.table` containing (at minimum) the following columns:
#' - `AssessmentKey` (integer)
#' - `AssessmentYear` (integer)
#' - `StockKeyLabel` (character; from ASD `stockCode`)
#' - `adviceKey` (integer)
#' - `adviceSentence`, `adviceDOI`, `adviceValue`, `adviceComponent`
#' - `adviceReleasedDate`, `adviceApplicableFrom`, `adviceApplicableUntil`
#' - `adviceViewPublished` (logical)
#'
#' If ASD returns an empty/non-data.frame object (common for future years), or if
#' filtering leaves no rows, returns an empty `data.table()`.
#'
#' @details
#' This function is memoised using [asd_thin_year_cache] to prevent repeated API calls
#' for the same year within a session/time window.
#'
#' The function enforces stable types up-front (`assessmentKey`, `assessmentYear`,
#' `adviceKey` to integer) so that downstream ordering and joins behave predictably.
#'
#' @examples
#' \dontrun{
#' asd_2024 <- getASD_thin_year(2024)
#' unique(asd_2024$AssessmentKey)[1:10]
#' }
#'
#' @export
getASD_thin_year <- memoise::memoise(function(y) {
    dt <- icesASD::getAdviceViewRecord(year = as.integer(y))

    # Robust empty handling (ASD can return [] for future years)
    if (is.null(dt) || length(dt) == 0 || NROW(dt) == 0 || !is.data.frame(dt)) {
        return(data.table::data.table())
    }

    data.table::setDT(dt)

    # Keep only published advice rows
    dt <- dt[adviceViewPublished == TRUE & adviceStatus == "Advice"]
    if (nrow(dt) == 0) {
        return(data.table::data.table())
    }

    # Normalise types up-front
    dt[, assessmentKey := suppressWarnings(as.integer(assessmentKey))]
    dt[, assessmentYear := suppressWarnings(as.integer(assessmentYear))]
    dt[, adviceKey := suppressWarnings(as.integer(adviceKey))]

    # Keep only columns you want later (plus stockCode/assessmentYear for diagnostics)
    out <- dt[, .(
        AssessmentKey = assessmentKey,
        AssessmentYear = assessmentYear,
        StockKeyLabel = stockCode,
        adviceSentence = adviceSentence,
        adviceDOI = adviceDOI,
        adviceStatus = adviceStatus,
        adviceValue = adviceValue,
        adviceComponent = as.character(adviceComponent),
        adviceReleasedDate = adviceReleasedDate,
        adviceApplicableFrom = adviceApplicableFrom,
        adviceApplicableUntil = adviceApplicableUntil,
        adviceKey = adviceKey,
        adviceViewPublished = adviceViewPublished
    )]

    out <- out[!is.na(AssessmentKey) & !is.na(AssessmentYear)]
    out[]
}, cache = asd_thin_year_cache)


#' Build an ASD cache for the current active year using SID snapshot years
#'
#' Constructs a consolidated ASD table for an `active_year` by fetching thinned ASD
#' records for the assessment years implied by a SID snapshot (typically the unique
#' `YearOfLastAssessment` values present in the SID table), restricted to years
#' not exceeding `active_year`.
#'
#' @param active_year Integer-like. The year selected in the app.
#' @param sid_dt A SID snapshot `data.frame`/`data.table` containing a
#'   `YearOfLastAssessment` column. Used to determine which ASD years to fetch.
#'
#' @return
#' A `data.table` with published ASD advice records pooled across relevant years.
#' The table is keyed by `AssessmentKey` (via `setkey()`), enabling fast lookups
#' of all advice rows for a selected `AssessmentKey`.
#'
#' If no relevant years exist (e.g., SID has no valid `YearOfLastAssessment`),
#' or if no ASD records are available, returns an empty `data.table()`.
#'
#' @details
#' Steps:
#' 1. Derive relevant years from `sid_dt$YearOfLastAssessment`, keep `<= active_year`,
#'    and ensure `active_year` itself is included.
#' 2. Fetch each year via [getASD_thin_year()] and row-bind.
#' 3. Drop rows with `AssessmentYear > active_year` (safety guard).
#' 4. Remove exact duplicates (same AssessmentKey/Year/adviceKey/applicability window).
#' 5. Key the result by `AssessmentKey` for fast subset operations later in the server.
#'
#' @examples
#' \dontrun{
#' sid <- getSID_meta(2025)
#' asd_cache <- build_ASD_cache_for_active_year(2025, sid)
#' asd_cache[AssessmentKey == 19120]
#' }
#'

#' @export
build_ASD_cache_for_active_year <- function(active_year, sid_dt) {
    active_year <- as.integer(active_year)
    data.table::setDT(sid_dt)

    yrs <- sort(unique(as.integer(sid_dt$YearOfLastAssessment)))
    yrs <- yrs[!is.na(yrs) & yrs != 0 & yrs <= active_year]
    if (!(active_year %in% yrs)) yrs <- c(yrs, active_year)
    if (!length(yrs)) {
        return(data.table::data.table())
    }

    asd_all <- data.table::rbindlist(lapply(yrs, getASD_thin_year), fill = TRUE)
    if (!nrow(asd_all)) {
        return(asd_all)
    }

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



#' Pick the best ASD advice record for an active year (and optional component)
#'
#' Given a set of ASD advice rows (typically all rows for a single `AssessmentKey`),
#' select the single "best" record for the app's current `active_year`, optionally
#' constrained to an assessment/advice component.
#'
#' @param df A `data.frame`/`data.table` of ASD rows. In practice this is usually
#'   a subset such as `asd_cache[AssessmentKey == ...]`.
#' @param active_year Integer-like. The year selected in the app.
#' @param assessment_component Character or `NULL`. Component label used in the app UI.
#'   The UI uses `"NA"` where ASD often uses `"N.A."`.
#'
#' @return
#' A one-row tibble (or `NULL` if no suitable rows exist), corresponding to the
#' most relevant published advice record for `active_year` under the selection rules.
#'
#' @details
#' Filtering and ranking rules:
#' 1. Filter to `adviceViewPublished == TRUE` and `adviceStatus == "Advice"`.
#' 2. If `assessment_component` is provided:
#'    - Treat `"NA"` as `"N.A."` to match ASD conventions.
#'    - Prefer rows where `adviceComponent` matches; if no rows match, fall back to
#'      the unfiltered set (i.e., do not force an empty result when ASD has missing
#'      components).
#' 3. Use `active_date = <active_year>-07-01` as the reference date for selecting a
#'    record based on `adviceApplicableFrom`/`adviceApplicableUntil`:
#'    - Records whose applicability interval contains `active_date` are preferred.
#'    - Otherwise, prefer the nearest future interval start; past intervals are pushed
#'      behind future ones via a large penalty.
#' 4. Tie-breakers: later `until`, then higher `adviceKey`.
#'
#' This logic is designed to handle cases where ASD contains multiple advice rows for
#' the same `AssessmentKey` with different applicability windows (e.g., multi-year advice
#' for consecutive years).
#'
#' @examples
#' \dontrun{
#' out <- asd_cache[AssessmentKey == 19120]
#' pick_asd_record_for_year(out, active_year = 2025, assessment_component = "NA")
#' }
#'

#' @export
pick_asd_record_for_year <- function(df, active_year, assessment_component = NULL) {
    if (is.null(df) || !inherits(df, "data.frame") || nrow(df) == 0) {
        return(NULL)
    }

    df <- tibble::as_tibble(df) %>%
        dplyr::filter(.data$adviceViewPublished == TRUE, .data$adviceStatus == "Advice")

    if (nrow(df) == 0) {
        return(NULL)
    }

    # Component filter (ASD uses "N.A." where your UI uses "NA")
    comp <- assessment_component
    if (is.null(comp) || is.na(comp) || comp == "NA") comp <- "N.A."

    if (nzchar(comp)) {
        df_comp <- df %>%
            dplyr::filter(.data$adviceComponent == comp | (is.na(.data$adviceComponent) & comp == "N.A."))
        if (nrow(df_comp) > 0) df <- df_comp
    }
    if (nrow(df) == 0) {
        return(NULL)
    }

    active_date <- as.Date(sprintf("%d-07-01", as.integer(active_year)))

    df <- df %>%
        dplyr::mutate(
            from = as.Date(substr(.data$adviceApplicableFrom, 1, 10)),
            until = as.Date(substr(.data$adviceApplicableUntil, 1, 10)),
            contains_year = (is.na(.data$from) | .data$from <= active_date) &
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
                .data$is_future ~ .data$start_signed_dist, # 10 days ahead beats 100 days ahead
                TRUE ~ abs(.data$start_signed_dist) + 1e6 # push past intervals behind future ones
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



#' Convert UI component label to ASD component label
#'
#' The app UI uses `"NA"` for missing/blank component values, while ASD commonly uses
#' `"N.A."`. This helper converts UI values to the ASD convention for filtering.
#'
#' @param assessment_component Character scalar. Typically `"NA"` or a component string.
#'
#' @return A character scalar, where `"NA"` (or `NA`) is mapped to `"N.A."`.
#'
#' @examples
#' replace_na_with_na_string("NA")   # "N.A."
#' replace_na_with_na_string("2025") # "2025"
#'

#' @export
replace_na_with_na_string <- function(assessment_component) {
    if (is.na(assessment_component) || assessment_component == "NA") {
      return("N.A.")
    } else {
      return(assessment_component)
    }
  }

#' Normalise component labels to the app convention
#'
#' Standardises component values so downstream grouping/join logic is stable.
#' Specifically, blank values and ASD variants of "not applicable" are mapped to `"NA"`.
#'
#' @param x A vector of component labels (character/factor/numeric).
#'
#' @return A character vector with missing/blank/"N.A." variants mapped to `"NA"`.
#'
#' @details
#' This is used when building SAG maps and when deduplicating assessments by
#' (StockKeyLabel, AssessmentComponent), ensuring that component values behave as a
#' stable grouping key.
#'
#' @examples
#' #' normalise_component(c(NA, "", "N.A.", "2025"))
#'
#' @export
normalise_component <- function(x) {
    x <- as.character(x)
    x[is.na(x) | x %in% c("", "N.A.", "N.A", "NA")] <- "NA"
    x
}



#' Keep only the latest assessment per stock and component
#'
#' Deduplicates a table of assessment mappings so that, for each
#' (`StockKeyLabel`, `AssessmentComponent`) pair, only the most recent record is kept.
#'
#' @param dt A `data.frame`/`data.table` containing (at minimum):
#'   - `StockKeyLabel`
#'   - `AssessmentComponent`
#'   - `AssessmentYear`
#'   - `AssessmentKey`
#'
#' @return
#' A `data.table` with at most one row per (`StockKeyLabel`, `AssessmentComponent`),
#' selected by sorting descending `AssessmentYear` then descending `AssessmentKey`.
#'
#' @details
#' This function is critical before joining a per-stock mapping (SAG) to a per-ecoregion
#' SID table. Without deduplication, multiple assessment rows per stock/component can
#' create a cartesian expansion when joining to SID, producing duplicated UI rows and
#' inflated table sizes.
#'
#' The function normalises key columns to stable types and normalises component labels
#' via [normalise_component()] prior to ordering/grouping.
#'
#' @examples
#' \dontrun{
#' deduped <- keep_latest_assessment_by_component(sag_map)
#' }
#'

#' @export
keep_latest_assessment_by_component <- function(dt) {
    data.table::setDT(dt)
    if (!nrow(dt)) {
        return(dt)
    }

    dt[, StockKeyLabel := as.character(StockKeyLabel)]
    dt[, AssessmentKey := suppressWarnings(as.integer(AssessmentKey))]
    dt[, AssessmentYear := suppressWarnings(as.integer(AssessmentYear))]
    dt[, AssessmentComponent := normalise_component(AssessmentComponent)]

    dt <- dt[!is.na(StockKeyLabel) & nzchar(StockKeyLabel) & !is.na(AssessmentYear)]

    data.table::setorder(dt, StockKeyLabel, AssessmentComponent, -AssessmentYear, -AssessmentKey)
    dt <- dt[, .SD[1], by = .(StockKeyLabel, AssessmentComponent)]
    dt[]
}


