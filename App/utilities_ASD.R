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


  replace_na_with_na_string <- function(assessment_component) {
    if (is.na(assessment_component) || assessment_component == "NA") {
      return("N.A.")
    } else {
      return(assessment_component)
    }
  }


  normalise_component <- function(x) {
    x <- as.character(x)
    x[is.na(x) | x %in% c("", "N.A.", "N.A", "NA")] <- "NA"
    x
}



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


