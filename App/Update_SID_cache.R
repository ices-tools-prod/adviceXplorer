# Choose a cache dir inside your repo so it gets deployed with the app
SID_CACHE_DIR <- "App/Data/SID_cache"

sid_cache_dir <- function(dir = SID_CACHE_DIR) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

sid_cache_path <- function(year, dir = SID_CACHE_DIR, ext = "rds") {
  dir <- sid_cache_dir(dir)
  file.path(dir, sprintf("SID_%d.%s", as.integer(year), ext))
}

# --- 1) Build the SID table in the shape your app expects (fast joins later)
build_SID_table <- function(year) {
  sid <- download_SID(year) |>
    separate_ecoregions()

  data.table::setDT(sid)

  sid[, icon := sprintf(
  '<img src="%s" height="40">',
  match_stockcode_to_illustration_offline(StockKeyLabel, disk_www_dir = "App/www", web_prefix = "")
)]
  # Location (also stored in cache)
  sid[, stock_location := parse_location_from_stock_description(StockKeyDescription)]

  # Keep meaningful last assessment info (as you used historically)
  sid <- sid[!is.na(YearOfLastAssessment) & YearOfLastAssessment != 0]

  # Stable dedupe (prevents accidental multiplicity)
  sid <- unique(sid, by = c("StockKeyLabel", "EcoRegion"))

  sid[]
}

# --- 2) Save SID locally (run this on your machine before deploying)
save_SID_cache <- function(year, dir = SID_CACHE_DIR, force = FALSE) {
  year <- as.integer(year)
  out  <- sid_cache_path(year, dir = dir, ext = "rds")

  if (!force && file.exists(out)) return(invisible(out))

  sid <- build_SID_table(year)

  # compress=FALSE => much faster read on shinyapps; bigger file, but manageable
  saveRDS(as.data.frame(sid), out, compress = FALSE)

  message("Saved SID cache: ", out, " (rows=", nrow(sid), ")")
  invisible(out)
}

# --- 3) Bulk builder: generate all years you want, commit Data/SID_cache/*
save_SID_cache_many <- function(years, dir = SID_CACHE_DIR, force = FALSE) {
  years <- unique(as.integer(years))
  years <- years[!is.na(years)]
  paths <- vapply(years, function(y) save_SID_cache(y, dir = dir, force = force), character(1))
  invisible(paths)
}