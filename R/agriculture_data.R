# ── R/agriculture_data.R ──────────────────────────────────────────────────────
# Loads 10 cleaned CSVs from Dashboard_data/Agriculture/clean/
# Returns a named list of dataframes.

load_agriculture_data <- function() {

  logf <- function(...) message(sprintf(...))

  # ── resolve base path ───────────────────────────────────────────────────────
  candidates <- c(
    file.path(getwd(), "Dashboard_data", "Agriculture", "clean"),
    file.path(dirname(sys.frame(1)$ofile %||% ""), "..",
              "Dashboard_data", "Agriculture", "clean"),
    "/srv/shiny-server/Dashboard_data/Agriculture/clean"
  )
  base <- NULL
  for (p in candidates) {
    if (dir.exists(p)) { base <- normalizePath(p, mustWork = FALSE); break }
  }
  if (is.null(base)) {
    logf("[AGRI] clean/ directory not found – trying filename search")
    hits <- list.files(".", pattern = "agri_hh_summary\\.csv$",
                       recursive = TRUE, full.names = TRUE)
    if (length(hits)) base <- dirname(hits[1])
  }
  if (is.null(base)) stop("[AGRI] Cannot locate Agriculture/clean directory")
  logf("[AGRI] Loading from: %s", base)

  # ── safe CSV reader ─────────────────────────────────────────────────────────
  read_csv_safe <- function(fname) {
    path <- file.path(base, fname)
    if (!file.exists(path)) {
      # case-insensitive fallback
      all_files <- list.files(base, full.names = TRUE)
      hit <- all_files[tolower(basename(all_files)) == tolower(fname)]
      if (length(hit)) path <- hit[1] else {
        logf("[AGRI] Missing: %s", fname); return(NULL)
      }
    }
    tryCatch(
      readr::read_csv(path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")),
      error = function(e) { logf("[AGRI] read error %s: %s", fname, e$message); NULL }
    )
  }

  # ── load all tables ─────────────────────────────────────────────────────────
  hh_summary      <- read_csv_safe("agri_hh_summary.csv")
  land_ownership  <- read_csv_safe("agri_land_ownership.csv")
  land_access     <- read_csv_safe("agri_land_access.csv")
  land_rights     <- read_csv_safe("agri_land_rights.csv")
  extension       <- read_csv_safe("agri_extension.csv")
  community_groups<- read_csv_safe("agri_community_groups.csv")
  inputs          <- read_csv_safe("agri_inputs.csv")
  workers_trend   <- read_csv_safe("agri_workers_trend.csv")
  girinka         <- read_csv_safe("agri_girinka.csv")
  livestock       <- read_csv_safe("agri_livestock.csv")

  # ── coerce types ────────────────────────────────────────────────────────────
  coerce_num <- function(df, cols) {
    for (col in cols) if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    df
  }
  coerce_chr <- function(df, cols) {
    for (col in cols) if (col %in% names(df))
      df[[col]] <- trimws(as.character(df[[col]]))
    df
  }

  if (!is.null(hh_summary))
    hh_summary <- coerce_num(hh_summary, c("total","male_headed","female_headed"))

  if (!is.null(land_ownership))
    land_ownership <- coerce_num(land_ownership, c("pct_2018","pct_2021"))

  if (!is.null(land_access))
    land_access <- coerce_num(land_access, c("own_land","rented_land","complemented"))

  if (!is.null(land_rights))
    land_rights <- coerce_num(land_rights, c("rwanda","male","female"))

  if (!is.null(extension))
    extension <- coerce_num(extension, c("total_pct","female_pct","male_pct"))

  if (!is.null(community_groups))
    community_groups <- coerce_num(community_groups,
                                   c("cooperatives","twigire_muhinzi","farmer_field_school"))

  if (!is.null(inputs))
    inputs <- coerce_num(inputs, c("improved_seeds","organic_fert","inorganic_fert","pesticides"))

  if (!is.null(workers_trend)) {
    workers_trend <- coerce_chr(workers_trend, c("worker_type","sex"))
    workers_trend <- coerce_num(workers_trend, c("year","pct"))
  }

  if (!is.null(girinka))
    girinka <- coerce_num(girinka, c("rwanda","male","female"))

  if (!is.null(livestock))
    livestock <- coerce_num(livestock, c("total","male_headed","female_headed"))

  logf("[AGRI] Data ready.")
  list(
    hh_summary       = hh_summary,
    land_ownership   = land_ownership,
    land_access      = land_access,
    land_rights      = land_rights,
    extension        = extension,
    community_groups = community_groups,
    inputs           = inputs,
    workers_trend    = workers_trend,
    girinka          = girinka,
    livestock        = livestock
  )
}
