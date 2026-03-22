# R/education_data.R
# Loads preprocessed Rwanda PHC Education gender-disaggregated CSVs.

load_education_data <- function() {

  # ---- logging ----
  debug_dir  <- file.path(getwd(), "logs")
  if (!dir.exists(debug_dir)) dir.create(debug_dir, showWarnings = FALSE, recursive = TRUE)
  debug_path <- file.path(debug_dir, "education_loader_debug.txt")
  logf <- function(...) {
    msg <- paste0(..., collapse = "")
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", msg, "\n",
        file = debug_path, append = TRUE)
  }

  # ---- search roots ----
  app_path <- tryCatch(shiny::getCurrentAppPath(), error = function(e) NULL)
  if (is.null(app_path) || !dir.exists(app_path)) {
    app_path <- tryCatch(getwd(), error = function(e) ".")
  }

  script_path <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
    error = function(e) NULL
  )
  script_dir   <- if (!is.null(script_path)) dirname(script_path) else NULL
  project_root <- if (!is.null(script_dir))  dirname(script_dir)  else NULL

  search_roots <- unique(c(app_path, getwd(), project_root))
  logf("[EDU] app_path=", app_path, " | wd=", getwd(), " | project_root=", project_root)

  # ---- locate clean CSV directory ----
  clean_dir <- tryCatch(
    find_dashboard_path(search_roots, c("Education", "clean"), is_dir = TRUE),
    error = function(e) NULL
  )
  logf("[EDU] clean_dir after find_dashboard_path=", ifelse(is.null(clean_dir), "NULL", clean_dir))

  # Direct path checks
  if (is.null(clean_dir)) {
    for (r in search_roots) {
      if (is.null(r) || is.na(r)) next
      for (dd in c("Dashboard_data", "dashboard_data")) {
        cand <- file.path(r, dd, "Education", "clean")
        if (dir.exists(cand)) { clean_dir <- cand; logf("[EDU] clean_dir direct: ", cand); break }
      }
      if (!is.null(clean_dir)) break
    }
  }

  # Filename-based fallback
  if (is.null(clean_dir)) {
    for (r in search_roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
      matches <- suppressWarnings(list.files(
        r0, pattern = "^edu_literacy_sex\\.csv$",
        recursive = TRUE, full.names = TRUE, ignore.case = TRUE
      ))
      if (length(matches) > 0) {
        clean_dir <- dirname(matches[[1]])
        logf("[EDU] clean_dir found by filename search: ", clean_dir)
        break
      }
    }
  }

  logf("[EDU] Final clean_dir=", ifelse(is.null(clean_dir), "NULL", clean_dir))
  if (is.null(clean_dir)) {
    message("[EDU] Could not locate Education clean CSV directory.")
    return(NULL)
  }

  # ---- helpers ----
  read_csv_safe <- function(fname) {
    paths <- c(
      file.path(clean_dir, fname),
      file.path(clean_dir, tolower(fname))
    )
    for (p in paths) {
      if (file.exists(p)) {
        logf("[EDU] Reading: ", p)
        return(readr::read_csv(p, show_col_types = FALSE))
      }
    }
    logf("[EDU] MISSING: ", fname)
    return(NULL)
  }

  safe_coerce <- function(fn, ...) tryCatch(fn(...), error = function(e) {
    logf("[EDU] coerce error: ", e$message); NULL
  })

  # ---- type coercion ----
  coerce_edu <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if ("pct"        %in% names(df)) df$pct        <- suppressWarnings(as.numeric(df$pct))
    if ("count"      %in% names(df)) df$count      <- suppressWarnings(as.numeric(df$count))
    if ("sex"        %in% names(df)) df$sex        <- trimws(as.character(df$sex))
    if ("area"       %in% names(df)) df$area       <- trimws(as.character(df$area))
    if ("level"      %in% names(df)) df$level      <- trimws(as.character(df$level))
    if ("year"       %in% names(df)) df$year       <- suppressWarnings(as.integer(df$year))
    if ("province"   %in% names(df)) df$province   <- trimws(as.character(df$province))
    if ("age_group"  %in% names(df)) df$age_group  <- trimws(as.character(df$age_group))
    if ("disability" %in% names(df)) df$disability <- trimws(as.character(df$disability))
    if ("phone_type" %in% names(df)) df$phone_type <- trimws(as.character(df$phone_type))
    df
  }

  coerce_literacy <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    for (cn in c("illiterate","literate","total","pct_literate",
                 "pct_no_longer","pct_currently","pct_never",
                 "pct_nat","pct_urb","pct_rur",
                 "count_nat","count_urb","count_rur",
                 "total","attending","pct")) {
      if (cn %in% names(df)) df[[cn]] <- suppressWarnings(as.numeric(df[[cn]]))
    }
    if ("sex"       %in% names(df)) df$sex       <- trimws(as.character(df$sex))
    if ("area"      %in% names(df)) df$area      <- trimws(as.character(df$area))
    if ("province"  %in% names(df)) df$province  <- trimws(as.character(df$province))
    if ("age_group" %in% names(df)) df$age_group <- trimws(as.character(df$age_group))
    df
  }

  # ---- load CSVs ----
  clean <- list(
    attainment_area       = safe_coerce(coerce_edu,      read_csv_safe("edu_attainment_area.csv")),
    attainment_trend      = safe_coerce(coerce_edu,      read_csv_safe("edu_attainment_trend.csv")),
    attainment_15plus     = safe_coerce(coerce_edu,      read_csv_safe("edu_attainment_15plus.csv")),
    attainment_disability = safe_coerce(coerce_edu,      read_csv_safe("edu_attainment_disability.csv")),
    attendance_status     = safe_coerce(coerce_literacy, read_csv_safe("edu_attendance_status.csv")),
    attendance_3to5       = safe_coerce(coerce_literacy, read_csv_safe("edu_attendance_3to5.csv")),
    attendance_6to17      = safe_coerce(coerce_literacy, read_csv_safe("edu_attendance_6to17.csv")),
    attendance_agegroup   = safe_coerce(coerce_literacy, read_csv_safe("edu_attendance_agegroup.csv")),
    literacy_sex          = safe_coerce(coerce_literacy, read_csv_safe("edu_literacy_sex.csv")),
    literacy_age          = safe_coerce(coerce_literacy, read_csv_safe("edu_literacy_age.csv")),
    ict_province          = safe_coerce(coerce_literacy, read_csv_safe("edu_ict_province.csv")),
    ict_count             = safe_coerce(coerce_literacy, read_csv_safe("edu_ict_count.csv")),
    mobile_phone          = safe_coerce(coerce_literacy, read_csv_safe("edu_mobile_phone.csv"))
  )

  required <- c("attainment_area", "attainment_trend", "literacy_sex", "attendance_agegroup")
  missing  <- required[sapply(clean[required], function(x) is.null(x) || nrow(x) == 0)]

  if (length(missing) == 0) {
    logf("[EDU] All required CSVs loaded. attainment_area=", nrow(clean$attainment_area),
         " literacy_sex=", nrow(clean$literacy_sex))
    return(clean)
  }

  logf("[EDU] Missing required tables: ", paste(missing, collapse = ", "))
  message("[EDU] Some Education CSVs missing: ", paste(missing, collapse = ", "))
  # Return what we have even if incomplete
  if (!all(sapply(clean, is.null))) return(clean)
  return(NULL)
}
