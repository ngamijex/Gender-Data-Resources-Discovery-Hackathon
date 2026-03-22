# R/employment_data.R
# Loads preprocessed Rwanda LFS Employment gender-disaggregated CSVs.
# Falls back to reading directly from the Excel when CSVs are unavailable.

load_employment_data <- function() {

  # ---- logging ----
  debug_dir  <- file.path(getwd(), "logs")
  if (!dir.exists(debug_dir)) dir.create(debug_dir, showWarnings = FALSE, recursive = TRUE)
  debug_path <- file.path(debug_dir, "employment_loader_debug.txt")
  logf <- function(...) {
    msg <- paste0(..., collapse = "")
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", msg, "\n",
        file = debug_path, append = TRUE)
  }

  # ---- search roots (same pattern as governance_data.R) ----
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
  logf("[EMP] app_path=", app_path, " | wd=", getwd(), " | project_root=", project_root)

  # ---- locate clean CSV directory ----
  clean_dir <- find_dashboard_path(search_roots, c("Employment", "clean"), is_dir = TRUE)
  logf("[EMP] clean_dir after find_dashboard_path=", ifelse(is.null(clean_dir), "NULL", clean_dir))

  # Direct path checks — most reliable for local runs
  if (is.null(clean_dir)) {
    for (r in search_roots) {
      if (is.null(r) || is.na(r)) next
      cand <- file.path(r, "Dashboard_data", "Employment", "clean")
      if (dir.exists(cand)) { clean_dir <- cand; logf("[EMP] clean_dir via direct check: ", cand); break }
      cand2 <- file.path(r, "dashboard_data", "Employment", "clean")
      if (dir.exists(cand2)) { clean_dir <- cand2; logf("[EMP] clean_dir via direct check lc: ", cand2); break }
    }
  }

  # Filename-based fallback
  if (is.null(clean_dir)) {
    for (r in search_roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
      matches <- suppressWarnings(list.files(
        r0, pattern = "^emp_lf_sex\\.csv$",
        recursive = TRUE, full.names = TRUE, ignore.case = TRUE
      ))
      if (length(matches) > 0) {
        clean_dir <- dirname(matches[[1]])
        logf("[EMP] clean_dir found by filename search: ", clean_dir)
        break
      }
    }
  }

  if (is.null(clean_dir)) {
    message("[EMP] Employment clean CSV dir not found — trying Excel fallback.")
    logf("[EMP] clean_dir still NULL after fallback search.")
  }

  # ---- CSV reader ----
  read_csv_safe <- function(fname) {
    if (is.null(clean_dir)) return(NULL)
    p <- file.path(clean_dir, fname)
    if (!file.exists(p)) { logf("[EMP] Missing CSV: ", p); return(NULL) }
    tryCatch(readr::read_csv(p, show_col_types = FALSE), error = function(e) {
      logf("[EMP] read_csv error for ", fname, ": ", e$message)
      NULL
    })
  }

  # ---- type coercion helpers ----
  coerce_lf <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df$year    <- suppressWarnings(as.integer(df$year))
    df$value   <- suppressWarnings(as.numeric(df$value))
    df$quarter <- trimws(as.character(df$quarter))
    if ("sex"       %in% names(df)) df$sex       <- trimws(as.character(df$sex))
    if ("indicator" %in% names(df)) df$indicator <- trimws(as.character(df$indicator))
    if ("group"     %in% names(df)) df$group     <- trimws(as.character(df$group))
    df
  }

  coerce_cat <- function(df, cat_col) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df$year    <- suppressWarnings(as.integer(df$year))
    df$value   <- suppressWarnings(as.numeric(df$value))
    df$quarter <- trimws(as.character(df$quarter))
    if ("sex"    %in% names(df)) df$sex    <- trimws(as.character(df$sex))
    if (cat_col  %in% names(df)) df[[cat_col]] <- trimws(as.character(df[[cat_col]]))
    df
  }

  coerce_agri <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df$year     <- suppressWarnings(as.integer(df$year))
    df$pct_agri <- suppressWarnings(as.numeric(df$pct_agri))
    df$quarter  <- trimws(as.character(df$quarter))
    df$group    <- trimws(as.character(df$group))
    df
  }

  # ---- load CSVs ----
  safe_coerce <- function(fn, ...) tryCatch(fn(...), error = function(e) {
    logf("[EMP] coerce error: ", e$message); NULL
  })

  clean <- list(
    lf_sex      = safe_coerce(coerce_lf,   read_csv_safe("emp_lf_sex.csv")),
    lf_youth    = safe_coerce(coerce_lf,   read_csv_safe("emp_lf_youth.csv")),
    occupations = safe_coerce(coerce_cat,  read_csv_safe("emp_occupations_sex.csv"),       "occupation"),
    status      = safe_coerce(coerce_cat,  read_csv_safe("emp_status_sex.csv"),             "status"),
    education   = safe_coerce(coerce_cat,  read_csv_safe("emp_education_sex.csv"),          "education"),
    agri        = safe_coerce(coerce_agri, read_csv_safe("emp_agri_sex.csv")),
    economic    = safe_coerce(coerce_cat,  read_csv_safe("emp_economic_activity_sex.csv"),  "sector")
  )

  required <- c("lf_sex", "occupations", "status", "education", "agri")
  missing  <- required[sapply(clean[required], function(x) is.null(x) || nrow(x) == 0)]

  if (length(missing) == 0) {
    logf("[EMP] All CSVs loaded. lf_sex=", nrow(clean$lf_sex),
         " occ=", nrow(clean$occupations), " agri=", nrow(clean$agri))
    message("[EMP] Employment CSVs loaded from clean folder.")
    return(clean)
  }

  logf("[EMP] Missing required tables: ", paste(missing, collapse = ", "),
       " — falling back to Excel.")
  message("[EMP] Missing required tables (", paste(missing, collapse = ", "),
          ") — falling back to Excel.")

  # ---- Excel fallback ----
  excel_path <- find_dashboard_path(
    search_roots,
    c("Employment", "RW_Quarterly_LFS_Tables_2025Q4.xlsx"),
    is_dir = FALSE
  )
  if (is.null(excel_path)) {
    for (r in search_roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
      m <- suppressWarnings(list.files(
        r0, pattern = "^RW_Quarterly_LFS_Tables.*\\.xlsx$",
        recursive = TRUE, full.names = TRUE, ignore.case = TRUE
      ))
      if (length(m) > 0) { excel_path <- m[[1]]; break }
    }
  }
  if (is.null(excel_path)) {
    message("[EMP] Excel not found — Employment dashboard unavailable.")
    logf("[EMP] Excel not found.")
    return(NULL)
  }
  message("[EMP] Using Excel fallback at: ", excel_path)
  logf("[EMP] Excel fallback at: ", excel_path)

  # ---- Excel extraction helpers ----
  safe_num_r <- function(x) suppressWarnings(as.numeric(as.character(x)))

  build_time_axis_r <- function(df_raw) {
    yr_row <- as.character(unlist(df_raw[3, ]))
    qt_row <- as.character(unlist(df_raw[4, ]))
    cur_yr <- NA_integer_
    out <- list()
    for (ci in seq(2, ncol(df_raw))) {
      yr_val <- suppressWarnings(as.integer(yr_row[ci]))
      if (!is.na(yr_val) && yr_val >= 2015 && yr_val <= 2030) cur_yr <- yr_val
      qt_val <- trimws(qt_row[ci])
      if (!is.na(cur_yr) && grepl("^Q[1-4]$", qt_val))
        out[[length(out) + 1]] <- list(year = cur_yr, quarter = qt_val, col = ci)
    }
    out
  }

  read_xl <- function(sheet) {
    tryCatch(
      readxl::read_excel(excel_path, sheet = sheet, col_names = FALSE),
      error = function(e) { logf("[EMP] read_excel error on sheet ", sheet, ": ", e$message); NULL }
    )
  }

  # Extract Male/Female blocks (no "Sex:" prefix — just "Male" / "Female")
  extract_lf_sex <- function() {
    df <- read_xl("LFIndicatorsSex")
    if (is.null(df)) return(NULL)
    time_ax <- build_time_axis_r(df)
    if (length(time_ax) == 0) return(NULL)
    col0 <- as.character(unlist(df[, 1]))
    sex_rows <- which(col0 %in% c("Male", "Female"))
    blocks <- list()
    for (k in seq_along(sex_rows)) {
      r_s <- sex_rows[k] + 3L
      r_e <- if (k < length(sex_rows)) sex_rows[k + 1] - 1L else nrow(df)
      blocks[[col0[sex_rows[k]]]] <- c(r_s, r_e)
    }
    rows_out <- list()
    for (sex in names(blocks)) {
      r_s <- blocks[[sex]][1]; r_e <- blocks[[sex]][2]
      for (ri in seq(r_s, r_e)) {
        ind <- trimws(as.character(df[[1]][ri]))
        if (is.na(ind) || ind == "" || tolower(ind) == "nan") next
        if (startsWith(tolower(ind), "source")) next
        for (ta in time_ax) {
          v <- safe_num_r(df[[ta$col]][ri])
          if (is.na(v)) next
          rows_out[[length(rows_out) + 1]] <- data.frame(
            year = ta$year, quarter = ta$quarter,
            sex = sex, indicator = ind, value = v, stringsAsFactors = FALSE
          )
        }
      }
    }
    coerce_lf(dplyr::bind_rows(rows_out))
  }

  # Generic "Sex: All/Male/Female" block extractor
  extract_sex_cat <- function(sheet, cat_col) {
    df <- read_xl(sheet)
    if (is.null(df)) return(NULL)
    time_ax <- build_time_axis_r(df)
    if (length(time_ax) == 0) return(NULL)
    col0 <- as.character(unlist(df[, 1]))
    sex_start_rows <- which(startsWith(col0, "Sex: "))
    blocks <- list()
    for (k in seq_along(sex_start_rows)) {
      r_s <- sex_start_rows[k] + 3L
      r_e <- if (k < length(sex_start_rows)) sex_start_rows[k + 1] - 1L else nrow(df)
      sx  <- trimws(sub("^Sex: ", "", col0[sex_start_rows[k]]))
      blocks[[sx]] <- c(r_s, r_e)
    }
    rows_out <- list()
    for (sex in names(blocks)) {
      r_s <- blocks[[sex]][1]; r_e <- blocks[[sex]][2]
      for (ri in seq(r_s, r_e)) {
        cat_v <- trimws(as.character(df[[1]][ri]))
        if (is.na(cat_v) || cat_v == "" || tolower(cat_v) == "nan") next
        if (startsWith(tolower(cat_v), "source")) next
        for (ta in time_ax) {
          v <- safe_num_r(df[[ta$col]][ri])
          if (is.na(v)) next
          row_df <- data.frame(year = ta$year, quarter = ta$quarter,
                               sex = sex, value = v, stringsAsFactors = FALSE)
          row_df[[cat_col]] <- cat_v
          rows_out[[length(rows_out) + 1]] <- row_df
        }
      }
    }
    coerce_cat(dplyr::bind_rows(rows_out), cat_col)
  }

  extract_agri <- function() {
    df <- read_xl("ShareAgriWorkersSex")
    if (is.null(df)) return(NULL)
    time_ax <- build_time_axis_r(df)
    if (length(time_ax) == 0) return(NULL)
    col0    <- as.character(unlist(df[, 1]))
    targets <- c("Rwanda", "Male", "Female")
    rows_out <- list()
    for (ri in seq_len(nrow(df))) {
      grp <- trimws(col0[ri])
      if (!(grp %in% targets)) next
      for (ta in time_ax) {
        v <- safe_num_r(df[[ta$col]][ri])
        if (is.na(v)) next
        rows_out[[length(rows_out) + 1]] <- data.frame(
          year = ta$year, quarter = ta$quarter,
          group = grp, pct_agri = v, stringsAsFactors = FALSE
        )
      }
    }
    coerce_agri(dplyr::bind_rows(rows_out))
  }

  out <- list(
    lf_sex      = tryCatch(extract_lf_sex(),  error = function(e) { logf("[EMP] extract_lf_sex error: ", e$message); NULL }),
    lf_youth    = NULL,
    occupations = tryCatch(extract_sex_cat("OccupationsSex",        "occupation"), error = function(e) NULL),
    status      = tryCatch(extract_sex_cat("StatusInEmploymentSex", "status"),     error = function(e) NULL),
    education   = tryCatch(extract_sex_cat("EmployedEducaSex",      "education"),  error = function(e) NULL),
    agri        = tryCatch(extract_agri(),  error = function(e) { logf("[EMP] extract_agri error: ", e$message); NULL }),
    economic    = tryCatch(extract_sex_cat("EconomicActivitySex",   "sector"),     error = function(e) NULL)
  )

  if (all(sapply(out, is.null))) {
    message("[EMP] Excel extraction produced no usable tables.")
    logf("[EMP] Excel extraction: all NULL.")
    return(NULL)
  }

  logf("[EMP] Excel extraction OK. lf_sex=",
       ifelse(is.null(out$lf_sex), "NULL", nrow(out$lf_sex)),
       " agri=", ifelse(is.null(out$agri), "NULL", nrow(out$agri)))
  out
}
