# R/employment_data.R
# Loads preprocessed Rwanda LFS Employment gender-disaggregated CSVs.
# Falls back to reading directly from the Excel when CSVs are unavailable
# (e.g. shinyapps.io deployments where the clean/ folder is absent).

load_employment_data <- function() {

  app_path <- tryCatch(shiny::getCurrentAppPath(), error = function(e) NULL)
  if (is.null(app_path) || !dir.exists(app_path)) {
    app_path <- tryCatch(getwd(), error = function(e) ".")
  }
  search_roots <- unique(c(app_path, getwd()))

  # Locate the clean CSV directory
  clean_dir <- find_dashboard_path(search_roots, c("Employment", "clean"), is_dir = TRUE)

  if (is.null(clean_dir)) {
    for (r in search_roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
      matches <- suppressWarnings(list.files(
        r0, pattern = "^emp_lf_sex\\.csv$",
        recursive = TRUE, full.names = TRUE, ignore.case = TRUE
      ))
      if (length(matches) > 0) { clean_dir <- dirname(matches[[1]]); break }
    }
  }

  read_csv_safe <- function(fname) {
    if (is.null(clean_dir)) return(NULL)
    p <- file.path(clean_dir, fname)
    if (!file.exists(p)) { message("[EMP] Missing CSV: ", p); return(NULL) }
    tryCatch(readr::read_csv(p, show_col_types = FALSE), error = function(e) NULL)
  }

  # ---- coerce types ----
  coerce_lf <- function(df) {
    if (is.null(df)) return(NULL)
    df$year    <- suppressWarnings(as.integer(df$year))
    df$value   <- suppressWarnings(as.numeric(df$value))
    df$sex     <- trimws(as.character(df$sex))
    df$quarter <- trimws(as.character(df$quarter))
    if ("indicator" %in% names(df))  df$indicator  <- trimws(as.character(df$indicator))
    if ("group"     %in% names(df))  df$group      <- trimws(as.character(df$group))
    df
  }

  coerce_cat <- function(df, cat_col) {
    if (is.null(df)) return(NULL)
    df$year    <- suppressWarnings(as.integer(df$year))
    df$value   <- suppressWarnings(as.numeric(df$value))
    df$sex     <- trimws(as.character(df$sex))
    df$quarter <- trimws(as.character(df$quarter))
    if (cat_col %in% names(df)) df[[cat_col]] <- trimws(as.character(df[[cat_col]]))
    df
  }

  clean <- list(
    lf_sex        = coerce_lf(read_csv_safe("emp_lf_sex.csv")),
    lf_youth      = coerce_lf(read_csv_safe("emp_lf_youth.csv")),
    occupations   = coerce_cat(read_csv_safe("emp_occupations_sex.csv"), "occupation"),
    status        = coerce_cat(read_csv_safe("emp_status_sex.csv"),      "status"),
    education     = coerce_cat(read_csv_safe("emp_education_sex.csv"),   "education"),
    agri          = {
      df <- read_csv_safe("emp_agri_sex.csv")
      if (!is.null(df)) {
        df$year     <- suppressWarnings(as.integer(df$year))
        df$pct_agri <- suppressWarnings(as.numeric(df$pct_agri))
        df$group    <- trimws(as.character(df$group))
        df$quarter  <- trimws(as.character(df$quarter))
      }
      df
    },
    economic      = coerce_cat(read_csv_safe("emp_economic_activity_sex.csv"), "sector")
  )

  required <- c("lf_sex", "occupations", "status", "education", "agri")
  missing  <- required[sapply(clean[required], is.null)]

  if (length(missing) == 0) {
    message("[EMP] Employment CSVs loaded from clean folder.")
    return(clean)
  }

  # ---- Excel fallback ----
  message("[EMP] Some CSVs missing (", paste(missing, collapse = ", "),
          ") — falling back to Excel extraction.")

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
    return(NULL)
  }

  message("[EMP] Using Excel fallback at: ", excel_path)

  # Helper: build (year, quarter, col_idx) axis from rows 3-4 (1-based), cols 2+
  build_time_axis_r <- function(df_raw) {
    yr_row <- as.character(unlist(df_raw[3, ]))
    qt_row <- as.character(unlist(df_raw[4, ]))
    cur_yr <- NA_integer_
    out <- list()
    for (ci in seq(2, ncol(df_raw))) {
      yr_val <- suppressWarnings(as.integer(yr_row[ci]))
      if (!is.na(yr_val) && yr_val >= 2015 && yr_val <= 2030) cur_yr <- yr_val
      qt_val <- trimws(qt_row[ci])
      if (!is.na(cur_yr) && grepl("^Q[1-4]$", qt_val)) {
        out[[length(out) + 1]] <- list(year = cur_yr, quarter = qt_val, col = ci)
      }
    }
    out
  }

  safe_num_r <- function(x) suppressWarnings(as.numeric(as.character(x)))

  # Extract lf_sex (Male/Female blocks, no "Sex:" prefix)
  extract_lf_sex <- function() {
    df <- tryCatch(
      readxl::read_excel(excel_path, sheet = "LFIndicatorsSex", col_names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df)) return(NULL)
    time_ax <- build_time_axis_r(df)
    if (length(time_ax) == 0) return(NULL)

    col0 <- as.character(unlist(df[, 1]))
    sex_starts <- which(col0 %in% c("Male", "Female"))
    blocks <- list()
    for (k in seq_along(sex_starts)) {
      r_s <- sex_starts[k] + 3L  # skip internal year/quarter rows
      r_e <- if (k < length(sex_starts)) sex_starts[k + 1] - 1L else nrow(df)
      blocks[[col0[sex_starts[k]]]] <- c(r_s, r_e)
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
            sex = sex, indicator = ind, value = v,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    dplyr::bind_rows(rows_out)
  }

  # Generic "Sex: All/Male/Female" block extractor
  extract_sex_cat <- function(sheet, cat_col) {
    df <- tryCatch(
      readxl::read_excel(excel_path, sheet = sheet, col_names = FALSE),
      error = function(e) NULL
    )
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
    dplyr::bind_rows(rows_out)
  }

  extract_agri <- function() {
    df <- tryCatch(
      readxl::read_excel(excel_path, sheet = "ShareAgriWorkersSex", col_names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df)) return(NULL)
    time_ax <- build_time_axis_r(df)
    if (length(time_ax) == 0) return(NULL)

    col0 <- as.character(unlist(df[, 1]))
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
          group = grp, pct_agri = v,
          stringsAsFactors = FALSE
        )
      }
    }
    dplyr::bind_rows(rows_out)
  }

  out <- list(
    lf_sex      = tryCatch(extract_lf_sex(), error = function(e) NULL),
    lf_youth    = NULL,
    occupations = tryCatch(extract_sex_cat("OccupationsSex",       "occupation"), error = function(e) NULL),
    status      = tryCatch(extract_sex_cat("StatusInEmploymentSex", "status"),    error = function(e) NULL),
    education   = tryCatch(extract_sex_cat("EmployedEducaSex",      "education"), error = function(e) NULL),
    agri        = tryCatch(extract_agri(),  error = function(e) NULL),
    economic    = tryCatch(extract_sex_cat("EconomicActivitySex",   "sector"),    error = function(e) NULL)
  )

  # Coerce types from Excel extraction
  out$lf_sex    <- coerce_lf(out$lf_sex)
  out$lf_youth  <- coerce_lf(out$lf_youth)
  out$agri      <- if (!is.null(out$agri)) {
    out$agri$year     <- suppressWarnings(as.integer(out$agri$year))
    out$agri$pct_agri <- suppressWarnings(as.numeric(out$agri$pct_agri))
    out$agri
  }
  for (col_nm in c("occupation", "status", "education", "sector")) {
    tbl <- switch(col_nm,
      occupation = "occupations", status = "status",
      education  = "education",   sector = "economic"
    )
    out[[tbl]] <- coerce_cat(out[[tbl]], col_nm)
  }

  if (all(sapply(out, is.null))) {
    message("[EMP] Excel extraction produced no usable tables.")
    return(NULL)
  }
  out
}
