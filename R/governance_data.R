# ── R/governance_data.R ─────────────────────────────────────────────────────
# Loads preprocessed Governance dashboard CSVs (long/tidy format).

to_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  s <- gsub("\u00a0", " ", as.character(x))
  s <- gsub("[^0-9\\.\\-]", "", s)
  if (trimws(s) == "") return(NA_real_)
  suppressWarnings(as.numeric(s))
}

norm_text <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z
}

#' Add workbook metadata (full-sheet manifest, paths) for the Data library tab.
attach_governance_meta <- function(obj, clean_dir, excel_source = NULL) {
  manifest <- NULL
  if (!is.null(clean_dir)) {
    mp <- file.path(clean_dir, "governance_sheet_manifest.csv")
    if (file.exists(mp)) {
      manifest <- tryCatch(
        readr::read_csv(mp, show_col_types = FALSE),
        error = function(e) NULL
      )
    }
  }
  obj$sheet_manifest <- manifest
  obj$clean_dir_path <- clean_dir
  obj$excel_source <- excel_source
  obj
}

load_governance_data <- function() {
  debug_dir <- file.path(getwd(), "logs")
  if (!dir.exists(debug_dir)) dir.create(debug_dir, showWarnings = FALSE, recursive = TRUE)
  debug_path <- file.path(debug_dir, "governance_loader_debug.txt")
  logf <- function(...) {
    msg <- paste0(..., collapse = "")
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", msg, "\n", file = debug_path, append = TRUE)
  }

  rel_dir <- file.path("dashboard_data", "Governance_data", "clean")

  # Robust base roots (deployed Shiny working directory can differ).
  app_path <- tryCatch(shiny::getCurrentAppPath(), error = function(e) NULL)
  if (is.null(app_path) || !dir.exists(app_path)) {
    app_path <- tryCatch(getwd(), error = function(e) ".")
  }

  # Anchor to actual script location (avoids getwd()/appPath quirks).
  script_path <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
    error = function(e) NULL
  )
  script_dir <- if (!is.null(script_path)) dirname(script_path) else NULL
  project_root <- if (!is.null(script_dir)) dirname(script_dir) else NULL

  search_roots <- unique(c(app_path, getwd(), project_root))
  logf("[GOV] app_path=", app_path, " | wd=", getwd(), " | project_root=", project_root)

  find_dir <- function(roots, dir_rel, max_up = 6) {
    for (r in roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "") next
      cur <- r0
      for (i in seq_len(max_up + 1)) {
        cand <- file.path(cur, dir_rel)
        if (dir.exists(cand)) return(cand)
        parent <- dirname(cur)
        if (identical(parent, cur)) break
        cur <- parent
      }
    }
    NULL
  }

  # Prefer direct path from project root (reliable in local + deployed).
  clean_dir <- NULL
  if (!is.null(project_root)) {
    cand <- file.path(project_root, rel_dir)
    if (dir.exists(cand)) clean_dir <- cand
  }
  if (is.null(clean_dir)) clean_dir <- find_dir(search_roots, rel_dir)
  logf("[GOV] clean_dir=", ifelse(is.null(clean_dir), "NULL", clean_dir))

  # If not found, try to locate by filename within roots.
  if (is.null(clean_dir)) {
    for (r in search_roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
      matches <- suppressWarnings(list.files(
        r0,
        pattern = "^governance_ministers_gender\\.csv$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      ))
      if (length(matches) > 0) {
        clean_dir <- dirname(matches[[1]])
        break
      }
    }
  }

  if (is.null(clean_dir)) {
    message("[GOV] Governance clean CSV dir not found: rel=", rel_dir,
            " — falling back to Excel extraction.")
  }

  read_csv_rel <- function(fname) {
    # If clean_dir isn't available in this runtime (common on deployed builds),
    # skip CSV loading (and let the Excel fallback handle it).
    if (is.null(clean_dir)) return(NULL)

    p <- file.path(clean_dir, fname)
    if (!file.exists(p)) {
      message("[GOV] Missing CSV: ", p)
      return(NULL)
    }
    readr::read_csv(p, show_col_types = FALSE)
  }

  clean <- list(
    ministers = read_csv_rel("governance_ministers_gender.csv"),
    parliament = read_csv_rel("governance_parliament_gender.csv"),
    prosecutors = read_csv_rel("governance_prosecutors_gender.csv"),
    judiciary = read_csv_rel("governance_judiciary_gender.csv"),
    local_leaders = read_csv_rel("governance_local_leaders_gender.csv")
  )

  # If any required table is missing, fall back to extracting from Excel.
  required_missing <- any(sapply(clean[c("ministers", "parliament", "prosecutors", "judiciary", "local_leaders")],
    function(x) is.null(x)))

  if (!required_missing) {
    logf("[GOV] loaded CSVs successfully: ministers=", nrow(clean$ministers),
         " parliament=", nrow(clean$parliament),
         " prosecutors=", nrow(clean$prosecutors),
         " judiciary=", nrow(clean$judiciary),
         " local=", nrow(clean$local_leaders))
    message("[GOV] Governance CSVs loaded from clean folder.")
    return(attach_governance_meta(clean, clean_dir, NULL))
  }

  # ── Fallback: read from the original Excel ────────────────────────────────
  rel_excel <- file.path("dashboard_data", "Governance_data", "Governace data -nisr.xlsx")

  find_excel <- function(roots, rel_path, max_up = 6) {
    for (r in roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "") next
      cur <- r0
      for (i in seq_len(max_up + 1)) {
        cand <- file.path(cur, rel_path)
        if (file.exists(cand)) return(cand)
        parent <- dirname(cur)
        if (identical(parent, cur)) break
        cur <- parent
      }
    }
    NULL
  }

  search_roots <- unique(c(app_path, getwd(), project_root))
  excel_path <- find_excel(search_roots, rel_excel)
  if (is.null(excel_path)) {
    # Last attempt: filename search
    for (r in search_roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
      matches <- suppressWarnings(list.files(
        r0,
        pattern = "^Governace data -nisr\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      ))
      if (length(matches) > 0) {
        excel_path <- matches[[1]]
        break
      }
    }
  }

  if (is.null(excel_path)) {
    message("[GOV] Excel not found for fallback: ", rel_excel)
    logf("[GOV] Excel not found for fallback: ", rel_excel)
    return(NULL)
  }
  message("[GOV] Using Excel fallback at: ", excel_path)
  logf("[GOV] using Excel fallback at: ", excel_path)

  # Extracts only the few sheets we currently visualize.
  # This keeps runtime light but makes the deployed dashboard work
  # even when the generated CSVs are not packaged.
  extract_sheet_ministers <- function() {
    df <- readxl::read_excel(excel_path, sheet = "Figure47", col_names = TRUE)
    # Label column is usually 2nd col; year columns are the rest.
    label_col <- 2
    year_cols <- 3:ncol(df)

    female_row <- which(grepl("female", as.character(df[[label_col]]), ignore.case = TRUE))
    male_row   <- which(grepl("male",   as.character(df[[label_col]]), ignore.case = TRUE))
    if (length(female_row) == 0 || length(male_row) == 0) return(NULL)
    female_row <- female_row[[1]]
    male_row <- male_row[[1]]

    # Find the row that contains the year numbers:
    # choose the row with the most numeric values between 2000 and 2035.
    year_like_counts <- sapply(seq_len(nrow(df)), function(r) {
      yrs_r <- sapply(year_cols, function(j) to_num(df[[j]][r]))
      sum(!is.na(yrs_r) & yrs_r >= 2000 & yrs_r <= 2035)
    })
    year_row <- which.max(year_like_counts)
    if (is.na(year_row) || year_like_counts[year_row] == 0) return(NULL)

    years <- sapply(year_cols, function(j) to_num(df[[j]][year_row]))
    ok <- !is.na(years)
    years <- years[ok]
    year_cols <- year_cols[ok]

    female_vals <- sapply(year_cols, function(j) to_num(df[[j]][female_row]))
    male_vals   <- sapply(year_cols, function(j) to_num(df[[j]][male_row]))

    out <- rbind(
      data.frame(indicator = "ministers_gender_equality", year = as.integer(years), sex = "Female", pct = female_vals),
      data.frame(indicator = "ministers_gender_equality", year = as.integer(years), sex = "Male", pct = male_vals)
    )
    out[!is.na(out$pct), ]
  }

  extract_sheet_parliament <- function() {
    df <- readxl::read_excel(excel_path, sheet = "Figure50", col_names = TRUE)
    # sex labels appear in column 2; year/value columns start at 3
    label_col <- 2
    val_cols <- 3:ncol(df)
    female_row <- which(grepl("female", as.character(df[[label_col]]), ignore.case = TRUE))
    male_row   <- which(grepl("male",   as.character(df[[label_col]]), ignore.case = TRUE))
    if (length(female_row) == 0 || length(male_row) == 0) return(NULL)
    female_row <- female_row[[1]]
    male_row <- male_row[[1]]

    # year values are likely in the 3rd row of excel (index 3), but we try first numeric row
    year_row <- NULL
    for (r in 1:nrow(df)) {
      cand <- sapply(val_cols, function(j) to_num(df[[j]][r]))
      if (sum(!is.na(cand) & cand >= 1970 & cand <= 2035) >= 3) { year_row <- r; break }
    }
    if (is.null(year_row)) return(NULL)

    years <- sapply(val_cols, function(j) to_num(df[[j]][year_row]))
    ok <- !is.na(years)
    val_cols <- val_cols[ok]; years <- years[ok]

    female_vals <- sapply(val_cols, function(j) to_num(df[[j]][female_row]))
    male_vals   <- sapply(val_cols, function(j) to_num(df[[j]][male_row]))

    out <- rbind(
      data.frame(indicator = "parliament_senate_seats_gender", year = as.integer(years), sex = "Female", pct = female_vals),
      data.frame(indicator = "parliament_senate_seats_gender", year = as.integer(years), sex = "Male", pct = male_vals)
    )
    out[!is.na(out$pct), ]
  }

  extract_sheet_prosecutors <- function() {
    df <- readxl::read_excel(excel_path, sheet = "Figure52", col_names = TRUE)
    # Expect: col2=year, col3=female, col4=male. Filter numeric years.
    year_col <- 2; female_col <- 3; male_col <- 4
    years <- sapply(df[[year_col]], to_num)
    ok <- !is.na(years) & years >= 1970 & years <= 2035
    df2 <- df[ok, , drop = FALSE]
    if (nrow(df2) == 0) return(NULL)

    out <- rbind(
      data.frame(indicator = "national_prosecutors_gender", year = as.integer(sapply(df2[[year_col]], to_num)), sex = "Female", pct = sapply(df2[[female_col]], to_num)),
      data.frame(indicator = "national_prosecutors_gender", year = as.integer(sapply(df2[[year_col]], to_num)), sex = "Male",   pct = sapply(df2[[male_col]], to_num))
    )
    out[!is.na(out$pct), ]
  }

  extract_sheet_judiciary <- function() {
    df <- readxl::read_excel(excel_path, sheet = "Figure53", col_names = TRUE)
    # Pair columns: (3,4), (5,6), (7,8), (9,10) in python -> (3,4),(5,6)... in R (1-based)
    if (ncol(df) < 10) return(NULL)
    pair_f <- c(3, 5, 7, 9)
    pair_m <- c(4, 6, 8, 10)

    # Find a row that contains year numbers at pair_f columns.
    year_row <- NULL
    for (r in 1:nrow(df)) {
      years <- sapply(pair_f, function(j) to_num(df[[j]][r]))
      if (sum(!is.na(years) & years >= 1970 & years <= 2035) >= 3) { year_row <- r; break }
    }
    if (is.null(year_row)) return(NULL)
    years <- sapply(pair_f, function(j) to_num(df[[j]][year_row]))
    ok <- !is.na(years)
    pair_f <- pair_f[ok]; pair_m <- pair_m[ok]; years <- years[ok]

    entity_col <- 2
    out <- list()
    idx <- 1
    for (r in 1:nrow(df)) {
      ent <- df[[entity_col]][r]
      if (is.na(ent)) next
      ent <- as.character(ent)
      ent_low <- tolower(trimws(ent))
      if (ent_low %in% c("female", "male")) next
      if (startsWith(ent_low, "source")) next
      if (ent_low == "") next

      for (i in seq_along(years)) {
        vf <- to_num(df[[pair_f[i]]][r])
        vm <- to_num(df[[pair_m[i]]][r])
        out[[idx]] <- data.frame(indicator = "judiciary_representation_gender", year = as.integer(years[i]), entity = ent, sex = "Female", pct = vf)
        idx <- idx + 1
        out[[idx]] <- data.frame(indicator = "judiciary_representation_gender", year = as.integer(years[i]), entity = ent, sex = "Male", pct = vm)
        idx <- idx + 1
      }
    }
    out_df <- dplyr::bind_rows(out)
    out_df <- out_df[!is.na(out_df$pct), ]
    out_df
  }

  extract_sheet_local <- function() {
    df <- readxl::read_excel(excel_path, sheet = "Table22", col_names = TRUE)
    # Expect: col2=Position entity, and year/value columns follow in M/F alternating.
    if (ncol(df) < 10) return(NULL)
    entity_col <- 2
    # year values likely at header row index 2 (0-based 1). We'll locate numeric row for year list.
    year_row <- NULL
    candidate_cols <- c(3,5,7,9,11)
    for (r in 1:nrow(df)) {
      yrs <- sapply(candidate_cols, function(j) to_num(df[[j]][r]))
      if (sum(!is.na(yrs) & yrs >= 1970 & yrs <= 2035) >= 4) { year_row <- r; break }
    }
    if (is.null(year_row)) year_row <- 2

    yrs <- sapply(candidate_cols, function(j) to_num(df[[j]][year_row]))
    ok <- !is.na(yrs)
    candidate_cols <- candidate_cols[ok]; yrs <- yrs[ok]

    # For each year col (M at j), female is j+1.
    out <- list()
    idx <- 1
    for (r in 1:nrow(df)) {
      ent <- df[[entity_col]][r]
      if (is.na(ent)) next
      ent <- as.character(ent)
      ent_low <- tolower(trimws(ent))
      if (ent_low %in% c("position", "m", "f")) next
      if (startsWith(ent_low, "source")) next
      if (ent_low == "") next

      for (i in seq_along(yrs)) {
        mcol <- candidate_cols[i]
        fcol <- mcol + 1
        vm <- to_num(df[[mcol]][r])
        vf <- to_num(df[[fcol]][r])
        out[[idx]] <- data.frame(indicator = "local_government_leaders_gender", year = as.integer(yrs[i]), entity = ent, sex = "Male", pct = vm); idx <- idx + 1
        out[[idx]] <- data.frame(indicator = "local_government_leaders_gender", year = as.integer(yrs[i]), entity = ent, sex = "Female", pct = vf); idx <- idx + 1
      }
    }
    out_df <- dplyr::bind_rows(out)
    out_df[!is.na(out_df$pct), ]
  }

  out <- list(
    ministers   = tryCatch(extract_sheet_ministers(),   error = function(e) NULL),
    parliament  = tryCatch(extract_sheet_parliament(),  error = function(e) NULL),
    prosecutors = tryCatch(extract_sheet_prosecutors(), error = function(e) NULL),
    judiciary   = tryCatch(extract_sheet_judiciary(),   error = function(e) NULL),
    local_leaders = tryCatch(extract_sheet_local(),    error = function(e) NULL)
  )

  if (all(sapply(out, is.null))) {
    message("[GOV] Excel extraction produced no usable tables.")
    logf("[GOV] Excel extraction produced no usable tables.")
    return(NULL)
  }
  logf("[GOV] Excel extraction counts: ministers=",
       ifelse(is.null(out$ministers), "NULL", nrow(out$ministers)),
       " parliament=", ifelse(is.null(out$parliament), "NULL", nrow(out$parliament)),
       " prosecutors=", ifelse(is.null(out$prosecutors), "NULL", nrow(out$prosecutors)),
       " judiciary=", ifelse(is.null(out$judiciary), "NULL", nrow(out$judiciary)),
       " local=", ifelse(is.null(out$local_leaders), "NULL", nrow(out$local_leaders)))
  attach_governance_meta(out, clean_dir, excel_path)
}

