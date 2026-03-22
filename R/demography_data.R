# ── R/demography_data.R ─────────────────────────────────────────────────────
# Loads preprocessed Demography dashboard CSVs (tidy/long format).
#
# Data model returned by `load_demography_data()`:
# - population_geo: geo, geo_type (Province/District), province, sex, value (counts)
# - population_change: year, sex, value (counts)
# - age_distribution: age_group, residence (Rwanda/Urban/Rural), sex, value (counts)
# - internet_use: age_group, province, residence (Rwanda/Urban/Rural), sex, value (counts)
# - elderly_share: geo, geo_type, province, residence (Rwanda/Urban/Rural), sex, value (%)
# - geo_province_district_map: province, district
# - intervention_age_groups: age_group, sex, count, pct (Table 3)
# - school_7_18_attendance: residence, sex, attendance_status, value_pct, population_count (Table 11)
# - school_13_18_by_geo: indicator, geo, geo_type, province, residence, sex, value (Table 15)
# - youth_share_by_geo: indicator, geo, geo_type, province, residence, sex, value (Table 45)
# - sheet_manifest: sheet_name, file, rows, cols (all workbook sheets exported to clean/sheets/)
#

to_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  s <- gsub("\u00a0", " ", as.character(x))
  s <- gsub("[^0-9\\.\\-]", "", s)
  if (trimws(s) == "") return(NA_real_)
  suppressWarnings(as.numeric(s))
}

norm_text <- function(x) {
  if (is.null(x)) return(NA_character_)
  z <- tolower(trimws(as.character(x)))
  z
}

# Flatten matrix-like columns when helpers.R is loaded (avoids subset errors on deploy).
norm_demo_df <- function(df) {
  if (!is.data.frame(df)) return(df)
  fn <- get0("normalize_df_for_indexing", ifnotfound = NULL, inherits = TRUE)
  if (is.null(fn)) return(df)
  fn(df)
}

norm_demo_list <- function(L) {
  if (!is.list(L)) return(L)
  for (nm in names(L)) {
    if (inherits(L[[nm]], "data.frame")) L[[nm]] <- norm_demo_df(L[[nm]])
  }
  L
}

# If Dashboard_data/ vs dashboard_data/ path fails, locate clean/ by marker file (Linux deploys).
find_demography_clean_dir_fallback <- function(roots) {
  roots <- unique(roots[!is.na(roots) & nzchar(as.character(roots))])
  for (r in roots) {
    r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
    if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
    hits <- suppressWarnings(list.files(
      r0,
      pattern = "^demo_population_geo_by_sex\\.csv$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ))
    if (length(hits) >= 1L) {
      return(dirname(hits[[1]]))
    }
  }
  NULL
}

load_demography_data <- function() {
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

  clean_dir <- find_dashboard_path(search_roots, c("Demography", "clean"), is_dir = TRUE)
  if (is.null(clean_dir)) {
    clean_dir <- find_demography_clean_dir_fallback(search_roots)
  }
  if (is.null(clean_dir)) {
    message("[DEMO] Demography clean/ not found under: ",
            paste(search_roots, collapse = " | "))
  } else {
    message("[DEMO] Using Demography clean dir: ", clean_dir)
  }

  read_csv_rel <- function(fname) {
    if (is.null(clean_dir)) return(NULL)
    p <- file.path(clean_dir, fname)
    if (!file.exists(p)) return(NULL)
    suppressMessages(readr::read_csv(p, show_col_types = FALSE))
  }

  clean <- list(
    population_geo = read_csv_rel("demo_population_geo_by_sex.csv"),
    population_change = read_csv_rel("demo_population_change_by_sex_year.csv"),
    age_distribution = read_csv_rel("demo_age_distribution.csv"),
    internet_use = read_csv_rel("demo_internet_use.csv"),
    elderly_share = read_csv_rel("demo_elderly_share.csv"),
    education_attendance = read_csv_rel("demo_education_attendance.csv"),
    geo_province_district_map = read_csv_rel("demo_geo_province_district_map.csv"),
    intervention_age_groups = read_csv_rel("demo_intervention_age_groups.csv"),
    school_7_18_attendance = read_csv_rel("demo_school_7_18_attendance.csv"),
    school_13_18_by_geo = read_csv_rel("demo_school_13_18_by_geo.csv"),
    youth_share_by_geo = read_csv_rel("demo_youth_share_by_geo.csv"),
    sheet_manifest = read_csv_rel("demo_sheet_manifest.csv")
  )

  # Stable types for charts (read_csv may infer character on Linux / edge rows).
  coerce_demography_tables <- function(L) {
    if (!is.null(L$population_change)) {
      d <- L$population_change
      if ("year" %in% names(d)) d$year <- suppressWarnings(as.integer(as.numeric(d$year)))
      if ("value" %in% names(d)) d$value <- suppressWarnings(as.numeric(d$value))
      L$population_change <- d
    }
    if (!is.null(L$population_geo)) {
      d <- L$population_geo
      if ("value" %in% names(d)) d$value <- suppressWarnings(as.numeric(d$value))
      for (nm in c("geo", "geo_type", "province", "sex")) {
        if (nm %in% names(d)) d[[nm]] <- trimws(as.character(d[[nm]]))
      }
      L$population_geo <- d
    }
    if (!is.null(L$internet_use)) {
      d <- L$internet_use
      if ("value" %in% names(d)) d$value <- suppressWarnings(as.numeric(d$value))
      for (nm in c("age_group", "province", "residence", "sex")) {
        if (nm %in% names(d)) d[[nm]] <- trimws(as.character(d[[nm]]))
      }
      L$internet_use <- d
    }
    L
  }
  clean <- coerce_demography_tables(clean)

  required_missing <- any(sapply(clean[c(
    "population_geo",
    "population_change",
    "age_distribution",
    "internet_use",
    "education_attendance",
    "elderly_share"
  )], is.null))

  if (!required_missing) {
    # If map is missing, rebuild it from population_geo.
    if (is.null(clean$geo_province_district_map) && !is.null(clean$population_geo)) {
      d <- clean$population_geo
      mp <- d[d$geo_type == "District" & !is.na(d$province) & !is.na(d$geo), c("province", "geo"), drop = FALSE]
      colnames(mp) <- c("province", "district")
      clean$geo_province_district_map <- unique(mp)
    }
    clean$clean_dir_path <- clean_dir
    return(norm_demo_list(clean))
  }

  # ── Fallback: read from the original Excel ────────────────────────────────
  search_roots <- unique(c(app_path, getwd(), project_root))
  excel_path <- find_dashboard_path(
    search_roots,
    c("Demography", "PHC5-2022_Main_Indicators.xlsx"),
    is_dir = FALSE
  )

  if (is.null(excel_path)) {
    # Last attempt: filename search.
    for (r in search_roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next
      matches <- suppressWarnings(list.files(
        r0,
        pattern = "^PHC5-2022_Main_Indicators\\.xlsx$",
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

  if (is.null(excel_path) || !file.exists(excel_path)) {
    return(NULL)
  }

  extract_table_1 <- function() {
    raw <- readxl::read_excel(excel_path, sheet = "Table 1", col_names = FALSE)
    if (ncol(raw) < 5) return(NULL)

    # Province-like labels: strings containing "Province" and "City of Kigali".
    labs <- raw[[2]]
    labs <- labs[!is.na(labs)]
    labs <- unique(vapply(labs, FUN.VALUE = character(1), FUN = function(z) {
      s <- gsub("\u00a0", " ", as.character(z))
      s <- trimws(s)
      s
    }))
    province_candidates <- labs[!(is.na(labs)) & (labs == "City of Kigali" | grepl("Province", labs))]

    rows <- list()
    idx <- 1
    current_prov <- NULL
    for (i in seq_len(nrow(raw))) {
      geo <- raw[[2]][i]
      if (is.na(geo)) next
      geo <- gsub("\u00a0", " ", as.character(geo))
      geo <- trimws(geo)
      if (geo == "" || is.na(geo)) next

      male <- to_num(raw[[4]][i])
      female <- to_num(raw[[5]][i])
      both <- to_num(raw[[3]][i])
      if (all(is.na(c(male, female, both)))) next

      if (geo == "Rwanda") next

      is_prov <- geo %in% province_candidates
      if (is_prov) {
        current_prov <- geo
        geo_type <- "Province"
        province <- geo
      } else {
        if (is.null(current_prov)) next
        geo_type <- "District"
        province <- current_prov
      }

      if (!is.na(male)) {
        rows[[idx]] <- data.frame(geo = geo, geo_type = geo_type, province = province, sex = "Male", value = male)
        idx <- idx + 1
      }
      if (!is.na(female)) {
        rows[[idx]] <- data.frame(geo = geo, geo_type = geo_type, province = province, sex = "Female", value = female)
        idx <- idx + 1
      }
    }

    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_table_4 <- function() {
    raw <- readxl::read_excel(excel_path, sheet = "Table 4", col_names = FALSE)
    if (ncol(raw) < 5) return(NULL)
    rows <- list()
    idx <- 1
    for (i in seq_len(nrow(raw))) {
      year <- to_num(raw[[2]][i])
      if (is.na(year)) next
      if (year < 1970 || year > 2035) next
      male <- to_num(raw[[4]][i])
      female <- to_num(raw[[5]][i])

      if (!is.na(male)) {
        rows[[idx]] <- data.frame(year = as.integer(round(year)), sex = "Male", value = male)
        idx <- idx + 1
      }
      if (!is.na(female)) {
        rows[[idx]] <- data.frame(year = as.integer(round(year)), sex = "Female", value = female)
        idx <- idx + 1
      }
    }
    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_table_5 <- function() {
    raw <- readxl::read_excel(excel_path, sheet = "Table 5", col_names = FALSE)
    if (ncol(raw) < 11) return(NULL)
    rows <- list()
    idx <- 1

    age_ok <- function(s) {
      if (is.na(s)) return(FALSE)
      s <- trimws(gsub("\u00a0", " ", as.character(s)))
      if (s %in% c("", "Age group", "group", "\uFFFDAge", "\uFFFDAge group")) return(FALSE)
      if (s == "Total") return(TRUE)
      if (grepl("^\\d{1,2}-\\d{1,2}$", s)) return(TRUE)
      if (grepl("^\\d{1,2}$", s)) return(TRUE)
      if (grepl("\\+$", s)) return(TRUE)
      FALSE
    }

    for (i in seq_len(nrow(raw))) {
      age_group <- raw[[2]][i]
      if (!age_ok(age_group)) next
      age_group <- trimws(gsub("\u00a0", " ", as.character(age_group)))

      # Rwanda: male col 4, female col 5 (1-based)
      male_rwanda <- to_num(raw[[4]][i])
      female_rwanda <- to_num(raw[[5]][i])
      # Urban: male col 7, female col 8
      male_urban <- to_num(raw[[7]][i])
      female_urban <- to_num(raw[[8]][i])
      # Rural: male col 10, female col 11
      male_rural <- to_num(raw[[10]][i])
      female_rural <- to_num(raw[[11]][i])

      entries <- list(
        list("Rwanda", male_rwanda, female_rwanda),
        list("Urban", male_urban, female_urban),
        list("Rural", male_rural, female_rural)
      )
      for (entry in entries) {
        residence <- entry[[1]]
        mval <- entry[[2]]
        fval <- entry[[3]]
        if (!is.na(mval)) {
          rows[[idx]] <- data.frame(age_group = age_group, residence = residence, sex = "Male", value = mval)
          idx <- idx + 1
        }
        if (!is.na(fval)) {
          rows[[idx]] <- data.frame(age_group = age_group, residence = residence, sex = "Female", value = fval)
          idx <- idx + 1
        }
      }
    }

    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_table_10_education_attendance <- function() {
    # Table 10: education attendance by sex and residence.
    # Sheet has residence sections (Rwanda/Urban/Rural), then education-level rows.
    raw <- readxl::read_excel(excel_path, sheet = "Table 10", col_names = FALSE)
    if (ncol(raw) < 8) return(NULL)

    rows <- list()
    idx <- 1
    residence <- NULL

    for (i in seq_len(nrow(raw))) {
      lab_raw <- raw[[2]][i] # column 2 (1-based) == label column in the Excel
      if (is.na(lab_raw)) next
      lab <- norm_text(lab_raw)

      # Section headers.
      if (!is.na(lab) && lab %in% c("rwanda", "urban", "rural")) {
        residence <- ifelse(lab == "rwanda", "Rwanda", lab)
        next
      }

      if (is.null(residence)) next

      # Skip header/metadata rows.
      if (lab %in% c(
        "level of education attended",
        "both sexes",
        "counts",
        "percentages"
      )) next

      education_level <- trimws(gsub("\u00a0", " ", as.character(raw[[2]][i])))

      male_count <- to_num(raw[[4]][i])
      female_count <- to_num(raw[[5]][i])
      male_pct <- to_num(raw[[7]][i])
      female_pct <- to_num(raw[[8]][i])

      # Skip non-data rows.
      if (all(is.na(c(male_count, female_count, male_pct, female_pct)))) next

      if (!is.na(male_pct)) {
        rows[[idx]] <- data.frame(
          residence = residence,
          education_level = education_level,
          sex = "Male",
          count = male_count,
          pct = male_pct
        )
        idx <- idx + 1
      }

      if (!is.na(female_pct)) {
        rows[[idx]] <- data.frame(
          residence = residence,
          education_level = education_level,
          sex = "Female",
          count = female_count,
          pct = female_pct
        )
        idx <- idx + 1
      }
    }

    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_table_20 <- function() {
    raw <- readxl::read_excel(excel_path, sheet = "Table 20", col_names = FALSE)
    if (ncol(raw) < 11) return(NULL)

    rows <- list()
    idx <- 1
    current_age_group <- NULL

    for (i in seq_len(nrow(raw))) {
      lab <- raw[[2]][i]
      if (is.na(lab)) next
      lab <- trimws(gsub("\u00a0", " ", as.character(lab)))

      if (!is.na(lab) && grepl("^Population\\s+\\d+\\s+years\\s+and\\s+above$", lab)) {
        current_age_group <- lab
        next
      }

      if (is.null(current_age_group) || current_age_group == "") next

      province <- lab

      male_rwanda <- to_num(raw[[4]][i])
      female_rwanda <- to_num(raw[[5]][i])
      male_urban <- to_num(raw[[7]][i])
      female_urban <- to_num(raw[[8]][i])
      male_rural <- to_num(raw[[10]][i])
      female_rural <- to_num(raw[[11]][i])

      entries <- list(
        list("Rwanda", male_rwanda, female_rwanda),
        list("Urban", male_urban, female_urban),
        list("Rural", male_rural, female_rural)
      )
      for (entry in entries) {
        residence <- entry[[1]]
        mval <- entry[[2]]
        fval <- entry[[3]]
        if (!is.na(mval)) {
          rows[[idx]] <- data.frame(age_group = current_age_group, province = province, residence = residence, sex = "Male", value = mval)
          idx <- idx + 1
        }
        if (!is.na(fval)) {
          rows[[idx]] <- data.frame(age_group = current_age_group, province = province, residence = residence, sex = "Female", value = fval)
          idx <- idx + 1
        }
      }
    }

    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_table_50 <- function() {
    raw <- readxl::read_excel(excel_path, sheet = "Table 50", col_names = FALSE)
    if (ncol(raw) < 11) return(NULL)

    labs <- raw[[2]]
    labs <- labs[!is.na(labs)]
    labs <- unique(vapply(labs, FUN.VALUE = character(1), FUN = function(z) {
      s <- gsub("\u00a0", " ", as.character(z))
      s <- trimws(s)
      s
    }))
    province_candidates <- labs[!(is.na(labs)) & (labs == "City of Kigali" | grepl("Province", labs))]

    rows <- list()
    idx <- 1
    current_prov <- NULL

    for (i in seq_len(nrow(raw))) {
      geo <- raw[[2]][i]
      if (is.na(geo)) next
      geo <- trimws(gsub("\u00a0", " ", as.character(geo)))
      if (geo %in% c("", "Province/District", "Total", "City of Kigali")) {
        # City of Kigali is a province candidate; we keep it below.
      }
      if (geo == "" || geo == "Province/District" || geo == "Total") next
      if (geo == "Rwanda") next

      male_total <- to_num(raw[[4]][i])
      female_total <- to_num(raw[[5]][i])
      male_urban <- to_num(raw[[7]][i])
      female_urban <- to_num(raw[[8]][i])
      male_rural <- to_num(raw[[10]][i])
      female_rural <- to_num(raw[[11]][i])

      if (all(is.na(c(male_total, female_total, male_urban, female_urban, male_rural, female_rural)))) next

      if (geo %in% province_candidates) {
        current_prov <- geo
        geo_type <- "Province"
        province <- geo
      } else {
        if (is.null(current_prov)) next
        geo_type <- "District"
        province <- current_prov
      }

      # residence in this table: Total, Urban, Rural
      entries <- list(
        list("Rwanda", male_total, female_total),
        list("Urban", male_urban, female_urban),
        list("Rural", male_rural, female_rural)
      )
      for (entry in entries) {
        residence <- entry[[1]]
        mval <- entry[[2]]
        fval <- entry[[3]]

        if (!is.na(mval)) {
          rows[[idx]] <- data.frame(
            geo = geo, geo_type = geo_type, province = province,
            residence = residence, sex = "Male", value = mval
          )
          idx <- idx + 1
        }
        if (!is.na(fval)) {
          rows[[idx]] <- data.frame(
            geo = geo, geo_type = geo_type, province = province,
            residence = residence, sex = "Female", value = fval
          )
          idx <- idx + 1
        }
      }
    }

    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_table_3 <- function() {
    raw <- readxl::read_excel(excel_path, sheet = "Table 3", col_names = FALSE)
    if (ncol(raw) < 8) return(NULL)
    rows <- list()
    idx <- 1
    for (i in seq_len(nrow(raw))) {
      label <- raw[[2]][i]
      if (is.na(label)) next
      label <- trimws(gsub("\u00a0", " ", as.character(label)))
      if (label == "" || grepl("^Table ", label)) next
      low <- norm_text(label)
      if (low %in% c("group", "specific age", "population")) next

      both_c <- to_num(raw[[3]][i])
      male_c <- to_num(raw[[4]][i])
      female_c <- to_num(raw[[5]][i])
      both_p <- to_num(raw[[6]][i])
      male_p <- to_num(raw[[7]][i])
      female_p <- to_num(raw[[8]][i])
      if (all(is.na(c(both_c, male_c, female_c, both_p, male_p, female_p)))) next

      for (entry in list(
        list("Both sexes", both_c, both_p),
        list("Male", male_c, male_p),
        list("Female", female_c, female_p)
      )) {
        sx <- entry[[1]]
        cct <- entry[[2]]
        pct <- entry[[3]]
        if (!all(is.na(c(cct, pct)))) {
          rows[[idx]] <- data.frame(age_group = label, sex = sx, count = cct, pct = pct, stringsAsFactors = FALSE)
          idx <- idx + 1
        }
      }
    }
    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_table_11 <- function() {
    raw <- readxl::read_excel(excel_path, sheet = "Table 11", col_names = FALSE)
    if (ncol(raw) < 8) return(NULL)
    rows <- list()
    idx <- 1
    current_res <- NULL
    for (i in seq_len(nrow(raw))) {
      c1 <- raw[[2]][i]
      c2 <- raw[[3]][i]
      c1s <- if (is.na(c1)) NA_character_ else trimws(gsub("\u00a0", " ", as.character(c1)))
      if (!is.na(c1s) && c1s %in% c("Rwanda", "Urban", "Rural")) current_res <- c1s
      c2s <- if (is.na(c2)) NA_character_ else trimws(gsub("\u00a0", " ", as.character(c2)))
      if (!c2s %in% c("Both sexes", "Male", "Female")) next
      if (is.null(current_res)) next

      count <- to_num(raw[[4]][i])
      for (entry in list(
        list("No longer attending", 6),
        list("Currently attending", 7),
        list("Never attended", 8)
      )) {
        metric <- entry[[1]]
        j <- entry[[2]]
        v <- to_num(raw[[j]][i])
        if (!is.na(v)) {
          rows[[idx]] <- data.frame(
            residence = current_res,
            sex = c2s,
            attendance_status = metric,
            value_pct = v,
            population_count = count,
            stringsAsFactors = FALSE
          )
          idx <- idx + 1
        }
      }
    }
    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  extract_triplet_geo <- function(sheet_name, indicator) {
    raw <- readxl::read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
    if (ncol(raw) < 11) return(NULL)

    labs <- raw[[2]]
    labs <- labs[!is.na(labs)]
    labs <- unique(vapply(labs, FUN.VALUE = character(1), FUN = function(z) {
      s <- gsub("\u00a0", " ", as.character(z))
      trimws(s)
    }))
    province_candidates <- labs[!(is.na(labs)) & (labs == "City of Kigali" | grepl("Province", labs))]

    rows <- list()
    idx <- 1
    current_prov <- NULL

    for (i in seq_len(nrow(raw))) {
      geo <- raw[[2]][i]
      if (is.na(geo)) next
      geo <- trimws(gsub("\u00a0", " ", as.character(geo)))
      if (geo %in% c("", "Province/District", "Province/ District")) next
      if (grepl("^Table ", geo)) next
      if (geo %in% c("Both sexes", "Male", "Female", "Bothsexea")) next

      vals <- vapply(3:11, function(j) to_num(raw[[j]][i]), numeric(1))
      if (all(is.na(vals))) next

      if (geo == "Rwanda") {
        geo_type <- "National"
        province <- "Rwanda"
      } else if (geo %in% province_candidates) {
        current_prov <- geo
        geo_type <- "Province"
        province <- geo
      } else {
        if (is.null(current_prov)) next
        geo_type <- "District"
        province <- current_prov
      }

      blocks <- list(list("Rwanda", 0L), list("Urban", 3L), list("Rural", 6L))
      sexes <- c("Both sexes", "Male", "Female")
      for (b in blocks) {
        res_name <- b[[1]]
        off <- b[[2]]
        for (si in seq_along(sexes)) {
          v <- vals[off + si]
          if (!is.na(v)) {
            rows[[idx]] <- data.frame(
              indicator = indicator,
              geo = geo,
              geo_type = geo_type,
              province = province,
              residence = res_name,
              sex = sexes[si],
              value = v,
              stringsAsFactors = FALSE
            )
            idx <- idx + 1
          }
        }
      }
    }

    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  out <- list(
    population_geo = tryCatch(extract_table_1(), error = function(e) NULL),
    population_change = tryCatch(extract_table_4(), error = function(e) NULL),
    age_distribution = tryCatch(extract_table_5(), error = function(e) NULL),
    internet_use = tryCatch(extract_table_20(), error = function(e) NULL),
    education_attendance = tryCatch(extract_table_10_education_attendance(), error = function(e) NULL),
    elderly_share = tryCatch(extract_table_50(), error = function(e) NULL),
    intervention_age_groups = tryCatch(extract_table_3(), error = function(e) NULL),
    school_7_18_attendance = tryCatch(extract_table_11(), error = function(e) NULL),
    school_13_18_by_geo = tryCatch(extract_triplet_geo("Table 15", "School attendance 13-18 (%)"), error = function(e) NULL),
    youth_share_by_geo = tryCatch(extract_triplet_geo("Table 45", "Youth share of population (%)"), error = function(e) NULL),
    sheet_manifest = NULL
  )

  if (all(sapply(out, is.null))) return(NULL)

  if (!is.null(out$population_geo)) {
    d <- out$population_geo
    mp <- d[d$geo_type == "District" & !is.na(d$province) & !is.na(d$geo), c("province", "geo"), drop = FALSE]
    colnames(mp) <- c("province", "district")
    out$geo_province_district_map <- unique(mp)
  } else {
    out$geo_province_district_map <- NULL
  }

  out$clean_dir_path <- file.path(dirname(excel_path), "clean")

  norm_demo_list(coerce_demography_tables(out))
}

