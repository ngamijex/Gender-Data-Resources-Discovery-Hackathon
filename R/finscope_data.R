# ── R/finscope_data.R ──────────────────────────────────────────────────────────
# FinScope 2024 Rwanda — data loading, labeling, and aggregation.
# Loads a pre-processed CSV (value-labelled by Python preprocessing),
# derives binary indicators, and produces aggregated summary tables.
#
# Source CSV: dashboard_data/Financial_Inclusion/finscope_2024_clean.csv

# ── 1. Raw data loader ─────────────────────────────────────────────────────────
load_finscope_data <- function() {
  # Make the path robust for deployed environments (working directory can differ).
  app_path <- tryCatch(
    shiny::getCurrentAppPath(),
    error = function(e) NULL
  )
  if (is.null(app_path) || !dir.exists(app_path)) {
    app_path <- tryCatch(getwd(), error = function(e) ".")
  }

  rel <- file.path("dashboard_data", "Financial_Inclusion", "finscope_2024_clean.csv")

  # Search for the CSV:
  #  1) Try a few parent-folder walkups from candidate roots.
  #  2) If still not found, do a targeted recursive filename search inside roots
  #     (useful for some deployed layouts where the relative path differs).
  search_roots <- unique(c(app_path, getwd()))
  find_csv <- function(roots, rel_path, max_up = 6) {
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

    # Recursive fallback: search by filename inside the roots.
    for (r in roots) {
      r0 <- suppressWarnings(normalizePath(r, winslash = "/", mustWork = FALSE))
      if (is.na(r0) || r0 == "" || !dir.exists(r0)) next

      matches <- suppressWarnings(
        list.files(
          r0,
          pattern = "^finscope_2024_clean\\.csv$",
          recursive = TRUE,
          full.names = TRUE,
          ignore.case = TRUE
        )
      )
      if (length(matches) > 0) {
        # Prefer the expected folder if present
        preferred <- matches[grepl("dashboard_data/.*/Financial_Inclusion", gsub("\\\\", "/", matches))]
        if (length(preferred) > 0) return(preferred[[1]])
        return(matches[[1]])
      }
    }

    NULL
  }

  path <- find_csv(search_roots, rel)

  if (is.null(path)) {
    tried <- paste(search_roots, collapse = ", ")
    message("[FI] Clean CSV not found. Tried roots: ", tried, " | rel: ", rel)
    return(NULL)
  }

  tryCatch({
    message("[FI] Loading FinScope 2024 Rwanda clean CSV...")
    df <- readr::read_csv(path, show_col_types = FALSE)
    message("[FI] Loaded: ", nrow(df), " respondents x ", ncol(df), " variables")

    # ── Core demographics ──────────────────────────────────────────────────────
    df$gender <- dplyr::case_when(
      df$b2 == "Male"   ~ "Male",
      df$b2 == "Female" ~ "Female",
      TRUE ~ NA_character_
    )
    df$area <- dplyr::case_when(
      df$a6 == "Urban" ~ "Urban",
      df$a6 == "Rural" ~ "Rural",
      TRUE ~ NA_character_
    )
    df$province <- dplyr::case_when(
      df$a1 == "Kigali" ~ "Kigali",
      df$a1 == "South"  ~ "South",
      df$a1 == "West"   ~ "West",
      df$a1 == "North"  ~ "North",
      df$a1 == "East"   ~ "East",
      TRUE ~ NA_character_
    )
    age_num    <- suppressWarnings(as.numeric(df$b1))
    df$age     <- age_num                              # raw numeric for age slider

    # District from column a2 (clean whitespace / "NA" strings)
    dist_raw   <- trimws(as.character(df$a2))
    df$district <- ifelse(is.na(df$a2) | dist_raw == "NA" | dist_raw == "", NA_character_, dist_raw)

    df$age_group <- dplyr::case_when(
      age_num >= 16 & age_num <= 24 ~ "16-24",
      age_num >= 25 & age_num <= 34 ~ "25-34",
      age_num >= 35 & age_num <= 44 ~ "35-44",
      age_num >= 45 & age_num <= 54 ~ "45-54",
      age_num >= 55                 ~ "55+",
      TRUE ~ NA_character_
    )

    # ── Financial products (CSV value = "Yes") ────────────────────────────────
    yes1 <- function(x) as.integer(!is.na(x) & trimws(as.character(x)) == "Yes")

    df$has_bank    <- yes1(df$qf1_01)   # Commercial bank
    df$has_digital <- yes1(df$qf1_02)   # Digital bank
    df$has_mfi     <- yes1(df$qf1_03)   # Microfinance (non-SACCO)
    df$has_mm      <- yes1(df$qf1_05)   # Mobile money operator
    df$has_sacco   <- yes1(df$qf1_06)   # Umurenge SACCO
    df$has_insure  <- yes1(df$qf1_07)   # Insurance
    df$has_pension <- yes1(df$qf1_08)   # Pension fund
    df$has_ejoheza <- yes1(df$qf1_09)   # EJOHEZA long-term savings
    df$has_savgrp  <- yes1(df$qf1_10)   # Savings group (informal)
    df$none_prod   <- yes1(df$qf1_12)   # None of the above

    # Formal financial inclusion
    df$any_formal <- as.integer(
      df$has_bank == 1 | df$has_digital == 1 | df$has_mfi == 1 |
      df$has_mm   == 1 | df$has_sacco   == 1 | df$has_insure == 1 |
      df$has_pension == 1
    )
    df$any_product <- as.integer(df$none_prod == 0)

    # Currently using
    df$curr_bank  <- yes1(df$qf4_01)
    df$curr_mm    <- yes1(df$qf4_05)
    df$curr_sacco <- yes1(df$qf4_06)

    # ── Mobile money account (Section L) ──────────────────────────────────────
    df$mm_account <- yes1(df$l1)

    # ── Savings behavior ──────────────────────────────────────────────────────
    df$saving_freq <- dplyr::case_when(
      df$e2c == "Always"      ~ "Always",
      df$e2c == "Sometimes"   ~ "Sometimes",
      df$e2c == "Do not save" ~ "Never",
      TRUE ~ NA_character_
    )
    df$saves <- as.integer(df$saving_freq %in% c("Always", "Sometimes"))

    # ── Credit ────────────────────────────────────────────────────────────────
    df$has_credit <- yes1(df$k1)

    # ── Financial wellbeing ───────────────────────────────────────────────────
    df$fin_trend <- dplyr::case_when(
      df$e9a == "Worsened"          ~ "Worsened",
      df$e9a == "Remained the same" ~ "Unchanged",
      df$e9a == "Improved"          ~ "Improved",
      TRUE ~ NA_character_
    )
    df$fin_control <- dplyr::case_when(
      stringr::str_detect(tolower(df$e1c), "lot of control")           ~ "A lot of control",
      stringr::str_detect(tolower(df$e1c), "scarce control")           ~ "Scarce control",
      stringr::str_detect(tolower(df$e1c), "some control")             ~ "Some control",
      stringr::str_detect(tolower(df$e1c), "no control")               ~ "No control",
      stringr::str_detect(tolower(df$e1c), "have control")             ~ "In control",
      TRUE ~ NA_character_
    )
    df$emergency_fund <- yes1(df$e9b)
    df$own_money      <- yes1(df$e2a)

    df

  }, error = function(e) {
    message("[FI] Load error: ", e$message)
    NULL
  })
}

# ── 2. Aggregation — produces pre-computed summary tables ─────────────────────
aggregate_finscope <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  tryCatch({
    pct <- function(x) round(mean(x, na.rm = TRUE) * 100, 1)

    # ── KPI summary by gender ─────────────────────────────────────────────────
    kpis <- dplyr::bind_rows(lapply(c("Male", "Female"), function(g) {
      sub <- df[!is.na(df$gender) & df$gender == g, ]
      data.frame(
        gender        = g,
        n             = nrow(sub),
        formal_pct    = pct(sub$any_formal),
        bank_pct      = pct(sub$has_bank),
        mm_pct        = pct(sub$has_mm),
        sacco_pct     = pct(sub$has_sacco),
        credit_pct    = pct(sub$has_credit),
        saves_pct     = pct(sub$saves),
        mm_acc_pct    = pct(sub$mm_account),
        emergency_pct = pct(sub$emergency_fund),
        stringsAsFactors = FALSE
      )
    }))

    # ── Product access by gender ──────────────────────────────────────────────
    products <- c(
      "Savings Group"   = "has_savgrp",
      "Mobile Money"    = "has_mm",
      "Umurenge SACCO"  = "has_sacco",
      "Commercial Bank" = "has_bank",
      "EJOHEZA"         = "has_ejoheza",
      "Microfinance"    = "has_mfi",
      "Insurance"       = "has_insure",
      "Pension Fund"    = "has_pension"
    )
    prod_gender <- dplyr::bind_rows(lapply(names(products), function(prod_name) {
      col <- products[[prod_name]]
      dplyr::bind_rows(lapply(c("Male", "Female"), function(g) {
        sub <- df[!is.na(df$gender) & df$gender == g, ]
        data.frame(
          product = prod_name, gender = g, n = nrow(sub),
          users   = sum(sub[[col]], na.rm = TRUE),
          pct     = pct(sub[[col]]),
          stringsAsFactors = FALSE
        )
      }))
    }))

    # ── Formal inclusion by province x gender ─────────────────────────────────
    prod_province <- df |>
      dplyr::filter(!is.na(gender), !is.na(province)) |>
      dplyr::group_by(province, gender) |>
      dplyr::summarise(
        n          = dplyr::n(),
        formal_pct = round(mean(any_formal, na.rm = TRUE) * 100, 1),
        bank_pct   = round(mean(has_bank,   na.rm = TRUE) * 100, 1),
        mm_pct     = round(mean(has_mm,     na.rm = TRUE) * 100, 1),
        sacco_pct  = round(mean(has_sacco,  na.rm = TRUE) * 100, 1),
        .groups = "drop"
      )

    # ── Savings by gender ─────────────────────────────────────────────────────
    savings_gender <- df |>
      dplyr::filter(!is.na(gender), !is.na(saving_freq)) |>
      dplyr::count(gender, saving_freq) |>
      dplyr::group_by(gender) |>
      dplyr::mutate(pct = round(n / sum(n) * 100, 1)) |>
      dplyr::ungroup()

    # ── Credit by gender ──────────────────────────────────────────────────────
    credit_gender <- dplyr::bind_rows(lapply(c("Male", "Female"), function(g) {
      sub <- df[!is.na(df$gender) & df$gender == g, ]
      data.frame(
        gender     = g,
        has_credit = pct(sub$has_credit),
        no_credit  = 100 - pct(sub$has_credit),
        stringsAsFactors = FALSE
      )
    }))

    # ── Financial inclusion by age group x gender ─────────────────────────────
    prod_age <- df |>
      dplyr::filter(!is.na(gender), !is.na(age_group)) |>
      dplyr::group_by(age_group, gender) |>
      dplyr::summarise(
        n          = dplyr::n(),
        formal_pct = round(mean(any_formal,  na.rm = TRUE) * 100, 1),
        mm_pct     = round(mean(has_mm,      na.rm = TRUE) * 100, 1),
        credit_pct = round(mean(has_credit,  na.rm = TRUE) * 100, 1),
        .groups = "drop"
      ) |>
      dplyr::mutate(age_group = factor(
        age_group, levels = c("16-24", "25-34", "35-44", "45-54", "55+")))

    # ── Financial control by gender ───────────────────────────────────────────
    ctrl_order <- c("A lot of control", "In control", "Some control",
                    "Scarce control",   "No control")
    control_gender <- df |>
      dplyr::filter(!is.na(gender), !is.na(fin_control)) |>
      dplyr::count(gender, fin_control) |>
      dplyr::group_by(gender) |>
      dplyr::mutate(pct = round(n / sum(n) * 100, 1)) |>
      dplyr::ungroup() |>
      dplyr::mutate(fin_control = factor(fin_control, levels = ctrl_order))

    # ── Financial trend by gender ─────────────────────────────────────────────
    trend_gender <- df |>
      dplyr::filter(!is.na(gender), !is.na(fin_trend)) |>
      dplyr::count(gender, fin_trend) |>
      dplyr::group_by(gender) |>
      dplyr::mutate(pct = round(n / sum(n) * 100, 1)) |>
      dplyr::ungroup()

    # ── Mobile money by area x gender ────────────────────────────────────────
    mm_area <- df |>
      dplyr::filter(!is.na(gender), !is.na(area)) |>
      dplyr::group_by(area, gender) |>
      dplyr::summarise(
        n       = dplyr::n(),
        mm_pct  = round(mean(has_mm,     na.rm = TRUE) * 100, 1),
        acc_pct = round(mean(mm_account, na.rm = TRUE) * 100, 1),
        .groups = "drop"
      )

    list(
      kpis           = kpis,
      prod_gender    = prod_gender,
      prod_province  = prod_province,
      savings_gender = savings_gender,
      credit_gender  = credit_gender,
      prod_age       = prod_age,
      control_gender = control_gender,
      trend_gender   = trend_gender,
      mm_area        = mm_area
    )

  }, error = function(e) {
    message("[FI] Aggregation error: ", e$message)
    NULL
  })
}
