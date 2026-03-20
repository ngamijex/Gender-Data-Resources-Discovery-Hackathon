# ── R/data_load.R ─────────────────────────────────────────────────────────────
# Loads and merges the three NISR CSV inventory files on startup.
# Produces two global objects used throughout the app:
#   catalog   — merged study-level data (73 rows)
#   resources — resource-level data (387 rows)

# ── Survey series classifier ───────────────────────────────────────────────────
classify_collection <- function(title) {
  t <- tolower(title)
  dplyr::case_when(
    stringr::str_detect(t, "demographic and health|\\bdhs\\b")                           ~ "DHS",
    stringr::str_detect(t, "integrated household|\\beicv\\b|living conditions survey")    ~ "EICV",
    stringr::str_detect(t, "population and housing census|recensement|general census|post enumeration") ~ "Census",
    stringr::str_detect(t, "labour force|labor force|\\blfs\\b")                          ~ "LFS",
    stringr::str_detect(t, "seasonal agri|agricultural household|\\bahs\\b|\\bsas\\b|season agri") ~ "Agriculture",
    stringr::str_detect(t, "finscope|financial inclusion")                                ~ "FinScope",
    stringr::str_detect(t, "food security|cfsva|cfsvans")                                 ~ "Food Security",
    stringr::str_detect(t, "establishment census")                                        ~ "Business Census",
    stringr::str_detect(t, "child labour")                                                ~ "Child Labour",
    stringr::str_detect(t, "manpower")                                                    ~ "Manpower",
    stringr::str_detect(t, "service provision")                                           ~ "Health Services",
    stringr::str_detect(t, "user satisfaction")                                           ~ "Governance",
    stringr::str_detect(t, "vision 2020|umurenge|\\bvup\\b")                              ~ "Social Protection",
    stringr::str_detect(t, "enterprise survey|micro.enterprise")                          ~ "Enterprise",
    TRUE                                                                                  ~ "Other"
  )
}

# ── Gender relevance scorer (0–10) ────────────────────────────────────────────
score_gender_relevance <- function(title, abstract, scope, collection) {
  corpus <- paste(title, abstract, scope, sep = " ") |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9 ]", " ")

  terms <- c(
    "gender", "women", "woman", "female", "girl", "maternal",
    "fertility", "reproductive", "contraception", "family planning",
    "pregnancy", "birth", "nutrition", "child health",
    "household head", "sex disaggregated", "antenatal", "breastfeed",
    "marriage", "marital", "widowed", "domestic violence", "gbv",
    "female headed", "female employ", "female labor", "female land"
  )
  hits <- sum(vapply(terms,
    function(x) as.integer(stringr::str_detect(corpus, x)),
    integer(1)
  ))

  bonus <- switch(collection,
    "DHS"          = 8,
    "EICV"         = 5,
    "Census"       = 3,
    "Food Security"= 3,
    "LFS"          = 2,
    "Agriculture"  = 1,
    0L
  )
  min(hits + bonus, 10L)
}

# ── Main loader ────────────────────────────────────────────────────────────────
load_catalog <- function() {
  studies   <- readr::read_csv("Data/studies.csv",
                               show_col_types = FALSE,
                               locale = readr::locale(encoding = "UTF-8")) |>
    suppressWarnings()
  resources <- readr::read_csv("Data/study_resources.csv", show_col_types = FALSE)
  quality   <- readr::read_csv("Data/quality_report.csv",  show_col_types = FALSE)

  # Resource summary per study
  res_sum <- resources |>
    dplyr::group_by(study_id) |>
    dplyr::summarise(
      n_resources       = dplyr::n(),
      resource_types    = paste(unique(type), collapse = ", "),
      has_report        = any(stringr::str_detect(tolower(name), "report|final")),
      has_questionnaire = any(stringr::str_detect(tolower(name), "questionnaire|quest")),
      .groups = "drop"
    )

  # Quality fields to enrich study metadata
  qual_trim <- quality |>
    dplyr::select(study_id, missing_field_count, resource_quality_flags)

  catalog <- studies |>
    dplyr::left_join(qual_trim, by = "study_id") |>
    dplyr::left_join(res_sum,   by = "study_id") |>
    dplyr::mutate(
      year       = suppressWarnings(as.integer(year)),
      views_num  = suppressWarnings(as.numeric(stringr::str_remove_all(as.character(views), ","))),
      collection = classify_collection(title),
      gender_score = mapply(
        score_gender_relevance,
        title,
        ifelse(is.na(abstract),    "", abstract),
        ifelse(is.na(scope_notes), "", scope_notes),
        collection,
        USE.NAMES = FALSE
      ),
      quality_status = dplyr::case_when(
        is.na(missing_field_count) | missing_field_count == 0 ~ "Complete",
        missing_field_count <= 2                              ~ "Minor Issues",
        TRUE                                                  ~ "Incomplete"
      ),
      access_clean = dplyr::case_when(
        stringr::str_detect(tolower(tidyr::replace_na(data_access_type, "")), "public")              ~ "Public",
        stringr::str_detect(tolower(tidyr::replace_na(data_access_type, "")), "licensed|restricted") ~ "Licensed",
        TRUE                                                                                          ~ "Other"
      ),
      abstract_card = dplyr::case_when(
        is.na(abstract)       ~ "No abstract available for this study.",
        nchar(abstract) > 280 ~ paste0(stringr::str_sub(abstract, 1, 280), "\u2026"),
        TRUE                  ~ abstract
      )
    )

  list(catalog = catalog, resources = resources)
}

# ── Execute on source — populate global objects ────────────────────────────────
message("[GDDP] Loading catalog data\u2026")
.app_data <- tryCatch(
  load_catalog(),
  error = function(e) {
    warning("[GDDP] Data load failed: ", e$message)
    list(catalog = data.frame(), resources = data.frame())
  }
)
catalog   <- .app_data$catalog
resources <- .app_data$resources
rm(.app_data)
message("[GDDP] Catalog ready: ", nrow(catalog), " studies | ",
        nrow(resources), " resources | ",
        min(catalog$year, na.rm = TRUE), "\u2013",
        max(catalog$year, na.rm = TRUE))

# Pre-compute search corpus (runs once at startup, reused on every query)
# Note: build_search_corpus is defined in R/search.R which is sourced after this.
# We defer corpus build to first search call via .catalog_corpus_cache below.
.catalog_corpus_cache <- NULL
