# ── R/search.R ─────────────────────────────────────────────────────────────────
# Advanced full-text search engine — fully vectorised (no row loops).
#
# Features:
#   • 30+ fields searched with weighted relevance scoring
#   • Multi-word AND logic  |  Quoted phrase matching  |  Negative terms (-word)
#   • OR groups  |  Year detection bonus
#   • Domain synonym expansion (women → female / gender / girl …)
#   • Relevance-first ranking with .search_score column
#   • Robust null/NA handling throughout

# ── 1. Domain synonym dictionary ──────────────────────────────────────────────
SEARCH_SYNONYMS <- list(
  women       = c("women","woman","female","girl","girls","femme"),
  men         = c("men","man","male","boy","boys"),
  gender      = c("gender","sex","gbv","inequality","parity"),
  maternal    = c("maternal","maternity","mother","mothers","birth",
                  "pregnancy","antenatal","postnatal","obstetric"),
  health      = c("health","medical","hospital","disease","mortality",
                  "nutrition","hiv","aids","malaria","vaccination","morbidity"),
  education   = c("education","school","literacy","enrollment","learning",
                  "student","pupil","academic","completion"),
  poverty     = c("poverty","poor","income","consumption","welfare",
                  "expenditure","living conditions","eicv","household"),
  labor       = c("labor","labour","work","employment","workforce",
                  "occupation","lfp","job","wage","salary"),
  violence    = c("violence","gbv","domestic","abuse","assault",
                  "rape","harassment","femicide"),
  land        = c("land","property","ownership","tenure"),
  census      = c("census","population","demographic","enumeration"),
  dhs         = c("dhs","demographic health","demographic and health"),
  eicv        = c("eicv","integrated household","living conditions"),
  lfs         = c("lfs","labour force","labor force"),
  fertility   = c("fertility","birth rate","tfr","total fertility",
                  "family planning","contraception","contraceptive"),
  child       = c("child","children","infant","under-five","u5",
                  "stunting","wasting","malnutrition"),
  food        = c("food","hunger","crop","agriculture","farming","harvest"),
  finance     = c("finance","financial","credit","saving","loan","bank",
                  "finscope","microfinance"),
  district    = c("district","province","kigali","northern","southern",
                  "eastern","western","rural","urban"),
  hiv         = c("hiv","aids","antiretroviral","prevalence"),
  water       = c("water","sanitation","hygiene","wash")
)

# ── 2. Field weights ───────────────────────────────────────────────────────────
FIELD_WEIGHTS <- c(
  title              = 18,
  abstract           = 10,
  scope_notes        = 7,
  study_description  = 5,
  data_description   = 4,
  study_type         = 4,
  kind_of_data       = 3,
  geographic_coverage= 3,
  geographic_unit    = 3,
  organization       = 3,
  universe           = 2,
  notes              = 2,
  producers          = 2,
  overview_summary   = 2,
  documentation      = 1,
  resource_corpus    = 4
)

# ── 3. Build lowercase corpus once at startup ──────────────────────────────────
build_search_corpus <- function(catalog_df, resources_df) {
  tryCatch({
    # Safe lowercase coercion for any column type
    slo <- function(x) {
      v <- suppressWarnings(as.character(x))
      v[is.na(v)] <- ""
      tolower(trimws(v))
    }

    # Aggregate resource text per study
    res_corp <- resources_df |>
      dplyr::group_by(study_id) |>
      dplyr::summarise(
        res_corpus = paste(
          slo(name), slo(label), slo(type),
          collapse = " "
        ),
        .groups = "drop"
      )

    df <- catalog_df |>
      dplyr::left_join(res_corp, by = "study_id")

    # Add corpus columns with safe prefix "srch_" (no dots)
    df[["srch_title"]]    <- slo(df[["title"]])
    df[["srch_abstract"]] <- slo(df[["abstract"]])
    df[["srch_scope"]]    <- slo(df[["scope_notes"]])
    df[["srch_sdesc"]]    <- slo(df[["study_description"]])
    df[["srch_ddesc"]]    <- slo(df[["data_description"]])
    df[["srch_stype"]]    <- slo(df[["study_type"]])
    df[["srch_kind"]]     <- slo(df[["kind_of_data"]])
    df[["srch_geo"]]      <- slo(df[["geographic_coverage"]])
    df[["srch_geounit"]]  <- slo(df[["geographic_unit"]])
    df[["srch_org"]]      <- slo(df[["organization"]])
    df[["srch_universe"]] <- slo(df[["universe"]])
    df[["srch_notes"]]    <- slo(df[["notes"]])
    df[["srch_prod"]]     <- slo(df[["producers_and_sponsors"]])
    df[["srch_overview"]] <- slo(df[["overview_summary"]])
    df[["srch_docs"]]     <- slo(df[["documentation"]])
    df[["srch_res"]]      <- slo(df[["res_corpus"]])

    # Combined full-text column used for AND-check and negative filtering
    df[["srch_full"]] <- paste(
      df[["srch_title"]], df[["srch_abstract"]], df[["srch_scope"]],
      df[["srch_sdesc"]], df[["srch_ddesc"]],   df[["srch_notes"]],
      df[["srch_universe"]], df[["srch_stype"]], df[["srch_geo"]],
      df[["srch_res"]]
    )

    df
  }, error = function(e) {
    message("[GDDP] build_search_corpus error: ", e$message,
            " — falling back to raw catalog.")
    NULL
  })
}

# ── 4. Query parser ────────────────────────────────────────────────────────────
parse_query <- function(q) {
  q <- trimws(tolower(q))

  # Quoted exact phrases
  phrases     <- regmatches(q, gregexpr('"[^"]+"', q))[[1]]
  phrases     <- gsub('"', '', phrases)
  q_noph      <- gsub('"[^"]+"', ' ', q)

  # Negative terms  -word
  negatives   <- regmatches(q_noph, gregexpr('-\\S+', q_noph))[[1]]
  negatives   <- gsub('^-', '', negatives)
  negatives   <- negatives[nchar(negatives) >= 2]
  q_noneg     <- gsub('-\\S+', ' ', q_noph)

  # OR groups  (word OR word)
  or_groups   <- list()
  or_m        <- gregexpr('\\S+\\s+or\\s+\\S+', q_noneg, perl = TRUE)
  or_matches  <- regmatches(q_noneg, or_m)[[1]]
  for (m in or_matches) {
    parts <- trimws(strsplit(m, "\\bor\\b")[[1]])
    or_groups <- c(or_groups, list(parts))
    q_noneg   <- sub(m, ' ', q_noneg, fixed = TRUE)
  }

  # Year detection (1970–2030)
  years <- as.integer(regmatches(q,
    gregexpr("\\b(197[0-9]|198[0-9]|199[0-9]|200[0-9]|201[0-9]|202[0-9]|2030)\\b", q)
  )[[1]])

  # Individual keywords (stop-words removed, min 2 chars)
  stop_words <- c("the","a","an","of","in","and","to","for","is","was",
                  "are","were","with","by","on","at","this","that","which",
                  "from","as","its","be","has","have","had","will","do","did")
  words    <- stringr::str_split(trimws(q_noneg), "\\s+")[[1]]
  keywords <- unique(words[nchar(words) >= 2 & !words %in% stop_words])

  list(phrases=phrases, negatives=negatives, or_groups=or_groups,
       keywords=keywords, years=years, raw=q)
}

# ── 5. Synonym expansion ───────────────────────────────────────────────────────
expand_synonyms <- function(keywords) {
  expanded <- keywords
  for (kw in keywords)
    for (grp in SEARCH_SYNONYMS)
      if (kw %in% grp) { expanded <- unique(c(expanded, grp)); break }
  expanded
}

# ── 6. Vectorised column scorer — operates on entire column at once ────────────
score_column <- function(col_vec, terms, weight) {
  # col_vec : character vector (one element per study)
  # terms   : character vector of search terms
  # returns : numeric vector of scores, same length as col_vec
  if (length(col_vec) == 0 || length(terms) == 0 || weight == 0) {
    return(rep(0, length(col_vec)))
  }
  sc <- rep(0, length(col_vec))
  for (term in terms) {
    pat  <- stringr::fixed(term)
    hits <- stringr::str_detect(col_vec, pat)
    hits[is.na(hits)] <- FALSE
    sc <- sc + (hits * weight)

    # Whole-word bonus
    wb   <- stringr::str_detect(col_vec, paste0("\\b", term, "\\b"))
    wb[is.na(wb)] <- FALSE
    sc <- sc + (wb * ceiling(weight * 0.5))
  }
  sc
}

# ── 7. Main search entry point ─────────────────────────────────────────────────
advanced_search <- function(corpus, q) {

  # Null / empty guards
  if (is.null(corpus) || !is.data.frame(corpus) || nrow(corpus) == 0)
    return(corpus)
  if (nchar(trimws(q)) == 0) return(corpus)

  parsed    <- parse_query(q)
  kw_core   <- parsed$keywords
  kw_all    <- expand_synonyms(kw_core)
  phrases   <- parsed$phrases
  negatives <- parsed$negatives
  years     <- parsed$years
  or_groups <- parsed$or_groups

  nothing_to_search <- length(kw_core) == 0 && length(phrases) == 0 &&
                       length(years)   == 0 && length(or_groups) == 0
  if (nothing_to_search) return(corpus)

  n      <- nrow(corpus)
  scores <- rep(0, n)

  # ── Helper: get corpus column safely ────────────────────────────────────────
  col <- function(name) {
    if (name %in% names(corpus)) corpus[[name]] else rep("", n)
  }

  # ── Score every field with keyword list ─────────────────────────────────────
  all_terms <- unique(c(kw_all, phrases))

  scores <- scores + score_column(col("srch_title"),    all_terms, FIELD_WEIGHTS[["title"]])
  scores <- scores + score_column(col("srch_abstract"), all_terms, FIELD_WEIGHTS[["abstract"]])
  scores <- scores + score_column(col("srch_scope"),    all_terms, FIELD_WEIGHTS[["scope_notes"]])
  scores <- scores + score_column(col("srch_sdesc"),    all_terms, FIELD_WEIGHTS[["study_description"]])
  scores <- scores + score_column(col("srch_ddesc"),    all_terms, FIELD_WEIGHTS[["data_description"]])
  scores <- scores + score_column(col("srch_stype"),    all_terms, FIELD_WEIGHTS[["study_type"]])
  scores <- scores + score_column(col("srch_kind"),     all_terms, FIELD_WEIGHTS[["kind_of_data"]])
  scores <- scores + score_column(col("srch_geo"),      all_terms, FIELD_WEIGHTS[["geographic_coverage"]])
  scores <- scores + score_column(col("srch_geounit"),  all_terms, FIELD_WEIGHTS[["geographic_unit"]])
  scores <- scores + score_column(col("srch_org"),      all_terms, FIELD_WEIGHTS[["organization"]])
  scores <- scores + score_column(col("srch_universe"), all_terms, FIELD_WEIGHTS[["universe"]])
  scores <- scores + score_column(col("srch_notes"),    all_terms, FIELD_WEIGHTS[["notes"]])
  scores <- scores + score_column(col("srch_prod"),     all_terms, FIELD_WEIGHTS[["producers"]])
  scores <- scores + score_column(col("srch_overview"), all_terms, FIELD_WEIGHTS[["overview_summary"]])
  scores <- scores + score_column(col("srch_docs"),     all_terms, FIELD_WEIGHTS[["documentation"]])
  scores <- scores + score_column(col("srch_res"),      all_terms, FIELD_WEIGHTS[["resource_corpus"]])

  # ── OR group bonus ───────────────────────────────────────────────────────────
  full <- col("srch_full")
  for (grp in or_groups) {
    grp_hits <- rep(FALSE, n)
    for (w in grp) grp_hits <- grp_hits | stringr::str_detect(full, stringr::fixed(w))
    grp_hits[is.na(grp_hits)] <- FALSE
    scores <- scores + (grp_hits * FIELD_WEIGHTS[["abstract"]])
  }

  # ── Year exact-match bonus ───────────────────────────────────────────────────
  if (length(years) > 0 && "year" %in% names(corpus)) {
    yr_vec <- suppressWarnings(as.integer(corpus[["year"]]))
    yr_hit <- !is.na(yr_vec) & (yr_vec %in% years)
    scores <- scores + (yr_hit * 25)
  }

  # ── Gender-score boost (domain relevance signal) ──────────────────────────
  if ("gender_score" %in% names(corpus)) {
    gs <- suppressWarnings(as.numeric(corpus[["gender_score"]]))
    gs[is.na(gs)] <- 0
    scores <- scores + gs
  }

  # ── Negative terms — zero out matching rows ──────────────────────────────────
  if (length(negatives) > 0) {
    neg_hit <- rep(FALSE, n)
    for (ng in negatives)
      neg_hit <- neg_hit | stringr::str_detect(full, stringr::fixed(ng))
    neg_hit[is.na(neg_hit)] <- FALSE
    scores[neg_hit] <- -1
  }

  # ── AND logic: penalise partial multi-keyword matches ───────────────────────
  if (length(kw_core) >= 2 && length(phrases) == 0 && length(or_groups) == 0) {
    for (i in which(scores > 0)) {
      n_present <- sum(vapply(kw_core, function(kw)
        stringr::str_detect(full[i], stringr::fixed(kw)), logical(1)))
      if (n_present < length(kw_core))
        scores[i] <- scores[i] * (n_present / length(kw_core)) * 0.6
    }
  }

  corpus[[".search_score"]] <- round(scores, 1)

  # Keep only positive-scoring rows, sorted best first
  result <- corpus[scores > 0, ]
  result[order(-result[[".search_score"]]), ]
}
