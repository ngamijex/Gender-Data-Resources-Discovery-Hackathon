# ── R/server.R ────────────────────────────────────────────────────────────────
# Full server logic for GDDP.
# Reads from global objects: catalog, resources, reference data frames, CLR.

server <- function(input, output, session) {

  # ── Navigation shortcuts from Home ────────────────────────────────────────
  shiny::observeEvent(input$btn_go_disc,
    shiny::updateNavbarPage(session, "main_nav", selected = "Data Discovery"))
  shiny::observeEvent(input$btn_go_dash,
    shiny::updateNavbarPage(session, "main_nav", selected = "Dashboard"))

  # ── Committed query — updated only when Search is explicitly triggered ───────
  committed_query <- shiny::reactiveVal("")

  # Search button commits the query
  shiny::observeEvent(input$btn_do_search, {
    committed_query(stringr::str_trim(tidyr::replace_na(input$srch_text, "")))
  })

  # ── Reset all discovery filters ────────────────────────────────────────────
  shiny::observeEvent(input$btn_reset, {
    committed_query("")
    shiny::updateTextInput(session, "srch_text", value = "")
    shiny::updateCheckboxGroupInput(session, "flt_collection",
      selected = c("DHS","EICV","Census","LFS","Agriculture","FinScope",
                   "Food Security","Business Census","Child Labour",
                   "Social Protection","Enterprise","Manpower",
                   "Health Services","Governance","Other"))
    shiny::updateSliderInput(session, "flt_year",   value = c(1978, 2024))
    shiny::updateSliderInput(session, "flt_gender", value = c(0, 10))
    shiny::updateCheckboxGroupInput(session, "flt_quality",
      selected = c("Complete","Minor Issues","Incomplete"))
    shiny::updateCheckboxGroupInput(session, "flt_access",
      selected = c("Public","Licensed","Other"))
    shiny::updateSelectInput(session, "sort_by", selected = "year_desc")
  })

  # ── Quick topic tags — fill the bar AND commit immediately ─────────────────
  # These quick tags should be "reliable": when a user clicks a tag we
  # slightly broaden the active filters so the keyword has a better chance
  # to match something in the studies catalog.
  apply_broad_qtag_filters <- function() {
    shiny::updateCheckboxGroupInput(session, "flt_collection",
      selected = c("DHS","EICV","Census","LFS","Agriculture","FinScope",
                   "Food Security","Business Census","Child Labour",
                   "Social Protection","Enterprise","Manpower",
                   "Health Services","Governance","Other")
    )
    shiny::updateSliderInput(session, "flt_year",   value = c(1978, 2024))
    shiny::updateSliderInput(session, "flt_gender", value = c(0, 10))
    shiny::updateCheckboxGroupInput(session, "flt_quality",
      selected = c("Complete","Minor Issues","Incomplete")
    )
    shiny::updateCheckboxGroupInput(session, "flt_access",
      selected = c("Public","Licensed","Other")
    )
    # Keep sort_by unchanged to respect user's preference
  }

  # Validate quick-tag queries against the *no-AI* search engine,
  # so we don't end up committing a query that produces 0 results.
  # We try multiple candidates and pick the first one that yields hits.
  qtag_hits <- function(q) {
    q <- trimws(q)
    if (nchar(q) == 0) return(0)

    # Prefer the prebuilt corpus search (same as no-AI search path)
    if (!is.null(.catalog_corpus_cache) &&
        is.data.frame(.catalog_corpus_cache) &&
        nrow(.catalog_corpus_cache) > 0) {
      df <- tryCatch(
        advanced_search(.catalog_corpus_cache, q),
        error = function(e) NULL
      )
      if (!is.null(df)) return(nrow(df))
    }

    # Fallback: simple search on raw catalog
    ql <- tolower(q)
    df <- tryCatch(
      catalog |> dplyr::filter(
        stringr::str_detect(tolower(title),                                     ql) |
          stringr::str_detect(tolower(tidyr::replace_na(abstract,    "")),        ql) |
          stringr::str_detect(tolower(tidyr::replace_na(scope_notes, "")),        ql) |
          stringr::str_detect(tolower(tidyr::replace_na(study_type,  "")),        ql) |
          stringr::str_detect(tolower(tidyr::replace_na(organization,"")),       ql)
      ),
      error = function(e) NULL
    )
    if (is.null(df)) 0 else nrow(df)
  }

  qtag_choose_query <- function(candidates) {
    for (cand in candidates) {
      if (qtag_hits(cand) > 0) return(cand)
    }
    # If all fail, return the first candidate (still better than empty)
    candidates[[1]]
  }

  shiny::observeEvent(input$qt_maternal, {
    apply_broad_qtag_filters()
    q <- "pregnancy"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_fertility, {
    apply_broad_qtag_filters()
    q <- "fertility"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_gbv, {
    apply_broad_qtag_filters()
    q <- "gbv"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_women_emp, {
    apply_broad_qtag_filters()
    q <- "employment"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_girls_edu, {
    apply_broad_qtag_filters()
    q <- "education"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_hiv, {
    apply_broad_qtag_filters()
    q <- "hiv"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_child, {
    apply_broad_qtag_filters()
    q <- "child"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_land, {
    apply_broad_qtag_filters()
    q <- "land"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_finc, {
    apply_broad_qtag_filters()
    q <- "financial"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_femhh, {
    apply_broad_qtag_filters()
    q <- "household"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_dhs, {
    apply_broad_qtag_filters()
    q <- "DHS"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })
  shiny::observeEvent(input$qt_eicv, {
    apply_broad_qtag_filters()
    q <- "EICV"
    shiny::updateTextInput(session, "srch_text", value = q)
    committed_query(q)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # HOME — static content only; no server outputs needed for PMV section
  # ══════════════════════════════════════════════════════════════════════════

  # ══════════════════════════════════════════════════════════════════════════
  # DATA DISCOVERY
  # ══════════════════════════════════════════════════════════════════════════

  filtered_catalog <- shiny::reactive({
    q <- trimws(committed_query())

    # ── Step 1: Full-text search ─────────────────────────────────────────────
    if (nchar(q) > 0) {
      if (!is.null(.catalog_corpus_cache) &&
          is.data.frame(.catalog_corpus_cache) &&
          nrow(.catalog_corpus_cache) > 0) {
        # Advanced vectorised search over prebuilt corpus
        df <- tryCatch(
          advanced_search(.catalog_corpus_cache, q),
          error = function(e) {
            message("[GDDP] advanced_search error: ", e$message)
            NULL
          }
        )
        # If advanced search returned NULL or empty, fall back to simple search
        if (is.null(df) || nrow(df) == 0) {
          ql <- tolower(q)
          df <- catalog |> dplyr::filter(
            stringr::str_detect(tolower(title),                                     ql) |
            stringr::str_detect(tolower(tidyr::replace_na(abstract,    "")),        ql) |
            stringr::str_detect(tolower(tidyr::replace_na(scope_notes, "")),        ql) |
            stringr::str_detect(tolower(tidyr::replace_na(study_type,  "")),        ql) |
            stringr::str_detect(tolower(tidyr::replace_na(organization,"")),        ql)
          )
        }
      } else {
        # No corpus — simple multi-field keyword search
        ql <- tolower(q)
        df <- catalog |> dplyr::filter(
          stringr::str_detect(tolower(title),                                     ql) |
          stringr::str_detect(tolower(tidyr::replace_na(abstract,    "")),        ql) |
          stringr::str_detect(tolower(tidyr::replace_na(scope_notes, "")),        ql) |
          stringr::str_detect(tolower(tidyr::replace_na(study_type,  "")),        ql) |
          stringr::str_detect(tolower(tidyr::replace_na(organization,"")),        ql)
        )
      }
    } else {
      df <- if (!is.null(.catalog_corpus_cache) &&
                is.data.frame(.catalog_corpus_cache))
              .catalog_corpus_cache
            else
              catalog
    }

    # Ensure df is a valid data frame
    if (is.null(df) || !is.data.frame(df)) df <- catalog

    # ── Step 2: Panel filters ────────────────────────────────────────────────
    if (length(input$flt_collection) > 0)
      df <- df |> dplyr::filter(collection %in% input$flt_collection)

    yr <- input$flt_year
    if (!is.null(yr))
      df <- df |> dplyr::filter(is.na(year) | (year >= yr[1] & year <= yr[2]))

    gs <- input$flt_gender
    if (!is.null(gs))
      df <- df |> dplyr::filter(gender_score >= gs[1] & gender_score <= gs[2])

    if (length(input$flt_quality) > 0)
      df <- df |> dplyr::filter(quality_status %in% input$flt_quality)

    if (length(input$flt_access) > 0)
      df <- df |> dplyr::filter(access_clean %in% input$flt_access)

    # ── Step 3: Sort ─────────────────────────────────────────────────────────
    has_score <- ".search_score" %in% names(df)

    df <- switch(input$sort_by,
      "relevance"   = if (nchar(q) > 0 && has_score)
                        df[order(-df[[".search_score"]]), ]
                      else
                        df |> dplyr::arrange(dplyr::desc(year)),
      "year_desc"   = df |> dplyr::arrange(dplyr::desc(year)),
      "year_asc"    = df |> dplyr::arrange(year),
      "gender_desc" = df |> dplyr::arrange(dplyr::desc(gender_score)),
      "views_desc"  = df |> dplyr::arrange(dplyr::desc(views_num)),
      "title_asc"   = df |> dplyr::arrange(title),
      if (nchar(q) > 0 && has_score)
        df[order(-df[[".search_score"]]), ]
      else
        df |> dplyr::arrange(dplyr::desc(year))
    )
    df
  })

  # ── AI Overview — runs independently over the full catalog, not filtered results
  ai_overview_result <- shiny::reactive({
    q <- committed_query()
    if (nchar(q) == 0) return(NULL)
    build_ai_overview(query = q, studies_df = catalog)
  })

  output$ai_overview_panel <- shiny::renderUI({
    q      <- committed_query()
    if (nchar(q) == 0) return(NULL)

    result <- ai_overview_result()

    # ── No API key or API error ────────────────────────────────────────────────
    if (is.null(result)) {
      api_key <- Sys.getenv("OPENAI_API_KEY")
      if (is.null(api_key) || nchar(trimws(api_key)) == 0) return(NULL)
      return(
        shiny::div(class = "ai-ov ai-ov--warn",
          shiny::div(class = "ai-ov__header",
            shiny::tags$i(class = "fas fa-exclamation-circle fa-xs"),
            " AI Overview unavailable"
          ),
          shiny::div(class = "ai-ov__body",
            shiny::tags$p(class = "ai-ov__summary",
              "Could not reach the AI service. Please check your OPENAI_API_KEY and internet connection."
            )
          )
        )
      )
    }

    # ── Safe scalar extractor (same pattern as study detail panel) ───────────
    ai_safe <- function(x, fallback = "\u2014") {
      v <- tryCatch(as.character(x[[1]]), error = function(e) NA_character_)
      if (is.null(v) || length(v) == 0 || is.na(v) || v == "NA") fallback else v
    }

    # ── Build recommendation cards ────────────────────────────────────────────
    rec_cards <- lapply(result$recommendations, function(r) {

      ai_title  <- tidyr::replace_na(as.character(r$title),  "")
      ai_year   <- tidyr::replace_na(as.character(r$year),   "")
      ai_series <- tidyr::replace_na(as.character(r$series), "")
      ai_reason <- tidyr::replace_na(as.character(r$reason), "")

      # Match back to catalog by exact title (case-insensitive fallback to partial)
      matched <- catalog |>
        dplyr::filter(tolower(trimws(title)) == tolower(trimws(ai_title)))
      if (nrow(matched) == 0)
        matched <- catalog |>
          dplyr::filter(stringr::str_detect(
            tolower(title), stringr::fixed(tolower(substr(ai_title, 1, 40)))
          ))

      # Use real catalog data if found, otherwise fall back to AI-returned values
      if (nrow(matched) > 0) {
        s         <- matched[1, ]
        sid       <- ai_safe(s$study_id, "")
        s_title   <- ai_safe(s$title,           ai_title)
        s_year    <- ai_safe(s$year,             ai_year)
        s_coll    <- ai_safe(s$collection,       ai_series)
        s_score   <- max(0L, min(10L, suppressWarnings(
                       as.integer(ai_safe(s$gender_score, "0")))))
        s_access  <- ai_safe(s$access_clean,     "\u2014")
        s_quality <- ai_safe(s$quality_status,   "Unknown")
        s_geo     <- ai_safe(s$geographic_coverage, "National")
        s_org     <- stringr::str_trunc(ai_safe(s$organization, "NISR"), 60)
        s_url     <- ai_safe(s$url, "")
        s_abs     <- ai_safe(s$abstract, "No abstract available.")

        n_res <- nrow(resources |>
          dplyr::filter(as.character(study_id) == as.character(sid)))
      } else {
        sid       <- ""
        s_title   <- ai_title
        s_year    <- ai_year
        s_coll    <- ai_series
        s_score   <- suppressWarnings(
                       as.integer(gsub("/.*", "", tidyr::replace_na(
                         as.character(r$gender_score), "0"))))
        s_score   <- if (is.na(s_score)) 0L else max(0L, min(10L, s_score))
        s_access  <- "\u2014"
        s_quality <- "Unknown"
        s_geo     <- "National"
        s_org     <- "NISR"
        s_url     <- tidyr::replace_na(as.character(r$url), "")
        s_abs     <- ""
        n_res     <- 0L
      }

      score_chip_cls <- paste("ai-rec__chip ai-rec__chip--score",
        if (s_score >= 7) "ai-rec__chip--score-hi"
        else if (s_score >= 4) "ai-rec__chip--score-mid"
        else "ai-rec__chip--score-lo"
      )

      # Title: opens detail panel if catalog match found, else opens NISR URL
      title_node <- if (nchar(sid) > 0)
        shiny::tags$a(
          class   = "ai-rec__title",
          href    = "#",
          onclick = paste0(
            "Shiny.setInputValue('study_click','", sid,
            "|'+Date.now(),{priority:'event'});return false;"
          ),
          shiny::tags$i(class = "fas fa-book-open fa-xs"), " ", s_title
        )
      else if (nchar(s_url) > 4)
        shiny::tags$a(
          class  = "ai-rec__title",
          href   = s_url, target = "_blank",
          shiny::tags$i(class = "fas fa-external-link-alt fa-xs"), " ", s_title
        )
      else
        shiny::div(class = "ai-rec__title", s_title)

      shiny::div(class = "ai-rec",

        # Chips row
        shiny::div(class = "ai-rec__meta",
          if (nchar(s_year) > 0)
            shiny::tags$span(class = "ai-rec__chip ai-rec__chip--year", s_year),
          if (nchar(s_coll) > 0)
            shiny::tags$span(class = "ai-rec__chip ai-rec__chip--ser",  s_coll),
          shiny::tags$span(class = score_chip_cls,
            shiny::tags$i(class = "fas fa-venus fa-xs"),
            paste0(" ", s_score, "/10")
          ),
          if (s_access != "\u2014")
            shiny::tags$span(class = paste("ai-rec__chip",
              if (s_access == "Public") "ai-rec__chip--public"
              else "ai-rec__chip--licensed"), s_access)
        ),

        # Clickable title
        title_node,

        # AI reason
        shiny::div(class = "ai-rec__reason",
          shiny::tags$i(class = "fas fa-lightbulb fa-xs ai-rec__reason-icon"),
          " ", ai_reason
        ),

        # Dataset details row
        shiny::div(class = "ai-rec__details",
          if (nchar(s_org) > 0 && s_org != "\u2014")
            shiny::div(class = "ai-rec__detail-item",
              shiny::tags$i(class = "fas fa-building fa-xs"),
              " ", s_org
            ),
          if (nchar(s_geo) > 0 && s_geo != "\u2014")
            shiny::div(class = "ai-rec__detail-item",
              shiny::tags$i(class = "fas fa-map-marker-alt fa-xs"),
              " ", s_geo
            ),
          if (nchar(s_quality) > 0 && s_quality != "Unknown")
            shiny::div(class = "ai-rec__detail-item",
              shiny::tags$i(class = "fas fa-check-circle fa-xs"),
              " ", s_quality
            ),
          if (n_res > 0)
            shiny::div(class = "ai-rec__detail-item ai-rec__detail-item--res",
              shiny::tags$i(class = "fas fa-paperclip fa-xs"),
              paste0(" ", n_res, " resource", if (n_res != 1) "s")
            )
        ),

        # Abstract snippet
        if (nchar(s_abs) > 10)
          shiny::div(class = "ai-rec__abstract",
            substr(s_abs, 1, 180),
            if (nchar(s_abs) > 180) "\u2026"
          ),

        # Action footer
        shiny::div(class = "ai-rec__actions",
          if (nchar(sid) > 0)
            shiny::tags$a(
              class   = "ai-rec__action-btn ai-rec__action-btn--primary",
              href    = "#",
              onclick = paste0(
                "Shiny.setInputValue('study_click','", sid,
                "|'+Date.now(),{priority:'event'});return false;"
              ),
              shiny::tags$i(class = "fas fa-info-circle fa-xs"), " Full Details"
            ),
          if (nchar(s_url) > 4)
            shiny::tags$a(
              class  = "ai-rec__action-btn ai-rec__action-btn--secondary",
              href   = s_url, target = "_blank",
              shiny::tags$i(class = "fas fa-external-link-alt fa-xs"),
              " Open in NISR"
            )
        )
      )
    })

    # ── Render the full overview card ─────────────────────────────────────────
    shiny::div(class = "ai-ov",
      shiny::div(class = "ai-ov__header",
        shiny::div(class = "ai-ov__header-left",
          shiny::tags$i(class = "fas fa-magic ai-ov__icon"),
          shiny::tags$span(class = "ai-ov__label", "AI Overview"),
          shiny::tags$span(class = "ai-ov__model", "GPT-4o")
        ),
        shiny::div(class = "ai-ov__header-right",
          shiny::tags$span(class = "ai-ov__query",
            shiny::tags$i(class = "fas fa-search fa-xs"), " ", q
          )
        )
      ),
      shiny::div(class = "ai-ov__body",
        shiny::tags$p(class = "ai-ov__summary",
          tidyr::replace_na(as.character(result$overview), "")
        ),
        if (length(rec_cards) > 0)
          shiny::div(class = "ai-ov__recs",
            shiny::div(class = "ai-recs__label",
              shiny::tags$i(class = "fas fa-star fa-xs"), " Recommended Studies"
            ),
            shiny::div(class = "ai-recs__grid", rec_cards)
          )
      ),
      shiny::div(class = "ai-ov__footer",
        shiny::tags$i(class = "fas fa-info-circle fa-xs"),
        " AI-generated overview based on NISR catalog metadata. Always verify findings with the original source."
      )
    )
  })

  # ── Search engine meta bar ─────────────────────────────────────────────────
  output$sep_meta_bar <- shiny::renderUI({
    q <- committed_query()
    if (nchar(q) == 0) return(NULL)

    n   <- nrow(filtered_catalog())
    tot <- nrow(catalog)

    shiny::div(class = "sep-meta-bar",
      shiny::div(class = "sep-meta-left",
        shiny::tags$span(class = "sep-meta-count",
          paste0("About ", n, " result", if (n != 1) "s")
        ),
        shiny::tags$span(class = "sep-meta-query",
          paste0(' for \u201c', q, '\u201d')
        )
      ),
      shiny::div(class = "sep-meta-right",
        shiny::tags$span(class = "sep-meta-total",
          paste0(tot, " surveys in catalog")
        )
      )
    )
  })

  # ── Search result items ────────────────────────────────────────────────────
  output$sep_result_items <- shiny::renderUI({
    q  <- committed_query()

    # ── Landing state (no query yet) ────────────────────────────────────────
    if (nchar(q) == 0) return(NULL)

    df <- filtered_catalog()

    # ── No results state ────────────────────────────────────────────────────
    if (nrow(df) == 0) {
      return(shiny::div(class = "sep-no-results",
        shiny::tags$i(class = "fas fa-search-minus sep-no-results__icon"),
        shiny::tags$h3(class = "sep-no-results__title",
          "No results for ", shiny::tags$em(q)
        ),
        shiny::tags$p(class = "sep-no-results__sub",
          "Try different keywords, or use the Advanced filters to broaden your search."
        ),
        shiny::div(class = "sep-no-results__tips",
          shiny::tags$span("Suggestions: "),
          shiny::tags$span(class = "sep-tip", "Check spelling"),
          shiny::tags$span(class = "sep-tip", "Use broader terms"),
          shiny::tags$span(class = "sep-tip", "Try a series name: DHS, EICV, Census")
        )
      ))
    }

    # ── Results list ────────────────────────────────────────────────────────
    items <- lapply(seq_len(min(nrow(df), 50)), function(i) build_search_result(df[i, ], q))
    shiny::div(class = "sep-result-list", items)
  })

  # ── Study detail panel — pure renderUI, no modals, no observers ──────────────
  # ── Study detail popup — fires on every study_click, shows a modal ───────────
  shiny::observeEvent(input$study_click, {
    # Value format: "study_id" or "study_id|timestamp" — strip the timestamp part
    raw <- trimws(as.character(input$study_click))
    id  <- trimws(strsplit(raw, "\\|")[[1]][1])
    if (nchar(id) == 0) return()

    tryCatch({
      s <- catalog |> dplyr::filter(as.character(study_id) == id)
      if (nrow(s) == 0) return()
      s <- s[1, ]

      # Safe scalar extractor
      safe <- function(x, fallback = "\u2014") {
        v <- tryCatch(as.character(x[[1]]), error = function(e) NA_character_)
        if (is.null(v) || length(v) == 0 || is.na(v) || v == "NA") fallback else v
      }

      s_year    <- safe(s$year,                "Unknown")
      s_type    <- safe(s$study_type,          safe(s$collection, "\u2014"))
      s_score   <- max(0L, min(10L, suppressWarnings(as.integer(safe(s$gender_score, "0")))))
      s_org     <- stringr::str_trunc(safe(s$organization, "NISR"), 80)
      s_access  <- safe(s$access_clean,        "Other")
      s_quality <- safe(s$quality_status,      "Unknown")
      s_geo     <- safe(s$geographic_coverage, "National coverage")
      s_abs     <- safe(s$abstract,            "No abstract available for this study.")
      s_title   <- safe(s$title,               "Untitled Study")
      s_url     <- safe(s$url,                 "")
      s_coll    <- safe(s$collection,          "Survey")

      study_res <- resources |> dplyr::filter(as.character(study_id) == id)

      res_rows <- if (nrow(study_res) > 0) {
        lapply(seq_len(min(nrow(study_res), 20)), function(ri) {
          r      <- study_res[ri, ]
          r_type <- toupper(safe(r$type, "FILE"))
          r_name <- safe(r$name, "Resource")
          r_url  <- safe(r$url,  "")
          shiny::div(class = "dp-res-row",
            shiny::tags$span(class = "dp-res-type", r_type),
            if (nchar(r_url) > 4)
              shiny::tags$a(class = "dp-res-name", href = r_url, target = "_blank", r_name)
            else
              shiny::tags$span(class = "dp-res-name", r_name)
          )
        })
      } else {
        list(shiny::tags$p(class = "dp-no-res", "No downloadable resources listed."))
      }

      # ── Modal content ───────────────────────────────────────────────────────
      modal_content <- shiny::div(class = "dp-modal-body",

        # Chips
        shiny::div(class = "dp-chips",
          shiny::tags$span(class = "sep-chip sep-chip--year", s_year),
          shiny::tags$span(class = "sep-chip sep-chip--series", s_coll),
          shiny::tags$span(class = paste("sep-chip",
            if (s_score >= 7) "sep-chip--g-hi"
            else if (s_score >= 4) "sep-chip--g-mid"
            else "sep-chip--g-lo"),
            paste0("Gender: ", s_score, "/10")),
          shiny::tags$span(class = paste("sep-chip",
            if (s_access == "Public") "sep-chip--public" else "sep-chip--licensed"),
            s_access),
          shiny::tags$span(class = paste("sep-chip",
            if (s_quality == "Complete") "sep-chip--complete" else "sep-chip--warn"),
            s_quality)
        ),

        # Two-column body
        shiny::div(class = "dp-body",

          # Left — metadata + resources
          shiny::div(class = "dp-col dp-col--meta",
            shiny::div(class = "dp-field",
              shiny::div(class = "dp-field__lbl",
                shiny::tags$i(class = "fas fa-building fa-xs"), " Organization"),
              shiny::div(class = "dp-field__val", s_org)
            ),
            shiny::div(class = "dp-field",
              shiny::div(class = "dp-field__lbl",
                shiny::tags$i(class = "fas fa-map-marker-alt fa-xs"), " Geographic Coverage"),
              shiny::div(class = "dp-field__val", s_geo)
            ),
            shiny::div(class = "dp-field",
              shiny::div(class = "dp-field__lbl",
                shiny::tags$i(class = "fas fa-venus fa-xs"), " Gender Relevance Score"),
              shiny::div(class = "dp-field__val", gender_bar_html(s_score))
            ),
            shiny::div(class = "dp-field",
              shiny::div(class = "dp-field__lbl",
                shiny::tags$i(class = "fas fa-unlock-alt fa-xs"), " Data Access"),
              shiny::div(class = "dp-field__val", s_access)
            ),
            shiny::div(class = "dp-field",
              shiny::div(class = "dp-field__lbl",
                shiny::tags$i(class = "fas fa-layer-group fa-xs"), " Survey Type"),
              shiny::div(class = "dp-field__val", s_type)
            ),
            shiny::div(class = "dp-field dp-field--res",
              shiny::div(class = "dp-field__lbl",
                shiny::tags$i(class = "fas fa-paperclip fa-xs"),
                paste0(" ", nrow(study_res), " Available Resource",
                       if (nrow(study_res) != 1) "s")
              ),
              shiny::div(class = "dp-res-list", res_rows)
            ),
            if (nchar(s_url) > 4)
              shiny::tags$a(
                href   = s_url, target = "_blank",
                class  = "dp-nisr-btn",
                shiny::tags$i(class = "fas fa-external-link-alt fa-xs"),
                " Open in NISR Catalog"
              )
          ),

          # Right — abstract
          shiny::div(class = "dp-col dp-col--abstract",
            shiny::div(class = "dp-field__lbl",
              shiny::tags$i(class = "fas fa-align-left fa-xs"), " Abstract"),
            shiny::tags$p(class = "dp-abstract", s_abs)
          )
        )
      )

      # ── Custom modal header ─────────────────────────────────────────────────
      modal_title <- shiny::div(class = "dp-modal-header",
        shiny::div(class = "dp-breadcrumb",
          shiny::tags$i(class = "fas fa-database fa-xs"),
          paste0(" ", s_coll, " \u203a ", s_year)
        ),
        shiny::tags$h4(class = "dp-title", s_title)
      )

      shiny::showModal(
        shiny::modalDialog(
          title    = modal_title,
          modal_content,
          size     = "l",
          easyClose = TRUE,
          footer   = shiny::tagList(
            if (nchar(s_url) > 4)
              shiny::tags$a(
                href   = s_url, target = "_blank",
                class  = "btn dp-modal-nisr-btn",
                shiny::tags$i(class = "fas fa-external-link-alt fa-xs"),
                " Open in NISR Catalog"
              ),
            shiny::modalButton(
              shiny::tagList(shiny::tags$i(class = "fas fa-times fa-xs"), " Close")
            )
          )
        )
      )
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title    = "Study Details",
        shiny::tags$p("Could not load study details. Please try another study."),
        easyClose = TRUE,
        footer    = shiny::modalButton("Close")
      ))
    })
  })

  # ══════════════════════════════════════════════════════════════════════════
  # DASHBOARD
  # ══════════════════════════════════════════════════════════════════════════

  output$dash_kpis <- shiny::renderUI({
    n_hi  <- sum(catalog$gender_score >= 7, na.rm = TRUE)
    n_ok  <- sum(catalog$quality_status == "Complete", na.rm = TRUE)
    pct_p <- round(100 * mean(catalog$access_clean == "Public", na.rm = TRUE))

    shiny::div(class = "dash-kpi-row",
      shiny::div(class = "dash-kpi",
        shiny::div(class = "dash-kpi__lbl", "Total Surveys"),
        shiny::div(class = "dash-kpi__val", nrow(catalog)),
        shiny::div(class = "dash-kpi__sub", "1978 \u2013 2024")),
      shiny::div(class = "dash-kpi dash-kpi--gold",
        shiny::div(class = "dash-kpi__lbl", "High Gender Relevance"),
        shiny::div(class = "dash-kpi__val", n_hi),
        shiny::div(class = "dash-kpi__sub", "Score \u2265 7/10")),
      shiny::div(class = "dash-kpi dash-kpi--success",
        shiny::div(class = "dash-kpi__lbl", "Complete Metadata"),
        shiny::div(class = "dash-kpi__val", n_ok),
        shiny::div(class = "dash-kpi__sub", paste0("of ", nrow(catalog), " studies"))),
      shiny::div(class = "dash-kpi dash-kpi--ink",
        shiny::div(class = "dash-kpi__lbl", "Resources Available"),
        shiny::div(class = "dash-kpi__val", nrow(resources)),
        shiny::div(class = "dash-kpi__sub", paste0(pct_p, "% publicly accessible")))
    )
  })

  # ── Section 1: Survey Landscape ───────────────────────────────────────────

  output$ch_by_year <- plotly::renderPlotly({
    df <- catalog |>
      dplyr::filter(!is.na(year)) |>
      dplyr::count(year, name = "n") |>
      dplyr::arrange(year)

    plotly::plot_ly(df, x = ~year, y = ~n, type = "bar",
            marker = list(color = CLR["primary"],
                          line  = list(color = CLR["primary_dark"], width = .5)),
            hovertemplate = "%{x}: %{y} studies<extra></extra>") |>
      plotly::add_lines(x = ~year, y = ~n,
                line       = list(color = CLR["accent"], width = 2, dash = "dot"),
                showlegend = FALSE, hoverinfo = "skip") |>
      gddp_theme(xlab = "Year", ylab = "Studies") |>
      plotly::layout(showlegend = FALSE)
  })

  output$ch_access_donut <- plotly::renderPlotly({
    df <- catalog |> dplyr::count(access_clean, name = "n")
    plotly::plot_ly(df, labels = ~access_clean, values = ~n, type = "pie", hole = 0.55,
            marker = list(
              colors = c(CLR["success"], CLR["accent"], CLR["ink_300"]),
              line   = list(color = CLR["bg_page"], width = 3)
            ),
            textinfo = "percent",
            hovertemplate = "%{label}: %{value}<extra></extra>") |>
      gddp_theme() |>
      plotly::layout(showlegend = TRUE,
             legend = list(orientation = "v", x = 1, y = .5))
  })

  output$ch_by_collection <- plotly::renderPlotly({
    col_clr <- c(
      "DHS"            = CLR["primary"],  "EICV"         = CLR["accent"],
      "Census"         = CLR["ink_700"],  "LFS"          = CLR["info"],
      "Agriculture"    = CLR["success"],  "Food Security"= CLR["warning"],
      "FinScope"       = CLR["accent_light"], "Business Census" = CLR["ink_500"],
      "Social Protection" = CLR["primary_light"], "Other" = CLR["ink_100"]
    )
    df <- catalog |>
      dplyr::count(collection, name = "n") |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        collection = factor(collection, levels = rev(collection)),
        bar_color  = tidyr::replace_na(unname(col_clr[as.character(collection)]), CLR["border"])
      )

    plotly::plot_ly(df, x = ~n, y = ~collection, type = "bar", orientation = "h",
            marker = list(color = ~bar_color),
            text = ~n, textposition = "outside",
            hovertemplate = "%{y}: %{x} studies<extra></extra>") |>
      gddp_theme(xlab = "Studies") |>
      plotly::layout(yaxis = list(title = ""), margin = list(l = 140, r = 40))
  })

  output$ch_top_viewed <- plotly::renderPlotly({
    df <- catalog |>
      dplyr::filter(!is.na(views_num)) |>
      dplyr::arrange(dplyr::desc(views_num)) |>
      head(10) |>
      dplyr::mutate(
        short = paste0(stringr::str_sub(title, 1, 42), "\u2026"),
        short = make.unique(short, sep = " "),
        short = factor(short, levels = rev(short))
      )

    plotly::plot_ly(df, x = ~views_num, y = ~short, type = "bar", orientation = "h",
            marker = list(color  = CLR["primary_light"],
                          line   = list(color = CLR["primary"], width = 1.5)),
            hovertemplate = "<b>%{y}</b><br>%{x:,} views<extra></extra>") |>
      gddp_theme(xlab = "Page Views") |>
      plotly::layout(yaxis = list(title = ""), margin = list(l = 360, r = 40))
  })

  # ── Section 2: Gender Data Coverage ──────────────────────────────────────

  output$ch_gender_by_coll <- plotly::renderPlotly({
    df <- catalog |>
      dplyr::group_by(collection) |>
      dplyr::summarise(avg = round(mean(gender_score, na.rm = TRUE), 1),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(avg)) |>
      dplyr::mutate(collection = factor(collection, levels = rev(collection)))

    plotly::plot_ly(df, x = ~avg, y = ~collection, type = "bar", orientation = "h",
            marker = list(
              color = ~avg,
              colorscale = list(
                c(0, CLR["primary_light"]),
                c(.5, CLR["accent"]),
                c(1,  CLR["primary"])
              ),
              showscale = FALSE
            ),
            text = ~paste0(avg, "/10"), textposition = "outside",
            hovertemplate = "%{y}: %{x:.1f}/10<extra></extra>") |>
      gddp_theme(xlab = "Avg. Gender Score") |>
      plotly::layout(xaxis = list(range = c(0, 12)),
             yaxis = list(title = ""), margin = list(l = 140, r = 40))
  })

  output$ch_topic_radar <- plotly::renderPlotly({
    df <- gender_topics_df
    plotly::plot_ly(type = "scatterpolar", fill = "toself") |>
      plotly::add_trace(r = df$studies_covering, theta = df$topic,
                name      = "Studies Covering",
                line      = list(color = CLR["primary"]),
                fillcolor = paste0(CLR["primary"], "28")) |>
      plotly::add_trace(r = df$data_quality, theta = df$topic,
                name      = "Data Quality",
                line      = list(color = CLR["accent"]),
                fillcolor = paste0(CLR["accent"], "28")) |>
      plotly::add_trace(r = df$recency_score, theta = df$topic,
                name      = "Recency",
                line      = list(color = CLR["success"]),
                fillcolor = paste0(CLR["success"], "28")) |>
      plotly::layout(
        polar = list(
          bgcolor     = CLR["bg_page"],
          radialaxis  = list(visible = TRUE, range = c(0, 14),
                             gridcolor = CLR["bg_inset"],
                             tickfont  = list(size = 9, color = CLR["ink_300"])),
          angularaxis = list(tickfont = list(size = 10, color = CLR["ink_500"]))
        ),
        paper_bgcolor = CLR["bg_page"],
        showlegend    = TRUE,
        legend        = list(font = list(size = 10, color = CLR["ink_500"]), x = 1.1),
        margin        = list(l = 30, r = 80, t = 20, b = 20)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$ch_quality_stack <- plotly::renderPlotly({
    df <- catalog |>
      dplyr::count(collection, quality_status, name = "n") |>
      dplyr::mutate(quality_status = factor(quality_status,
               levels = c("Complete","Minor Issues","Incomplete")))

    qcol <- c("Complete"     = CLR["success"],
              "Minor Issues" = CLR["warning"],
              "Incomplete"   = CLR["primary"])

    plotly::plot_ly(df, x = ~collection, y = ~n, color = ~quality_status,
            type = "bar", barmode = "stack", colors = qcol,
            hovertemplate = "%{x} \u2014 %{fullData.name}: %{y}<extra></extra>") |>
      gddp_theme(ylab = "Studies") |>
      plotly::layout(xaxis = list(title = ""),
             legend = list(orientation = "h", x = 0, y = 1.18,
                           font = list(size = 11, color = CLR["ink_500"])))
  })

  # ── Section 3: Rwanda Gender Indicators (conditional) ─────────────────────

  output$indicator_ui <- shiny::renderUI({
    grp <- input$indicator_grp

    if (grp == "maternal") {
      shiny::fluidRow(
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-heartbeat fa-xs"),
            "\u00a0 Maternal Mortality Ratio (per 100,000 live births)"),
          plotly::plotlyOutput("ch_mmr", height = "250px"))),
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-user-nurse fa-xs"),
            "\u00a0 Skilled Birth Attendance & ANC 4+ (%)"),
          plotly::plotlyOutput("ch_sba_anc", height = "250px"))),
        shiny::column(12, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-pills fa-xs"),
            "\u00a0 Modern Contraceptive Prevalence Rate (%)"),
          plotly::plotlyOutput("ch_cpr", height = "220px")))
      )
    } else if (grp == "child") {
      shiny::fluidRow(
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-child fa-xs"),
            "\u00a0 Under-5 Mortality Rate (per 1,000 live births)"),
          plotly::plotlyOutput("ch_u5mr", height = "260px"))),
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-weight fa-xs"),
            "\u00a0 Stunting Rate (%) & HIV Prevalence by Sex (%)"),
          plotly::plotlyOutput("ch_stunt_hiv", height = "260px")))
      )
    } else if (grp == "education") {
      shiny::fluidRow(
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-graduation-cap fa-xs"),
            "\u00a0 Gender Parity Index \u2014 Primary & Secondary"),
          plotly::plotlyOutput("ch_gpi", height = "260px"))),
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-school fa-xs"),
            "\u00a0 Secondary Completion Rate by Sex (%)"),
          plotly::plotlyOutput("ch_edu_comp", height = "260px")))
      )
    } else if (grp == "labor") {
      shiny::fluidRow(
        shiny::column(12, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-briefcase fa-xs"),
            "\u00a0 Labor Force Participation Rate by Sex (%) \u2014 RLFS 2017\u20132023"),
          plotly::plotlyOutput("ch_lfp", height = "280px")))
      )
    } else if (grp == "geography") {
      shiny::fluidRow(
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-map-marked-alt fa-xs"),
            "\u00a0 Female-Headed Households by Province (%)"),
          plotly::plotlyOutput("ch_prov_hh", height = "280px"))),
        shiny::column(6, shiny::div(class = "dash-chart-card",
          shiny::div(class = "dash-chart-title",
            shiny::tags$i(class = "fas fa-map-pin fa-xs"),
            "\u00a0 Female Land Ownership & Literacy by Province (%)"),
          plotly::plotlyOutput("ch_prov_land", height = "280px")))
      )
    }
  })

  # Maternal plots
  output$ch_mmr <- plotly::renderPlotly({
    plotly::plot_ly(dhs_maternal, x = ~year, y = ~mmr,
            type = "scatter", mode = "lines+markers",
            line   = list(color = CLR["primary"], width = 2.5),
            marker = list(color = CLR["primary_dark"], size = 9),
            text   = ~survey,
            hovertemplate = "<b>%{text}</b><br>MMR: %{y:,}/100k<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "Per 100,000 live births") |>
      plotly::layout(annotations = list(list(
        x = 2019, y = 203, text = "2020: 203",
        showarrow = TRUE, arrowhead = 2, arrowcolor = CLR["accent"],
        font = list(size = 10, color = CLR["accent"])
      )))
  })

  output$ch_sba_anc <- plotly::renderPlotly({
    plotly::plot_ly(dhs_maternal, x = ~year) |>
      plotly::add_trace(y = ~skilled_birth_pct, name = "Skilled Birth Attendance",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["primary"], width = 2),
                marker = list(color = CLR["primary"], size = 7),
                hovertemplate = "SBA: %{y}%<extra></extra>") |>
      plotly::add_trace(data = dhs_maternal[!is.na(dhs_maternal$anc4_pct), ],
                x = ~year, y = ~anc4_pct, name = "ANC 4+ Visits",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["accent"], width = 2, dash = "dash"),
                marker = list(color = CLR["accent_light"], size = 7, symbol = "square"),
                hovertemplate = "ANC 4+: %{y}%<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "Percentage (%)") |>
      plotly::layout(legend = list(orientation = "h", x = 0, y = 1.18))
  })

  output$ch_cpr <- plotly::renderPlotly({
    plotly::plot_ly(dhs_maternal, x = ~year, y = ~cpr_modern_pct, type = "bar",
            marker = list(color = CLR["secondary"],
                          line  = list(color = CLR["ink_700"], width = .5)),
            text = ~paste0(cpr_modern_pct, "%"), textposition = "outside",
            hovertemplate = "%{x}: %{y}% CPR<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "CPR (%)") |>
      plotly::layout(xaxis = list(tickmode = "array", tickvals = dhs_maternal$year))
  })

  # Child health plots
  output$ch_u5mr <- plotly::renderPlotly({
    plotly::plot_ly(dhs_child, x = ~year, y = ~u5mr,
            type = "scatter", mode = "lines+markers",
            fill = "tozeroy", fillcolor = paste0(CLR["primary_light"], "40"),
            line   = list(color = CLR["primary"], width = 2.5),
            marker = list(color = CLR["primary_dark"], size = 9),
            hovertemplate = "%{x}: %{y}/1000<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "Deaths per 1,000 live births")
  })

  output$ch_stunt_hiv <- plotly::renderPlotly({
    plotly::plot_ly() |>
      plotly::add_bars(data = dhs_child, x = ~year, y = ~stunting_pct,
               name = "Child Stunting (%)",
               marker = list(color = CLR["accent_light"],
                             line  = list(color = CLR["accent"], width = 1)),
               hovertemplate = "%{x}: %{y}% stunting<extra></extra>") |>
      plotly::add_trace(data = dhs_child[!is.na(dhs_child$hiv_female), ],
                x = ~year, y = ~hiv_female, name = "HIV Women (%)",
                type = "scatter", mode = "lines+markers", yaxis = "y2",
                line   = list(color = CLR["primary"], width = 2),
                marker = list(color = CLR["primary"], size = 7),
                hovertemplate = "HIV women: %{y}%<extra></extra>") |>
      plotly::add_trace(data = dhs_child[!is.na(dhs_child$hiv_male), ],
                x = ~year, y = ~hiv_male, name = "HIV Men (%)",
                type = "scatter", mode = "lines+markers", yaxis = "y2",
                line   = list(color = CLR["info"], width = 2, dash = "dot"),
                marker = list(color = CLR["info"], size = 7),
                hovertemplate = "HIV men: %{y}%<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "Stunting Rate (%)") |>
      plotly::layout(
        yaxis2 = list(title = "HIV Prevalence (%)", overlaying = "y", side = "right",
                      gridcolor = "transparent",
                      tickfont = list(size = 10, color = CLR["ink_300"])),
        legend = list(orientation = "h", x = 0, y = 1.18)
      )
  })

  # Education plots
  output$ch_gpi <- plotly::renderPlotly({
    plotly::plot_ly(education_df, x = ~year) |>
      plotly::add_trace(y = ~gpi_primary, name = "Primary GPI",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["primary"], width = 2.5),
                marker = list(color = CLR["primary"], size = 8),
                hovertemplate = "Primary GPI: %{y:.2f}<extra></extra>") |>
      plotly::add_trace(y = ~gpi_secondary, name = "Secondary GPI",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["accent"], width = 2.5, dash = "dash"),
                marker = list(color = CLR["accent"], size = 8, symbol = "square"),
                hovertemplate = "Secondary GPI: %{y:.2f}<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "Gender Parity Index") |>
      plotly::layout(
        shapes = list(list(type = "line", x0 = 2005, x1 = 2022, y0 = 1, y1 = 1,
                           line = list(color = CLR["success"], width = 1.5, dash = "dot"))),
        annotations = list(list(x = 2022, y = 1.01, text = "Parity (1.0)",
                                showarrow = FALSE,
                                font = list(size = 10, color = CLR["success"]))),
        legend = list(orientation = "h", x = 0, y = 1.18)
      )
  })

  output$ch_edu_comp <- plotly::renderPlotly({
    plotly::plot_ly(education_df, x = ~year) |>
      plotly::add_bars(y = ~female_secondary_completion, name = "Female",
               marker = list(color = CLR["primary"]),
               hovertemplate = "%{x} Female: %{y}%<extra></extra>") |>
      plotly::add_bars(y = ~male_secondary_completion, name = "Male",
               marker = list(color = CLR["ink_300"]),
               hovertemplate = "%{x} Male: %{y}%<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "Completion Rate (%)") |>
      plotly::layout(barmode = "group",
             legend = list(orientation = "h", x = 0, y = 1.18))
  })

  # Labor plot
  output$ch_lfp <- plotly::renderPlotly({
    plotly::plot_ly(labor_df, x = ~year) |>
      plotly::add_trace(y = ~female, name = "Female",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["primary"], width = 2.5),
                marker = list(color = CLR["primary"], size = 9),
                hovertemplate = "%{x} Female: %{y}%<extra></extra>") |>
      plotly::add_trace(y = ~male, name = "Male",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["ink_500"], width = 2.5, dash = "dash"),
                marker = list(color = CLR["ink_500"], size = 9, symbol = "square"),
                hovertemplate = "%{x} Male: %{y}%<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "LFP Rate (%)") |>
      plotly::layout(
        yaxis  = list(range = c(76, 95)),
        legend = list(orientation = "h", x = 0, y = 1.1),
        xaxis  = list(tickmode = "array", tickvals = labor_df$year)
      )
  })

  # Geography plots
  output$ch_prov_hh <- plotly::renderPlotly({
    df <- province_df |>
      dplyr::mutate(province = factor(province, levels = rev(province)))

    plotly::plot_ly(df, x = ~female_hh_head_pct, y = ~province, type = "bar",
            orientation = "h",
            marker = list(
              color = c(CLR["primary"], CLR["secondary"], CLR["accent"],
                        CLR["accent_light"], CLR["primary_light"]),
              line  = list(color = CLR["ink_700"], width = .5)
            ),
            text = ~paste0(female_hh_head_pct, "%"), textposition = "outside",
            hovertemplate = "%{y}: %{x}%<extra></extra>") |>
      gddp_theme(xlab = "Female-Headed Households (%)") |>
      plotly::layout(yaxis = list(title = ""), xaxis = list(range = c(0, 52)))
  })

  output$ch_prov_land <- plotly::renderPlotly({
    plotly::plot_ly(province_df, x = ~province) |>
      plotly::add_bars(y = ~female_land_pct, name = "Land Ownership (%)",
               marker = list(color = CLR["accent"]),
               hovertemplate = "%{x} \u2014 Land: %{y}%<extra></extra>") |>
      plotly::add_bars(y = ~female_literacy_pct, name = "Literacy (%)",
               marker = list(color = CLR["primary_light"]),
               hovertemplate = "%{x} \u2014 Literacy: %{y}%<extra></extra>") |>
      gddp_theme(xlab = "Province", ylab = "Percentage (%)") |>
      plotly::layout(barmode = "group",
             legend = list(orientation = "h", x = 0, y = 1.18))
  })

  # ── Section 4: Quality Intelligence ──────────────────────────────────────

  output$ch_missing_fields <- plotly::renderPlotly({
    df <- catalog |>
      dplyr::filter(!is.na(missing_field_count), missing_field_count > 0) |>
      dplyr::arrange(dplyr::desc(missing_field_count)) |>
      head(15) |>
      dplyr::mutate(
        short = paste0(stringr::str_sub(title, 1, 36), "\u2026"),
        short = make.unique(short, sep = " "),
        short = factor(short, levels = rev(short))
      )

    if (nrow(df) == 0)
      return(
        plotly::plot_ly() |>
          plotly::layout(title = list(text = "No missing fields detected"),
                 paper_bgcolor = CLR["bg_page"],
                 plot_bgcolor  = CLR["bg_page"]) |>
          plotly::config(displayModeBar = FALSE)
      )

    plotly::plot_ly(df, x = ~missing_field_count, y = ~short, type = "bar",
            orientation = "h",
            marker = list(color = CLR["primary_light"],
                          line  = list(color = CLR["primary"], width = 1.5)),
            text = ~missing_field_count, textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Missing: %{x} fields<extra></extra>") |>
      gddp_theme(xlab = "Missing Fields") |>
      plotly::layout(yaxis = list(title = ""), margin = list(l = 340, r = 40))
  })

  output$ch_resource_completeness <- plotly::renderPlotly({
    df <- catalog |>
      dplyr::mutate(
        has_rep = tidyr::replace_na(has_report,        FALSE),
        has_que = tidyr::replace_na(has_questionnaire, FALSE),
        status  = dplyr::case_when(
          has_rep & has_que  ~ "Report & Questionnaire",
          has_rep & !has_que ~ "Report Only",
          !has_rep & has_que ~ "Questionnaire Only",
          TRUE               ~ "No Resources"
        )
      ) |>
      dplyr::count(status, name = "n") |>
      dplyr::mutate(status = factor(status,
        levels = c("Report & Questionnaire","Report Only",
                   "Questionnaire Only","No Resources")))

    rcol <- c(
      "Report & Questionnaire" = CLR["success"],
      "Report Only"            = CLR["accent"],
      "Questionnaire Only"     = CLR["info"],
      "No Resources"           = CLR["primary_light"]
    )

    plotly::plot_ly(df, x = ~n, y = ~status, type = "bar", orientation = "h",
            marker = list(color = unname(rcol[as.character(df$status)])),
            text = ~n, textposition = "outside",
            hovertemplate = "%{y}: %{x} studies<extra></extra>") |>
      gddp_theme(xlab = "Number of Studies") |>
      plotly::layout(yaxis = list(title = ""), margin = list(l = 210, r = 40))
  })

  # ════════════════════════════════════════════════════════════════════════════
  # FINANCIAL INCLUSION DASHBOARD — FinScope 2024 Rwanda microdata
  # ════════════════════════════════════════════════════════════════════════════

  # ── Chart colour constants ─────────────────────────────────────────────────
  FI_MALE   <- "#3B82F6"   # blue  — male
  FI_FEMALE <- "#E85A4F"   # red   — female (brand colour)
  FI_FONT   <- "Inter, system-ui, -apple-system, sans-serif"

  fi_plotly_theme <- function(p, xlab = "", ylab = "", title = "") {
    p |>
      plotly::layout(
        font       = list(family = FI_FONT, size = 12),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        xaxis = list(
          title      = list(text = xlab, font = list(size = 11)),
          gridcolor  = "#F0F0F0",
          zerolinecolor = "#E5E5E5",
          tickfont   = list(size = 11)
        ),
        yaxis = list(
          title      = list(text = ylab, font = list(size = 11)),
          gridcolor  = "#F0F0F0",
          tickfont   = list(size = 11)
        ),
        legend = list(
          orientation = "h",
          x = 0, y = -0.18,
          font = list(size = 11)
        ),
        margin = list(t = 10, b = 10, l = 10, r = 10)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  fi_plot_no_data <- function(title = "No data available") {
    plotly::plot_ly() |>
      plotly::layout(
        title = list(text = title, x = 0.02, xanchor = "left"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = "FinScope data is not loaded (or filters returned no rows).",
          showarrow = FALSE,
          font = list(size = 12, color = "#6B7280")
        )),
        margin = list(l = 40, r = 20, t = 55, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  gov_plot_no_data <- function(title = "No Governance data available") {
    plotly::plot_ly() |>
      plotly::layout(
        title = list(text = title, x = 0.02, xanchor = "left"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = "Governance data could not be loaded (or filters returned no rows).",
          showarrow = FALSE,
          font = list(size = 12, color = "#6B7280")
        )),
        margin = list(l = 40, r = 20, t = 55, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  # ── Reactive: filtered FinScope data ──────────────────────────────────────
  fi_data <- shiny::reactive({
    d <- tryCatch(
      get(".finscope_data", envir = globalenv(), inherits = FALSE),
      error = function(e) {
        tryCatch(load_finscope_data(), error = function(e2) NULL)
      }
    )
    if (is.null(d)) return(NULL)

    prov    <- input$fi_province
    dist    <- input$fi_district
    area    <- input$fi_area
    age_rng <- input$fi_age

    if (!is.null(prov) && prov != "all")
      d <- d[!is.na(d$province) & d$province == prov, ]

    if (!is.null(dist) && dist != "all")
      d <- d[!is.na(d$district) & d$district == dist, ]

    if (!is.null(area) && area != "all")
      d <- d[!is.na(d$area) & d$area == area, ]

    if (!is.null(age_rng) && length(age_rng) == 2)
      d <- d[!is.na(d$age) & d$age >= age_rng[1] & d$age <= age_rng[2], ]

    d
  })

  # ── Province → District cascading update ──────────────────────────────────
  shiny::observeEvent(input$fi_province, {
    d_full <- tryCatch(
      get(".finscope_data", envir = globalenv(), inherits = FALSE),
      error = function(e) NULL
    )
    if (is.null(d_full)) {
      d_full <- tryCatch(load_finscope_data(), error = function(e2) NULL)
    }
    if (is.null(d_full)) return()

    prov      <- input$fi_province
    # Normalize province text to avoid UI-vs-data label mismatches
    # (e.g., "Kigali" vs "Kigali City", "West" vs "Western").
    norm_prov <- function(x) {
      z <- tolower(trimws(x))
      z <- gsub("\\s+", " ", z)
      z <- sub("\\s*city$", "", z)
      z <- sub("\\s*province$", "", z)
      z
    }

    if (!is.null(prov) && prov != "all") {
      prov_n <- norm_prov(prov)
      prov_map <- c(
        "north" = "northern",
        "south" = "southern",
        "east"  = "eastern",
        "west"  = "western"
      )
      if (prov_n %in% names(prov_map)) prov_n <- prov_map[[prov_n]]

      d_prov_n <- norm_prov(d_full$province)

      districts <- sort(unique(d_full$district[
        !is.na(d_full$district) &
          !is.na(d_prov_n) &
          d_prov_n == prov_n
      ]))

      # Fallback: if exact normalization match fails, try partial matching.
      if (length(districts) == 0) {
        keep <- stringr::str_detect(d_prov_n, fixed(prov_n)) |
          stringr::str_detect(prov_n, fixed(d_prov_n))
        districts <- sort(unique(d_full$district[
          !is.na(d_full$district) & keep
        ]))
      }
    } else {
      districts <- sort(unique(d_full$district[!is.na(d_full$district)]))
    }

    shiny::updateSelectInput(session, "fi_district",
      choices  = c("All Districts" = "all", setNames(districts, districts)),
      selected = "all"
    )
  }, ignoreInit = TRUE)

  # ── Live sample count ──────────────────────────────────────────────────────
  output$fi_sample_n <- shiny::renderText({
    d <- fi_data()
    if (is.null(d) || nrow(d) == 0) return("No data")
    paste0(format(nrow(d), big.mark = ","), " respondents")
  })

  # ── Reset all FI filters ───────────────────────────────────────────────────
  shiny::observeEvent(input$fi_reset, {
    shiny::updateSelectInput(session, "fi_province", selected = "all")
    shiny::updateSelectInput(session, "fi_district",
      choices = c("All Districts" = "all"), selected = "all")
    shiny::updateSelectInput(session, "fi_area",    selected = "all")
    shiny::updateSliderInput(session, "fi_age",     value    = c(16, 90))
  }, ignoreInit = TRUE)

  # ── Reactive: aggregated summaries ────────────────────────────────────────
  fi_agg <- shiny::reactive({
    aggregate_finscope(fi_data())
  })

  # ── KPI Cards ─────────────────────────────────────────────────────────────
  output$fi_kpis <- shiny::renderUI({
    agg <- fi_agg()
    if (is.null(agg)) {
      return(shiny::div(class = "fi-nodata",
        shiny::tags$i(class = "fas fa-database"),
        " FinScope data could not be loaded. Please check the dashboard_data folder."))
    }

    k    <- agg$kpis
    male <- k[k$gender == "Male",   ]
    fem  <- k[k$gender == "Female", ]

    make_kpi <- function(icon, label, male_val, fem_val, gap_positive_for_male = TRUE) {
      gap    <- round(male_val - fem_val, 1)
      gap_lbl <- if (abs(gap) < 0.5) "No gap" else
                 paste0(if (gap > 0) "Men +" else "Women +", abs(gap), "%")
      gap_cls <- if (abs(gap) < 0.5) "fi-kpi__gap--neutral"  else
                 if ((gap > 0) == gap_positive_for_male) "fi-kpi__gap--warn" else "fi-kpi__gap--ok"

      shiny::div(class = "fi-kpi",
        shiny::div(class = "fi-kpi__icon", shiny::tags$i(class = paste("fas", icon))),
        shiny::div(class = "fi-kpi__body",
          shiny::div(class = "fi-kpi__label", label),
          shiny::div(class = "fi-kpi__bars",
            shiny::div(class = "fi-kpi__bar-row",
              shiny::div(class = "fi-kpi__bar-lbl fi-kpi__bar-lbl--m", "Men"),
              shiny::div(class = "fi-kpi__bar-track",
                shiny::div(class = "fi-kpi__bar fi-kpi__bar--m",
                           style = paste0("width:", male_val, "%"))
              ),
              shiny::div(class = "fi-kpi__val", paste0(male_val, "%"))
            ),
            shiny::div(class = "fi-kpi__bar-row",
              shiny::div(class = "fi-kpi__bar-lbl fi-kpi__bar-lbl--f", "Women"),
              shiny::div(class = "fi-kpi__bar-track",
                shiny::div(class = "fi-kpi__bar fi-kpi__bar--f",
                           style = paste0("width:", fem_val, "%"))
              ),
              shiny::div(class = "fi-kpi__val", paste0(fem_val, "%"))
            )
          ),
          shiny::div(class = paste("fi-kpi__gap", gap_cls), gap_lbl)
        )
      )
    }

    shiny::div(class = "fi-kpis-row",
      make_kpi("fa-university",       "Formal Financial Inclusion",   male$formal_pct,  fem$formal_pct),
      make_kpi("fa-landmark",         "Bank Account",                 male$bank_pct,    fem$bank_pct),
      make_kpi("fa-mobile-alt",       "Mobile Money",                 male$mm_pct,      fem$mm_pct),
      make_kpi("fa-sacco",            "Umurenge SACCO",               male$sacco_pct,   fem$sacco_pct),
      make_kpi("fa-hand-holding-usd", "Credit Access",                male$credit_pct,  fem$credit_pct),
      make_kpi("fa-piggy-bank",       "Saves Regularly",              male$saves_pct,   fem$saves_pct,
               gap_positive_for_male = FALSE)
    )
  })

  # ── Sidebar stat outputs ──────────────────────────────────────────────────

  output$fi_overview_sb_stats <- shiny::renderUI({
    d <- fi_data()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    n_m <- sum(!is.na(d$gender) & d$gender == "Male")
    n_f <- sum(!is.na(d$gender) & d$gender == "Female")
    n_t <- n_m + n_f
    pm  <- if (n_t > 0) paste0(" (", round(n_m / n_t * 100), "%)") else ""
    pf  <- if (n_t > 0) paste0(" (", round(n_f / n_t * 100), "%)") else ""
    shiny::div(class = "fi-tabsb__stats",
      shiny::div(class = "fi-tabsb__stat",
        shiny::div(class = "fi-tabsb__stat-val", format(n_t, big.mark = ",")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Total respondents")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--m",
        shiny::div(class = "fi-tabsb__stat-val", paste0(format(n_m, big.mark=","), pm)),
        shiny::div(class = "fi-tabsb__stat-lbl", "Male")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--f",
        shiny::div(class = "fi-tabsb__stat-val", paste0(format(n_f, big.mark=","), pf)),
        shiny::div(class = "fi-tabsb__stat-lbl", "Female")
      )
    )
  })

  output$fi_savings_sb_stats <- shiny::renderUI({
    agg <- fi_agg()
    if (is.null(agg)) return(NULL)
    k <- agg$kpis
    m <- k[k$gender == "Male",   ]
    f <- k[k$gender == "Female", ]
    shiny::div(class = "fi-tabsb__stats",
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--m",
        shiny::div(class = "fi-tabsb__stat-val", paste0(m$saves_pct,  "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Men who save")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--f",
        shiny::div(class = "fi-tabsb__stat-val", paste0(f$saves_pct,  "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Women who save")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--m",
        shiny::div(class = "fi-tabsb__stat-val", paste0(m$credit_pct, "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Men with credit")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--f",
        shiny::div(class = "fi-tabsb__stat-val", paste0(f$credit_pct, "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Women with credit")
      )
    )
  })

  output$fi_digital_sb_stats <- shiny::renderUI({
    agg <- fi_agg()
    if (is.null(agg)) return(NULL)
    k <- agg$kpis
    m <- k[k$gender == "Male",   ]
    f <- k[k$gender == "Female", ]
    shiny::div(class = "fi-tabsb__stats",
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--m",
        shiny::div(class = "fi-tabsb__stat-val", paste0(m$mm_pct, "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Men \u2014 Mobile Money")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--f",
        shiny::div(class = "fi-tabsb__stat-val", paste0(f$mm_pct, "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Women \u2014 Mobile Money")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--m",
        shiny::div(class = "fi-tabsb__stat-val", paste0(m$mm_acc_pct, "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Men \u2014 MM Account")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--f",
        shiny::div(class = "fi-tabsb__stat-val", paste0(f$mm_acc_pct, "%")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Women \u2014 MM Account")
      )
    )
  })

  output$fi_wellbeing_sb_stats <- shiny::renderUI({
    d <- fi_data()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    pct_impr <- function(g) {
      sub <- d[!is.na(d$gender) & d$gender == g & !is.na(d$fin_trend), ]
      if (nrow(sub) == 0) return("\u2014")
      paste0(round(mean(sub$fin_trend == "Improved") * 100, 1), "%")
    }
    pct_ctrl <- function(g) {
      sub <- d[!is.na(d$gender) & d$gender == g & !is.na(d$fin_control), ]
      if (nrow(sub) == 0) return("\u2014")
      hi <- c("A lot of control", "In control")
      paste0(round(mean(sub$fin_control %in% hi) * 100, 1), "%")
    }
    shiny::div(class = "fi-tabsb__stats",
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--m",
        shiny::div(class = "fi-tabsb__stat-val", pct_impr("Male")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Men \u2014 improved finances")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--f",
        shiny::div(class = "fi-tabsb__stat-val", pct_impr("Female")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Women \u2014 improved finances")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--m",
        shiny::div(class = "fi-tabsb__stat-val", pct_ctrl("Male")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Men \u2014 feel in control")
      ),
      shiny::div(class = "fi-tabsb__stat fi-tabsb__stat--f",
        shiny::div(class = "fi-tabsb__stat-val", pct_ctrl("Female")),
        shiny::div(class = "fi-tabsb__stat-lbl", "Women \u2014 feel in control")
      )
    )
  })

  # ── Chart 1: Financial product uptake by gender ────────────────────────────
  output$fi_product_gender <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Product Uptake by Gender"))

    # Filter by sidebar product selection
    prod_labels <- c(
      "has_bank"    = "Commercial Bank",  "has_mm"      = "Mobile Money",
      "has_sacco"   = "Umurenge SACCO",   "has_mfi"     = "Microfinance",
      "has_insure"  = "Insurance",        "has_pension" = "Pension Fund",
      "has_ejoheza" = "EJOHEZA",          "has_savgrp"  = "Savings Group"
    )
    sel <- input$fi_prod_select
    if (is.null(sel) || length(sel) == 0) sel <- names(prod_labels)
    keep <- unname(prod_labels[sel])

    d <- agg$prod_gender[agg$prod_gender$product %in% keep, ]
    # Order products by average pct descending
    avg_ord <- d |>
      dplyr::group_by(product) |>
      dplyr::summarise(avg = mean(pct), .groups = "drop") |>
      dplyr::arrange(avg)
    d$product <- factor(d$product, levels = avg_ord$product)

    male_d   <- d[d$gender == "Male",   ]
    female_d <- d[d$gender == "Female", ]

    plotly::plot_ly() |>
      plotly::add_trace(
        data = male_d, x = ~pct, y = ~product, type = "bar", orientation = "h",
        name = "Male", marker = list(color = FI_MALE),
        text = ~paste0(pct, "%"), textposition = "outside",
        hovertemplate = "<b>%{y}</b><br>Male: %{x}%<extra></extra>"
      ) |>
      plotly::add_trace(
        data = female_d, x = ~pct, y = ~product, type = "bar", orientation = "h",
        name = "Female", marker = list(color = FI_FEMALE),
        text = ~paste0(pct, "%"), textposition = "outside",
        hovertemplate = "<b>%{y}</b><br>Female: %{x}%<extra></extra>"
      ) |>
      fi_plotly_theme(xlab = "% of respondents") |>
      plotly::layout(
        barmode = "group",
        xaxis   = list(range = c(0, 70), title = list(text = "% of respondents", font = list(size = 11))),
        yaxis   = list(title = "", tickfont = list(size = 11)),
        margin  = list(l = 130, r = 60, t = 10, b = 40)
      )
  })

  # ── Chart 2: Formal inclusion by province × gender ─────────────────────────
  output$fi_province_gender <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Formal Financial Inclusion by Province & Gender"))

    d       <- agg$prod_province
    prov_ord <- d |>
      dplyr::group_by(province) |>
      dplyr::summarise(avg = mean(formal_pct), .groups = "drop") |>
      dplyr::arrange(avg)
    d$province <- factor(d$province, levels = prov_ord$province)

    male_d   <- d[d$gender == "Male",   ]
    female_d <- d[d$gender == "Female", ]

    plotly::plot_ly() |>
      plotly::add_trace(
        data = male_d, x = ~province, y = ~formal_pct, type = "bar",
        name = "Male", marker = list(color = FI_MALE),
        text = ~paste0(formal_pct, "%"), textposition = "outside",
        hovertemplate = "<b>%{x}</b><br>Male: %{y}%<extra></extra>"
      ) |>
      plotly::add_trace(
        data = female_d, x = ~province, y = ~formal_pct, type = "bar",
        name = "Female", marker = list(color = FI_FEMALE),
        text = ~paste0(formal_pct, "%"), textposition = "outside",
        hovertemplate = "<b>%{x}</b><br>Female: %{y}%<extra></extra>"
      ) |>
      fi_plotly_theme(ylab = "% formally included") |>
      plotly::layout(
        barmode = "group",
        yaxis   = list(range = c(0, 105)),
        margin  = list(l = 50, r = 20, t = 10, b = 60)
      )
  })

  # ── Chart 3: Savings frequency by gender ──────────────────────────────────
  output$fi_savings_gender <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Savings Frequency by Gender"))

    d <- agg$savings_gender
    d$saving_freq <- factor(d$saving_freq, levels = c("Always", "Sometimes", "Never"))

    sav_colours <- c("Always" = "#10B981", "Sometimes" = "#F59E0B", "Never" = "#EF4444")

    p <- plotly::plot_ly()
    for (freq in c("Always", "Sometimes", "Never")) {
      sub <- d[d$saving_freq == freq, ]
      p <- plotly::add_trace(p,
        data = sub, x = ~gender, y = ~pct, type = "bar",
        name = freq,
        marker = list(color = sav_colours[[freq]]),
        text = ~paste0(pct, "%"), textposition = "inside",
        hovertemplate = paste0("<b>%{x}</b><br>", freq, ": %{y}%<extra></extra>")
      )
    }
    p |>
      fi_plotly_theme(ylab = "% of respondents") |>
      plotly::layout(
        barmode = "stack",
        yaxis   = list(range = c(0, 105), ticksuffix = "%"),
        margin  = list(l = 50, r = 10, t = 10, b = 50)
      )
  })

  # ── Chart 4: Credit access by gender ──────────────────────────────────────
  output$fi_credit_gender <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Credit Access by Gender"))

    d <- agg$credit_gender

    plotly::plot_ly() |>
      plotly::add_trace(
        data = d, x = ~gender, y = ~has_credit, type = "bar",
        name = "Has Credit",
        marker = list(color = c(FI_MALE, FI_FEMALE)),
        text = ~paste0(has_credit, "%"), textposition = "outside",
        hovertemplate = "<b>%{x}</b><br>Has credit: %{y}%<extra></extra>"
      ) |>
      fi_plotly_theme(ylab = "% with active credit") |>
      plotly::layout(
        showlegend = FALSE,
        yaxis      = list(range = c(0, 45), ticksuffix = "%"),
        xaxis      = list(title = ""),
        margin     = list(l = 60, r = 20, t = 10, b = 50),
        shapes = list(list(
          type = "line", x0 = -0.5, x1 = 1.5,
          y0 = mean(d$has_credit), y1 = mean(d$has_credit),
          line = list(color = "#9CA3AF", width = 1.5, dash = "dot")
        )),
        annotations = list(list(
          x = 1.6, y = mean(d$has_credit),
          text = paste0("Avg ", round(mean(d$has_credit), 1), "%"),
          showarrow = FALSE, font = list(size = 10, color = "#6B7280")
        ))
      )
  })

  # ── Chart 5: Mobile money by area × gender ────────────────────────────────
  output$fi_mm_area <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Mobile Money Uptake — Urban vs Rural"))

    d <- agg$mm_area
    d$grp <- paste0(d$area, " — ", d$gender)

    clrs <- c(
      "Urban — Male" = FI_MALE,   "Urban — Female" = FI_FEMALE,
      "Rural — Male" = "#93C5FD",  "Rural — Female" = "#FCA5A5"
    )

    plotly::plot_ly(
      data = d, x = ~grp, y = ~mm_pct, type = "bar",
      marker = list(color = unname(clrs[d$grp])),
      text = ~paste0(mm_pct, "%"), textposition = "outside",
      hovertemplate = "<b>%{x}</b><br>Mobile money: %{y}%<extra></extra>"
    ) |>
      fi_plotly_theme(ylab = "% using mobile money") |>
      plotly::layout(
        showlegend = FALSE,
        yaxis  = list(range = c(0, 60), ticksuffix = "%"),
        xaxis  = list(title = "", tickangle = -25, tickfont = list(size = 10)),
        margin = list(l = 55, r = 10, t = 10, b = 80)
      )
  })

  # ── Chart 6: Financial inclusion by age group × gender ───────────────────
  output$fi_age_gender <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Financial Inclusion by Age Group"))

    age_grps <- input$fi_age_groups
    if (is.null(age_grps) || length(age_grps) == 0)
      age_grps <- c("16-24", "25-34", "35-44", "45-54", "55+")

    d <- agg$prod_age |>
      dplyr::filter(as.character(age_group) %in% age_grps) |>
      dplyr::arrange(age_group)

    male_d   <- d[d$gender == "Male",   ]
    female_d <- d[d$gender == "Female", ]

    plotly::plot_ly() |>
      plotly::add_trace(
        data = male_d, x = ~as.character(age_group), y = ~formal_pct,
        type = "scatter", mode = "lines+markers",
        name = "Male", line = list(color = FI_MALE, width = 2.5),
        marker = list(color = FI_MALE, size = 8),
        hovertemplate = "<b>%{x}</b><br>Male: %{y}%<extra></extra>"
      ) |>
      plotly::add_trace(
        data = female_d, x = ~as.character(age_group), y = ~formal_pct,
        type = "scatter", mode = "lines+markers",
        name = "Female", line = list(color = FI_FEMALE, width = 2.5),
        marker = list(color = FI_FEMALE, size = 8),
        hovertemplate = "<b>%{x}</b><br>Female: %{y}%<extra></extra>"
      ) |>
      fi_plotly_theme(xlab = "Age Group", ylab = "% formally included") |>
      plotly::layout(
        yaxis  = list(range = c(0, 105), ticksuffix = "%"),
        margin = list(l = 55, r = 10, t = 10, b = 50)
      )
  })

  # ── Chart 7: Financial control by gender ──────────────────────────────────
  output$fi_control_gender <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Degree of Financial Control by Gender"))

    d <- agg$control_gender
    ctrl_colours <- c(
      "A lot of control" = "#10B981",
      "In control"       = "#6EE7B7",
      "Some control"     = "#FCD34D",
      "Scarce control"   = "#F97316",
      "No control"       = "#EF4444"
    )

    p <- plotly::plot_ly()
    for (lvl in c("A lot of control","In control","Some control","Scarce control","No control")) {
      sub <- d[!is.na(d$fin_control) & as.character(d$fin_control) == lvl, ]
      if (nrow(sub) == 0) next
      p <- plotly::add_trace(p,
        data = sub, x = ~gender, y = ~pct, type = "bar",
        name = lvl,
        marker = list(color = ctrl_colours[[lvl]]),
        text = ~paste0(pct, "%"), textposition = "inside",
        hovertemplate = paste0("<b>%{x}</b><br>", lvl, ": %{y}%<extra></extra>")
      )
    }
    p |>
      fi_plotly_theme(ylab = "% of respondents") |>
      plotly::layout(
        barmode    = "stack",
        yaxis      = list(range = c(0, 105), ticksuffix = "%"),
        xaxis      = list(title = ""),
        legend     = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10)),
        margin     = list(l = 55, r = 10, t = 10, b = 70)
      )
  })

  # ── Chart 8: Financial situation trend by gender ──────────────────────────
  output$fi_trend_gender <- plotly::renderPlotly({
    agg <- fi_agg()
    if (is.null(agg)) return(fi_plot_no_data("Financial Situation Trend (past 12 months)"))

    d   <- agg$trend_gender
    hl  <- input$fi_trend_hl   # highlight filter from Wellbeing sidebar

    base_colours <- c("Improved" = "#10B981", "Unchanged" = "#9CA3AF", "Worsened" = "#EF4444")

    # When a specific trend is highlighted, dim the others
    get_colour <- function(trend_name) {
      if (is.null(hl) || hl == "all" || hl == trend_name) {
        base_colours[[trend_name]]
      } else {
        paste0(base_colours[[trend_name]], "44")   # add alpha for dimming
      }
    }

    p <- plotly::plot_ly()
    for (t in c("Improved", "Unchanged", "Worsened")) {
      sub <- d[d$fin_trend == t, ]
      if (nrow(sub) == 0) next
      p <- plotly::add_trace(p,
        data = sub, x = ~gender, y = ~pct, type = "bar",
        name = t,
        marker = list(color = get_colour(t)),
        text = ~paste0(pct, "%"), textposition = "inside",
        hovertemplate = paste0("<b>%{x}</b><br>", t, ": %{y}%<extra></extra>")
      )
    }
    p |>
      fi_plotly_theme(ylab = "% of respondents") |>
      plotly::layout(
        barmode = "stack",
        yaxis   = list(range = c(0, 105), ticksuffix = "%"),
        xaxis   = list(title = ""),
        legend  = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10)),
        margin  = list(l = 55, r = 10, t = 10, b = 70)
      )
  })

  # ── Governance dashboard (gender representation, justice, local leadership) ──
  # Load governance tables once at server init.
  # If the global variable isn't available, governance charts should show
  # a clear "no data" notice instead of silently failing.
  gov_data_once <- NULL
  if (exists(".governance_data", envir = globalenv(), inherits = FALSE)) {
    gov_data_once <- tryCatch(
      get(".governance_data", envir = globalenv(), inherits = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(gov_data_once)) {
    message("[GOV] .governance_data not available in server (init).")
  } else {
    message("[GOV] .governance_data available in server (init): ",
            paste(names(gov_data_once), collapse = ", "))
  }

  gov_tbls <- shiny::reactive({
    if (!is.null(gov_data_once)) return(gov_data_once)
    # Fallback: try to load again (covers rare scoping/startup order issues)
    tryCatch(load_governance_data(), error = function(e) NULL)
  })

  gov_filters <- shiny::reactive({
    d <- gov_tbls()
    shiny::req(!is.null(d))

    yr <- input$gov_year
    if (is.null(yr) || length(yr) != 2) yr <- c(2003, 2024)

    sex <- input$gov_sex
    if (is.null(sex) || length(sex) == 0) sex <- c("Female", "Male")

    entity <- input$gov_entity
    if (is.null(entity) || length(entity) == 0) entity <- "all"

    pos <- input$gov_position
    if (is.null(pos) || length(pos) == 0) pos <- "all"

    list(yr = yr, sex = sex, entity = entity, pos = pos)
  })

  # Workbook sheet list (Data library tab)
  shiny::observe({
    d <- gov_tbls()
    if (is.null(d)) return()
    if (is.null(input$gov_tab) || input$gov_tab != "library") return()

    sheets <- character(0)
    if (!is.null(d$sheet_manifest) && nrow(d$sheet_manifest) > 0) {
      sheets <- as.character(d$sheet_manifest$sheet_name)
    } else if (!is.null(d$excel_source) && nzchar(d$excel_source) && file.exists(d$excel_source)) {
      sheets <- tryCatch(
        readxl::excel_sheets(d$excel_source),
        error = function(e) character(0)
      )
    }

    if (length(sheets) == 0) {
      shiny::updateSelectInput(session, "gov_wb_sheet",
        choices = c("No sheets found" = ""),
        selected = ""
      )
      return()
    }

    cur <- input$gov_wb_sheet
    sel <- if (!is.null(cur) && cur %in% sheets) cur else sheets[[1]]
    shiny::updateSelectInput(session, "gov_wb_sheet",
      choices = stats::setNames(sheets, sheets),
      selected = sel
    )
  })

  output$gov_sample_n <- shiny::renderText({
    if (!is.null(input$gov_tab) && input$gov_tab == "library") {
      d <- gov_tbls()
      if (is.null(d)) return("No data")
      m <- d$sheet_manifest
      if (!is.null(m) && nrow(m) > 0) {
        return(paste0(format(nrow(m), big.mark = ","), " sheets"))
      }
      if (!is.null(d$excel_source) && file.exists(d$excel_source)) {
        ns <- tryCatch(
          length(readxl::excel_sheets(d$excel_source)),
          error = function(e) 0L
        )
        if (ns > 0) return(paste0(ns, " sheets"))
      }
      return("Workbook")
    }

    d <- gov_tbls()
    if (is.null(d)) return("No data")
    f <- gov_filters()

    ds <- d$ministers
    if (is.null(ds)) return("No data")
    ds <- ds[
      ds$year >= f$yr[1] & ds$year <= f$yr[2] &
        ds$sex %in% f$sex,
    , drop = FALSE]
    paste0(format(nrow(ds), big.mark = ","), " records")
  })

  # ── Governance Overview: latest-year gender KPI strip ─────────────────────
  output$gov_overview_kpis <- shiny::renderUI({
    shiny::req(!is.null(input$gov_tab) && input$gov_tab == "overview")
    d <- gov_tbls()
    if (is.null(d)) {
      return(shiny::tags$div(
        class = "demo-overview-kpis demo-overview-kpis--empty fi-nodata",
        shiny::tags$i(class = "fas fa-database"),
        " Governance indicators could not be loaded."
      ))
    }

    gov_ov_make_kpi <- function(icon, label, male_val, fem_val, gap_positive_for_male = TRUE) {
      male_val <- suppressWarnings(as.numeric(male_val))
      fem_val <- suppressWarnings(as.numeric(fem_val))
      if (length(male_val) < 1L || length(fem_val) < 1L) return(NULL)
      male_val <- male_val[[1L]]
      fem_val <- fem_val[[1L]]
      if (is.na(male_val) || is.na(fem_val)) return(NULL)
      mv <- max(0, min(100, male_val))
      fv <- max(0, min(100, fem_val))
      gap <- round(mv - fv, 1)
      gap_lbl <- if (abs(gap) < 0.5) {
        "Balanced"
      } else if (gap > 0) {
        paste0("Men +", abs(gap), "%")
      } else {
        paste0("Women +", abs(gap), "%")
      }
      gap_cls <- if (abs(gap) < 0.5) {
        "fi-kpi__gap--neutral"
      } else if ((gap > 0) == gap_positive_for_male) {
        "fi-kpi__gap--warn"
      } else {
        "fi-kpi__gap--ok"
      }

      shiny::tags$div(
        class = "fi-kpi",
        shiny::tags$div(class = "fi-kpi__icon", shiny::tags$i(class = paste("fas", icon))),
        shiny::tags$div(
          class = "fi-kpi__body",
          shiny::tags$div(class = "fi-kpi__label", label),
          shiny::tags$div(
            class = "fi-kpi__bars",
            shiny::tags$div(
              class = "fi-kpi__bar-row",
              shiny::tags$div(class = "fi-kpi__bar-lbl fi-kpi__bar-lbl--m", "Men"),
              shiny::tags$div(
                class = "fi-kpi__bar-track",
                shiny::tags$div(class = "fi-kpi__bar fi-kpi__bar--m", style = paste0("width:", mv, "%"))
              ),
              shiny::tags$div(class = "fi-kpi__val", paste0(round(mv, 1), "%"))
            ),
            shiny::tags$div(
              class = "fi-kpi__bar-row",
              shiny::tags$div(class = "fi-kpi__bar-lbl fi-kpi__bar-lbl--f", "Women"),
              shiny::tags$div(
                class = "fi-kpi__bar-track",
                shiny::tags$div(class = "fi-kpi__bar fi-kpi__bar--f", style = paste0("width:", fv, "%"))
              ),
              shiny::tags$div(class = "fi-kpi__val", paste0(round(fv, 1), "%"))
            )
          ),
          shiny::tags$div(class = paste("fi-kpi__gap", gap_cls), gap_lbl)
        )
      )
    }

    latest_mf <- function(df) {
      if (is.null(df) || nrow(df) < 1) {
        return(list(m = NA_real_, f = NA_real_, yr = NA_real_))
      }
      df <- df[!is.na(df$year), , drop = FALSE]
      if (nrow(df) < 1) return(list(m = NA_real_, f = NA_real_, yr = NA_real_))
      ly <- max(df$year, na.rm = TRUE)
      if (!is.finite(ly)) return(list(m = NA_real_, f = NA_real_, yr = NA_real_))
      sub <- df[df$year == ly & df$sex %in% c("Male", "Female"), , drop = FALSE]
      if (nrow(sub) < 1) return(list(m = NA_real_, f = NA_real_, yr = NA_real_))
      mv <- mean(sub$pct[sub$sex == "Male"], na.rm = TRUE)
      fv <- mean(sub$pct[sub$sex == "Female"], na.rm = TRUE)
      list(m = mv, f = fv, yr = ly)
    }

    k_min <- latest_mf(d$ministers)
    k_par <- latest_mf(d$parliament)
    k_pro <- latest_mf(d$prosecutors)
    k_jud <- latest_mf(d$judiciary)
    k_loc <- latest_mf(d$local_leaders)

    y_label <- function(yr) {
      if (length(yr) != 1L || is.na(yr) || !is.finite(yr)) "" else paste0(" (", as.integer(yr), ")")
    }

    card_min <- gov_ov_make_kpi(
      "fa-user-tie",
      paste0("Cabinet / ministers", y_label(k_min$yr)),
      k_min$m, k_min$f, TRUE
    )
    card_par <- gov_ov_make_kpi(
      "fa-landmark",
      paste0("Parliament / Senate seats", y_label(k_par$yr)),
      k_par$m, k_par$f, TRUE
    )
    card_pro <- gov_ov_make_kpi(
      "fa-gavel",
      paste0("National prosecutors", y_label(k_pro$yr)),
      k_pro$m, k_pro$f, TRUE
    )
    card_jud <- gov_ov_make_kpi(
      "fa-scale-balanced",
      paste0("Judiciary & institutions (avg.)", y_label(k_jud$yr)),
      k_jud$m, k_jud$f, TRUE
    )
    card_loc <- gov_ov_make_kpi(
      "fa-city",
      paste0("Local government leaders (avg.)", y_label(k_loc$yr)),
      k_loc$m, k_loc$f, TRUE
    )

    cards <- Filter(Negate(is.null), list(
      card_min, card_par, card_pro, card_jud, card_loc
    ))

    kpi_row <- if (length(cards) < 1L) {
      shiny::tags$div(
        class = "demo-overview-kpis demo-overview-kpis--empty fi-nodata",
        shiny::tags$i(class = "fas fa-info-circle"),
        " No comparable gender shares for the latest year (check source data)."
      )
    } else {
      shiny::tags$div(
        class = "fi-kpis-row fi-kpis-row--demo-overview",
        cards
      )
    }

    shiny::tags$div(
      class = "demo-overview-kpis gov-overview-kpis",
      shiny::tags$p(
        class = "demo-overview-lead",
        shiny::tags$strong("Latest year snapshot"),
        " — Male vs female shares (%) at the most recent year in each series. ",
        "Use the charts below for full time series."
      ),
      kpi_row
    )
  })

  output$gov_workbook_preview <- shiny::renderTable({
    shiny::req(!is.null(input$gov_tab) && input$gov_tab == "library")
    sn <- input$gov_wb_sheet
    if (is.null(sn) || !nzchar(as.character(sn))) {
      return(data.frame(Message = "Select a sheet above."))
    }
    d <- gov_tbls()
    if (is.null(d)) return(data.frame(Message = "No governance data loaded."))

    df <- NULL
    msg <- NULL

    res <- tryCatch({
      if (!is.null(d$sheet_manifest) && nrow(d$sheet_manifest) > 0) {
        row <- d$sheet_manifest[d$sheet_manifest$sheet_name == sn, , drop = FALSE]
        if (nrow(row) < 1) {
          list(df = NULL, msg = "Sheet not listed in manifest (re-run preprocess).")
        } else {
          rel <- as.character(row$file[[1]])
          if (!nzchar(rel)) {
            list(df = NULL, msg = "No export file for this sheet.")
          } else if (is.null(d$clean_dir_path)) {
            list(df = NULL, msg = "Clean folder path unavailable.")
          } else {
            fp <- file.path(d$clean_dir_path, rel)
            if (!file.exists(fp)) {
              list(df = NULL, msg = paste0("Missing file: ", rel))
            } else {
              list(
                df = readr::read_csv(fp,
                  show_col_types = FALSE, n_max = 500,
                  col_types = readr::cols(.default = readr::col_character())
                ),
                msg = NULL
              )
            }
          }
        }
      } else if (!is.null(d$excel_source) && file.exists(d$excel_source)) {
        raw <- readxl::read_excel(d$excel_source, sheet = sn, col_names = FALSE, n_max = 120)
        list(df = as.data.frame(raw, stringsAsFactors = FALSE), msg = NULL)
      } else {
        list(df = NULL, msg = "No workbook manifest or Excel source. Run dashboard_data/Governance_data/preprocess_governance.py")
      }
    }, error = function(e) {
      list(df = NULL, msg = paste0("Read error: ", conditionMessage(e)))
    })

    df <- res$df
    msg <- res$msg

    if (!is.null(msg) && nzchar(msg)) return(data.frame(Message = msg))
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(Message = "Empty or unreadable sheet."))
    }
    nc <- min(18L, ncol(df))
    df <- df[, seq_len(nc), drop = FALSE]
    nr <- min(60L, nrow(df))
    head(df, nr)
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%", align = "l", na = "")

  # Update choice lists for justice/local tabs
  shiny::observeEvent(input$gov_tab, {
    d <- gov_tbls()
    if (is.null(d)) return()

    if (!is.null(input$gov_tab) && input$gov_tab == "justice") {
      if (is.null(d$judiciary) || nrow(d$judiciary) == 0) return()
      ents <- sort(unique(as.character(d$judiciary$entity)))
      shiny::updateSelectInput(session, "gov_entity",
        choices  = c("All" = "all", stats::setNames(ents, ents)),
        selected = ifelse(is.null(input$gov_entity), "all", input$gov_entity)
      )
    }

    if (!is.null(input$gov_tab) && input$gov_tab == "local") {
      if (is.null(d$local_leaders) || nrow(d$local_leaders) == 0) return()
      poss <- sort(unique(as.character(d$local_leaders$entity)))
      shiny::updateSelectInput(session, "gov_position",
        choices  = c("All" = "all", stats::setNames(poss, poss)),
        selected = ifelse(is.null(input$gov_position), "all", input$gov_position)
      )
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$gov_reset, {
    shiny::updateSliderInput(session, "gov_year", value = c(2003, 2024))
    shiny::updateCheckboxGroupInput(session, "gov_sex",
      selected = c("Female", "Male")
    )
    shiny::updateSelectInput(session, "gov_entity", selected = "all")
    shiny::updateSelectInput(session, "gov_position", selected = "all")
  }, ignoreInit = TRUE)

  output$gov_ministers_gender <- plotly::renderPlotly({
    d <- gov_tbls()
    if (is.null(d) || is.null(d$ministers)) return(gov_plot_no_data("Ministers data unavailable"))
    f <- gov_filters()
    ds <- d$ministers
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2] & ds$sex %in% f$sex, , drop = FALSE]
    if (nrow(ds) == 0) return(gov_plot_no_data("No ministers data for selected filters"))

    ds$sex <- factor(ds$sex, levels = c("Female", "Male"))

    plotly::plot_ly(
      data = ds, x = ~year, y = ~pct, color = ~sex,
      type = "scatter", mode = "lines+markers",
      colors = c("Female" = FI_FEMALE, "Male" = FI_MALE),
      hovertemplate = "<b>%{x}</b><br>%{y}% of seats<br>Sex: %{legendgroup}<extra></extra>"
    ) |>
      plotly::layout(
        xaxis = list(title = "Year", tickfont = list(size = 11)),
        yaxis = list(title = "%", range = c(0, 100)),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
        margin = list(l = 55, r = 20, t = 10, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$gov_parliament_gender <- plotly::renderPlotly({
    d <- gov_tbls()
    if (is.null(d) || is.null(d$parliament)) return(gov_plot_no_data("Parliament data unavailable"))
    f <- gov_filters()
    ds <- d$parliament
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2] & ds$sex %in% f$sex, , drop = FALSE]
    if (nrow(ds) == 0) return(gov_plot_no_data("No parliament data for selected filters"))
    ds$sex <- factor(ds$sex, levels = c("Female", "Male"))

    plotly::plot_ly(
      data = ds, x = ~year, y = ~pct, color = ~sex,
      type = "scatter", mode = "lines+markers",
      colors = c("Female" = FI_FEMALE, "Male" = FI_MALE),
      hovertemplate = "<b>%{x}</b><br>%{y}%<br>Sex: %{legendgroup}<extra></extra>"
    ) |>
      plotly::layout(
        xaxis = list(title = "Year", tickfont = list(size = 11)),
        yaxis = list(title = "% of seats", range = c(0, 100)),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
        margin = list(l = 55, r = 20, t = 10, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$gov_prosecutors_gender <- plotly::renderPlotly({
    d <- gov_tbls()
    if (is.null(d) || is.null(d$prosecutors)) return(gov_plot_no_data("Prosecutors data unavailable"))
    f <- gov_filters()
    ds <- d$prosecutors
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2] & ds$sex %in% f$sex, , drop = FALSE]
    if (nrow(ds) == 0) return(gov_plot_no_data("No prosecutors data for selected filters"))

    ds$sex <- factor(ds$sex, levels = c("Female", "Male"))
    plotly::plot_ly(
      data = ds, x = ~year, y = ~pct, color = ~sex,
      type = "bar", marker = list(line = list(width = 0)),
      colors = c("Female" = FI_FEMALE, "Male" = FI_MALE),
      hovertemplate = "<b>%{x}</b><br>%{y}%<br>Sex: %{legendgroup}<extra></extra>"
    ) |>
      plotly::layout(
        barmode = "group",
        xaxis = list(title = "Year", tickfont = list(size = 11)),
        yaxis = list(title = "%", range = c(0, 100), ticksuffix = "%"),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
        margin = list(l = 55, r = 20, t = 10, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$gov_judiciary_gender <- plotly::renderPlotly({
    d <- gov_tbls()
    if (is.null(d) || is.null(d$judiciary)) return(gov_plot_no_data("Judiciary data unavailable"))
    f <- gov_filters()
    ds <- d$judiciary
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2] & ds$sex %in% f$sex, , drop = FALSE]

    if (!is.null(f$entity) && f$entity != "all") {
      ds <- ds[ds$entity == f$entity, , drop = FALSE]
    }

    if (nrow(ds) == 0) return(gov_plot_no_data("No judiciary data for selected filters"))

    # If "All", keep only the most relevant entities to keep the plot readable
    if (!is.null(f$entity) && f$entity == "all") {
      avg_src <- ds
      if (any(ds$sex == "Female")) {
        avg_src <- ds[ds$sex == "Female", , drop = FALSE]
      } else if (any(ds$sex == "Male")) {
        avg_src <- ds[ds$sex == "Male", , drop = FALSE]
      }

      top_ents <- avg_src |>
        dplyr::group_by(entity) |>
        dplyr::summarise(avg = mean(pct), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(avg)) |>
        dplyr::slice_head(n = 3) |>
        dplyr::pull(entity)
      ds <- ds[ds$entity %in% top_ents, , drop = FALSE]
    }

    ds$sex <- factor(ds$sex, levels = c("Female", "Male"))

    plotly::plot_ly(
      data = ds, x = ~year, y = ~pct,
      type = "scatter", mode = "lines+markers",
      color = ~sex, colors = c("Female" = FI_FEMALE, "Male" = FI_MALE),
      line = list(width = 3),
      hovertemplate = "<b>%{x}</b><br>%{y}%<br>Sex: %{legendgroup}<br>Entity: %{customdata[0]}<extra></extra>",
      customdata = ~entity
    ) |>
      plotly::layout(
        xaxis = list(title = "Year", tickfont = list(size = 11)),
        yaxis = list(title = "%", range = c(0, 100)),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
        margin = list(l = 55, r = 20, t = 10, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$gov_local_heatmap <- plotly::renderPlotly({
    d <- gov_tbls()
    if (is.null(d) || is.null(d$local_leaders)) return(gov_plot_no_data("Local leaders data unavailable"))
    f <- gov_filters()
    ds <- d$local_leaders
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2] & ds$sex %in% f$sex, , drop = FALSE]
    if (!is.null(f$pos) && f$pos != "all") {
      ds <- ds[ds$entity == f$pos, , drop = FALSE]
    }
    if (nrow(ds) == 0) return(gov_plot_no_data("No local leadership data for selected filters"))

    z_sex <- if ("Female" %in% f$sex) "Female" else "Male"
    ds <- ds[ds$sex == z_sex, , drop = FALSE]

    # Heatmap: year (x) vs position/entity (y)
    ds$entity <- factor(ds$entity, levels = sort(unique(ds$entity)))

    plotly::plot_ly(
      data = ds, x = ~year, y = ~entity, z = ~pct,
      type = "heatmap",
      # Plotly expects a palette vector for `colors` in R.
      colors = c("#EEF2FF", FI_FEMALE),
      zmin = 0, zmax = 100,
      hovertemplate = "<b>%{x}</b><br>%{y}<br>%{z}%<extra></extra>",
      showscale = TRUE
    ) |>
      plotly::layout(
        xaxis = list(title = "Year", tickfont = list(size = 11)),
        yaxis = list(title = ""),
        margin = list(l = 90, r = 20, t = 10, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # ── Demography dashboard (Census 2022 indicators) ───────────────────────
  demo_data_once <- NULL
  if (exists(".demography_data", envir = globalenv(), inherits = FALSE)) {
    demo_data_once <- tryCatch(
      get(".demography_data", envir = globalenv(), inherits = FALSE),
      error = function(e) NULL
    )
  }

  demo_tbls <- shiny::reactive({
    if (!is.null(demo_data_once)) return(demo_data_once)
    tryCatch(load_demography_data(), error = function(e) NULL)
  })

  demo_plot_no_data <- function(title = "No Demography data available") {
    plotly::plot_ly() |>
      plotly::layout(
        title = list(text = title, x = 0.02, xanchor = "left"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = "Demography data could not be loaded (or filters returned no rows).",
          showarrow = FALSE,
          font = list(size = 12, color = "#6B7280")
        )),
        margin = list(l = 40, r = 20, t = 55, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  # ── Demography Overview: Rwanda gender KPI strip (Census 2022) ───────────
  output$demo_overview_kpis <- shiny::renderUI({
    shiny::req(input$demo_tab == "overview")
    d <- demo_tbls()
    if (is.null(d)) {
      return(shiny::tags$div(
        class = "demo-overview-kpis demo-overview-kpis--empty fi-nodata",
        shiny::tags$i(class = "fas fa-database"),
        " Demography indicators could not be loaded."
      ))
    }

    demo_ov_make_kpi <- function(icon, label, male_val, fem_val, gap_positive_for_male = TRUE) {
      male_val <- suppressWarnings(as.numeric(male_val))
      fem_val <- suppressWarnings(as.numeric(fem_val))
      if (length(male_val) < 1 || length(fem_val) < 1) return(NULL)
      male_val <- male_val[[1L]]
      fem_val <- fem_val[[1L]]
      if (is.na(male_val) || is.na(fem_val)) return(NULL)
      mv <- max(0, min(100, male_val))
      fv <- max(0, min(100, fem_val))
      gap <- round(mv - fv, 1)
      gap_lbl <- if (abs(gap) < 0.5) {
        "Balanced"
      } else if (gap > 0) {
        paste0("Men +", abs(gap), "%")
      } else {
        paste0("Women +", abs(gap), "%")
      }
      gap_cls <- if (abs(gap) < 0.5) {
        "fi-kpi__gap--neutral"
      } else if ((gap > 0) == gap_positive_for_male) {
        "fi-kpi__gap--warn"
      } else {
        "fi-kpi__gap--ok"
      }

      shiny::tags$div(
        class = "fi-kpi",
        shiny::tags$div(class = "fi-kpi__icon", shiny::tags$i(class = paste("fas", icon))),
        shiny::tags$div(
          class = "fi-kpi__body",
          shiny::tags$div(class = "fi-kpi__label", label),
          shiny::tags$div(
            class = "fi-kpi__bars",
            shiny::tags$div(
              class = "fi-kpi__bar-row",
              shiny::tags$div(class = "fi-kpi__bar-lbl fi-kpi__bar-lbl--m", "Men"),
              shiny::tags$div(
                class = "fi-kpi__bar-track",
                shiny::tags$div(class = "fi-kpi__bar fi-kpi__bar--m", style = paste0("width:", mv, "%"))
              ),
              shiny::tags$div(class = "fi-kpi__val", paste0(round(mv, 1), "%"))
            ),
            shiny::tags$div(
              class = "fi-kpi__bar-row",
              shiny::tags$div(class = "fi-kpi__bar-lbl fi-kpi__bar-lbl--f", "Women"),
              shiny::tags$div(
                class = "fi-kpi__bar-track",
                shiny::tags$div(class = "fi-kpi__bar fi-kpi__bar--f", style = paste0("width:", fv, "%"))
              ),
              shiny::tags$div(class = "fi-kpi__val", paste0(round(fv, 1), "%"))
            )
          ),
          shiny::tags$div(class = paste("fi-kpi__gap", gap_cls), gap_lbl)
        )
      )
    }

    hero <- NULL
    ia <- d$intervention_age_groups
    if (!is.null(ia)) {
      tr <- ia[ia$age_group == "Total Resident", , drop = FALSE]
      mrow <- tr[tr$sex == "Male", , drop = FALSE]
      frow <- tr[tr$sex == "Female", , drop = FALSE]
      brow <- tr[tr$sex == "Both sexes", , drop = FALSE]
      if (nrow(brow) == 1 && nrow(mrow) == 1 && nrow(frow) == 1) {
        tot <- suppressWarnings(as.numeric(brow$count[[1]]))[[1L]]
        mc <- suppressWarnings(as.numeric(mrow$count[[1]]))[[1L]]
        fc <- suppressWarnings(as.numeric(frow$count[[1]]))[[1L]]
        mp <- suppressWarnings(as.numeric(mrow$pct[[1]]))[[1L]]
        fp <- suppressWarnings(as.numeric(frow$pct[[1]]))[[1L]]
        ratio <- if (isTRUE(length(mc) == 1L && mc > 0)) {
          round(fc / mc * 100, 1)
        } else {
          NA_real_
        }
        hero <- shiny::tags$div(
          class = "demo-overview-hero",
          shiny::tags$div(
            class = "demo-stat-tile demo-stat-tile--total",
            shiny::tags$div(class = "demo-stat-tile__eyebrow", "Rwanda · total population"),
            shiny::tags$div(class = "demo-stat-tile__value", format(round(tot), big.mark = ",")),
            shiny::tags$div(class = "demo-stat-tile__sub", "Census 2022 · national estimate")
          ),
          shiny::tags$div(
            class = "demo-stat-tile demo-stat-tile--m",
            shiny::tags$div(class = "demo-stat-tile__eyebrow", "Male"),
            shiny::tags$div(class = "demo-stat-tile__value", format(round(mc), big.mark = ",")),
            shiny::tags$div(class = "demo-stat-tile__sub", paste0(round(mp, 1), "% of total"))
          ),
          shiny::tags$div(
            class = "demo-stat-tile demo-stat-tile--f",
            shiny::tags$div(class = "demo-stat-tile__eyebrow", "Female"),
            shiny::tags$div(class = "demo-stat-tile__value", format(round(fc), big.mark = ",")),
            shiny::tags$div(
              class = "demo-stat-tile__sub",
              paste0(round(fp, 1), "% of total",
                if (length(ratio) == 1L && is.finite(ratio)) {
                  paste0(" · ", ratio, " women per 100 men")
                } else {
                  ""
                })
            )
          )
        )
      }
    }

    yth <- NULL
    if (!is.null(d$youth_share_by_geo)) {
      y <- d$youth_share_by_geo
      y <- y[y$geo == "Rwanda" & y$geo_type == "National" & y$residence == "Rwanda", , drop = FALSE]
      ym <- suppressWarnings(as.numeric(y$value[as.character(y$sex) == "Male"]))
      yf <- suppressWarnings(as.numeric(y$value[as.character(y$sex) == "Female"]))
      ym <- if (length(ym)) mean(ym, na.rm = TRUE) else NA_real_
      yf <- if (length(yf)) mean(yf, na.rm = TRUE) else NA_real_
      if (!is.na(ym) && !is.na(yf)) {
        yth <- demo_ov_make_kpi("fa-person-running", "Youth share of population", ym, yf, TRUE)
      }
    }

    sch13 <- NULL
    if (!is.null(d$school_13_18_by_geo)) {
      s <- d$school_13_18_by_geo
      s <- s[s$geo == "Rwanda" & s$geo_type == "National" & s$residence == "Rwanda", , drop = FALSE]
      sm <- suppressWarnings(as.numeric(s$value[as.character(s$sex) == "Male"]))
      sf <- suppressWarnings(as.numeric(s$value[as.character(s$sex) == "Female"]))
      sm <- if (length(sm)) mean(sm, na.rm = TRUE) else NA_real_
      sf <- if (length(sf)) mean(sf, na.rm = TRUE) else NA_real_
      if (!is.na(sm) && !is.na(sf)) {
        sch13 <- demo_ov_make_kpi("fa-book-open", "Adolescents in school (13–18)", sm, sf, TRUE)
      }
    }

    sch7 <- NULL
    if (!is.null(d$school_7_18_attendance)) {
      u <- d$school_7_18_attendance
      u <- u[u$residence == "Rwanda" & u$attendance_status == "Currently attending", , drop = FALSE]
      um <- suppressWarnings(as.numeric(u$value_pct[as.character(u$sex) == "Male"]))
      uf <- suppressWarnings(as.numeric(u$value_pct[as.character(u$sex) == "Female"]))
      um <- if (length(um)) mean(um, na.rm = TRUE) else NA_real_
      uf <- if (length(uf)) mean(uf, na.rm = TRUE) else NA_real_
      if (!is.na(um) && !is.na(uf)) {
        sch7 <- demo_ov_make_kpi("fa-school", "School-age children in school (7–18)", um, uf, TRUE)
      }
    }

    inet <- NULL
    if (!is.null(d$internet_use)) {
      net <- d$internet_use
      net <- net[
        net$age_group == "Population 10 years and above" &
          as.character(net$province) == "Total" &
          net$residence == "Rwanda",
        ,
        drop = FALSE
      ]
      nm <- suppressWarnings(as.numeric(net$value[as.character(net$sex) == "Male"]))
      nf <- suppressWarnings(as.numeric(net$value[as.character(net$sex) == "Female"]))
      tnet <- sum(nm, na.rm = TRUE) + sum(nf, na.rm = TRUE)
      if (isTRUE(tnet > 0)) {
        pm <- 100 * sum(nm, na.rm = TRUE) / tnet
        pf <- 100 * sum(nf, na.rm = TRUE) / tnet
        inet <- demo_ov_make_kpi("fa-wifi", "Internet users (10+) — share by gender", pm, pf, TRUE)
      }
    }

    pop_comp <- NULL
    ia_pc <- d$intervention_age_groups
    if (!is.null(ia_pc)) {
      tr2 <- ia_pc[ia_pc$age_group == "Total Resident", , drop = FALSE]
      pm <- suppressWarnings(as.numeric(tr2$pct[as.character(tr2$sex) == "Male"]))
      pf <- suppressWarnings(as.numeric(tr2$pct[as.character(tr2$sex) == "Female"]))
      pm <- if (length(pm)) mean(pm, na.rm = TRUE) else NA_real_
      pf <- if (length(pf)) mean(pf, na.rm = TRUE) else NA_real_
      if (!is.na(pm) && !is.na(pf)) {
        pop_comp <- demo_ov_make_kpi("fa-venus-mars", "Population composition", pm, pf, FALSE)
      }
    }

    kpi_row <- shiny::tags$div(
      class = "fi-kpis-row fi-kpis-row--demo-overview",
      pop_comp,
      yth,
      sch13,
      sch7,
      inet
    )

    shiny::tags$div(
      class = "demo-overview-kpis",
      shiny::tags$p(
        class = "demo-overview-lead",
        shiny::tags$strong("National overview"),
        " — Gender-disaggregated headline indicators (Rwanda). Use the charts below for provincial detail."
      ),
      hero,
      kpi_row
    )
  })

  demo_filters <- shiny::reactive({
    d <- demo_tbls()
    shiny::req(!is.null(d))

    prov <- input$demo_province
    if (is.null(prov) || length(prov) == 0) prov <- "all"

    dist <- input$demo_district
    if (is.null(dist) || length(dist) == 0) dist <- "all"

    res <- input$demo_residence
    if (is.null(res) || length(res) == 0) res <- "Rwanda"

    sex <- input$demo_sex
    if (is.null(sex) || length(sex) == 0) sex <- c("Female", "Male")

    yrs <- input$demo_years
    if (is.null(yrs) || length(yrs) != 2) yrs <- c(1978, 2022)

    age_grps <- input$demo_age_groups
    if (is.null(age_grps) || length(age_grps) == 0) {
      age_grps <- c("0-4","5-9","10-14","15-19","20-24")
    }

    internet_age <- input$demo_internet_age_group
    if (is.null(internet_age) || length(internet_age) == 0) {
      internet_age <- "Population 10 years and above"
    }

    edu_res <- input$demo_education_residence
    if (is.null(edu_res) || length(edu_res) == 0) edu_res <- "Rwanda"

    edu_levels <- input$demo_education_levels
    if (is.null(edu_levels) || length(edu_levels) == 0) {
      edu_levels <- c(
        "Never attended School",
        "Pre-nursery/ECD",
        "Nursery",
        "Primary",
        "Lower secondary",
        "Upper secondary",
        "University",
        "INGOBOKA/Vocational",
        "Not Stated"
      )
    }

    list(
      province = prov, district = dist, residence = res,
      sex = sex, years = yrs, age_groups = age_grps,
      internet_age_group = internet_age,
      education_residence = edu_res,
      education_levels = edu_levels
    )
  })

  # Province → District cascade
  shiny::observeEvent(input$demo_province, {
    d <- demo_tbls()
    if (is.null(d) || is.null(d$geo_province_district_map)) return()
    mp <- d$geo_province_district_map

    prov <- input$demo_province
    if (is.null(prov) || prov == "all") {
      districts <- sort(unique(mp$district))
    } else {
      districts <- sort(unique(mp$district[mp$province == prov]))
    }

    shiny::updateSelectInput(session, "demo_district",
      choices  = c("All Districts" = "all", setNames(districts, districts)),
      selected = "all"
    )
  }, ignoreInit = TRUE)

  # Live sample count (based on active demography tab)
  output$demo_sample_n <- shiny::renderText({
    d <- demo_tbls()
    if (is.null(d)) return("No data")
    f <- demo_filters()

    tab <- input$demo_tab
    if (is.null(tab)) tab <- "overview"

    if (tab %in% c("population", "overview")) {
      ds <- d$population_geo
      ds$sex <- as.character(ds$sex)
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      if (!is.null(f$district) && f$district != "all") {
        ds <- ds[ds$geo == f$district, , drop = FALSE]
      } else if (!is.null(f$province) && f$province != "all") {
        ds <- ds[ds$geo_type == "District" & ds$province == f$province, , drop = FALSE]
      } else {
        ds <- ds[ds$geo_type == "Province", , drop = FALSE]
      }
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark=","), " records"))
    }

    if (tab == "age") {
      ds <- d$age_distribution
      ds <- ds[ds$residence %in% f$residence, , drop = FALSE]
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      ds <- ds[ds$age_group %in% f$age_groups, , drop = FALSE]
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark=","), " records"))
    }

    if (tab == "internet") {
      ds <- d$internet_use
      ds <- ds[ds$residence %in% f$residence, , drop = FALSE]
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      ds <- ds[ds$age_group %in% f$internet_age_group, , drop = FALSE]
      if (!is.null(f$province) && f$province != "all") ds <- ds[ds$province == f$province, , drop = FALSE]
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark=","), " records"))
    }

    if (tab == "education") {
      ds <- d$education_attendance
      if (is.null(ds)) return("0 records")
      ds <- ds[ds$residence %in% f$education_residence, , drop = FALSE]
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      ds <- ds[ds$education_level %in% f$education_levels, , drop = FALSE]
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark=","), " records"))
    }

    if (tab == "intervention") {
      ds <- d$intervention_age_groups
      if (is.null(ds)) return("0 records")
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
    }

    if (tab == "school718") {
      ds <- d$school_7_18_attendance
      if (is.null(ds)) return("0 records")
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
    }

    if (tab == "school1318") {
      ds <- d$school_13_18_by_geo
      if (is.null(ds)) return("0 records")
      ds$sex <- as.character(ds$sex)
      ds <- ds[ds$residence == f$residence, , drop = FALSE]
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      if (!is.null(f$district) && f$district != "all") {
        ds <- ds[ds$geo == f$district, , drop = FALSE]
      } else if (!is.null(f$province) && f$province != "all") {
        ds <- ds[ds$geo_type == "District" & ds$province == f$province, , drop = FALSE]
      } else {
        nat <- ds[ds$geo == "Rwanda" & ds$geo_type == "National", , drop = FALSE]
        prov <- ds[ds$geo_type == "Province", , drop = FALSE]
        ds <- rbind(nat, prov)
      }
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
    }

    if (tab == "youth") {
      ds <- d$youth_share_by_geo
      if (is.null(ds)) return("0 records")
      ds$sex <- as.character(ds$sex)
      ds <- ds[ds$residence == f$residence, , drop = FALSE]
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
      if (!is.null(f$district) && f$district != "all") {
        ds <- ds[ds$geo == f$district, , drop = FALSE]
      } else if (!is.null(f$province) && f$province != "all") {
        ds <- ds[ds$geo_type == "District" & ds$province == f$province, , drop = FALSE]
      } else {
        nat <- ds[ds$geo == "Rwanda" & ds$geo_type == "National", , drop = FALSE]
        prov <- ds[ds$geo_type == "Province", , drop = FALSE]
        ds <- rbind(nat, prov)
      }
      return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
    }

    if (tab != "elderly") {
      return("—")
    }

    ds <- d$elderly_share
    ds$sex <- as.character(ds$sex)
    ds <- ds[ds$residence %in% f$residence, , drop = FALSE]
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    if (!is.null(f$district) && f$district != "all") {
      ds <- ds[ds$geo == f$district, , drop = FALSE]
    } else if (!is.null(f$province) && f$province != "all") {
      ds <- ds[ds$geo_type == "District" & ds$province == f$province, , drop = FALSE]
    } else {
      ds <- ds[ds$geo_type == "Province", , drop = FALSE]
    }
    if (nrow(ds) == 0) return("0 records")
    paste0(format(nrow(ds), big.mark=","), " records")
  })

  # Reset filters
  shiny::observeEvent(input$demo_reset, {
    shiny::updateSelectInput(session, "demo_province", selected = "all")
    shiny::updateSelectInput(session, "demo_district",
      choices = c("All Districts" = "all"), selected = "all")
    shiny::updateSelectInput(session, "demo_residence", selected = "Rwanda")
    shiny::updateCheckboxGroupInput(session, "demo_sex",
      selected = c("Female", "Male")
    )
    shiny::updateSliderInput(session, "demo_years", value = c(1978, 2022))
    shiny::updateSelectInput(session, "demo_age_groups", selected = c("0-4","5-9","10-14","15-19","20-24"))
    shiny::updateSelectInput(session, "demo_internet_age_group",
      selected = "Population 10 years and above"
    )
    shiny::updateSelectInput(session, "demo_education_residence", selected = "Rwanda")
    shiny::updateSelectInput(session, "demo_education_levels", selected = c(
      "Never attended School",
      "Pre-nursery/ECD",
      "Nursery",
      "Primary",
      "Lower secondary",
      "Upper secondary",
      "University",
      "INGOBOKA/Vocational",
      "Not Stated"
    ))
    shiny::updateSelectInput(session, "demo_t3_measure", selected = "pct")
  }, ignoreInit = TRUE)

  # Shared builders (Overview used duplicate output IDs before — Shiny only binds one ID;
  # separate overview outputs so both tabs show full Male/Female data.)
  demo_build_pop_gender_plot <- function() {
    d <- demo_tbls()
    if (is.null(d) || is.null(d$population_geo)) return(demo_plot_no_data("Population data unavailable"))
    f <- demo_filters()

    ds <- d$population_geo
    ds$sex <- as.character(ds$sex)
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]

    if (!is.null(f$district) && f$district != "all") {
      ds <- ds[ds$geo == f$district, , drop = FALSE]
    } else if (!is.null(f$province) && f$province != "all") {
      ds <- ds[ds$geo_type == "District" & ds$province == f$province, , drop = FALSE]
    } else {
      ds <- ds[ds$geo_type == "Province", , drop = FALSE]
    }

    if (nrow(ds) == 0) return(demo_plot_no_data("No population rows for selected filters"))

    prov_ord <- ds |>
      dplyr::group_by(geo) |>
      dplyr::summarise(avg = mean(value), .groups = "drop") |>
      dplyr::arrange(avg) |>
      dplyr::pull(geo)
    ds$geo <- factor(ds$geo, levels = prov_ord)

    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~value, y = ~geo,
        type = "bar", orientation = "h",
        name = sx,
        marker = list(color = col),
        text = ~paste0(round(value, 0)),
        textposition = "outside",
        hovertemplate = "<b>%{y}</b><br>%{x} people<br>Sex: %{customdata[0]}<extra></extra>",
        customdata = ~sex
      )
    }

    p |>
      fi_plotly_theme(xlab = "Population counts", ylab = "",
                      title = "") |>
      plotly::layout(
        barmode = "group",
        xaxis = list(title = "Population counts"),
        margin = list(l = 140, r = 20, t = 10, b = 50),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      )
  }

  # ── Chart: Population by gender ─────────────────────────────────────────
  output$demo_pop_gender <- plotly::renderPlotly({ demo_build_pop_gender_plot() })
  output$demo_pop_gender_overview <- plotly::renderPlotly({ demo_build_pop_gender_plot() })

  # ── Chart: Population change by gender ────────────────────────────────
  output$demo_pop_change <- plotly::renderPlotly({
    d <- demo_tbls()
    if (is.null(d) || is.null(d$population_change)) return(demo_plot_no_data("Population change data unavailable"))
    f <- demo_filters()

    ds <- d$population_change
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    ds <- ds[ds$year >= f$years[1] & ds$year <= f$years[2], , drop = FALSE]
    if (nrow(ds) == 0) return(demo_plot_no_data("No population change rows for selected filters"))

    ds$sex <- factor(ds$sex, levels = c("Female", "Male"))

    plotly::plot_ly(
      data = ds, x = ~year, y = ~value,
      color = ~sex, type = "scatter", mode = "lines+markers",
      colors = c("Female" = FI_FEMALE, "Male" = FI_MALE),
      line = list(width = 3),
      hovertemplate = "<b>%{x}</b><br>%{y} people<br>Sex: %{legendgroup}<extra></extra>"
    ) |>
      fi_plotly_theme(xlab = "Year", ylab = "Population counts") |>
      plotly::layout(
        yaxis = list(tickformat = ","),
        margin = list(l = 60, r = 20, t = 10, b = 40),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      )
  })

  # ── Chart: Age distribution by gender and residence ───────────────────
  output$demo_age_structure <- plotly::renderPlotly({
    d <- demo_tbls()
    if (is.null(d) || is.null(d$age_distribution)) return(demo_plot_no_data("Age structure data unavailable"))
    f <- demo_filters()

    ds <- d$age_distribution
    ds <- ds[ds$residence == f$residence, , drop = FALSE]
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    ds <- ds[ds$age_group %in% f$age_groups, , drop = FALSE]

    if (nrow(ds) == 0) return(demo_plot_no_data("No age structure rows for selected filters"))

    age_order <- c("Total","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                   "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
                   "80-84","85+")
    ds$age_group <- factor(ds$age_group, levels = age_order)

    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~age_group, y = ~value,
        type = "bar",
        name = sx,
        marker = list(color = col),
        text = ~paste0(round(value, 0)),
        textposition = "outside",
        hovertemplate = "<b>%{x}</b><br>%{y} people<br>Sex: %{legendgroup}<extra></extra>",
        showlegend = TRUE
      )
    }

    both <- all(c("Male", "Female") %in% f$sex)
    p |>
      fi_plotly_theme(xlab = "Age group", ylab = "Counts") |>
      plotly::layout(
        barmode = if (both) "stack" else "group",
        xaxis = list(tickangle = -35),
        margin = list(l = 55, r = 20, t = 10, b = 80),
        legend = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10))
      )
  })

  # ── Chart: Internet use ───────────────────────────────────────────────
  output$demo_internet_use <- plotly::renderPlotly({
    d <- demo_tbls()
    if (is.null(d) || is.null(d$internet_use)) return(demo_plot_no_data("Internet use data unavailable"))
    f <- demo_filters()

    ds <- d$internet_use
    ds <- ds[ds$residence == f$residence, , drop = FALSE]
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    ds <- ds[ds$age_group == f$internet_age_group, , drop = FALSE]
    if (!is.null(f$province) && f$province != "all") {
      ds <- ds[ds$province == f$province, , drop = FALSE]
    }
    if (nrow(ds) == 0) return(demo_plot_no_data("No internet rows for selected filters"))

    prov_ord <- ds |>
      dplyr::group_by(province) |>
      dplyr::summarise(avg = mean(value), .groups = "drop") |>
      dplyr::arrange(avg) |>
      dplyr::pull(province)
    ds$province <- factor(ds$province, levels = prov_ord)

    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~province, y = ~value,
        type = "bar",
        name = sx,
        marker = list(color = col),
        text = ~paste0(round(value, 0)),
        textposition = "outside",
        hovertemplate = "<b>%{x}</b><br>%{y} users<br>Sex: %{legendgroup}<extra></extra>"
      )
    }

    p |>
      fi_plotly_theme(xlab = "Province", ylab = "Internet users (counts)") |>
      plotly::layout(
        barmode = "group",
        xaxis = list(tickangle = -25),
        yaxis = list(tickformat = ","),
        margin = list(l = 55, r = 20, t = 10, b = 70),
        legend = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10))
      )
  })

  # ── Chart: Education attendance ───────────────────────────────────────
  output$demo_education_attendance <- plotly::renderPlotly({
    d <- demo_tbls()
    if (is.null(d) || is.null(d$education_attendance)) {
      return(demo_plot_no_data("Education data unavailable"))
    }
    f <- demo_filters()

    ds <- d$education_attendance
    ds <- ds[ds$residence == f$education_residence, , drop = FALSE]
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    ds <- ds[ds$education_level %in% f$education_levels, , drop = FALSE]

    if (nrow(ds) == 0) return(demo_plot_no_data("No education rows for selected filters"))

    # Order levels by average attendance share across selected sexes.
    ord <- ds |>
      dplyr::group_by(education_level) |>
      dplyr::summarise(avg = mean(pct, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(avg) |>
      dplyr::pull(education_level)
    ds$education_level <- factor(ds$education_level, levels = ord)

    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~education_level, y = ~pct,
        type = "bar",
        name = sx,
        marker = list(color = col),
        text = ~paste0(round(pct, 1), "%"),
        textposition = "outside",
        customdata = ~sex,
        hovertemplate = "<b>%{x}</b><br>%{y}%<br>Sex: %{customdata[0]}<extra></extra>"
      )
    }

    p |>
      fi_plotly_theme(xlab = "Education level", ylab = "Attendance share (%)") |>
      plotly::layout(
        barmode = "group",
        yaxis = list(range = c(0, 100), ticksuffix = "%"),
        xaxis = list(tickangle = -25),
        margin = list(l = 60, r = 20, t = 10, b = 70),
        legend = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10))
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  demo_build_elderly_share_plot <- function() {
    d <- demo_tbls()
    if (is.null(d) || is.null(d$elderly_share)) return(demo_plot_no_data("Elderly share data unavailable"))
    f <- demo_filters()

    ds <- d$elderly_share
    ds$sex <- as.character(ds$sex)
    ds <- ds[ds$residence == f$residence, , drop = FALSE]
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]

    if (!is.null(f$district) && f$district != "all") {
      ds <- ds[ds$geo == f$district, , drop = FALSE]
    } else if (!is.null(f$province) && f$province != "all") {
      ds <- ds[ds$geo_type == "District" & ds$province == f$province, , drop = FALSE]
    } else {
      ds <- ds[ds$geo_type == "Province", , drop = FALSE]
    }

    if (nrow(ds) == 0) return(demo_plot_no_data("No elderly-share rows for selected filters"))

    geo_ord <- ds |>
      dplyr::group_by(geo) |>
      dplyr::summarise(avg = mean(value), .groups = "drop") |>
      dplyr::arrange(avg) |>
      dplyr::pull(geo)
    ds$geo <- factor(ds$geo, levels = geo_ord)

    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~value, y = ~geo,
        type = "bar",
        orientation = "h",
        name = sx,
        marker = list(color = col),
        text = ~paste0(round(value, 1), "%"),
        textposition = "outside",
        hovertemplate = "<b>%{y}</b><br>%{x}%<br>Sex: %{customdata[0]}<extra></extra>",
        customdata = ~sex
      )
    }

    p |>
      fi_plotly_theme(xlab = "Elderly share (%)", ylab = "") |>
      plotly::layout(
        barmode = "group",
        xaxis = list(title = "Elderly share (%)", range = c(0, 25)),
        margin = list(l = 140, r = 20, t = 10, b = 50),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      )
  }

  # ── Chart: Elderly share ──────────────────────────────────────────────
  output$demo_elderly_share <- plotly::renderPlotly({ demo_build_elderly_share_plot() })
  output$demo_elderly_share_overview <- plotly::renderPlotly({ demo_build_elderly_share_plot() })

  # ── Demography: additional census indicators (interactive charts) ───────
  demo_plotly_interactive <- function(p) {
    p |>
      plotly::layout(hovermode = "closest") |>
      plotly::config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToRemove = list("select2d", "lasso2d"),
        toImageButtonOptions = list(format = "png")
      )
  }

  demo_triplet_geo_filter <- function(ds, f) {
    if (is.null(ds) || nrow(ds) == 0) return(ds)
    ds$sex <- as.character(ds$sex)
    ds <- ds[ds$residence == f$residence, , drop = FALSE]
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    if (!is.null(f$district) && f$district != "all") {
      ds <- ds[ds$geo == f$district, , drop = FALSE]
    } else if (!is.null(f$province) && f$province != "all") {
      ds <- ds[ds$geo_type == "District" & ds$province == f$province, , drop = FALSE]
    } else {
      nat <- ds[ds$geo == "Rwanda" & ds$geo_type == "National", , drop = FALSE]
      prov <- ds[ds$geo_type == "Province", , drop = FALSE]
      ds <- rbind(nat, prov)
    }
    ds
  }

  demo_national_mf_gap_text <- function(ds) {
    if (is.null(ds) || nrow(ds) == 0) return("Indicator data could not be loaded.")
    s <- ds[
      ds$geo == "Rwanda" & ds$geo_type == "National" & ds$residence == "Rwanda" &
        ds$sex %in% c("Male", "Female"),
      ,
      drop = FALSE
    ]
    if (nrow(s) < 2) return("National male–female comparison is not available for this selection.")
    mv <- s$value[s$sex == "Male"]
    fv <- s$value[s$sex == "Female"]
    if (length(mv) != 1 || length(fv) != 1) return("National estimates are incomplete.")
    gap <- fv - mv
    paste0(
      "Rwanda (national): male ", format(round(mv, 1), nsmall = 1), "% · female ",
      format(round(fv, 1), nsmall = 1), "% · ",
      if (gap >= 0) "female higher by " else "male higher by ",
      format(abs(round(gap, 1)), nsmall = 1), " percentage points."
    )
  }

  output$demo_intervention_kpis <- shiny::renderUI({
    shiny::req(input$demo_tab == "intervention")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$intervention_age_groups)) {
      return(shiny::tags$div(class = "text-muted", "Headline indicators are unavailable or still loading."))
    }
    tr <- d$intervention_age_groups[d$intervention_age_groups$age_group == "Total Resident", , drop = FALSE]
    if (nrow(tr) < 1) return(NULL)
    m <- tr[tr$sex == "Male", , drop = FALSE]
    fem <- tr[tr$sex == "Female", , drop = FALSE]
    b <- tr[tr$sex == "Both sexes", , drop = FALSE]
    kpi <- function(label, val, sub) {
      shiny::tags$div(
        class = "fi-chart-card",
        style = "padding: 14px 16px; min-height: 0;",
        shiny::tags$div(style = "font-size: 11px; color: #6B7280; text-transform: uppercase; letter-spacing: .04em;", label),
        shiny::tags$div(style = "font-size: 22px; font-weight: 700; color: #111827; margin-top: 4px;", val),
        shiny::tags$div(style = "font-size: 12px; color: #4B5563; margin-top: 4px;", sub)
      )
    }
    shiny::tags$div(
      class = "fi-charts-grid",
      style = "margin-bottom: 14px;",
      if (nrow(b) == 1) kpi("Total population", format(round(b$count[[1]]), big.mark = ","), "National total · both sexes") else NULL,
      if (nrow(m) == 1) kpi("Male", format(round(m$count[[1]]), big.mark = ","), paste0(format(round(m$pct[[1]], 1), nsmall = 1), "% of total")) else NULL,
      if (nrow(fem) == 1) kpi("Female", format(round(fem$count[[1]]), big.mark = ","), paste0(format(round(fem$pct[[1]], 1), nsmall = 1), "% of total")) else NULL
    )
  })

  output$demo_intervention_bars <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "intervention")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$intervention_age_groups)) return(demo_plot_no_data("Data not available for this view."))
    f <- demo_filters()
    ds <- d$intervention_age_groups
    ds$sex <- as.character(ds$sex)
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    meas <- input$demo_t3_measure
    if (is.null(meas)) meas <- "pct"
    ycol <- if (identical(meas, "count")) "count" else "pct"
    if (nrow(ds) == 0) return(demo_plot_no_data("No rows for selected genders"))

    both_ref <- d$intervention_age_groups[d$intervention_age_groups$sex == "Both sexes", , drop = FALSE]
    ord <- both_ref$age_group[order(-both_ref[[ycol]], na.last = TRUE)]
    ord <- unique(c("Total Resident", setdiff(ord, "Total Resident")))
    ds$age_group <- factor(ds$age_group, levels = ord)

    ylab <- if (ycol == "count") "Population (headcount)" else "Share of national population (%)"
    ttl <- "Programme age groups — distribution by gender"
    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      sub$metric <- if (identical(ycol, "count")) sub$count else sub$pct
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~age_group, y = ~metric,
        type = "bar",
        name = sx,
        marker = list(color = col),
        hovertemplate = paste0(
          "<b>%{x}</b><br>%{y}",
          if (identical(ycol, "pct")) "%" else "",
          "<br>", sx, "<extra></extra>"
        )
      )
    }
    p |>
      fi_plotly_theme(xlab = "Age group / programme band", ylab = ylab) |>
      plotly::layout(
        title = list(text = ttl, font = list(size = 14)),
        barmode = "group",
        xaxis = list(tickangle = -45),
        yaxis = if (identical(ycol, "pct")) {
          list(range = c(0, 105), ticksuffix = "%")
        } else {
          list(tickformat = ",")
        },
        margin = list(l = 60, r = 20, t = 50, b = 120),
        legend = list(orientation = "h", x = 0, y = -0.28, font = list(size = 10))
      ) |>
      demo_plotly_interactive()
  })

  output$demo_intervention_gap <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "intervention")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$intervention_age_groups)) return(demo_plot_no_data("Data not available for this view."))
    ia <- d$intervention_age_groups
    ia <- ia[ia$sex %in% c("Male", "Female"), , drop = FALSE]
    if (nrow(ia) == 0) return(demo_plot_no_data("No data"))

    wide <- tryCatch(
      tidyr::pivot_wider(ia, id_cols = "age_group", names_from = "sex", values_from = "pct"),
      error = function(e) NULL
    )
    if (is.null(wide) || !all(c("Female", "Male") %in% names(wide))) {
      return(demo_plot_no_data("Unable to compute the gender disparity for this indicator."))
    }
    wide$gap <- wide$Female - wide$Male
    wide <- wide[wide$age_group != "Total Resident", , drop = FALSE]
    both_ref <- d$intervention_age_groups[d$intervention_age_groups$sex == "Both sexes", , drop = FALSE]
    ord <- both_ref$age_group[order(-both_ref$pct, na.last = TRUE)]
    wide$age_group <- factor(wide$age_group, levels = setdiff(ord, "Total Resident"))

    plotly::plot_ly(
      data = wide, x = ~age_group, y = ~gap,
      type = "scatter", mode = "lines+markers",
      line = list(color = "#7C3AED", width = 2),
      marker = list(size = 8, color = "#7C3AED"),
      hovertemplate = "<b>%{x}</b><br>Female % − Male %: %{y:.1f} pp.<extra></extra>"
    ) |>
      fi_plotly_theme(xlab = "Age group / programme band", ylab = "Difference (percentage points)") |>
      plotly::layout(
        title = list(text = "Female minus male share of population — deviation from parity", font = list(size = 13)),
        shapes = list(list(
          type = "line", x0 = 0, x1 = 1, xref = "paper",
          y0 = 0, y1 = 0, yref = "y",
          line = list(color = "#9CA3AF", dash = "dash")
        )),
        xaxis = list(tickangle = -45),
        margin = list(l = 55, r = 20, t = 50, b = 120)
      ) |>
      demo_plotly_interactive()
  })

  output$demo_school718_kpis <- shiny::renderUI({
    shiny::req(input$demo_tab == "school718")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$school_7_18_attendance)) {
      return(shiny::tags$div(class = "text-muted", "School participation indicators are unavailable."))
    }
    ds <- d$school_7_18_attendance
    ds <- ds[ds$residence == "Rwanda", , drop = FALSE]
    cur <- ds[ds$attendance_status == "Currently attending", , drop = FALSE]
    m <- as.numeric(cur$value_pct[cur$sex == "Male"])
    fv <- as.numeric(cur$value_pct[cur$sex == "Female"])
    m <- if (length(m)) m[[1]] else NA_real_
    fv <- if (length(fv)) fv[[1]] else NA_real_
    if (is.na(m) || is.na(fv)) {
      return(shiny::tags$div(class = "text-muted", "Headline rates unavailable."))
    }
    shiny::tags$div(
      class = "fi-charts-grid",
      style = "margin-bottom: 14px;",
      shiny::tags$div(
        class = "fi-chart-card",
        style = "padding: 14px 16px; min-height: 0;",
        shiny::tags$div(style = "font-size: 11px; color: #6B7280;", "National · currently attending school (ages 7–18)"),
        shiny::tags$div(style = "font-size: 15px; color: #111827; margin-top: 6px;",
          paste0(
            "Male ", round(m, 1), "% · Female ", round(fv, 1), "% · disparity (female − male): ",
            sprintf("%+.1f", fv - m), " pp."
          )
        )
      )
    )
  })

  output$demo_school718_facets <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "school718")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$school_7_18_attendance)) return(demo_plot_no_data("Data not available for this view."))
    f <- demo_filters()
    att_ord <- c("No longer attending", "Currently attending", "Never attended")

    build_panel <- function(res_lbl, show_leg) {
      ds <- d$school_7_18_attendance
      ds <- ds[ds$residence == res_lbl & ds$sex %in% f$sex, , drop = FALSE]
      ds$attendance_status <- factor(ds$attendance_status, levels = att_ord)
      p <- plotly::plot_ly()
      for (sx in c("Male", "Female")) {
        if (!sx %in% f$sex) next
        sub <- ds[ds$sex == sx, , drop = FALSE]
        col <- if (sx == "Male") FI_MALE else FI_FEMALE
        p <- plotly::add_trace(p,
          data = sub, x = ~attendance_status, y = ~value_pct,
          type = "bar",
          name = sx,
          legendgroup = sx,
          showlegend = show_leg,
          marker = list(color = col),
          hovertemplate = paste0("<b>", res_lbl, "</b><br>%{x}<br>%{y}% · ", sx, "<extra></extra>")
        )
      }
      p |>
        fi_plotly_theme(xlab = "", ylab = "Share (%)") |>
        plotly::layout(
          title = list(text = res_lbl, font = list(size = 13)),
          barmode = "group",
          yaxis = list(range = c(0, 105), ticksuffix = "%"),
          xaxis = list(tickangle = -25, title = ""),
          margin = list(t = 40, b = 70, l = 45, r = 12)
        )
    }

    s1 <- build_panel("Rwanda", TRUE)
    s2 <- build_panel("Urban", FALSE)
    s3 <- build_panel("Rural", FALSE)
    plotly::subplot(s1, s2, s3, nrows = 1, shareY = TRUE, margin = 0.045, titleX = TRUE) |>
      plotly::layout(
        title = list(text = "Attendance profile: national, urban, and rural", font = list(size = 14)),
        legend = list(orientation = "h", x = 0.05, y = -0.18)
      ) |>
      demo_plotly_interactive()
  })

  output$demo_school718_stacked <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "school718")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$school_7_18_attendance)) return(demo_plot_no_data("Data not available for this view."))
    f <- demo_filters()
    ds <- d$school_7_18_attendance
    ds <- ds[ds$residence == f$residence & ds$sex %in% f$sex, , drop = FALSE]
    att_ord <- c("No longer attending", "Currently attending", "Never attended")
    cols <- c(
      "No longer attending" = "#F59E0B",
      "Currently attending" = "#10B981",
      "Never attended" = "#94A3B8"
    )
    p <- plotly::plot_ly()
    for (st in att_ord) {
      sub <- ds[ds$attendance_status == st, , drop = FALSE]
      sub <- sub[order(match(sub$sex, c("Male", "Female"))), , drop = FALSE]
      if (nrow(sub) == 0) next
      col_st <- unname(cols[st])
      if (length(col_st) != 1 || is.na(col_st)) col_st <- "#64748B"
      p <- plotly::add_trace(p,
        data = sub, x = ~sex, y = ~value_pct,
        type = "bar",
        name = st,
        marker = list(color = col_st),
        hovertemplate = paste0(st, ": %{y}% · %{x}<extra></extra>")
      )
    }
    ttl <- paste0("Attendance status distribution — ", f$residence, " (shares sum to 100%)")
    p |>
      fi_plotly_theme(xlab = "Gender", ylab = "Share (%)") |>
      plotly::layout(
        title = list(text = ttl, font = list(size = 14)),
        barmode = "stack",
        yaxis = list(range = c(0, 105), ticksuffix = "%"),
        margin = list(l = 55, r = 20, t = 50, b = 45),
        legend = list(orientation = "h", x = 0, y = -0.22, font = list(size = 10))
      ) |>
      demo_plotly_interactive()
  })

  output$demo_school1318_kpis <- shiny::renderUI({
    shiny::req(input$demo_tab == "school1318")
    d <- demo_tbls()
    shiny::tags$div(
      class = "fi-chart-card",
      style = "padding: 12px 16px; margin-bottom: 14px; max-width: 900px;",
      shiny::tags$div(style = "font-size: 12px; color: #374151;", demo_national_mf_gap_text(d$school_13_18_by_geo))
    )
  })

  output$demo_youth_kpis <- shiny::renderUI({
    shiny::req(input$demo_tab == "youth")
    d <- demo_tbls()
    shiny::tags$div(
      class = "fi-chart-card",
      style = "padding: 12px 16px; margin-bottom: 14px; max-width: 900px;",
      shiny::tags$div(style = "font-size: 12px; color: #374151;", demo_national_mf_gap_text(d$youth_share_by_geo))
    )
  })

  output$demo_school1318_bars <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "school1318")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$school_13_18_by_geo)) return(demo_plot_no_data("Data not available for this view."))
    f <- demo_filters()
    ds <- demo_triplet_geo_filter(d$school_13_18_by_geo, f)
    if (is.null(ds) || nrow(ds) == 0) return(demo_plot_no_data("No rows for filters"))

    geo_ord <- ds |>
      dplyr::group_by(geo) |>
      dplyr::summarise(avg = mean(value), .groups = "drop") |>
      dplyr::arrange(avg) |>
      dplyr::pull(geo)
    ds$geo <- factor(ds$geo, levels = geo_ord)

    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~value, y = ~geo,
        type = "bar",
        orientation = "h",
        name = sx,
        marker = list(color = col),
        text = ~paste0(round(value, 1), "%"),
        textposition = "outside",
        hovertemplate = paste0("<b>%{y}</b><br>%{x}% · ", sx, "<extra></extra>")
      )
    }
    ttl <- paste0("Adolescents in school (ages 13–18) — ", f$residence)
    p |>
      fi_plotly_theme(xlab = "Currently in school (%)", ylab = "") |>
      plotly::layout(
        title = list(text = ttl, font = list(size = 14)),
        barmode = "group",
        xaxis = list(range = c(0, 105), ticksuffix = "%"),
        margin = list(l = 150, r = 20, t = 50, b = 40),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      ) |>
      demo_plotly_interactive()
  })

  output$demo_school1318_gap <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "school1318")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$school_13_18_by_geo)) return(demo_plot_no_data("Data not available for this view."))
    f <- demo_filters()
    ds <- demo_triplet_geo_filter(d$school_13_18_by_geo, f)
    if (is.null(ds) || nrow(ds) == 0) return(demo_plot_no_data("No rows"))

    ugeo <- unique(as.character(ds$geo))
    gapv <- vapply(ugeo, function(g) {
      m <- ds$value[ds$geo == g & ds$sex == "Male"]
      fv <- ds$value[ds$geo == g & ds$sex == "Female"]
      if (length(m) == 1 && length(fv) == 1) fv - m else NA_real_
    }, numeric(1))
    gg <- data.frame(geo = ugeo, gap = gapv, stringsAsFactors = FALSE)
    gg <- gg[!is.na(gg$gap), , drop = FALSE]
    gg$geo <- factor(gg$geo, levels = gg$geo[order(gg$gap)])
    gg$bar_col <- ifelse(gg$gap >= 0, FI_FEMALE, FI_MALE)

    plotly::plot_ly(
      data = gg, x = ~gap, y = ~geo,
      type = "bar",
      orientation = "h",
      marker = list(color = ~bar_col),
      hovertemplate = "<b>%{y}</b><br>Female − Male: %{x:.1f} pp.<extra></extra>"
    ) |>
      fi_plotly_theme(xlab = "Gap (percentage points)", ylab = "") |>
      plotly::layout(
        title = list(text = "Disparity by area — positive values: female enrolment rate higher than male", font = list(size = 12)),
        xaxis = list(zeroline = TRUE, zerolinewidth = 1, zerolinecolor = "#9CA3AF"),
        margin = list(l = 150, r = 20, t = 50, b = 40)
      ) |>
      demo_plotly_interactive()
  })

  output$demo_youth_bars <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "youth")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$youth_share_by_geo)) return(demo_plot_no_data("Data not available for this view."))
    f <- demo_filters()
    ds <- demo_triplet_geo_filter(d$youth_share_by_geo, f)
    if (is.null(ds) || nrow(ds) == 0) return(demo_plot_no_data("No rows for filters"))

    geo_ord <- ds |>
      dplyr::group_by(geo) |>
      dplyr::summarise(avg = mean(value), .groups = "drop") |>
      dplyr::arrange(avg) |>
      dplyr::pull(geo)
    ds$geo <- factor(ds$geo, levels = geo_ord)

    p <- plotly::plot_ly()
    for (sx in c("Male", "Female")) {
      if (!sx %in% f$sex) next
      sub <- ds[ds$sex == sx, , drop = FALSE]
      col <- if (sx == "Male") FI_MALE else FI_FEMALE
      p <- plotly::add_trace(p,
        data = sub, x = ~value, y = ~geo,
        type = "bar",
        orientation = "h",
        name = sx,
        marker = list(color = col),
        text = ~paste0(round(value, 1), "%"),
        textposition = "outside",
        hovertemplate = paste0("<b>%{y}</b><br>%{x}% · ", sx, "<extra></extra>")
      )
    }
    ttl <- paste0("Youth share of population — ", f$residence)
    p |>
      fi_plotly_theme(xlab = "Youth share (%)", ylab = "") |>
      plotly::layout(
        title = list(text = ttl, font = list(size = 14)),
        barmode = "group",
        xaxis = list(range = c(0, 55), ticksuffix = "%"),
        margin = list(l = 150, r = 20, t = 50, b = 40),
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      ) |>
      demo_plotly_interactive()
  })

  output$demo_youth_gap <- plotly::renderPlotly({
    shiny::req(input$demo_tab == "youth")
    d <- demo_tbls()
    if (is.null(d) || is.null(d$youth_share_by_geo)) return(demo_plot_no_data("Data not available for this view."))
    f <- demo_filters()
    ds <- demo_triplet_geo_filter(d$youth_share_by_geo, f)
    if (is.null(ds) || nrow(ds) == 0) return(demo_plot_no_data("No rows"))

    ugeo <- unique(as.character(ds$geo))
    gapv <- vapply(ugeo, function(g) {
      m <- ds$value[ds$geo == g & ds$sex == "Male"]
      fv <- ds$value[ds$geo == g & ds$sex == "Female"]
      if (length(m) == 1 && length(fv) == 1) fv - m else NA_real_
    }, numeric(1))
    gg <- data.frame(geo = ugeo, gap = gapv, stringsAsFactors = FALSE)
    gg <- gg[!is.na(gg$gap), , drop = FALSE]
    gg$geo <- factor(gg$geo, levels = gg$geo[order(gg$gap)])
    gg$bar_col <- ifelse(gg$gap >= 0, FI_FEMALE, FI_MALE)

    plotly::plot_ly(
      data = gg, x = ~gap, y = ~geo,
      type = "bar",
      orientation = "h",
      marker = list(color = ~bar_col),
      hovertemplate = "<b>%{y}</b><br>Female − Male: %{x:.1f} pp.<extra></extra>"
    ) |>
      fi_plotly_theme(xlab = "Gap (percentage points)", ylab = "") |>
      plotly::layout(
        title = list(text = "Disparity by area — positive values: female youth share higher than male", font = list(size = 12)),
        xaxis = list(zeroline = TRUE, zerolinewidth = 1, zerolinecolor = "#9CA3AF"),
        margin = list(l = 150, r = 20, t = 50, b = 40)
      ) |>
      demo_plotly_interactive()
  })

}
