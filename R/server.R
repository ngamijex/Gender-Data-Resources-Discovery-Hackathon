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
    plotly::plot_ly() |>
      plotly::add_trace(data = dhs_maternal, x = ~year, y = ~skilled_birth_pct, name = "Skilled Birth Attendance",
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
    plotly::plot_ly() |>
      plotly::add_trace(data = education_df, x = ~year, y = ~gpi_primary, name = "Primary GPI",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["primary"], width = 2.5),
                marker = list(color = CLR["primary"], size = 8),
                hovertemplate = "Primary GPI: %{y:.2f}<extra></extra>") |>
      plotly::add_trace(data = education_df, x = ~year, y = ~gpi_secondary, name = "Secondary GPI",
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
    plotly::plot_ly() |>
      plotly::add_bars(data = education_df, x = ~year, y = ~female_secondary_completion, name = "Female",
               marker = list(color = CLR["primary"]),
               hovertemplate = "%{x} Female: %{y}%<extra></extra>") |>
      plotly::add_bars(data = education_df, x = ~year, y = ~male_secondary_completion, name = "Male",
               marker = list(color = CLR["ink_300"]),
               hovertemplate = "%{x} Male: %{y}%<extra></extra>") |>
      gddp_theme(xlab = "Year", ylab = "Completion Rate (%)") |>
      plotly::layout(barmode = "group",
             legend = list(orientation = "h", x = 0, y = 1.18))
  })

  # Labor plot
  output$ch_lfp <- plotly::renderPlotly({
    plotly::plot_ly() |>
      plotly::add_trace(data = labor_df, x = ~year, y = ~female, name = "Female",
                type = "scatter", mode = "lines+markers",
                line   = list(color = CLR["primary"], width = 2.5),
                marker = list(color = CLR["primary"], size = 9),
                hovertemplate = "%{x} Female: %{y}%<extra></extra>") |>
      plotly::add_trace(data = labor_df, x = ~year, y = ~male, name = "Male",
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
    plotly::plot_ly() |>
      plotly::add_bars(data = province_df, x = ~province, y = ~female_land_pct, name = "Land Ownership (%)",
               marker = list(color = CLR["accent"]),
               hovertemplate = "%{x} \u2014 Land: %{y}%<extra></extra>") |>
      plotly::add_bars(data = province_df, x = ~province, y = ~female_literacy_pct, name = "Literacy (%)",
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
  # shinyapps.io / some hosts: globals from app.R may not be visible here — load directly.
  if (is.null(gov_data_once)) {
    gov_data_once <- tryCatch(load_governance_data(), error = function(e) NULL)
  }
  if (is.null(gov_data_once)) {
    message("[GOV] Governance data not available in server (init).")
  } else {
    message("[GOV] Governance data ready in server (init): ",
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

  output$gov_sample_n <- shiny::renderText({
    tab <- input$gov_tab
    d <- gov_tbls()
    if (is.null(d)) return("No data")

    f <- gov_filters()
    gov_count_rows <- function(df, use_entity_pos = TRUE) {
      if (is.null(df)) return(NA_integer_)
      df <- df[
        df$year >= f$yr[1] & df$year <= f$yr[2] & df$sex %in% f$sex,
        , drop = FALSE
      ]
      if (isTRUE(use_entity_pos)) {
        if (!is.null(f$entity) && isTRUE(f$entity != "all") && "entity" %in% names(df)) {
          df <- df[df$entity == f$entity, , drop = FALSE]
        }
        if (!is.null(f$pos) && isTRUE(f$pos != "all") && "entity" %in% names(df)) {
          df <- df[df$entity == f$pos, , drop = FALSE]
        }
      }
      as.integer(nrow(df))
    }

    n <- NA_integer_
    if (is.null(tab) || tab == "overview") {
      parts <- c(
        gov_count_rows(d$ministers, use_entity_pos = FALSE),
        gov_count_rows(d$parliament, use_entity_pos = FALSE),
        gov_count_rows(d$prosecutors, use_entity_pos = FALSE),
        gov_count_rows(d$judiciary, use_entity_pos = FALSE),
        gov_count_rows(d$local_leaders, use_entity_pos = FALSE)
      )
      parts <- parts[!is.na(parts)]
      n <- if (length(parts)) sum(parts) else NA_integer_
      if (!is.na(n) && n >= 0) {
        return(paste0(format(n, big.mark = ","), " points (all series)"))
      }
      return("—")
    }

    if (tab == "ministers") n <- gov_count_rows(d$ministers)
    else if (tab == "parliament") n <- gov_count_rows(d$parliament)
    else if (tab == "prosecutors") n <- gov_count_rows(d$prosecutors)
    else if (tab == "judiciary") n <- gov_count_rows(d$judiciary)
    else if (tab == "local") n <- gov_count_rows(d$local_leaders)

    if (is.na(n)) return("No data")
    paste0(format(n, big.mark = ","), " records")
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
        "Use the other sidebar tabs for full time series and charts."
      ),
      kpi_row
    )
  })

  # Update choice lists for judiciary/local tabs
  shiny::observeEvent(input$gov_tab, {
    d <- gov_tbls()
    if (is.null(d)) return()

    if (!is.null(input$gov_tab) && input$gov_tab == "judiciary") {
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

    ds$pct <- suppressWarnings(as.numeric(ds$pct))
    ds <- ds[!is.na(ds$pct), , drop = FALSE]
    if (nrow(ds) == 0) {
      return(gov_plot_no_data("No complete percentage values for local leadership (check source data)."))
    }

    single_pos <- !is.null(f$pos) && length(f$pos) == 1L && f$pos != "all"

    # One position: classic Male vs Female lines over time (same as other governance charts).
    if (single_pos) {
      ds$sex <- factor(ds$sex, levels = c("Female", "Male"))
      return(
        plotly::plot_ly(
          data = ds, x = ~year, y = ~pct, color = ~sex,
          type = "scatter", mode = "lines+markers",
          colors = c("Female" = FI_FEMALE, "Male" = FI_MALE),
          line = list(width = 2.5),
          hovertemplate = "<b>Year %{x}</b><br>%{y:.1f}% of posts<br>%{fullData.name}<extra></extra>"
        ) |>
          plotly::layout(
            title = list(
              text = paste0("Men vs women — ", as.character(f$pos)),
              font = list(size = 13)
            ),
            xaxis = list(title = "Year", tickfont = list(size = 11), dtick = 1),
            yaxis = list(title = "% of posts in this role", range = c(0, 100), ticksuffix = "%"),
            legend = list(orientation = "h", x = 0, y = -0.22, font = list(size = 10)),
            margin = list(l = 55, r = 20, t = 45, b = 55)
          ) |>
          plotly::config(displayModeBar = FALSE)
      )
    }

    # All positions: one line per role = women’s share (parity metric; interpretable at a glance).
    metric_sex <- if ("Female" %in% f$sex) "Female" else "Male"
    ds_line <- ds[ds$sex == metric_sex, , drop = FALSE]
    if (nrow(ds_line) == 0) {
      return(gov_plot_no_data(paste0("No ", metric_sex, " rows for selected years.")))
    }

    ents <- sort(unique(as.character(ds_line$entity)))
    ds_line$entity <- factor(ds_line$entity, levels = ents)

    ht <- if (metric_sex == "Female") {
      "<b>%{fullData.name}</b><br>Year %{x}<br>Women’s share: %{y:.1f}%<extra></extra>"
    } else {
      "<b>%{fullData.name}</b><br>Year %{x}<br>Men’s share: %{y:.1f}%<extra></extra>"
    }

    plotly::plot_ly(
      data = ds_line,
      x = ~year,
      y = ~pct,
      color = ~entity,
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 2),
      hovertemplate = ht
    ) |>
      plotly::layout(
        title = list(
          text = if (metric_sex == "Female") {
            "Women’s share of local leadership posts by position"
          } else {
            "Men’s share of local leadership posts by position"
          },
          font = list(size = 13)
        ),
        xaxis = list(title = "Year", tickfont = list(size = 11), dtick = 1),
        yaxis = list(
          title = if (metric_sex == "Female") "Women’s share of posts (%)" else "Men’s share of posts (%)",
          range = c(0, 100),
          ticksuffix = "%"
        ),
        legend = list(
          title = list(text = "Position"),
          orientation = "h",
          x = 0,
          y = -0.32,
          font = list(size = 10)
        ),
        margin = list(l = 58, r = 24, t = 48, b = 110)
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
  # shinyapps.io: globals from app.R are not always visible — load like Governance.
  if (is.null(demo_data_once)) {
    demo_data_once <- tryCatch(load_demography_data(), error = function(e) NULL)
  }
  if (is.null(demo_data_once)) {
    message("[DEMO] Demography data not available in server (init).")
  } else {
    message("[DEMO] Demography data ready in server (init).")
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
    .demo_df <- function(x) {
      if (is.null(x)) return(NULL)
      if (!inherits(x, "data.frame")) {
        x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
      }
      if (is.null(x) || nrow(x) == 0L) return(NULL)
      normalize_df_for_indexing(x)
    }
    tryCatch(
      {
        d <- demo_tbls()
        if (is.null(d)) return("No data")
        f <- demo_filters()

        tab <- input$demo_tab
        if (is.null(tab) || !nzchar(as.character(tab))) tab <- "overview"

        if (tab %in% c("population", "overview")) {
          ds <- .demo_df(d$population_geo)
          if (is.null(ds)) return("0 records")
          if (!"sex" %in% names(ds)) return("0 records")
          ds$sex <- as.character(ds$sex)
          n <- nrow(ds)
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, n), , drop = FALSE]
          if (!is.null(f$district) && length(f$district) == 1L && f$district != "all") {
            ds <- ds[demo_align_lgl(ds$geo == f$district, nrow(ds)), , drop = FALSE]
          } else if (!is.null(f$province) && length(f$province) == 1L && f$province != "all") {
            ds <- ds[demo_align_lgl(ds$geo_type == "District" & ds$province == f$province, nrow(ds)), , drop = FALSE]
          } else {
            ds <- ds[demo_align_lgl(ds$geo_type == "Province", nrow(ds)), , drop = FALSE]
          }
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab == "age") {
          ds <- .demo_df(d$age_distribution)
          if (is.null(ds)) return("0 records")
          n <- nrow(ds)
          ds <- ds[demo_align_lgl(ds$residence %in% f$residence, n), , drop = FALSE]
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
          ds <- ds[demo_align_lgl(ds$age_group %in% f$age_groups, nrow(ds)), , drop = FALSE]
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab == "internet") {
          ds <- .demo_df(d$internet_use)
          if (is.null(ds)) return("0 records")
          n <- nrow(ds)
          ds <- ds[demo_align_lgl(ds$residence %in% f$residence, n), , drop = FALSE]
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
          iag <- f$internet_age_group
          if (length(iag) == 1L) {
            ds <- ds[demo_align_lgl(ds$age_group == iag, nrow(ds)), , drop = FALSE]
          } else {
            ds <- ds[demo_align_lgl(ds$age_group %in% iag, nrow(ds)), , drop = FALSE]
          }
          if (!is.null(f$province) && length(f$province) == 1L && f$province != "all") {
            ds <- ds[demo_align_lgl(ds$province == f$province, nrow(ds)), , drop = FALSE]
          }
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab == "education") {
          ds <- .demo_df(d$education_attendance)
          if (is.null(ds)) return("0 records")
          n <- nrow(ds)
          ds <- ds[demo_align_lgl(ds$residence %in% f$education_residence, n), , drop = FALSE]
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
          ds <- ds[demo_align_lgl(ds$education_level %in% f$education_levels, nrow(ds)), , drop = FALSE]
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab == "intervention") {
          ds <- .demo_df(d$intervention_age_groups)
          if (is.null(ds)) return("0 records")
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab == "school718") {
          ds <- .demo_df(d$school_7_18_attendance)
          if (is.null(ds)) return("0 records")
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab == "school1318") {
          ds <- .demo_df(d$school_13_18_by_geo)
          if (is.null(ds)) return("0 records")
          ds$sex <- as.character(ds$sex)
          n <- nrow(ds)
          ds <- ds[demo_align_lgl(ds$residence == f$residence, n), , drop = FALSE]
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
          if (!is.null(f$district) && length(f$district) == 1L && f$district != "all") {
            ds <- ds[demo_align_lgl(ds$geo == f$district, nrow(ds)), , drop = FALSE]
          } else if (!is.null(f$province) && length(f$province) == 1L && f$province != "all") {
            ds <- ds[demo_align_lgl(ds$geo_type == "District" & ds$province == f$province, nrow(ds)), , drop = FALSE]
          } else {
            n2 <- nrow(ds)
            nat <- ds[demo_align_lgl(ds$geo == "Rwanda" & ds$geo_type == "National", n2), , drop = FALSE]
            prov <- ds[demo_align_lgl(ds$geo_type == "Province", n2), , drop = FALSE]
            ds <- dplyr::bind_rows(nat, prov)
          }
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab == "youth") {
          ds <- .demo_df(d$youth_share_by_geo)
          if (is.null(ds)) return("0 records")
          ds$sex <- as.character(ds$sex)
          n <- nrow(ds)
          ds <- ds[demo_align_lgl(ds$residence == f$residence, n), , drop = FALSE]
          ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
          if (!is.null(f$district) && length(f$district) == 1L && f$district != "all") {
            ds <- ds[demo_align_lgl(ds$geo == f$district, nrow(ds)), , drop = FALSE]
          } else if (!is.null(f$province) && length(f$province) == 1L && f$province != "all") {
            ds <- ds[demo_align_lgl(ds$geo_type == "District" & ds$province == f$province, nrow(ds)), , drop = FALSE]
          } else {
            n2 <- nrow(ds)
            nat <- ds[demo_align_lgl(ds$geo == "Rwanda" & ds$geo_type == "National", n2), , drop = FALSE]
            prov <- ds[demo_align_lgl(ds$geo_type == "Province", n2), , drop = FALSE]
            ds <- dplyr::bind_rows(nat, prov)
          }
          return(if (nrow(ds) == 0) "0 records" else paste0(format(nrow(ds), big.mark = ","), " records"))
        }

        if (tab != "elderly") {
          return("—")
        }

        ds <- .demo_df(d$elderly_share)
        if (is.null(ds)) return("0 records")
        ds$sex <- as.character(ds$sex)
        n <- nrow(ds)
        ds <- ds[demo_align_lgl(ds$residence %in% f$residence, n), , drop = FALSE]
        ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
        if (!is.null(f$district) && length(f$district) == 1L && f$district != "all") {
          ds <- ds[demo_align_lgl(ds$geo == f$district, nrow(ds)), , drop = FALSE]
        } else if (!is.null(f$province) && length(f$province) == 1L && f$province != "all") {
          ds <- ds[demo_align_lgl(ds$geo_type == "District" & ds$province == f$province, nrow(ds)), , drop = FALSE]
        } else {
          ds <- ds[demo_align_lgl(ds$geo_type == "Province", nrow(ds)), , drop = FALSE]
        }
        if (nrow(ds) == 0) return("0 records")
        paste0(format(nrow(ds), big.mark = ","), " records")
      },
      error = function(e) {
        message("[DEMO] demo_sample_n: ", conditionMessage(e))
        "—"
      }
    )
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

    ds <- normalize_df_for_indexing(d$population_geo)
    ds$sex <- trimws(as.character(ds$sex))
    ds$value <- suppressWarnings(as.numeric(ds$value))
    ds <- ds[demo_align_lgl(ds$sex %in% trimws(f$sex), nrow(ds)), , drop = FALSE]

    if (!is.null(f$district) && length(f$district) == 1L && f$district != "all") {
      ds <- ds[demo_align_lgl(trimws(as.character(ds$geo)) == trimws(f$district), nrow(ds)), , drop = FALSE]
    } else if (!is.null(f$province) && length(f$province) == 1L && f$province != "all") {
      gt <- tolower(trimws(as.character(ds$geo_type)))
      ds <- ds[
        demo_align_lgl(gt == "district" & trimws(as.character(ds$province)) == trimws(f$province), nrow(ds)),
        ,
        drop = FALSE
      ]
    } else {
      gt <- tolower(trimws(as.character(ds$geo_type)))
      ds <- ds[demo_align_lgl(!is.na(gt) & gt == "province", nrow(ds)), , drop = FALSE]
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

    ds <- normalize_df_for_indexing(d$population_change)
    ds$sex <- trimws(as.character(ds$sex))
    ds$year <- suppressWarnings(as.integer(as.numeric(ds$year)))
    ds$value <- suppressWarnings(as.numeric(ds$value))
    ds <- ds[!is.na(ds$year), , drop = FALSE]
    ds <- ds[demo_align_lgl(ds$sex %in% trimws(f$sex), nrow(ds)), , drop = FALSE]
    yr1 <- as.numeric(f$years[[1]])
    yr2 <- as.numeric(f$years[[2]])
    ds <- ds[demo_align_lgl(!is.na(ds$year) & ds$year >= yr1 & ds$year <= yr2, nrow(ds)), , drop = FALSE]
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

    ds <- normalize_df_for_indexing(d$internet_use)
    ds$value <- suppressWarnings(as.numeric(ds$value))
    for (nm in c("age_group", "province", "residence", "sex")) {
      if (nm %in% names(ds)) ds[[nm]] <- trimws(as.character(ds[[nm]]))
    }
    res <- trimws(as.character(f$residence))
    ds <- ds[demo_align_lgl(ds$residence == res, nrow(ds)), , drop = FALSE]
    ds <- ds[demo_align_lgl(ds$sex %in% trimws(f$sex), nrow(ds)), , drop = FALSE]
    iag <- f$internet_age_group
    if (length(iag) == 1L) {
      ds <- ds[demo_align_lgl(ds$age_group == trimws(as.character(iag)), nrow(ds)), , drop = FALSE]
    } else {
      ds <- ds[demo_align_lgl(ds$age_group %in% trimws(as.character(iag)), nrow(ds)), , drop = FALSE]
    }
    if (!is.null(f$province) && length(f$province) == 1L && f$province != "all") {
      ds <- ds[demo_align_lgl(ds$province == trimws(as.character(f$province)), nrow(ds)), , drop = FALSE]
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
    ds <- normalize_df_for_indexing(ds)
    ds$sex <- as.character(ds$sex)
    n <- nrow(ds)
    ds <- ds[demo_align_lgl(ds$residence == f$residence, n), , drop = FALSE]
    ds <- ds[demo_align_lgl(ds$sex %in% f$sex, nrow(ds)), , drop = FALSE]
    if (!is.null(f$district) && f$district != "all") {
      ds <- ds[demo_align_lgl(ds$geo == f$district, nrow(ds)), , drop = FALSE]
    } else if (!is.null(f$province) && f$province != "all") {
      ds <- ds[demo_align_lgl(ds$geo_type == "District" & ds$province == f$province, nrow(ds)), , drop = FALSE]
    } else {
      nat <- ds[demo_align_lgl(ds$geo == "Rwanda" & ds$geo_type == "National", nrow(ds)), , drop = FALSE]
      prov <- ds[demo_align_lgl(ds$geo_type == "Province", nrow(ds)), , drop = FALSE]
      ds <- dplyr::bind_rows(nat, prov)
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

  # ════════════════════════════════════════════════════════════════════════════
  # Employment Dashboard (Rwanda LFS 2025 Q4)
  # ════════════════════════════════════════════════════════════════════════════

  EMP_MALE    <- "#3B82F6"   # blue
  EMP_FEMALE  <- "#E85A4F"   # brand red
  EMP_YOUTH   <- "#8B5CF6"   # purple
  EMP_ADULT   <- "#F59E0B"   # amber
  EMP_FONT    <- "Inter, system-ui, -apple-system, sans-serif"

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  emp_plotly_theme <- function(p, xlab = "", ylab = "") {
    p |>
      plotly::layout(
        font          = list(family = EMP_FONT, size = 12),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        xaxis = list(
          title     = list(text = xlab, font = list(size = 11)),
          gridcolor = "#F0F0F0", zerolinecolor = "#E5E5E5",
          tickfont  = list(size = 11)
        ),
        yaxis = list(
          title     = list(text = ylab, font = list(size = 11)),
          gridcolor = "#F0F0F0",
          tickfont  = list(size = 11)
        ),
        legend = list(orientation = "h", x = 0, y = -0.18, font = list(size = 11)),
        margin = list(t = 20, b = 10, l = 10, r = 10)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  emp_no_data <- function(msg = "Employment data not available") {
    plotly::plot_ly() |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = msg, showarrow = FALSE,
          font = list(size = 12, color = "#6B7280")
        )),
        margin = list(l = 40, r = 20, t = 40, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  # ── Load employment tables once ───────────────────────────────────────────
  emp_data_once <- NULL
  if (exists(".employment_data", envir = globalenv(), inherits = FALSE)) {
    emp_data_once <- tryCatch(
      get(".employment_data", envir = globalenv(), inherits = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(emp_data_once)) {
    emp_data_once <- tryCatch(load_employment_data(), error = function(e) {
      message("[EMP] load_employment_data() error: ", conditionMessage(e))
      NULL
    })
  }
  if (is.null(emp_data_once)) {
    message("[EMP] Employment data not available in server (init).")
  } else {
    message("[EMP] Employment data ready in server (init).")
  }

  emp_tbls <- shiny::reactive({
    if (!is.null(emp_data_once)) return(emp_data_once)
    tryCatch(load_employment_data(), error = function(e) NULL)
  })

  # ── Reactive: employment filters ──────────────────────────────────────────
  emp_filters <- shiny::reactive({
    list(
      yr      = input$emp_year %||% c(2019L, 2025L),
      quarter = input$emp_quarter %||% "all",
      sex     = input$emp_sex %||% c("Female", "Male")
    )
  })

  # Null-coalescing operator shortcut (defined above with EMP_MALE etc.)

  # ── Helper: average over year+quarter range ───────────────────────────────
  emp_period_avg <- function(df, val_col, yr_range, quarter) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df <- df[df$year >= yr_range[1] & df$year <= yr_range[2], , drop = FALSE]
    if (quarter != "all") df <- df[df$quarter == quarter, , drop = FALSE]
    if (nrow(df) == 0) return(df)
    df
  }

  # ── sample count display ──────────────────────────────────────────────────
  output$emp_sample_n <- shiny::renderUI({
    d <- emp_tbls()
    if (is.null(d) || is.null(d$lf_sex)) return(shiny::tags$span("No data"))
    f  <- emp_filters()
    ds <- d$lf_sex
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2], , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    n_qtr <- length(unique(paste(ds$year, ds$quarter)))
    shiny::tags$span(
      class = "fi-gbar__count-inner",
      shiny::tags$strong(n_qtr), " quarters shown \u00b7 LFS 2019\u2013Q4 2025"
    )
  })

  # ── KPI card builder for Employment overview ──────────────────────────────
  emp_kpi_card <- function(label, male_val, female_val, fmt = "%.1f%%",
                            icon = "fas fa-chart-bar", note = NULL) {
    if (is.null(male_val) || is.null(female_val)) return(NULL)
    if (is.na(male_val) || is.na(female_val)) return(NULL)
    gap  <- female_val - male_val
    gap_cls <- if (gap >= 0) "fi-kpi__delta--up" else "fi-kpi__delta--down"
    gap_lbl <- sprintf("%+.1f pp", gap)

    shiny::div(class = "fi-kpi fi-kpi--emp",
      shiny::div(class = "fi-kpi__icon",
        shiny::tags$i(class = paste(icon, "fa-lg"))
      ),
      shiny::div(class = "fi-kpi__body",
        shiny::div(class = "fi-kpi__label", label),
        shiny::div(class = "fi-kpi__values",
          shiny::tags$span(class = "fi-kpi__val fi-kpi__val--female",
            shiny::tags$i(class = "fas fa-venus fa-xs"),
            " ", sprintf(fmt, female_val)
          ),
          shiny::tags$span(class = "fi-kpi__sep", " \u00b7 "),
          shiny::tags$span(class = "fi-kpi__val fi-kpi__val--male",
            shiny::tags$i(class = "fas fa-mars fa-xs"),
            " ", sprintf(fmt, male_val)
          )
        ),
        shiny::div(class = paste("fi-kpi__delta", gap_cls), gap_lbl,
          shiny::tags$span(class = "fi-kpi__delta-lbl", " F\u2212M gap")
        ),
        if (!is.null(note)) shiny::div(class = "fi-kpi__note", note)
      )
    )
  }

  # ── Overview KPIs ─────────────────────────────────────────────────────────
  output$emp_overview_kpis <- shiny::renderUI({
    d <- emp_tbls()
    if (is.null(d) || is.null(d$lf_sex)) {
      return(shiny::div(class = "fi-kpi-row",
        shiny::div(class = "fi-alert", "Employment data not loaded.")))
    }
    f  <- emp_filters()
    ds <- d$lf_sex
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2], , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    ds <- ds[ds$sex %in% f$sex, , drop = FALSE]

    get_mean <- function(sex_v, ind) {
      x <- ds$value[ds$sex == sex_v & ds$indicator == ind]
      if (length(x) == 0) return(NA_real_)
      mean(x, na.rm = TRUE)
    }

    agri_ds <- if (!is.null(d$agri)) {
      a <- d$agri[d$agri$year >= f$yr[1] & d$agri$year <= f$yr[2], , drop = FALSE]
      if (f$quarter != "all") a <- a[a$quarter == f$quarter, , drop = FALSE]
      a
    } else NULL
    get_agri <- function(grp) {
      if (is.null(agri_ds)) return(NA_real_)
      x <- agri_ds$pct_agri[agri_ds$group == grp]
      if (length(x) == 0) return(NA_real_)
      mean(x, na.rm = TRUE)
    }

    cards <- list(
      emp_kpi_card("Labour Force Participation Rate",
        get_mean("Male",   "Labour force participation rate(%)"),
        get_mean("Female", "Labour force participation rate(%)"),
        icon = "fas fa-users"),
      emp_kpi_card("Employment-to-Population Ratio",
        get_mean("Male",   "Employment-to-population ratio(%)"),
        get_mean("Female", "Employment-to-population ratio(%)"),
        icon = "fas fa-hard-hat"),
      emp_kpi_card("Unemployment Rate (LU1)",
        get_mean("Male",   "LU1-Unemployment rate (%)"),
        get_mean("Female", "LU1-Unemployment rate (%)"),
        icon = "fas fa-user-times"),
      emp_kpi_card("Agriculture Share of Workforce",
        get_agri("Male"), get_agri("Female"),
        icon = "fas fa-seedling")
    )
    cards <- Filter(Negate(is.null), cards)
    if (length(cards) == 0) {
      return(shiny::div(class = "fi-kpi-row",
        shiny::div(class = "fi-alert", "No KPI data for selected filters.")))
    }
    shiny::div(class = "fi-kpi-row", cards)
  })

  # ── Shared line chart helper ──────────────────────────────────────────────
  emp_line_chart <- function(ds, x_col, y_col, color_col,
                              color_map, ylab = "", pct_suffix = TRUE,
                              title_text = NULL) {
    if (is.null(ds) || nrow(ds) == 0) return(emp_no_data())
    ds[[y_col]]     <- as.numeric(ds[[y_col]])
    ds[[x_col]]     <- as.character(ds[[x_col]])   # keep as label, NOT numeric
    ds[[color_col]] <- as.character(ds[[color_col]])
    grps <- unique(ds[[color_col]])

    p <- plotly::plot_ly()
    for (g in grps) {
      sub  <- ds[ds[[color_col]] == g, , drop = FALSE]
      sub  <- sub[order(sub[[x_col]]), , drop = FALSE]
      clr  <- color_map[[g]] %||% "#999999"
      ht   <- if (pct_suffix)
        paste0("<b>", g, "</b><br>", sub[[x_col]], " \u00b7 %{y:.1f}%<extra></extra>")
      else
        paste0("<b>", g, "</b><br>", sub[[x_col]], " \u00b7 %{y:,.0f}<extra></extra>")

      p <- plotly::add_trace(p,
        x    = sub[[x_col]], y = sub[[y_col]],
        type = "scatter", mode = "lines+markers",
        name = g,
        line   = list(color = clr, width = 2.5),
        marker = list(color = clr, size = 7),
        hovertemplate = ht
      )
    }
    tick_sfx <- if (pct_suffix) "%" else ""
    emp_plotly_theme(p, ylab = ylab) |>
      plotly::layout(
        title  = list(text = title_text, font = list(size = 12)),
        xaxis  = list(categoryorder = "array",
                      categoryarray = sort(unique(ds[[x_col]])),
                      tickangle = -45, tickfont = list(size = 10)),
        yaxis  = list(ticksuffix = tick_sfx)
      )
  }

  # ── Build x-axis as "YYYY Qn" string ─────────────────────────────────────
  emp_mk_time_label <- function(ds) {
    paste(ds$year, ds$quarter)
  }

  # ── Overview trend charts ─────────────────────────────────────────────────
  output$emp_lfpr_trend <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$lf_sex)) return(emp_no_data())
    ds <- d$lf_sex
    ds <- ds[ds$sex %in% f$sex &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$indicator == "Labour force participation rate(%)", , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No LFPR data for selected filters"))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "sex",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = "LFPR (%)")
  })

  output$emp_unemp_trend <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$lf_sex)) return(emp_no_data())
    ds <- d$lf_sex
    ds <- ds[ds$sex %in% f$sex &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$indicator == "LU1-Unemployment rate (%)", , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No unemployment data for selected filters"))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "sex",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = "Unemployment Rate (%)")
  })

  # ── Labour Force tab charts ───────────────────────────────────────────────
  emp_lf_chart <- function(indicator, ylab) {
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$lf_sex)) return(emp_no_data())
    ds <- d$lf_sex
    ds <- ds[ds$sex %in% f$sex &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$indicator == indicator, , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data(paste("No data for:", indicator)))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "sex",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = ylab)
  }

  output$emp_lf_lfpr     <- plotly::renderPlotly(emp_lf_chart("Labour force participation rate(%)", "LFPR (%)"))
  output$emp_lf_emppop   <- plotly::renderPlotly(emp_lf_chart("Employment-to-population ratio(%)", "Emp/Pop ratio (%)"))
  output$emp_lf_unemp    <- plotly::renderPlotly(emp_lf_chart("LU1-Unemployment rate (%)", "Unemployment rate (%)"))

  output$emp_lf_earnings <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$lf_sex)) return(emp_no_data())
    ds <- d$lf_sex
    ds <- ds[ds$sex %in% f$sex &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$indicator == "Median monthly earnings at main job", , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No earnings data for selected filters"))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "sex",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = "Median monthly earnings (Rwf)", pct_suffix = FALSE)
  })

  # ── Youth tab charts ──────────────────────────────────────────────────────
  emp_youth_chart <- function(indicator, ylab) {
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$lf_youth)) return(emp_no_data())
    ds <- d$lf_youth
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$indicator == indicator, , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data(paste("No youth data for:", indicator)))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "group",
      color_map = list("Youth (16-30)" = EMP_YOUTH, "Adult (31+)" = EMP_ADULT),
      ylab = ylab)
  }

  output$emp_youth_lfpr  <- plotly::renderPlotly(
    emp_youth_chart("Labour force participation rate(%)", "LFPR (%)"))
  output$emp_youth_unemp <- plotly::renderPlotly(
    emp_youth_chart("LU1-Unemployment rate (%)", "Unemployment rate (%)"))

  output$emp_youth_neet  <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$lf_sex)) return(emp_no_data())
    ds <- d$lf_sex
    neet_ind <- "NEET rate-Youth  not in employment nor currently in education or training(%)"
    ds <- ds[ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$indicator == neet_ind, , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (!is.null(f$sex) && length(f$sex) > 0)
      ds <- ds[ds$sex %in% f$sex, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No NEET data for selected filters"))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "sex",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = "NEET rate (%)")
  })

  # ── Occupations charts ────────────────────────────────────────────────────
  output$emp_occupations_bar <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$occupations)) return(emp_no_data())
    ds <- d$occupations
    ds <- ds[ds$sex %in% c("Male", "Female") &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             !(ds$occupation %in% c("_n_employed", "Total", "Employed population aged 16+")), , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No occupation data for selected filters"))

    # Average over the selected period
    ds_avg <- ds |>
      dplyr::group_by(occupation, sex) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    # Order by Female value descending
    occ_order <- ds_avg |>
      dplyr::filter(sex == "Female") |>
      dplyr::arrange(value) |>
      dplyr::pull(occupation)
    ds_avg$occupation <- factor(ds_avg$occupation, levels = occ_order)
    ds_avg$sex        <- factor(ds_avg$sex, levels = c("Female", "Male"))

    plotly::plot_ly(
      data = ds_avg[ds_avg$sex == "Male", , drop = FALSE],
      y = ~occupation, x = ~value,
      type = "bar", orientation = "h", name = "Male",
      marker = list(color = EMP_MALE),
      hovertemplate = "<b>%{y}</b><br>Male: %{x:.1f}%<extra></extra>"
    ) |>
      plotly::add_trace(
        data = ds_avg[ds_avg$sex == "Female", , drop = FALSE],
        y = ~occupation, x = ~value,
        type = "bar", orientation = "h", name = "Female",
        marker = list(color = EMP_FEMALE),
        hovertemplate = "<b>%{y}</b><br>Female: %{x:.1f}%<extra></extra>"
      ) |>
      emp_plotly_theme(xlab = "% of employed population") |>
      plotly::layout(
        barmode = "group",
        yaxis   = list(tickfont = list(size = 10)),
        margin  = list(l = 250, r = 20, t = 20, b = 40)
      )
  })

  output$emp_occupations_gap <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$occupations)) return(emp_no_data())
    ds <- d$occupations
    ds <- ds[ds$sex %in% c("Male", "Female") &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             !(ds$occupation %in% c("_n_employed", "Total", "Employed population aged 16+")), , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data())

    ds_avg <- ds |>
      dplyr::group_by(occupation, sex) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = sex, values_from = value)

    if (!("Female" %in% names(ds_avg)) || !("Male" %in% names(ds_avg))) return(emp_no_data())

    ds_avg$gap <- ds_avg$Female - ds_avg$Male
    ds_avg <- ds_avg[!is.na(ds_avg$gap), , drop = FALSE]
    ds_avg <- ds_avg[order(ds_avg$gap), , drop = FALSE]
    ds_avg$occupation <- factor(ds_avg$occupation, levels = ds_avg$occupation)
    ds_avg$bar_col    <- ifelse(ds_avg$gap >= 0, EMP_FEMALE, EMP_MALE)

    plotly::plot_ly(
      data = ds_avg, y = ~occupation, x = ~gap,
      type = "bar", orientation = "h",
      marker = list(color = ~bar_col),
      hovertemplate = "<b>%{y}</b><br>F\u2212M: %{x:+.1f} pp<extra></extra>"
    ) |>
      emp_plotly_theme(xlab = "Gender gap (Female % \u2212 Male %, pp)") |>
      plotly::layout(
        xaxis  = list(zeroline = TRUE, zerolinewidth = 1.5, zerolinecolor = "#9CA3AF"),
        yaxis  = list(tickfont = list(size = 10)),
        margin = list(l = 250, r = 20, t = 20, b = 40)
      )
  })

  # ── Employment Status charts ──────────────────────────────────────────────
  output$emp_status_bar <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$status)) return(emp_no_data())
    ds <- d$status
    ds <- ds[ds$sex %in% c("Male", "Female") &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             !(ds$status %in% c("_n_employed", "Total", "Employed population aged 16+")), , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No employment status data for selected filters"))

    ds_avg <- ds |>
      dplyr::group_by(status, sex) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    # Sort by Female descending
    st_order <- ds_avg |>
      dplyr::filter(sex == "Female") |>
      dplyr::arrange(value) |>
      dplyr::pull(status)
    ds_avg$status <- factor(ds_avg$status, levels = st_order)
    ds_avg$sex    <- factor(ds_avg$sex, levels = c("Female", "Male"))

    plotly::plot_ly(
      data = ds_avg[ds_avg$sex == "Male", ],
      y = ~status, x = ~value, type = "bar", orientation = "h",
      name = "Male", marker = list(color = EMP_MALE),
      hovertemplate = "<b>%{y}</b><br>Male: %{x:.1f}%<extra></extra>"
    ) |>
      plotly::add_trace(
        data = ds_avg[ds_avg$sex == "Female", ],
        y = ~status, x = ~value, type = "bar", orientation = "h",
        name = "Female", marker = list(color = EMP_FEMALE),
        hovertemplate = "<b>%{y}</b><br>Female: %{x:.1f}%<extra></extra>"
      ) |>
      emp_plotly_theme(xlab = "% of employed") |>
      plotly::layout(
        barmode = "group",
        yaxis   = list(tickfont = list(size = 10)),
        margin  = list(l = 220, r = 20, t = 20, b = 40)
      )
  })

  output$emp_status_wage_trend <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$status)) return(emp_no_data())
    ds <- d$status
    ds <- ds[ds$sex %in% f$sex &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$status == "Employee", , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No wage employment data"))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "sex",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = "% in wage employment (Employee)")
  })

  # ── Education charts ──────────────────────────────────────────────────────
  EDU_ORDER <- c("No schooling", "Primary", "Lower secondary",
                 "Upper secondary", "University/Higher")

  output$emp_edu_bar <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$education)) return(emp_no_data())
    ds <- d$education
    ds <- ds[ds$sex %in% c("Male", "Female") &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$education %in% EDU_ORDER, , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No education data for selected filters"))

    ds_avg <- ds |>
      dplyr::group_by(education, sex) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    ds_avg$education <- factor(ds_avg$education, levels = EDU_ORDER)

    plotly::plot_ly(
      data = ds_avg[ds_avg$sex == "Male", ],
      x = ~education, y = ~value, type = "bar", name = "Male",
      marker = list(color = EMP_MALE),
      hovertemplate = "<b>%{x}</b><br>Male: %{y:.1f}%<extra></extra>"
    ) |>
      plotly::add_trace(
        data = ds_avg[ds_avg$sex == "Female", ],
        x = ~education, y = ~value, type = "bar", name = "Female",
        marker = list(color = EMP_FEMALE),
        hovertemplate = "<b>%{x}</b><br>Female: %{y:.1f}%<extra></extra>"
      ) |>
      emp_plotly_theme(ylab = "% of employed") |>
      plotly::layout(
        barmode = "group",
        yaxis   = list(ticksuffix = "%"),
        margin  = list(l = 50, r = 20, t = 20, b = 80)
      )
  })

  output$emp_edu_uni_trend <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$education)) return(emp_no_data())
    ds <- d$education
    ds <- ds[ds$sex %in% f$sex &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             ds$education == "University/Higher", , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No university-level data"))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "value", "sex",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = "% employed with university education")
  })

  # ── Agriculture chart ─────────────────────────────────────────────────────
  output$emp_agri_trend <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$agri)) return(emp_no_data())
    ds <- d$agri
    ds <- ds[ds$group %in% c("Male", "Female") &
             ds$year >= f$yr[1] & ds$year <= f$yr[2], , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No agriculture data for selected filters"))
    ds$time_lbl <- emp_mk_time_label(ds)
    ds <- ds[order(ds$year, ds$quarter), , drop = FALSE]
    emp_line_chart(ds, "time_lbl", "pct_agri", "group",
      color_map = list(Male = EMP_MALE, Female = EMP_FEMALE),
      ylab = "Agriculture share of workforce (%)")
  })

  # ── Economic Sectors charts ───────────────────────────────────────────────
  output$emp_sectors_bar <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$economic)) return(emp_no_data())
    ds <- d$economic
    ds <- ds[ds$sex %in% c("Male", "Female") &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             !(ds$sector %in% c("_n_employed", "Total", "Employed population aged 16+")), , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data("No economic sector data for selected filters"))

    ds_avg <- ds |>
      dplyr::group_by(sector, sex) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    sect_order <- ds_avg |>
      dplyr::filter(sex == "Female") |>
      dplyr::arrange(value) |>
      dplyr::pull(sector)
    ds_avg$sector <- factor(ds_avg$sector, levels = sect_order)

    plotly::plot_ly(
      data = ds_avg[ds_avg$sex == "Male", ],
      y = ~sector, x = ~value, type = "bar", orientation = "h",
      name = "Male", marker = list(color = EMP_MALE),
      hovertemplate = "<b>%{y}</b><br>Male: %{x:.1f}%<extra></extra>"
    ) |>
      plotly::add_trace(
        data = ds_avg[ds_avg$sex == "Female", ],
        y = ~sector, x = ~value, type = "bar", orientation = "h",
        name = "Female", marker = list(color = EMP_FEMALE),
        hovertemplate = "<b>%{y}</b><br>Female: %{x:.1f}%<extra></extra>"
      ) |>
      emp_plotly_theme(xlab = "% of employed") |>
      plotly::layout(
        barmode = "group",
        yaxis   = list(tickfont = list(size = 10)),
        margin  = list(l = 190, r = 20, t = 20, b = 40)
      )
  })

  output$emp_sectors_gap <- plotly::renderPlotly({
    d <- emp_tbls(); f <- emp_filters()
    if (is.null(d) || is.null(d$economic)) return(emp_no_data())
    ds <- d$economic
    ds <- ds[ds$sex %in% c("Male", "Female") &
             ds$year >= f$yr[1] & ds$year <= f$yr[2] &
             !(ds$sector %in% c("_n_employed", "Total", "Employed population aged 16+")), , drop = FALSE]
    if (f$quarter != "all") ds <- ds[ds$quarter == f$quarter, , drop = FALSE]
    if (nrow(ds) == 0) return(emp_no_data())

    ds_avg <- ds |>
      dplyr::group_by(sector, sex) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = sex, values_from = value)

    if (!("Female" %in% names(ds_avg)) || !("Male" %in% names(ds_avg))) return(emp_no_data())

    ds_avg$gap <- ds_avg$Female - ds_avg$Male
    ds_avg <- ds_avg[!is.na(ds_avg$gap), , drop = FALSE]
    ds_avg <- ds_avg[order(ds_avg$gap), , drop = FALSE]
    ds_avg$sector  <- factor(ds_avg$sector, levels = ds_avg$sector)
    ds_avg$bar_col <- ifelse(ds_avg$gap >= 0, EMP_FEMALE, EMP_MALE)

    plotly::plot_ly(
      data = ds_avg, y = ~sector, x = ~gap,
      type = "bar", orientation = "h",
      marker = list(color = ~bar_col),
      hovertemplate = "<b>%{y}</b><br>F\u2212M: %{x:+.1f} pp<extra></extra>"
    ) |>
      emp_plotly_theme(xlab = "Gender gap (Female % \u2212 Male %, pp)") |>
      plotly::layout(
        xaxis  = list(zeroline = TRUE, zerolinewidth = 1.5, zerolinecolor = "#9CA3AF"),
        yaxis  = list(tickfont = list(size = 10)),
        margin = list(l = 190, r = 20, t = 20, b = 40)
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # EDUCATION DASHBOARD
  # ═══════════════════════════════════════════════════════════════════════════

  # ── Education color guide (aligned with Demography & Employment) ─────────────
  EDU_MALE    <- "#3B82F6"   # blue  — same as FI_MALE / EMP_MALE
  EDU_FEMALE  <- "#E85A4F"   # red   — same as FI_FEMALE / EMP_FEMALE
  EDU_BOTH    <- "#8B5CF6"   # purple — combined / both sexes
  EDU_NOEDUC  <- "#F59E0B"   # amber  — no education
  EDU_UNIV    <- "#2d6e44"   # green  — university / higher (CLR success)

  EDU_LEVEL_ORDER <- c(
    "No Education","No Primary Schooling","Some Primary","Primary","Primary Completed",
    "Vocational/INGOBOKA","Lower Secondary","Upper Secondary",
    "Short Cycle Tertiary","Bachelor","Masters","Doctoral","University/Higher"
  )

  edu_plotly_theme <- function(p, xlab = "", ylab = "", title = "") {
    p |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor  = "rgba(0,0,0,0)",
        font    = list(family = "Inter, sans-serif", size = 11, color = "#374151"),
        xaxis   = list(title = xlab, gridcolor = "#F3F4F6", zerolinecolor = "#E5E7EB"),
        yaxis   = list(title = ylab, gridcolor = "#F3F4F6"),
        legend  = list(orientation = "h", x = 0.01, y = -0.15, bgcolor = "rgba(0,0,0,0)"),
        margin  = list(l = 10, r = 10, t = 30, b = 10),
        hoverlabel = list(bgcolor = "#1E293B", font = list(color = "#F8FAFC", size = 12))
      )
  }

  edu_no_data <- function(msg = "Education data not available.") {
    plotly::plotly_empty() |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        annotations  = list(list(
          text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 13, color = "#9CA3AF")
        ))
      )
  }

  # ── load education tables once (same safe pattern as Employment) ────────────
  edu_data_once <- NULL
  if (exists(".education_data", envir = globalenv(), inherits = FALSE)) {
    edu_data_once <- tryCatch(
      get(".education_data", envir = globalenv(), inherits = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(edu_data_once)) {
    edu_data_once <- tryCatch(load_education_data(), error = function(e) {
      message("[EDU] load_education_data() error: ", conditionMessage(e))
      NULL
    })
  }
  if (is.null(edu_data_once)) {
    message("[EDU] Education data not available in server (init).")
  } else {
    message("[EDU] Education data ready in server: ",
            length(edu_data_once), " tables loaded.")
  }

  edu_tbls <- reactive({ edu_data_once })

  # ── KPI card helper ─────────────────────────────────────────────────────────
  edu_kpi_card <- function(title, icon_cls, male_val, female_val, suffix = "%", note = NULL) {
    gap   <- if (!is.na(male_val) && !is.na(female_val)) female_val - male_val else NA
    gap_lbl <- if (!is.na(gap)) sprintf("%+.1f pp", gap) else "N/A"
    gap_cls <- if (!is.na(gap) && gap >= 0) "fi-kpi__gap--pos" else "fi-kpi__gap--neg"
    shiny::div(class = "fi-kpi fi-kpi--edu",
      shiny::div(class = "fi-kpi__icon", shiny::tags$i(class = icon_cls)),
      shiny::div(class = "fi-kpi__body",
        shiny::div(class = "fi-kpi__title", title),
        shiny::div(class = "fi-kpi__vals",
          shiny::span(class = "fi-kpi__val fi-kpi__val--male",
            shiny::tags$i(class = "fas fa-mars fa-xs"),
            sprintf(" %.1f%s", male_val, suffix)
          ),
          shiny::span(class = "fi-kpi__val fi-kpi__val--female",
            shiny::tags$i(class = "fas fa-venus fa-xs"),
            sprintf(" %.1f%s", female_val, suffix)
          )
        ),
        shiny::div(class = paste("fi-kpi__gap", gap_cls), "Gap (F\u2212M): ", gap_lbl),
        if (!is.null(note)) shiny::div(class = "fi-kpi__note", note)
      )
    )
  }

  output$edu_overview_kpis <- renderUI({
    d <- edu_tbls()
    if (is.null(d)) return(shiny::div(class = "fi-kpi-row"))

    # Literacy rate national
    lit <- if (!is.null(d$literacy_sex))
      d$literacy_sex[d$literacy_sex$area == "Rwanda", ] else NULL
    lit_m <- if (!is.null(lit)) lit$pct_literate[lit$sex == "Male"]   else NA
    lit_f <- if (!is.null(lit)) lit$pct_literate[lit$sex == "Female"] else NA
    lit_m <- if (length(lit_m) == 1) lit_m else NA
    lit_f <- if (length(lit_f) == 1) lit_f else NA

    # School attendance (age 6-11)
    att <- if (!is.null(d$attendance_agegroup))
      d$attendance_agegroup[d$attendance_agegroup$age_group == "6-11 years", ] else NULL
    att_m <- if (!is.null(att)) att$pct[att$sex == "Male"]   else NA
    att_f <- if (!is.null(att)) att$pct[att$sex == "Female"] else NA
    att_m <- if (length(att_m) == 1) att_m else NA
    att_f <- if (length(att_f) == 1) att_f else NA

    # University/Higher (Rwanda)
    uni <- if (!is.null(d$attainment_area))
      d$attainment_area[d$attainment_area$area == "Rwanda" &
                        d$attainment_area$level == "University/Higher", ] else NULL
    uni_m <- if (!is.null(uni)) uni$pct[uni$sex == "Male"]   else NA
    uni_f <- if (!is.null(uni)) uni$pct[uni$sex == "Female"] else NA
    uni_m <- if (length(uni_m) == 1) uni_m else NA
    uni_f <- if (length(uni_f) == 1) uni_f else NA

    # No Education (Rwanda)
    noed <- if (!is.null(d$attainment_area))
      d$attainment_area[d$attainment_area$area == "Rwanda" &
                        d$attainment_area$level == "No Education", ] else NULL
    noed_m <- if (!is.null(noed)) noed$pct[noed$sex == "Male"]   else NA
    noed_f <- if (!is.null(noed)) noed$pct[noed$sex == "Female"] else NA
    noed_m <- if (length(noed_m) == 1) noed_m else NA
    noed_f <- if (length(noed_f) == 1) noed_f else NA

    shiny::div(class = "fi-kpi-row",
      if (!any(is.na(c(lit_m, lit_f))))
        edu_kpi_card("Literacy Rate (National)", "fas fa-book fa-lg", lit_m, lit_f),
      if (!any(is.na(c(att_m, att_f))))
        edu_kpi_card("School Attendance (6-11 yrs)", "fas fa-school fa-lg", att_m, att_f),
      if (!any(is.na(c(uni_m, uni_f))))
        edu_kpi_card("University/Higher (%)", "fas fa-graduation-cap fa-lg", uni_m, uni_f),
      if (!any(is.na(c(noed_m, noed_f))))
        edu_kpi_card("Never Attended School (%)", "fas fa-times-circle fa-lg", noed_m, noed_f)
    )
  })

  # ── helpers ──────────────────────────────────────────────────────────────────
  edu_ordered_levels <- function(ds, col = "level") {
    lvls <- intersect(EDU_LEVEL_ORDER, unique(ds[[col]]))
    extra <- setdiff(unique(ds[[col]]), lvls)
    c(lvls, extra)
  }

  # ── OVERVIEW charts ──────────────────────────────────────────────────────────
  output$edu_literacy_overview <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$literacy_sex)) return(edu_no_data())
    ds <- d$literacy_sex[d$literacy_sex$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    ds$area <- factor(ds$area, levels = c("Rwanda","Urban","Rural"))
    plotly::plot_ly(ds, x = ~area, y = ~pct_literate, color = ~sex, type = "bar",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>Literacy: %{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "Literacy rate (%)") |>
      plotly::layout(barmode = "group", yaxis = list(range = c(0,100)))
  })

  output$edu_attendance_overview <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attendance_agegroup)) return(edu_no_data())
    ds <- d$attendance_agegroup[d$attendance_agegroup$sex %in% c("Male","Female") &
                                d$attendance_agegroup$age_group != "3-17 years (total)", ]
    if (nrow(ds) == 0) return(edu_no_data())
    plotly::plot_ly(ds, x = ~age_group, y = ~pct, color = ~sex, type = "bar",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>Attendance: %{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "Attendance rate (%)") |>
      plotly::layout(barmode = "group")
  })

  output$edu_attainment_overview <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attainment_area)) return(edu_no_data())
    ds <- d$attainment_area[d$attainment_area$area == "Rwanda" &
                            d$attainment_area$sex %in% c("Male","Female") &
                            !d$attainment_area$level %in% c("Not stated","Pre-Nursery/ECD","Pre-primary"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    lvls <- edu_ordered_levels(ds)
    ds$level <- factor(ds$level, levels = rev(lvls))
    plotly::plot_ly(ds, y = ~level, x = ~pct, color = ~sex, type = "bar", orientation = "h",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{y}</b> \u00b7 %{fullData.name}<br>%{x:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(xlab = "% of population") |>
      plotly::layout(barmode = "group",
        margin = list(l = 160, r = 20, t = 20, b = 40))
  })

  # ── ATTAINMENT tab charts ────────────────────────────────────────────────────
  output$edu_attainment_bar <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attainment_area)) return(edu_no_data())
    area_sel <- input$edu_area %||% "Rwanda"
    ds <- d$attainment_area[d$attainment_area$area == area_sel &
                            d$attainment_area$sex %in% c("Male","Female") &
                            !d$attainment_area$level %in% c("Not stated","Pre-Nursery/ECD","Pre-primary"), ]
    if (nrow(ds) == 0) return(edu_no_data("No data for selected area"))
    lvls <- edu_ordered_levels(ds)
    ds$level <- factor(ds$level, levels = rev(lvls))
    plotly::plot_ly(ds, y = ~level, x = ~pct, color = ~sex, type = "bar", orientation = "h",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{y}</b> \u00b7 %{fullData.name}<br>%{x:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(xlab = "% of population") |>
      plotly::layout(barmode = "group",
        margin = list(l = 170, r = 20, t = 20, b = 40))
  })

  edu_donut_chart <- function(d, sex_sel, title_text) {
    if (is.null(d) || is.null(d$attainment_area)) return(edu_no_data())
    area_sel <- isolate(input$edu_area %||% "Rwanda")
    ds <- d$attainment_area[d$attainment_area$area == area_sel &
                            d$attainment_area$sex == sex_sel &
                            !d$attainment_area$level %in% c("Not stated"), ]
    ds <- ds[!is.na(ds$pct) & ds$pct > 0, ]
    if (nrow(ds) == 0) return(edu_no_data())
    lvls <- edu_ordered_levels(ds)
    ds$level <- factor(ds$level, levels = lvls)
    ds <- ds[order(ds$level), ]
    pal <- c("#EF4444","#F97316","#EAB308","#22C55E","#06B6D4","#3B82F6","#8B5CF6","#EC4899","#6B7280")
    plotly::plot_ly(ds, labels = ~level, values = ~pct, type = "pie",
      hole = 0.52,
      marker = list(colors = pal[seq_len(nrow(ds))],
                    line = list(color = "#FFFFFF", width = 1.5)),
      textinfo = "label+percent",
      hovertemplate = "<b>%{label}</b><br>%{value:.1f}%<extra></extra>"
    ) |> edu_plotly_theme() |>
      plotly::layout(
        title  = list(text = title_text, font = list(size = 12)),
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 40, b = 10)
      )
  }

  output$edu_attainment_donut_m <- plotly::renderPlotly({
    edu_donut_chart(edu_tbls(), "Male", "Male education distribution")
  })
  output$edu_attainment_donut_f <- plotly::renderPlotly({
    edu_donut_chart(edu_tbls(), "Female", "Female education distribution")
  })

  output$edu_attainment_gap <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attainment_area)) return(edu_no_data())
    area_sel <- input$edu_area %||% "Rwanda"
    ds <- d$attainment_area[d$attainment_area$area == area_sel &
                            d$attainment_area$sex %in% c("Male","Female") &
                            !d$attainment_area$level %in% c("Not stated","Pre-Nursery/ECD","Pre-primary"), ]
    ds_wide <- tidyr::pivot_wider(ds, names_from = sex, values_from = pct, id_cols = level)
    if (!all(c("Male","Female") %in% names(ds_wide))) return(edu_no_data())
    ds_wide$gap     <- ds_wide$Female - ds_wide$Male
    ds_wide         <- ds_wide[!is.na(ds_wide$gap), ]
    lvls            <- edu_ordered_levels(ds_wide)
    ds_wide$level   <- factor(ds_wide$level, levels = lvls)
    ds_wide         <- ds_wide[order(ds_wide$level), ]
    ds_wide$bar_col <- ifelse(ds_wide$gap >= 0, EDU_FEMALE, EDU_MALE)
    plotly::plot_ly(ds_wide, y = ~level, x = ~gap, type = "bar", orientation = "h",
      marker = list(color = ~bar_col),
      hovertemplate = "<b>%{y}</b><br>F\u2212M: %{x:+.1f} pp<extra></extra>"
    ) |> edu_plotly_theme(xlab = "Gender gap (Female % \u2212 Male %, pp)") |>
      plotly::layout(
        xaxis  = list(zeroline = TRUE, zerolinewidth = 1.5, zerolinecolor = "#9CA3AF"),
        margin = list(l = 170, r = 20, t = 20, b = 40)
      )
  })

  # ── TREND tab charts ─────────────────────────────────────────────────────────
  output$edu_trend_line <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attainment_trend)) return(edu_no_data())
    lv_sel <- input$edu_trend_level %||% "No Education"
    ds <- d$attainment_trend[d$attainment_trend$level == lv_sel &
                             d$attainment_trend$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data("No trend data for selected level"))
    ds$year <- as.integer(ds$year)
    plotly::plot_ly(ds, x = ~year, y = ~pct, color = ~sex, type = "scatter", mode = "lines+markers",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      line = list(width = 2.5),
      marker = list(size = 8),
      hovertemplate = "<b>%{fullData.name}</b> %{x}<br>%{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = paste0(lv_sel, " (%)")) |>
      plotly::layout(
        xaxis = list(tickvals = c(1978,1991,2002,2012,2022),
                     ticktext = c("1978","1991","2002","2012","2022"))
      )
  })

  output$edu_trend_heatmap <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attainment_trend)) return(edu_no_data())
    ds <- d$attainment_trend[d$attainment_trend$sex == "Both sexes" &
                             d$attainment_trend$level != "No Education", ]
    if (nrow(ds) == 0) {
      ds <- d$attainment_trend[d$attainment_trend$sex %in% c("Male","Female"), ]
    }
    if (nrow(ds) == 0) return(edu_no_data())
    lvls <- c("University","Secondary","Post-Primary","Primary","No Education")
    lvls_present <- intersect(lvls, unique(ds$level))
    pal  <- c("#10B981","#3B82F6","#F59E0B","#6B7280","#EF4444")
    names(pal) <- lvls
    p <- plotly::plot_ly()
    for (lv in lvls_present) {
      sub <- ds[ds$level == lv, ]
      sub <- sub[order(sub$year), ]
      p <- plotly::add_trace(p, x = ~year, y = ~pct, data = sub,
        type = "scatter", mode = "lines+markers",
        fill = "tonexty", fillcolor = paste0(pal[[lv]], "40"),
        line = list(color = pal[[lv]], width = 2),
        marker = list(color = pal[[lv]], size = 6),
        name = lv,
        hovertemplate = paste0("<b>", lv, "</b> %{x}<br>%{y:.1f}%<extra></extra>")
      )
    }
    p |> edu_plotly_theme(ylab = "% of population") |>
      plotly::layout(
        xaxis  = list(tickvals = c(1978,1991,2002,2012,2022)),
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
  })

  # ── ATTENDANCE tab charts ────────────────────────────────────────────────────
  output$edu_attendance_agegroup <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attendance_agegroup)) return(edu_no_data())
    ds <- d$attendance_agegroup[d$attendance_agegroup$sex %in% c("Male","Female"), ]
    ds <- ds[ds$age_group != "3-17 years (total)", ]
    if (nrow(ds) == 0) return(edu_no_data())
    plotly::plot_ly(ds, x = ~age_group, y = ~pct, color = ~sex, type = "bar",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>%{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "Attendance rate (%)") |>
      plotly::layout(barmode = "group", yaxis = list(range = c(0, 100)))
  })

  output$edu_attendance_preprimary <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attendance_3to5)) return(edu_no_data())
    ds <- d$attendance_3to5[d$attendance_3to5$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    ds$area <- factor(ds$area, levels = c("Rwanda","Urban","Rural"))
    plotly::plot_ly(ds, x = ~area, y = ~pct_attending, color = ~sex, type = "bar",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>%{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "Attendance rate (%)") |>
      plotly::layout(barmode = "group")
  })

  output$edu_attendance_6to17 <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attendance_6to17)) return(edu_no_data())
    ds <- d$attendance_6to17[d$attendance_6to17$sex %in% c("Male","Female") &
                             d$attendance_6to17$area == "Rwanda", ]
    if (nrow(ds) == 0) return(edu_no_data())
    ds_long <- tidyr::pivot_longer(ds, cols = c("pct_currently","pct_previously","pct_never"),
                                   names_to = "status", values_to = "pct")
    ds_long$status <- dplyr::recode(ds_long$status,
      "pct_currently" = "Currently attending",
      "pct_previously" = "Previously attended",
      "pct_never" = "Never attended"
    )
    pal <- c("Currently attending" = "#10B981",
             "Previously attended" = "#F59E0B",
             "Never attended"      = "#EF4444")
    plotly::plot_ly(ds_long, x = ~sex, y = ~pct, color = ~status, type = "bar",
      colors = pal,
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>%{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "% of 6-17 yr population") |>
      plotly::layout(barmode = "stack")
  })

  output$edu_attendance_status <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attendance_status)) return(edu_no_data())
    ds <- d$attendance_status[d$attendance_status$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    ds$area <- factor(ds$area, levels = c("Rwanda","Urban","Rural"))
    # Stacked: currently + no_longer + never
    ds_long <- tidyr::pivot_longer(ds, cols = c("pct_currently","pct_no_longer","pct_never"),
                                   names_to = "status", values_to = "pct")
    ds_long$status <- dplyr::recode(ds_long$status,
      "pct_currently" = "Currently attending",
      "pct_no_longer" = "No longer attending",
      "pct_never"     = "Never attended"
    )
    ds_long$xlab <- paste(ds_long$area, ds_long$sex, sep = " \u00b7 ")
    pal <- c("Currently attending" = "#10B981",
             "No longer attending" = "#F59E0B",
             "Never attended"      = "#EF4444")
    plotly::plot_ly(ds_long, x = ~xlab, y = ~pct, color = ~status, type = "bar",
      colors = pal,
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>%{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "% of 3-17 yr population") |>
      plotly::layout(barmode = "stack",
        xaxis = list(tickangle = -30, tickfont = list(size = 9)))
  })

  # ── LITERACY tab charts ──────────────────────────────────────────────────────
  output$edu_literacy_area <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$literacy_sex)) return(edu_no_data())
    ds <- d$literacy_sex[d$literacy_sex$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    ds$area <- factor(ds$area, levels = c("Rwanda","Urban","Rural"))
    plotly::plot_ly(ds, x = ~area, y = ~pct_literate, color = ~sex, type = "bar",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>Literacy: %{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "Literacy rate (%)") |>
      plotly::layout(barmode = "group", yaxis = list(range = c(0,100)))
  })

  output$edu_literacy_gap_area <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$literacy_sex)) return(edu_no_data())
    ds <- d$literacy_sex[d$literacy_sex$sex %in% c("Male","Female"), ]
    ds_wide <- tidyr::pivot_wider(ds, names_from = sex, values_from = pct_literate, id_cols = area)
    if (!all(c("Male","Female") %in% names(ds_wide))) return(edu_no_data())
    ds_wide$gap <- ds_wide$Female - ds_wide$Male
    ds_wide$area <- factor(ds_wide$area, levels = c("Rwanda","Urban","Rural"))
    ds_wide$bar_col <- ifelse(ds_wide$gap >= 0, EDU_FEMALE, EDU_MALE)
    plotly::plot_ly(ds_wide, x = ~area, y = ~gap, type = "bar",
      marker = list(color = ~bar_col),
      hovertemplate = "<b>%{x}</b><br>F\u2212M gap: %{y:+.1f} pp<extra></extra>"
    ) |> edu_plotly_theme(ylab = "Literacy gap (F \u2212 M pp)") |>
      plotly::layout(
        shapes = list(list(type="line", x0=0, x1=1, xref="paper", y0=0, y1=0,
                          line = list(color="#9CA3AF", width=1, dash="dot")))
      )
  })

  output$edu_literacy_age <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$literacy_age)) return(edu_no_data())
    ds <- d$literacy_age[d$literacy_age$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    # Order age groups sensibly
    age_ord <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                 "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
    ds$age_group <- factor(ds$age_group, levels = age_ord)
    dm <- ds[ds$sex == "Male",   ]
    df <- ds[ds$sex == "Female", ]
    plotly::plot_ly(type = "scatter", mode = "lines+markers") |>
      plotly::add_trace(data = dm, x = ~age_group, y = ~pct_literate,
        name = "Male", fill = "tozeroy",
        fillcolor = paste0(EDU_MALE, "25"), line = list(color = EDU_MALE, width = 2.5),
        marker = list(color = EDU_MALE, size = 7),
        hovertemplate = "<b>%{x}</b> \u00b7 Male<br>%{y:.1f}%<extra></extra>"
      ) |>
      plotly::add_trace(data = df, x = ~age_group, y = ~pct_literate,
        name = "Female", fill = "tozeroy",
        fillcolor = paste0(EDU_FEMALE, "25"), line = list(color = EDU_FEMALE, width = 2.5),
        marker = list(color = EDU_FEMALE, size = 7),
        hovertemplate = "<b>%{x}</b> \u00b7 Female<br>%{y:.1f}%<extra></extra>"
      ) |>
      edu_plotly_theme(ylab = "Literacy rate (%)") |>
      plotly::layout(
        xaxis = list(tickangle = -45, tickfont = list(size = 9)),
        yaxis = list(range = c(0, 100))
      )
  })

  # ── DIGITAL ACCESS tab charts ────────────────────────────────────────────────
  output$edu_ict_province <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$ict_province)) return(edu_no_data())
    age_sel <- input$edu_ict_age %||% "10+"
    ds <- d$ict_province[d$ict_province$age_group == age_sel &
                         d$ict_province$sex %in% c("Male","Female") &
                         d$ict_province$province != "Rwanda", ]
    if (nrow(ds) == 0) ds <- d$ict_province[d$ict_province$age_group == age_sel &
                                             d$ict_province$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    prov_ord <- c("City of Kigali","Southern Province","Western Province",
                  "Northern Province","Eastern Province","Rwanda")
    ds$province <- factor(ds$province, levels = intersect(prov_ord, unique(ds$province)))
    plotly::plot_ly(ds, x = ~pct_nat, y = ~province, color = ~sex, type = "bar",
      orientation = "h",
      colors = c(Male = EDU_MALE, Female = EDU_FEMALE),
      hovertemplate = "<b>%{y}</b> \u00b7 %{fullData.name}<br>ICT: %{x:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(xlab = "ICT literate (%)") |>
      plotly::layout(barmode = "group",
        margin = list(l = 160, r = 20, t = 20, b = 40))
  })

  output$edu_mobile_sex <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$mobile_phone)) return(edu_no_data())
    ds <- d$mobile_phone[d$mobile_phone$area == "Rwanda" &
                         d$mobile_phone$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    # total per sex for pct
    totals <- tapply(ds$count, ds$sex, sum, na.rm = TRUE)
    ds$pct  <- ds$count / totals[ds$sex] * 100
    pal <- c("Smartphone" = "#10B981",
             "Basic Phone (with radio)" = "#3B82F6",
             "Basic Phone (no radio)"   = "#F59E0B")
    plotly::plot_ly(ds, x = ~sex, y = ~pct, color = ~phone_type, type = "bar",
      colors = pal,
      hovertemplate = "<b>%{x}</b> \u00b7 %{fullData.name}<br>%{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "% of mobile owners") |>
      plotly::layout(barmode = "stack")
  })

  output$edu_ict_urban_rural <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$ict_province)) return(edu_no_data())
    age_sel <- input$edu_ict_age %||% "10+"
    ds <- d$ict_province[d$ict_province$age_group == age_sel &
                         d$ict_province$sex %in% c("Male","Female") &
                         d$ict_province$province == "Rwanda", ]
    if (nrow(ds) == 0) return(edu_no_data())
    ds_long <- tidyr::pivot_longer(ds, cols = c("pct_nat","pct_urb","pct_rur"),
                                   names_to = "area_type", values_to = "pct")
    ds_long$area_type <- dplyr::recode(ds_long$area_type,
      "pct_nat" = "National", "pct_urb" = "Urban", "pct_rur" = "Rural")
    ds_long$xlab <- paste(ds_long$area_type, ds_long$sex, sep = " \u00b7 ")
    ds_long$bar_col <- ifelse(ds_long$sex == "Male", EDU_MALE, EDU_FEMALE)
    plotly::plot_ly(ds_long, x = ~xlab, y = ~pct, type = "bar",
      marker = list(color = ~bar_col),
      hovertemplate = "<b>%{x}</b><br>ICT: %{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "ICT literate (%)") |>
      plotly::layout(xaxis = list(tickangle = -30, tickfont = list(size = 9)))
  })

  # ── DISABILITY tab charts ────────────────────────────────────────────────────
  output$edu_disability_bar <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attainment_disability)) return(edu_no_data())
    ds <- d$attainment_disability[
      d$attainment_disability$sex %in% c("Male","Female") &
      d$attainment_disability$disability %in% c("With disability","Without disability") &
      !d$attainment_disability$level %in% c("Not stated","Pre-Nursery/ECD","Pre-primary"), ]
    if (nrow(ds) == 0) return(edu_no_data())
    ds$grp <- paste(ds$sex, "-", ds$disability)
    lvls <- edu_ordered_levels(ds)
    ds$level <- factor(ds$level, levels = rev(lvls))
    grp_pal <- c(
      "Male - With disability"      = "#93C5FD",
      "Male - Without disability"   = EDU_MALE,
      "Female - With disability"    = "#FBCFE8",
      "Female - Without disability" = EDU_FEMALE
    )
    plotly::plot_ly(ds, y = ~level, x = ~pct, color = ~grp, type = "bar",
      orientation = "h",
      colors = grp_pal,
      hovertemplate = "<b>%{y}</b> \u00b7 %{fullData.name}<br>%{x:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(xlab = "% of group") |>
      plotly::layout(barmode = "group",
        margin = list(l = 170, r = 20, t = 20, b = 40))
  })

  output$edu_disability_noedu <- plotly::renderPlotly({
    d <- edu_tbls()
    if (is.null(d) || is.null(d$attainment_disability)) return(edu_no_data())
    ds <- d$attainment_disability[
      d$attainment_disability$level == "No Education" &
      d$attainment_disability$sex %in% c("Male","Female"), ]
    if (nrow(ds) == 0) return(edu_no_data("No 'No Education' data for disability analysis"))
    ds$xlab     <- paste(ds$sex, "-", ds$disability)
    ds$bar_col  <- ifelse(ds$sex == "Male", EDU_MALE, EDU_FEMALE)
    ds$alpha    <- ifelse(ds$disability == "With disability", 1, 0.55)
    plotly::plot_ly(ds, x = ~xlab, y = ~pct, type = "bar",
      marker = list(color = ~bar_col, opacity = ~alpha),
      hovertemplate = "<b>%{x}</b><br>Never attended: %{y:.1f}%<extra></extra>"
    ) |> edu_plotly_theme(ylab = "Never attended school (%)") |>
      plotly::layout(xaxis = list(tickangle = -20, tickfont = list(size = 9)))
  })

}
