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
  shiny::observeEvent(input$qt_dhs, {
    shiny::updateTextInput(session, "srch_text", value = "DHS")
    committed_query("DHS")
  })
  shiny::observeEvent(input$qt_maternal, {
    shiny::updateTextInput(session, "srch_text", value = "maternal health")
    committed_query("maternal health")
  })
  shiny::observeEvent(input$qt_eicv, {
    shiny::updateTextInput(session, "srch_text", value = "EICV household")
    committed_query("EICV household")
  })
  shiny::observeEvent(input$qt_labor, {
    shiny::updateTextInput(session, "srch_text", value = "labor force")
    committed_query("labor force")
  })
  shiny::observeEvent(input$qt_census, {
    shiny::updateTextInput(session, "srch_text", value = "population census")
    committed_query("population census")
  })
  shiny::observeEvent(input$qt_gender, {
    shiny::updateTextInput(session, "srch_text", value = "gender violence women")
    committed_query("gender violence women")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # HOME — static content only; no server outputs needed for PMV section
  # ══════════════════════════════════════════════════════════════════════════

  # ══════════════════════════════════════════════════════════════════════════
  # DATA DISCOVERY
  # ══════════════════════════════════════════════════════════════════════════

  filtered_catalog <- shiny::reactive({
    df <- catalog

    q <- committed_query()
    if (nchar(q) > 0) {
      ql <- tolower(q)
      df <- df |> dplyr::filter(
        stringr::str_detect(tolower(title), ql) |
        stringr::str_detect(tolower(tidyr::replace_na(abstract,    "")), ql) |
        stringr::str_detect(tolower(tidyr::replace_na(scope_notes, "")), ql)
      )
    }

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

    df <- switch(input$sort_by,
      "year_desc"   = df |> dplyr::arrange(dplyr::desc(year)),
      "year_asc"    = df |> dplyr::arrange(year),
      "gender_desc" = df |> dplyr::arrange(dplyr::desc(gender_score)),
      "views_desc"  = df |> dplyr::arrange(dplyr::desc(views_num)),
      "title_asc"   = df |> dplyr::arrange(title),
      df |> dplyr::arrange(dplyr::desc(year))
    )
    df
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
    if (nchar(q) == 0) {
      return(shiny::div(class = "sep-landing-body",
        shiny::div(class = "sep-landing-hint",
          shiny::tags$i(class = "fas fa-lightbulb"),
          " Type any keyword above \u2014 survey title, topic, year, or series name \u2014 then click Search."
        )
      ))
    }

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

  # Study detail modals — registered for every study_id at startup
  if (nrow(catalog) > 0) {
    lapply(catalog$study_id, function(sid) {
      local({
        id <- sid
        shiny::observeEvent(input[[paste0("detail_", id)]], {
          s <- catalog |> dplyr::filter(study_id == id)
          if (nrow(s) == 0) return()
          s <- s[1, ]

          study_res <- resources |> dplyr::filter(study_id == id)

          res_block <- if (nrow(study_res) > 0) {
            shiny::tagList(
              shiny::div(class = "res-list-title",
                shiny::tags$i(class = "fas fa-paperclip fa-xs"),
                paste0("\u00a0", nrow(study_res),
                       " Available Resource", if (nrow(study_res) > 1) "s")
              ),
              lapply(seq_len(min(nrow(study_res), 18)), function(ri) {
                r <- study_res[ri, ]
                shiny::div(class = "res-row",
                  shiny::tags$span(class = "res-type-badge", toupper(r$type)),
                  shiny::tags$a(href = r$url, target = "_blank", r$name)
                )
              })
            )
          } else {
            shiny::tags$p(class = "no-resources-note",
              "No direct download resources listed for this study.")
          }

          shiny::showModal(shiny::modalDialog(
            title = s$title,
            shiny::div(class = "det-grid3",
              shiny::div(
                shiny::div(class = "det-field__lbl", "Year"),
                shiny::div(class = "det-field__val",
                           tidyr::replace_na(as.character(s$year), "Unknown"))
              ),
              shiny::div(
                shiny::div(class = "det-field__lbl", "Survey Type"),
                shiny::div(class = "det-field__val",
                           tidyr::replace_na(s$study_type, tidyr::replace_na(s$collection, "\u2014")))
              ),
              shiny::div(
                shiny::div(class = "det-field__lbl", "Gender Score"),
                shiny::div(class = "det-field__val",
                  paste0(s$gender_score, " / 10"),
                  gender_bar_html(s$gender_score)
                )
              )
            ),
            shiny::div(class = "det-grid3",
              shiny::div(
                shiny::div(class = "det-field__lbl", "Organization"),
                shiny::div(class = "det-field__val",
                  stringr::str_trunc(tidyr::replace_na(s$organization, "NISR"), 60)
                )
              ),
              shiny::div(
                shiny::div(class = "det-field__lbl", "Data Access"),
                shiny::div(class = "det-field__val", s$access_clean)
              ),
              shiny::div(
                shiny::div(class = "det-field__lbl", "Quality"),
                shiny::div(class = "det-field__val", quality_pill(s$quality_status))
              )
            ),
            shiny::div(class = "det-field",
              shiny::div(class = "det-field__lbl", "Geographic Coverage"),
              shiny::div(class = "det-field__val",
                         tidyr::replace_na(s$geographic_coverage, "National coverage"))
            ),
            shiny::div(class = "det-field det-field--abstract",
              shiny::div(class = "det-field__lbl", "Abstract"),
              shiny::div(class = "det-abstract",
                tidyr::replace_na(s$abstract, "No abstract available for this study."))
            ),
            res_block,
            footer = shiny::tagList(
              if (!is.na(s$url) && nchar(s$url) > 4)
                shiny::tags$a(href = s$url, target = "_blank",
                  class = "btn btn--primary",
                  shiny::tags$i(class = "fas fa-external-link-alt fa-xs"),
                  "\u00a0 Open in NISR Catalog"
                ),
              shiny::modalButton("Close")
            ),
            easyClose = TRUE,
            size      = "l"
          ))
        }, ignoreInit = TRUE)
      })
    })
  }

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
      dplyr::mutate(short = paste0(stringr::str_sub(title, 1, 36), "\u2026")) |>
      dplyr::arrange(dplyr::desc(missing_field_count)) |>
      head(15) |>
      dplyr::mutate(short = factor(short, levels = rev(short)))

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

}
