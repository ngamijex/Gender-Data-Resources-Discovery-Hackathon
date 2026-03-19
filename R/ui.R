# ── R/ui.R ────────────────────────────────────────────────────────────────────
# Full UI definition for GDDP.
# All visual styling lives in www/styles.css — zero inline CSS here.

ui <- shiny::tagList(

  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "styles.css"),
    shiny::tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    ),
    # Inject Femin.png on the right of the navbar after DOM is ready
    shiny::tags$script(shiny::HTML("
      $(document).ready(function() {
        var femin = $('<div class=\"navbar-femin\"><img src=\"Femin.png\" alt=\"Feminism in Action\"/></div>');
        $('.navbar-collapse').append(femin);
      });
    "))
  ),

  shiny::navbarPage(
    title = shiny::tags$img(
      src   = "logo.png",
      alt   = "GIZ Logo",
      class = "navbar-brand-img"
    ),
    id          = "main_nav",
    collapsible = TRUE,

    # ══════════════════════════════════════════════════════════════════════════
    # TAB 1 — HOME
    # ══════════════════════════════════════════════════════════════════════════
    shiny::tabPanel("Home",

      # ── Hero ───────────────────────────────────────────────────────────────
      shiny::div(class = "hero",
        shiny::div(class = "hero__eyebrow",
          shiny::tags$i(class = "fas fa-globe-africa fa-xs"),
          "\u00a0 Rwanda \u00b7 NISR Microdata Catalog \u00b7 GIZ Gender Data Challenge"
        ),
        shiny::tags$h1(class = "hero__title",
          "Discover Rwanda\u2019s", shiny::tags$br(),
          shiny::tags$em("Gender Data"), " with less friction."
        ),
        shiny::tags$p(class = "hero__desc",
          "A unified platform for CSOs, policy advocates, and researchers to find, ",
          "interpret, and use gender-related survey data from NISR\u2019s national catalog."
        ),
        shiny::div(class = "hero__cta",
          shiny::actionButton("btn_go_disc",
            shiny::tagList(shiny::tags$i(class = "fas fa-search"), "\u00a0 Search the Catalog"),
            class = "btn btn--lg-hero"
          ),
          shiny::actionButton("btn_go_dash",
            shiny::tagList(shiny::tags$i(class = "fas fa-chart-bar"), "\u00a0 View Dashboard"),
            class = "btn btn--ghost-hero"
          )
        )
      ),

      # ── KPI strip (server-rendered) ────────────────────────────────────────
      shiny::uiOutput("kpi_strip"),

      # ── Featured studies ───────────────────────────────────────────────────
      shiny::div(class = "home-sec",
        shiny::div(class = "sec-eyebrow",
          shiny::tags$i(class = "fas fa-star fa-xs"),
          "\u00a0 Featured Studies"
        ),
        shiny::tags$h2(class = "sec-title",
          "Highest Gender Relevance in the Catalog"
        ),
        shiny::uiOutput("featured_studies")
      ),

      # ── How it works + collection chart ───────────────────────────────────
      shiny::div(class = "home-sec home-sec--alt",
        shiny::fluidRow(
          shiny::column(5,
            shiny::div(class = "sec-eyebrow",
              shiny::tags$i(class = "fas fa-compass fa-xs"),
              "\u00a0 Platform Guide"
            ),
            shiny::tags$h2(class = "sec-title", "How the Platform Works"),
            shiny::div(class = "how-step",
              shiny::div(class = "how-step__num", "1"),
              shiny::div(class = "how-step__body",
                shiny::tags$h4("Search the Catalog"),
                shiny::tags$p("Use the Data Discovery tab to search 73 national surveys by keyword, year, survey series, quality status, and gender relevance score.")
              )
            ),
            shiny::div(class = "how-step",
              shiny::div(class = "how-step__num", "2"),
              shiny::div(class = "how-step__body",
                shiny::tags$h4("Review Metadata"),
                shiny::tags$p("Each card shows the study\u2019s abstract, coverage, quality flags, gender relevance score, and available downloadable files.")
              )
            ),
            shiny::div(class = "how-step",
              shiny::div(class = "how-step__num", "3"),
              shiny::div(class = "how-step__body",
                shiny::tags$h4("Access the Source"),
                shiny::tags$p("Link directly to NISR\u2019s microdata catalog to download questionnaires, reports, and raw data files.")
              )
            ),
            shiny::div(class = "how-step",
              shiny::div(class = "how-step__num", "4"),
              shiny::div(class = "how-step__body",
                shiny::tags$h4("Visualize Trends"),
                shiny::tags$p("The Dashboard surfaces Rwanda gender indicator trends from DHS, EICV, RLFS, and Census rounds.")
              )
            )
          ),
          shiny::column(7,
            shiny::div(class = "home-chart-panel",
              shiny::div(class = "sec-eyebrow",
                shiny::tags$i(class = "fas fa-layer-group fa-xs"),
                "\u00a0 Catalog Coverage"
              ),
              shiny::tags$h2(class = "sec-title", "What\u2019s in the Catalog"),
              plotly::plotlyOutput("home_collection_chart", height = "370px")
            )
          )
        )
      ),

      # ── Site footer ────────────────────────────────────────────────────────
      shiny::div(class = "site-footer",
        shiny::fluidRow(
          shiny::column(4,
            shiny::div(class = "site-footer__brand", "GDDP"),
            shiny::tags$p(
              "Gender Data Discovery Platform \u2014 built for the GIZ Gender Data Challenge, March 2026."
            ),
            shiny::tags$p(
              shiny::tags$a(
                href   = "https://microdata.statistics.gov.rw",
                target = "_blank",
                "NISR Microdata Catalog \u2192"
              )
            )
          ),
          shiny::column(4,
            shiny::tags$p(class = "footer-heading", "The Team"),
            shiny::tags$p("Dan Munyaneza \u2014 UI/UX Designer"),
            shiny::tags$p("Ngamije Didier \u2014 Data Science (Lead)"),
            shiny::tags$p("Gatete Bugingo Jimmy \u2014 Front-End Developer"),
            shiny::tags$p("Ishimwe Sibomana Christian \u2014 Back-End Developer")
          ),
          shiny::column(4,
            shiny::tags$p(class = "footer-heading", "Data"),
            shiny::tags$p("73 national surveys \u00b7 1978\u20132024"),
            shiny::tags$p("Source: National Institute of Statistics of Rwanda (NISR)"),
            shiny::tags$p(class = "footer-note",
              "Prototype uses the NISR baseline CSV inventory. All links point to official NISR sources."
            )
          )
        )
      )
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # TAB 2 — DATA DISCOVERY
    # ══════════════════════════════════════════════════════════════════════════
    shiny::tabPanel("Data Discovery",
      shiny::div(class = "disc-layout",

        # ── Sidebar filters ─────────────────────────────────────────────────
        shiny::div(class = "disc-sidebar",

          shiny::div(class = "sb-section",
            shiny::div(class = "sb-title",
              shiny::tags$i(class = "fas fa-search fa-xs"), "\u00a0 Keyword Search"
            ),
            shiny::textInput("srch_text", NULL,
              placeholder = "Title, abstract, keywords\u2026",
              width = "100%"
            )
          ),

          shiny::div(class = "sb-section",
            shiny::div(class = "sb-title",
              shiny::tags$i(class = "fas fa-layer-group fa-xs"), "\u00a0 Survey Series"
            ),
            shiny::checkboxGroupInput("flt_collection", NULL,
              choices  = c("DHS","EICV","Census","LFS","Agriculture",
                           "FinScope","Food Security","Business Census",
                           "Child Labour","Social Protection","Enterprise",
                           "Manpower","Health Services","Governance","Other"),
              selected = c("DHS","EICV","Census","LFS","Agriculture",
                           "FinScope","Food Security","Business Census",
                           "Child Labour","Social Protection","Enterprise",
                           "Manpower","Health Services","Governance","Other")
            )
          ),

          shiny::div(class = "sb-section",
            shiny::div(class = "sb-title",
              shiny::tags$i(class = "fas fa-calendar-alt fa-xs"), "\u00a0 Year Range"
            ),
            shiny::sliderInput("flt_year", NULL,
              min = 1978, max = 2024,
              value = c(1978, 2024), sep = "", width = "100%"
            )
          ),

          shiny::div(class = "sb-section",
            shiny::div(class = "sb-title",
              shiny::tags$i(class = "fas fa-venus fa-xs"), "\u00a0 Gender Relevance Score"
            ),
            shiny::sliderInput("flt_gender", NULL,
              min = 0, max = 10, value = c(0, 10), step = 1, width = "100%"
            )
          ),

          shiny::div(class = "sb-section",
            shiny::div(class = "sb-title",
              shiny::tags$i(class = "fas fa-check-circle fa-xs"), "\u00a0 Data Quality"
            ),
            shiny::checkboxGroupInput("flt_quality", NULL,
              choices  = c("Complete","Minor Issues","Incomplete"),
              selected = c("Complete","Minor Issues","Incomplete")
            )
          ),

          shiny::div(class = "sb-section",
            shiny::div(class = "sb-title",
              shiny::tags$i(class = "fas fa-unlock-alt fa-xs"), "\u00a0 Data Access"
            ),
            shiny::checkboxGroupInput("flt_access", NULL,
              choices  = c("Public","Licensed","Other"),
              selected = c("Public","Licensed","Other")
            )
          ),

          shiny::div(class = "sb-section",
            shiny::div(class = "sb-title",
              shiny::tags$i(class = "fas fa-sort fa-xs"), "\u00a0 Sort By"
            ),
            shiny::selectInput("sort_by", NULL,
              choices = c(
                "Year \u2014 Newest First"  = "year_desc",
                "Year \u2014 Oldest First"  = "year_asc",
                "Gender Relevance"          = "gender_desc",
                "Most Viewed"               = "views_desc",
                "Title A \u2013 Z"          = "title_asc"
              ),
              selected = "year_desc",
              width    = "100%"
            )
          ),

          shiny::actionButton("btn_reset",
            shiny::tagList(shiny::tags$i(class = "fas fa-undo fa-xs"), "\u00a0 Reset Filters"),
            class = "btn btn--outline-sidebar"
          )
        ),

        # ── Main results panel ───────────────────────────────────────────────
        shiny::div(class = "disc-main",
          shiny::uiOutput("results_hdr"),
          shiny::uiOutput("disc_cards")
        )
      )
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # TAB 3 — DASHBOARD
    # ══════════════════════════════════════════════════════════════════════════
    shiny::tabPanel("Dashboard",
      shiny::div(class = "dash-page",

        # Page header
        shiny::div(class = "dash-hdr",
          shiny::div(class = "sec-eyebrow",
            shiny::tags$i(class = "fas fa-chart-bar fa-xs"), "\u00a0 Analytics"
          ),
          shiny::tags$h1(class = "sec-title", "Rwanda Gender Data Landscape"),
          shiny::tags$p(class = "dash-intro",
            "Visualizations drawn from 73 NISR national surveys (1978\u20132024) and ",
            "curated gender indicators from the DHS, EICV, and RLFS series."
          )
        ),

        # KPI cards (server-rendered)
        shiny::uiOutput("dash_kpis"),

        # ── SECTION 1 — Survey Landscape ────────────────────────────────────
        shiny::div(class = "dash-section",
          shiny::div(class = "dash-sec-hdr",
            shiny::tags$h2(class = "dash-sec-title", "Survey Landscape"),
            shiny::tags$span(class = "dash-sec-sub",
              "Distribution and trends across the national catalog")
          ),
          shiny::fluidRow(
            shiny::column(8,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                  "\u00a0 Studies Published by Year"
                ),
                plotly::plotlyOutput("ch_by_year", height = "280px")
              )
            ),
            shiny::column(4,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-chart-pie fa-xs"),
                  "\u00a0 Data Access Types"
                ),
                plotly::plotlyOutput("ch_access_donut", height = "280px")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-layer-group fa-xs"),
                  "\u00a0 Studies by Survey Series"
                ),
                plotly::plotlyOutput("ch_by_collection", height = "320px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-eye fa-xs"),
                  "\u00a0 Top 10 Most Accessed Studies"
                ),
                plotly::plotlyOutput("ch_top_viewed", height = "320px")
              )
            )
          )
        ),

        # ── SECTION 2 — Gender Data Coverage ────────────────────────────────
        shiny::div(class = "dash-section",
          shiny::div(class = "dash-sec-hdr",
            shiny::tags$h2(class = "dash-sec-title", "Gender Data Coverage"),
            shiny::tags$span(class = "dash-sec-sub",
              "How well does the catalog serve gender analysis?")
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-venus fa-xs"),
                  "\u00a0 Avg. Gender Relevance Score by Survey Series"
                ),
                plotly::plotlyOutput("ch_gender_by_coll", height = "310px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-spider fa-xs"),
                  "\u00a0 Gender Topic Coverage \u2014 Radar"
                ),
                plotly::plotlyOutput("ch_topic_radar", height = "310px")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-check-double fa-xs"),
                  "\u00a0 Metadata Quality Status Across All Studies"
                ),
                plotly::plotlyOutput("ch_quality_stack", height = "230px")
              )
            )
          )
        ),

        # ── SECTION 3 — Rwanda Gender Indicators ────────────────────────────
        shiny::div(class = "dash-section",
          shiny::div(class = "dash-sec-hdr",
            shiny::tags$h2(class = "dash-sec-title", "Rwanda Gender Indicators"),
            shiny::tags$span(class = "dash-sec-sub",
              "Curated from DHS 1992\u20132020 \u00b7 EICV \u00b7 RLFS 2017\u20132023 \u00b7 PHC 2012/2022")
          ),
          shiny::div(class = "indicator-switcher",
            shiny::selectInput("indicator_grp", NULL,
              choices = c(
                "Maternal & Reproductive Health" = "maternal",
                "Child Health & Nutrition"        = "child",
                "Education Gender Parity"         = "education",
                "Labor Force Participation"       = "labor",
                "Geography & Household Structure" = "geography"
              ),
              selected = "maternal",
              width    = "380px"
            )
          ),
          shiny::uiOutput("indicator_ui")
        ),

        # ── SECTION 4 — Quality Intelligence ────────────────────────────────
        shiny::div(class = "dash-section",
          shiny::div(class = "dash-sec-hdr",
            shiny::tags$h2(class = "dash-sec-title", "Quality Intelligence"),
            shiny::tags$span(class = "dash-sec-sub",
              "Metadata completeness and resource availability")
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-exclamation-triangle fa-xs"),
                  "\u00a0 Studies with Most Missing Metadata Fields"
                ),
                plotly::plotlyOutput("ch_missing_fields", height = "310px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "dash-chart-card",
                shiny::div(class = "dash-chart-title",
                  shiny::tags$i(class = "fas fa-file-alt fa-xs"),
                  "\u00a0 Resource Completeness Across the Catalog"
                ),
                plotly::plotlyOutput("ch_resource_completeness", height = "310px")
              )
            )
          )
        )

      ) # end dash-page
    )   # end Dashboard tabPanel

  ) # end navbarPage
) # end tagList
