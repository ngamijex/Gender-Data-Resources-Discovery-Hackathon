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
    ")),

    # Infinite typewriter — 4 phrases cycling one by one
    shiny::tags$script(shiny::HTML("
      $(document).ready(function() {
        var el = document.getElementById('hero-typing');
        if (!el) return;

        // Each phrase is an array of segments { text, tag }
        // tag: null = plain text, 'em' = italic red highlight
        var phrases = [
          [
            { text: 'Discover Rwanda\u2019s ',  tag: null },
            { text: 'Gender Data',              tag: 'em'  },
            { text: ' with less friction.',     tag: null  }
          ],
          [
            { text: 'Empowering ',              tag: null },
            { text: 'CSOs & Advocates',         tag: 'em'  },
            { text: ' with evidence-based tools.', tag: null }
          ],
          [
            { text: 'Turning ',                 tag: null },
            { text: 'National Surveys',         tag: 'em'  },
            { text: ' into actionable insights.', tag: null }
          ],
          [
            { text: 'Closing the gap between ', tag: null },
            { text: 'data existence',           tag: 'em'  },
            { text: ' and data use.',           tag: null  }
          ]
        ];

        // Flatten a phrase into character array
        function flatten(phrase) {
          var chars = [];
          phrase.forEach(function(seg) {
            for (var i = 0; i < seg.text.length; i++) {
              chars.push({
                ch     : seg.text[i],
                tag    : seg.tag,
                isFirst: i === 0,
                isLast : i === seg.text.length - 1
              });
            }
          });
          return chars;
        }

        function buildHTML(chars, count) {
          var html = '';
          for (var i = 0; i < count && i < chars.length; i++) {
            var c = chars[i];
            if (c.tag && c.isFirst) html += '<' + c.tag + '>';
            html += c.ch === '&' ? '&amp;' : c.ch;
            if (c.tag && c.isLast)  html += '</' + c.tag + '>';
          }
          return html;
        }

        var phraseIdx  = 0;
        var charIdx    = 0;
        var isDeleting = false;
        var chars      = flatten(phrases[0]);

        function tick() {
          if (!isDeleting) {
            charIdx++;
            if (charIdx > chars.length) {
              // Fully typed — pause then start deleting
              isDeleting = true;
              setTimeout(tick, 2000);
              return;
            }
          } else {
            charIdx--;
            if (charIdx < 0) {
              // Fully deleted — move to next phrase
              isDeleting = false;
              phraseIdx  = (phraseIdx + 1) % phrases.length;
              chars      = flatten(phrases[phraseIdx]);
              charIdx    = 0;
              setTimeout(tick, 350);
              return;
            }
          }
          el.innerHTML = buildHTML(chars, charIdx) + '<span class=\"typing-cursor\"></span>';
          setTimeout(tick, isDeleting ? 20 : 55);
        }

        el.innerHTML = '';
        tick();
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
          "\u00a0 Rwanda \u00b7 Feminism in Action for Structural Transformation"
        ),
        shiny::tags$h1(class = "hero__title", id = "hero-typing",
          "Discover Rwanda\u2019s",
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

      # ── Problem / Mission / Vision — staggered services layout ───────────
      shiny::div(class = "home-pmv",

        # ── Top header row: title left, intro right
        shiny::div(class = "pmv-header",
          shiny::div(class = "pmv-header__left",
            shiny::div(class = "sec-eyebrow",
              shiny::tags$i(class = "fas fa-info-circle fa-xs"),
              "\u00a0 About GDDP"
            ),
            shiny::tags$h2(class = "pmv-main-title",
              "We built GDDP", shiny::tags$br(), "to close the gap."
            )
          ),
          shiny::div(class = "pmv-header__right",
            shiny::tags$p(class = "pmv-intro-text",
              "Across Rwanda, critical gender-focused data is produced every year through national surveys ",
              "\u2014 yet it remains invisible to those who need it most. CSOs, policy advocates, ",
              "journalists, and researchers working under the FAST program (Feminism in Action for ",
              "Structural Transformation) consistently face the same barriers: scattered repositories, ",
              "PDF-heavy outputs, inaccessible disaggregated data, and advocacy cycles that move faster ",
              "than current discovery workflows allow. GDDP exists to change that."
            )
          )
        ),

        # ── Three staggered items
        shiny::div(class = "pmv-items",

          # Item 01 — The Problem
          shiny::div(class = "pmv-item",
            shiny::div(class = "pmv-item__num", "01"),
            shiny::div(class = "pmv-item__title", "The Problem"),
            shiny::tags$p(class = "pmv-item__text",
              "Gender data exists but it\u2019s scattered, hard to find, and difficult to navigate. ",
              "No unified discovery interface. No quality signal. No way to move fast enough for ",
              "evidence-based advocacy."
            ),
            shiny::tags$img(src = "problem.png", class = "pmv-item__img", alt = "The Problem")
          ),

          # Item 02 — Our Mission (offset down)
          shiny::div(class = "pmv-item pmv-item--offset",
            shiny::div(class = "pmv-item__num", "02"),
            shiny::div(class = "pmv-item__title", "Our Mission"),
            shiny::tags$p(class = "pmv-item__text",
              "Make Rwanda\u2019s gender data discoverable, interpretable, and actionable. ",
              "Connect civil society and policy advocates directly to 73 national surveys, ",
              "enriched with quality scores and gender relevance ratings."
            ),
            shiny::tags$img(src = "mission.png", class = "pmv-item__img", alt = "Our Mission")
          ),

          # Item 03 — Our Vision
          shiny::div(class = "pmv-item",
            shiny::div(class = "pmv-item__num", "03"),
            shiny::div(class = "pmv-item__title", "Our Vision"),
            shiny::tags$p(class = "pmv-item__text",
              "A Rwanda where every gender advocate finds the data they need in under a minute, ",
              "understands its quality at a glance, and uses it immediately to drive structural ",
              "transformation."
            ),
            shiny::tags$img(src = "vision.png", class = "pmv-item__img", alt = "Our Vision")
          )
        )
      ),

      # ── Site footer ────────────────────────────────────────────────────────
      shiny::div(class = "site-footer",

        # Glass overlay panel
        shiny::div(class = "footer-glass",

          # Top row — logos
          shiny::div(class = "footer-logos",
            shiny::tags$img(src = "logo.png",  class = "footer-logo", alt = "GIZ Logo"),
            shiny::div(class = "footer-divider-v"),
            shiny::tags$img(src = "Femin.png", class = "footer-femin", alt = "Feminism in Action")
          ),

          shiny::tags$hr(class = "footer-hr"),

          # Middle row — event + team
          shiny::div(class = "footer-body",

            # Event block
            shiny::div(class = "footer-block",
              shiny::div(class = "footer-block__eyebrow", "The Event"),
              shiny::tags$h3(class = "footer-block__title",
                "GIZ Gender Data Resources", shiny::tags$br(),
                "Discovery Hackathon 2026"
              ),
              shiny::tags$p(class = "footer-block__text",
                "19\u201320 March 2026 \u00b7 GIZ Digital Technical Center, Kigali"
              ),
              shiny::tags$p(class = "footer-block__text",
                "A two-day innovation sprint dedicated to building digital tools that make ",
                "Rwanda\u2019s gender data more visible, accessible, and actionable for civil society."
              )
            ),

            # Divider
            shiny::div(class = "footer-divider-v footer-divider-v--tall"),

            # Team block
            shiny::div(class = "footer-block",
              shiny::div(class = "footer-block__eyebrow", "The Team"),
              shiny::tags$h3(class = "footer-block__title", "Built by"),
              shiny::div(class = "footer-team",
                shiny::div(class = "footer-member",
                  shiny::div(class = "footer-member__role", "Data Science \u00b7 Team Lead"),
                  shiny::div(class = "footer-member__name", "Ngamije Didier")
                ),
                shiny::div(class = "footer-member",
                  shiny::div(class = "footer-member__role", "UI / UX Designer"),
                  shiny::div(class = "footer-member__name", "Dan Munyaneza")
                ),
                shiny::div(class = "footer-member",
                  shiny::div(class = "footer-member__role", "Front-End Developer"),
                  shiny::div(class = "footer-member__name", "Gatete Bugingo Jimmy")
                ),
                shiny::div(class = "footer-member",
                  shiny::div(class = "footer-member__role", "Back-End Developer"),
                  shiny::div(class = "footer-member__name", "Ishimwe Sibomana Christian")
                )
              )
            )
          ),

          shiny::tags$hr(class = "footer-hr"),

          # Bottom bar
          shiny::div(class = "footer-bottom",
            shiny::tags$span(
              "\u00a9 2026 GDDP \u00b7 Gender Data Discovery Platform \u00b7 ",
              "Built for the GIZ FAST Program"
            ),
            shiny::tags$a(
              href = "https://microdata.statistics.gov.rw",
              target = "_blank",
              class = "footer-bottom__link",
              "NISR Microdata Catalog \u2197"
            )
          )

        ) # end footer-glass
      ) # end site-footer
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # TAB 2 — DATA DISCOVERY  (Google-search engine style)
    # ══════════════════════════════════════════════════════════════════════════
    shiny::tabPanel("Data Discovery",
      shiny::div(class = "sep-page", id = "sep-page",

        # ── JS: loading indicators, state toggle, advanced panel, clear ────
        shiny::tags$script(shiny::HTML("
          $(document).ready(function() {

            // ── Skeleton / loading HTML injected immediately on search ────────
            var aiLoaderHTML =
              '<div class=\"ai-ov-loading\">' +
                '<div class=\"ai-ov-loading__header\">' +
                  '<span class=\"ai-ov-loading__dots\">' +
                    '<span></span><span></span><span></span>' +
                  '</span>' +
                  '<span class=\"ai-ov-loading__label\">AI is analyzing the catalog\u2026</span>' +
                '</div>' +
                '<div class=\"ai-ov-loading__bars\">' +
                  '<div class=\"ai-sk-bar ai-sk-bar--wide\"></div>' +
                  '<div class=\"ai-sk-bar ai-sk-bar--med\"></div>' +
                  '<div class=\"ai-sk-bar ai-sk-bar--short\"></div>' +
                '</div>' +
                '<div class=\"ai-ov-loading__cards\">' +
                  '<div class=\"ai-sk-card\"><div class=\"ai-sk-chip\"></div><div class=\"ai-sk-line\"></div><div class=\"ai-sk-line ai-sk-line--sm\"></div></div>' +
                  '<div class=\"ai-sk-card\"><div class=\"ai-sk-chip\"></div><div class=\"ai-sk-line\"></div><div class=\"ai-sk-line ai-sk-line--sm\"></div></div>' +
                  '<div class=\"ai-sk-card\"><div class=\"ai-sk-chip\"></div><div class=\"ai-sk-line\"></div><div class=\"ai-sk-line ai-sk-line--sm\"></div></div>' +
                '</div>' +
              '</div>';

            var resultsLoaderHTML =
              '<div class=\"sep-results-loading\">' +
                '<div class=\"sep-sk-result\"><div class=\"sep-sk-path\"></div><div class=\"sep-sk-title\"></div><div class=\"sep-sk-text\"></div><div class=\"sep-sk-text sep-sk-text--s\"></div></div>' +
                '<div class=\"sep-sk-result\"><div class=\"sep-sk-path\"></div><div class=\"sep-sk-title\"></div><div class=\"sep-sk-text\"></div><div class=\"sep-sk-text sep-sk-text--s\"></div></div>' +
                '<div class=\"sep-sk-result\"><div class=\"sep-sk-path\"></div><div class=\"sep-sk-title\"></div><div class=\"sep-sk-text\"></div><div class=\"sep-sk-text sep-sk-text--s\"></div></div>' +
                '<div class=\"sep-sk-result\"><div class=\"sep-sk-path\"></div><div class=\"sep-sk-title\"></div><div class=\"sep-sk-text\"></div></div>' +
              '</div>';

            function showSearchLoaders() {
              $('#ai_overview_panel').html(aiLoaderHTML);
              $('#sep_result_items').html(resultsLoaderHTML);
            }

            // ── Toggle advanced filters panel ─────────────────────────────────
            $(document).on('click', '#btn_adv_toggle', function(e) {
              e.preventDefault();
              $('#sep-adv-panel').toggleClass('sep-adv-panel--open');
            });

            // ── Search button ─────────────────────────────────────────────────
            $(document).on('click', '#btn_do_search', function() {
              if (($('#srch_text').val() || '').trim().length > 0) {
                $('#sep-page').addClass('sep-page--searched');
                showSearchLoaders();
              }
            });

            // ── Quick topic tags ──────────────────────────────────────────────
            $(document).on('click', '.sep-qtag', function() {
              $('#sep-page').addClass('sep-page--searched');
              showSearchLoaders();
            });

            // ── Clear search ──────────────────────────────────────────────────
            $(document).on('click', '#btn_clear_search', function(e) {
              e.preventDefault();
              $('#sep-page').removeClass('sep-page--searched');
              $('#ai_overview_panel').empty();
              $('#sep_result_items').empty();
              Shiny.setInputValue('srch_text', '', {priority: 'event'});
              setTimeout(function() { $('#srch_text').val('').trigger('input'); }, 10);
            });

            // ── Reset button ──────────────────────────────────────────────────
            $(document).on('click', '#btn_reset', function() {
              $('#sep-page').removeClass('sep-page--searched');
              $('#ai_overview_panel').empty();
              $('#sep_result_items').empty();
            });

            // ── Enter key ─────────────────────────────────────────────────────
            $(document).on('keydown', '#srch_text', function(e) {
              if (e.key === 'Enter') {
                e.preventDefault();
                $('#btn_do_search').click();
              }
            });
          });
        ")),

        # ── Search section (hero → compact bar on activation) ─────────────
        shiny::div(class = "sep-search-section",

          # Brand: logo + subtitle (hidden in compact mode)
          shiny::div(class = "sep-brand",
            shiny::tags$img(src = "logo.png", class = "sep-brand__logo", alt = "GDDP"),
            shiny::div(class = "sep-brand__text",
              shiny::tags$h2(class = "sep-brand__title", "Rwanda Gender Data Discovery"),
              shiny::tags$p(class = "sep-brand__sub",
                "Search 73 NISR national surveys \u00b7 1978\u20132024")
            )
          ),

          # Search bar pill
          shiny::div(class = "sep-searchbar-outer",
            shiny::div(class = "sep-searchbar",
              shiny::tags$i(class = "fas fa-search sep-searchbar__icon"),
              shiny::textInput("srch_text", NULL,
                placeholder = "Search studies, surveys, topics, keywords\u2026",
                width = "100%"
              ),
              shiny::div(class = "sep-searchbar__end",
                shiny::actionButton("btn_clear_search", NULL,
                  icon  = shiny::icon("times"),
                  class = "sep-clear-btn"
                ),
                shiny::tags$span(class = "sep-bar-sep"),
                shiny::actionButton("btn_do_search", "Search",
                  class = "sep-search-btn"
                )
              )
            )
          ),

          # Hint text
          shiny::tags$p(class = "sep-hint-text",
            shiny::tags$i(class = "fas fa-lightbulb fa-xs"),
            " Type any keyword \u2014 survey title, topic, year, or series name \u2014 then click Search."
          ),

          # Quick topic tags (hidden in compact mode)
          shiny::div(class = "sep-quick-wrap",
            shiny::tags$span(class = "sep-quick-label", "Explore:"),
            shiny::actionButton("qt_dhs",      "DHS Surveys",      class = "sep-qtag"),
            shiny::actionButton("qt_maternal", "Maternal Health",   class = "sep-qtag"),
            shiny::actionButton("qt_eicv",     "EICV / Poverty",    class = "sep-qtag"),
            shiny::actionButton("qt_labor",    "Labor Force",       class = "sep-qtag"),
            shiny::actionButton("qt_census",   "Population Census", class = "sep-qtag"),
            shiny::actionButton("qt_gender",   "Gender Violence",   class = "sep-qtag")
          )
        ),

        # ── Filter strip (appears below search bar in active mode) ────────
        shiny::div(class = "sep-filters-strip",
          shiny::div(class = "sep-fstrip__label",
            shiny::tags$i(class = "fas fa-filter fa-xs"), " Filters"
          ),
          shiny::div(class = "sep-fstrip__controls",
            shiny::div(class = "sep-fstrip__item",
              shiny::selectInput("sort_by", NULL,
                choices = c(
                  "Best Match"       = "relevance",
                  "Newest First"     = "year_desc",
                  "Oldest First"     = "year_asc",
                  "Gender Relevance" = "gender_desc",
                  "Most Viewed"      = "views_desc",
                  "Title A\u2013Z"   = "title_asc"
                ),
                selected = "relevance",
                width = "165px"
              )
            ),
            shiny::div(class = "sep-fstrip__item sep-fstrip__item--slide",
              shiny::div(class = "sep-slide-lbl", "Year Range"),
              shiny::sliderInput("flt_year", NULL,
                min = 1978, max = 2024, value = c(1978, 2024), sep = "", width = "200px"
              )
            ),
            shiny::div(class = "sep-fstrip__item sep-fstrip__item--slide",
              shiny::div(class = "sep-slide-lbl", "Gender Score"),
              shiny::sliderInput("flt_gender", NULL,
                min = 0, max = 10, value = c(0, 10), step = 1, width = "150px"
              )
            ),
            shiny::actionButton("btn_reset", NULL,
              icon  = shiny::icon("undo"),
              class = "sep-reset-btn",
              title = "Reset all filters"
            ),
            shiny::actionButton("btn_adv_toggle",
              shiny::tagList(shiny::tags$i(class = "fas fa-sliders-h fa-xs"), " Advanced"),
              class = "sep-adv-btn"
            )
          )
        ),

        # ── Advanced filters panel (collapsible) ──────────────────────────
        shiny::div(class = "sep-adv-panel", id = "sep-adv-panel",
          shiny::div(class = "sep-adv-inner",
            shiny::div(class = "sep-adv-group",
              shiny::div(class = "sep-adv-lbl",
                shiny::tags$i(class = "fas fa-layer-group fa-xs"), " Survey Series"
              ),
              shiny::checkboxGroupInput("flt_collection", NULL,
                choices  = c("DHS","EICV","Census","LFS","Agriculture",
                             "FinScope","Food Security","Business Census",
                             "Child Labour","Social Protection","Enterprise",
                             "Manpower","Health Services","Governance","Other"),
                selected = c("DHS","EICV","Census","LFS","Agriculture",
                             "FinScope","Food Security","Business Census",
                             "Child Labour","Social Protection","Enterprise",
                             "Manpower","Health Services","Governance","Other"),
                inline = TRUE
              )
            ),
            shiny::div(class = "sep-adv-row",
              shiny::div(class = "sep-adv-group",
                shiny::div(class = "sep-adv-lbl",
                  shiny::tags$i(class = "fas fa-check-circle fa-xs"), " Data Quality"
                ),
                shiny::checkboxGroupInput("flt_quality", NULL,
                  choices  = c("Complete","Minor Issues","Incomplete"),
                  selected = c("Complete","Minor Issues","Incomplete")
                )
              ),
              shiny::div(class = "sep-adv-group",
                shiny::div(class = "sep-adv-lbl",
                  shiny::tags$i(class = "fas fa-unlock-alt fa-xs"), " Data Access"
                ),
                shiny::checkboxGroupInput("flt_access", NULL,
                  choices  = c("Public","Licensed","Other"),
                  selected = c("Public","Licensed","Other")
                )
              )
            )
          )
        ),

        # ── Meta bar + result list + detail panel ─────────────────────────
        shiny::div(class = "sep-results-area",
          shiny::uiOutput("sep_meta_bar"),
          shiny::uiOutput("ai_overview_panel"),
          shiny::uiOutput("sep_result_items"),
          shiny::uiOutput("study_detail_panel")
        )

      ) # end sep-page
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
