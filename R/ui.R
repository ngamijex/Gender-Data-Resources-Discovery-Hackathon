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
                  shiny::div(class = "footer-member__role", "Statistician"),
                  shiny::div(class = "footer-member__name", "Dan Munyaneza")
                ),
                shiny::div(class = "footer-member",
                  shiny::div(class = "footer-member__role", "Data Analyst"),
                  shiny::div(class = "footer-member__name", "Gatete Bugingo Jimmy")
                ),
                shiny::div(class = "footer-member",
                  shiny::div(class = "footer-member__role", "Research Associate"),
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
            shiny::actionButton("qt_maternal",  "Maternal Health",       class = "sep-qtag"),
            shiny::actionButton("qt_fertility", "Family Planning",       class = "sep-qtag"),
            shiny::actionButton("qt_gbv",       "Gender Violence",       class = "sep-qtag"),
            shiny::actionButton("qt_women_emp", "Women Employment",      class = "sep-qtag"),
            shiny::actionButton("qt_girls_edu", "Girls Education",       class = "sep-qtag"),
            shiny::actionButton("qt_hiv",       "HIV & Women",           class = "sep-qtag"),
            shiny::actionButton("qt_child",     "Child Nutrition",       class = "sep-qtag"),
            shiny::actionButton("qt_land",      "Women Land Rights",     class = "sep-qtag"),
            shiny::actionButton("qt_finc",      "Financial Inclusion",   class = "sep-qtag"),
            shiny::actionButton("qt_femhh",     "Female-Headed HH",      class = "sep-qtag"),
            shiny::actionButton("qt_dhs",       "DHS Surveys",           class = "sep-qtag"),
            shiny::actionButton("qt_eicv",      "EICV / Poverty",        class = "sep-qtag")
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
          shiny::uiOutput("sep_result_items")
        )

      ) # end sep-page
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # TAB 3 — DASHBOARD
    # ══════════════════════════════════════════════════════════════════════════
    shiny::tabPanel("Dashboard",
      shiny::div(class = "vizc-page",

        # ── JavaScript: panel open/close + dashboard tab switching ──────────────
        shiny::tags$script(shiny::HTML("
          /* ── Dashboard inner tab switching ── */
          function switchFiTab(tab) {
            var inp = document.getElementById('fi_tab');
            if (inp) { inp.value = tab; $(inp).trigger('change'); }
            document.querySelectorAll('.fi-sb__tabtn').forEach(function(b) {
              b.classList.remove('fi-sb__tabtn--active');
              if (b.getAttribute('data-tab') === tab) b.classList.add('fi-sb__tabtn--active');
            });
            setTimeout(function() { $(window).trigger('resize'); }, 150);
          }

          /* ── Governance inner tab switching ── */
          function switchGovTab(tab) {
            var inp = document.getElementById('gov_tab');
            if (inp) { inp.value = tab; $(inp).trigger('change'); }
            document.querySelectorAll('.gov-sb__tabtn').forEach(function(b) {
              b.classList.remove('gov-sb__tabtn--active');
              if (b.getAttribute('data-tab') === tab) b.classList.add('gov-sb__tabtn--active');
            });
            setTimeout(function() { $(window).trigger('resize'); }, 150);
          }

          /* ── Demography inner tab switching ── */
          function switchDemoTab(tab) {
            var inp = document.getElementById('demo_tab');
            if (inp) { inp.value = tab; $(inp).trigger('change'); }
            document.querySelectorAll('.demo-sb__tabtn').forEach(function(b) {
              b.classList.remove('demo-sb__tabtn--active');
              if (b.getAttribute('data-tab') === tab) b.classList.add('demo-sb__tabtn--active');
            });
            setTimeout(function() { $(window).trigger('resize'); }, 150);
          }

          /* ── Employment inner tab switching ── */
          function switchEmpTab(tab) {
            var inp = document.getElementById('emp_tab');
            if (inp) { inp.value = tab; $(inp).trigger('change'); }
            document.querySelectorAll('.emp-sb__tabtn').forEach(function(b) {
              b.classList.remove('emp-sb__tabtn--active');
              if (b.getAttribute('data-tab') === tab) b.classList.add('emp-sb__tabtn--active');
            });
            setTimeout(function() { $(window).trigger('resize'); }, 150);
          }

          /* ── Agriculture inner tab switching ── */
          function switchAgriTab(tab) {
            var inp = document.getElementById('agri_tab');
            if (inp) { inp.value = tab; $(inp).trigger('change'); }
            document.querySelectorAll('.agri-sb__tabtn').forEach(function(b) {
              b.classList.remove('agri-sb__tabtn--active');
              if (b.getAttribute('data-tab') === tab) b.classList.add('agri-sb__tabtn--active');
            });
            setTimeout(function() { $(window).trigger('resize'); }, 150);
          }

          /* ── Education inner tab switching ── */
          function switchEduTab(tab) {
            var inp = document.getElementById('edu_tab');
            if (inp) { inp.value = tab; $(inp).trigger('change'); }
            document.querySelectorAll('.edu-sb__tabtn').forEach(function(b) {
              b.classList.remove('edu-sb__tabtn--active');
              if (b.getAttribute('data-tab') === tab) b.classList.add('edu-sb__tabtn--active');
            });
            setTimeout(function() { $(window).trigger('resize'); }, 150);
          }

          /* ── Fullscreen sector panel ── */
          function openSectorPanel(sector) {
            var panel = document.getElementById('fi-fullscreen-panel');
            panel.classList.add('is-open');
            document.body.style.overflow = 'hidden';   // lock page scroll
            if (sector) Shiny.setInputValue('active_sector', sector, {priority: 'event'});

            if (sector === 'gov') {
              switchGovTab('overview');
            } else if (sector === 'demo') {
              switchDemoTab('overview');
            } else if (sector === 'emp') {
              switchEmpTab('overview');
            } else if (sector === 'edu') {
              switchEduTab('overview');
            } else if (sector === 'agri') {
              switchAgriTab('overview');
            } else {
              switchFiTab('overview');
            }
            setTimeout(function() { $(window).trigger('resize'); }, 400);
          }
          function closeSectorPanel() {
            document.getElementById('fi-fullscreen-panel').classList.remove('is-open');
            document.body.style.overflow = '';          // restore scroll
          }

          /* ── Data Hub sector card selection ── */
          function dhSelectSector(sector) {
            var inp = document.getElementById('dh_sector');
            if (inp) { inp.value = sector; $(inp).trigger('change'); }
            document.querySelectorAll('.dh-sector-card').forEach(function(c) {
              c.classList.remove('dh-sector-card--active');
              if (c.getAttribute('data-sector') === sector) c.classList.add('dh-sector-card--active');
            });
          }

          $(document).ready(function() {
            // Sector word click
            $(document).on('click', '.vizc-word--live', function() {
              $('.vizc-word--live').removeClass('vizc-word--active');
              $(this).addClass('vizc-word--active');
              openSectorPanel($(this).data('sector'));
            });
            // Close on Escape (back-button behaviour via keyboard)
            $(document).on('keydown', function(e) {
              if (e.key === 'Escape') closeSectorPanel();
            });
          });
        ")),

        # ── WORD CLOUD ────────────────────────────────────────────────────────
        shiny::div(class = "vizc-hero",

          shiny::div(class = "vizc-hero__top",
            shiny::div(class = "sec-eyebrow",
              shiny::tags$i(class = "fas fa-layer-group fa-xs"),
              "\u00a0 Data Visualization Center"
            ),
            shiny::tags$h1(class = "vizc-hero__title",
              "Rwanda Gender Data Explorer"
            ),
            shiny::tags$p(class = "vizc-hero__sub",
              "Click any sector to open a full gender-disaggregated microdata dashboard."
            )
          ),

          # ── True word cloud ───────────────────────────────────────────────
          shiny::div(class = "vizc-cloud",

            # Row 1 — top
            shiny::div(class = "vizc-row",
              shiny::span(class = "vizc-word vizc-word--s2 vizc-word--gray", "poverty"),
              shiny::span(class = "vizc-word vizc-word--s2 vizc-word--dark", "indicators"),
              shiny::span(class = "vizc-word vizc-word--s3 vizc-word--gray", "surveys"),
              shiny::span(class = "vizc-word vizc-word--s5 vizc-word--red vizc-word--vert vizc-word--soon", "Health"),
              shiny::span(class = "vizc-word vizc-word--s2 vizc-word--gray", "microdata"),
              shiny::span(class = "vizc-word vizc-word--s1 vizc-word--gray", "gender gap")
            ),

            # Row 2 — middle (dominant word)
            shiny::div(class = "vizc-row vizc-row--center",
              shiny::span(class = "vizc-word vizc-word--s2 vizc-word--gray", "EICV"),
              shiny::span(class = "vizc-word vizc-word--s3 vizc-word--dark vizc-word--soon", "Labor"),
              shiny::tags$span(
                class         = "vizc-word vizc-word--s7 vizc-word--main vizc-word--live vizc-word--active",
                `data-sector` = "finc",
                "Financial Inclusion"
              ),
              shiny::tags$span(
                class         = "vizc-word vizc-word--s4 vizc-word--dark vizc-word--live",
                `data-sector` = "gov",
                "Governance"
              ),
              shiny::tags$span(
                class         = "vizc-word vizc-word--s4 vizc-word--dark vizc-word--live",
                `data-sector` = "demo",
                "Demography"
              ),
              shiny::span(class = "vizc-word vizc-word--s3 vizc-word--red vizc-word--soon", "Land Rights"),
              shiny::span(class = "vizc-word vizc-word--s2 vizc-word--gray", "DHS")
            ),

            # Row 3 — bottom
            shiny::div(class = "vizc-row",
              shiny::span(class = "vizc-word vizc-word--s1 vizc-word--gray", "nutrition"),
              shiny::span(class = "vizc-word vizc-word--s3 vizc-word--dark vizc-word--soon", "Child Welfare"),
              shiny::span(class = "vizc-word vizc-word--s2 vizc-word--red vizc-word--vert vizc-word--soon", "GBV"),
              shiny::span(class = "vizc-word vizc-word--s4 vizc-word--dark vizc-word--soon", "Gender Violence"),
              shiny::span(class = "vizc-word vizc-word--s2 vizc-word--gray", "census"),
              shiny::tags$span(
                class         = "vizc-word vizc-word--s5 vizc-word--dark vizc-word--live",
                `data-sector` = "emp",
                "Employment"
              ),
              shiny::span(class = "vizc-word vizc-word--s3 vizc-word--red vizc-word--soon", "Water"),
              shiny::tags$span(
                class         = "vizc-word vizc-word--s4 vizc-word--dark vizc-word--live",
                `data-sector` = "edu",
                "Education"
              ),
              shiny::tags$span(
                class         = "vizc-word vizc-word--s3 vizc-word--green vizc-word--live",
                `data-sector` = "agri",
                "Agriculture"
              )
            )
          ),

          shiny::tags$p(class = "vizc-cloud__hint",
            shiny::tags$i(class = "fas fa-hand-pointer fa-xs"),
            "\u00a0 Click ",
            shiny::tags$strong("Financial Inclusion"),
            ", ",
            shiny::tags$strong("Governance"),
            ", ",
            shiny::tags$strong("Demography"),
            ", ",
            shiny::tags$strong("Employment"),
            ", ",
            shiny::tags$strong("Education"),
            ", or ",
            shiny::tags$strong("Agriculture"),
            " to open the dashboard"
          )
        ),

        # ── FULL-SCREEN PANEL (slides up on sector click) ─────────────────────
        shiny::div(id = "fi-fullscreen-panel", class = "vizc-panel",
          shiny::div(class = "vizc-panel__backdrop"),
          shiny::div(class = "vizc-panel__content",

            # Panel header bar
            shiny::div(class = "vizc-panel__bar",
              shiny::div(class = "vizc-panel__bar-left",
                shiny::conditionalPanel("input.active_sector == 'finc'",
                  shiny::tags$span(class = "vizc-panel__sector-badge",
                    shiny::tags$i(class = "fas fa-coins fa-xs"), " Financial Inclusion"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'gov'",
                  shiny::tags$span(class = "vizc-panel__sector-badge",
                    shiny::tags$i(class = "fas fa-landmark fa-xs"), " Governance"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'demo'",
                  shiny::tags$span(class = "vizc-panel__sector-badge",
                    shiny::tags$i(class = "fas fa-users fa-xs"), " Demography"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'emp'",
                  shiny::tags$span(class = "vizc-panel__sector-badge",
                    shiny::tags$i(class = "fas fa-briefcase fa-xs"), " Employment"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'edu'",
                  shiny::tags$span(class = "vizc-panel__sector-badge",
                    shiny::tags$i(class = "fas fa-graduation-cap fa-xs"), " Education"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'agri'",
                  shiny::tags$span(class = "vizc-panel__sector-badge",
                    shiny::tags$i(class = "fas fa-seedling fa-xs"), " Agriculture"
                  )
                ),

                shiny::conditionalPanel("input.active_sector == 'finc'",
                  shiny::tags$span(class = "vizc-panel__bar-title",
                    "FinScope 2024 Rwanda \u00b7 Gender Dashboard"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'gov'",
                  shiny::tags$span(class = "vizc-panel__bar-title",
                    "Rwanda Gender Governance \u00b7 Dashboard"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'demo'",
                  shiny::tags$span(class = "vizc-panel__bar-title",
                    "Rwanda \u00b7 Gender demography (Census 2022)"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'emp'",
                  shiny::tags$span(class = "vizc-panel__bar-title",
                    "Rwanda Labour Force Survey (LFS) 2025 Q4 \u00b7 Gender & Employment"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'edu'",
                  shiny::tags$span(class = "vizc-panel__bar-title",
                    "Rwanda PHC 2022 \u00b7 Gender & Education"
                  )
                ),
                shiny::conditionalPanel("input.active_sector == 'agri'",
                  shiny::tags$span(class = "vizc-panel__bar-title",
                    "Rwanda AHS 2020 \u00b7 Gender & Agriculture"
                  )
                )
              ),
              shiny::tags$button(
                class   = "vizc-panel__close",
                onclick = "closeSectorPanel()",
                shiny::tags$i(class = "fas fa-arrow-left fa-xs"),
                " Back"
              )
            ),

            # ── Dashboard: tabs-only sidebar | filter bar + big charts ──
            shiny::div(class = "fi-dash",

              # ━━━ LEFT: TABS ONLY (no content, just navigation) ━━━━━━━━
              shiny::div(class = "fi-sb",

              shiny::conditionalPanel("input.active_sector == 'finc'",
                shiny::div(class = "fi-sb__tabnav",
                  shiny::tags$button(
                    class = "fi-sb__tabtn fi-sb__tabtn--active",
                    `data-tab` = "overview",
                    onclick = "switchFiTab('overview')",
                    shiny::tags$i(class = "fas fa-tachometer-alt fa-fw"),
                    " Overview"
                  ),
                  shiny::tags$button(
                    class = "fi-sb__tabtn", `data-tab` = "products",
                    onclick = "switchFiTab('products')",
                    shiny::tags$i(class = "fas fa-university fa-fw"),
                    " Financial Products"
                  ),
                  shiny::tags$button(
                    class = "fi-sb__tabtn", `data-tab` = "savings",
                    onclick = "switchFiTab('savings')",
                    shiny::tags$i(class = "fas fa-piggy-bank fa-fw"),
                    " Savings & Credit"
                  ),
                  shiny::tags$button(
                    class = "fi-sb__tabtn", `data-tab` = "digital",
                    onclick = "switchFiTab('digital')",
                    shiny::tags$i(class = "fas fa-mobile-alt fa-fw"),
                    " Digital Finance"
                  ),
                  shiny::tags$button(
                    class = "fi-sb__tabtn", `data-tab` = "wellbeing",
                    onclick = "switchFiTab('wellbeing')",
                    shiny::tags$i(class = "fas fa-balance-scale fa-fw"),
                    " Wellbeing"
                  )
                )
              ),
              shiny::conditionalPanel("input.active_sector == 'gov'",
                shiny::div(class = "fi-sb__tabnav fi-sb__tabnav--gov",
                  shiny::tags$button(
                    class = "gov-sb__tabtn gov-sb__tabtn--active",
                    `data-tab` = "overview",
                    onclick = "switchGovTab('overview')",
                    shiny::tags$i(class = "fas fa-balance-scale fa-fw"),
                    " Overview"
                  ),
                  shiny::tags$button(
                    class = "gov-sb__tabtn", `data-tab` = "ministers",
                    onclick = "switchGovTab('ministers')",
                    shiny::tags$i(class = "fas fa-user-tie fa-fw"),
                    " Cabinet"
                  ),
                  shiny::tags$button(
                    class = "gov-sb__tabtn", `data-tab` = "parliament",
                    onclick = "switchGovTab('parliament')",
                    shiny::tags$i(class = "fas fa-landmark fa-fw"),
                    " Parliament"
                  ),
                  shiny::tags$button(
                    class = "gov-sb__tabtn", `data-tab` = "prosecutors",
                    onclick = "switchGovTab('prosecutors')",
                    shiny::tags$i(class = "fas fa-gavel fa-fw"),
                    " Prosecutors"
                  ),
                  shiny::tags$button(
                    class = "gov-sb__tabtn", `data-tab` = "judiciary",
                    onclick = "switchGovTab('judiciary')",
                    shiny::tags$i(class = "fas fa-scale-balanced fa-fw"),
                    " Judiciary"
                  ),
                  shiny::tags$button(
                    class = "gov-sb__tabtn", `data-tab` = "local",
                    onclick = "switchGovTab('local')",
                    shiny::tags$i(class = "fas fa-city fa-fw"),
                    " Local leaders"
                  )
                )
              ),
              shiny::conditionalPanel("input.active_sector == 'demo'",
                shiny::div(class = "fi-sb__tabnav",
                  shiny::tags$button(
                    class = "demo-sb__tabtn demo-sb__tabtn--active",
                    `data-tab` = "overview",
                    onclick = "switchDemoTab('overview')",
                    shiny::tags$i(class = "fas fa-tachometer-alt fa-fw"),
                    " Overview"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "population",
                    onclick = "switchDemoTab('population')",
                    shiny::tags$i(class = "fas fa-chart-column fa-fw"),
                    " Population"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "age",
                    onclick = "switchDemoTab('age')",
                    shiny::tags$i(class = "fas fa-layer-group fa-fw"),
                    " Age Structure"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "internet",
                    onclick = "switchDemoTab('internet')",
                    shiny::tags$i(class = "fas fa-wifi fa-fw"),
                    " Internet Use"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "education",
                    onclick = "switchDemoTab('education')",
                    shiny::tags$i(class = "fas fa-graduation-cap fa-fw"),
                    " Education"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "intervention",
                    onclick = "switchDemoTab('intervention')",
                    shiny::tags$i(class = "fas fa-bullseye fa-fw"),
                    " Programme ages"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "school718",
                    onclick = "switchDemoTab('school718')",
                    shiny::tags$i(class = "fas fa-school fa-fw"),
                    " School-age 7–18"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "school1318",
                    onclick = "switchDemoTab('school1318')",
                    shiny::tags$i(class = "fas fa-book-open fa-fw"),
                    " Adolescents 13–18"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "youth",
                    onclick = "switchDemoTab('youth')",
                    shiny::tags$i(class = "fas fa-person-running fa-fw"),
                    " Youth population"
                  ),
                  shiny::tags$button(
                    class = "demo-sb__tabtn", `data-tab` = "elderly",
                    onclick = "switchDemoTab('elderly')",
                    shiny::tags$i(class = "fas fa-user-clock fa-fw"),
                    " Elderly Share"
                  )
                )
              ),

              shiny::conditionalPanel("input.active_sector == 'emp'",
                shiny::div(class = "fi-sb__tabnav fi-sb__tabnav--emp",
                  shiny::tags$button(
                    class = "emp-sb__tabtn emp-sb__tabtn--active", `data-tab` = "overview",
                    onclick = "switchEmpTab('overview')",
                    shiny::tags$i(class = "fas fa-tachometer-alt fa-fw"), " Overview"
                  ),
                  shiny::tags$button(
                    class = "emp-sb__tabtn", `data-tab` = "labourforce",
                    onclick = "switchEmpTab('labourforce')",
                    shiny::tags$i(class = "fas fa-chart-line fa-fw"), " Labour Force"
                  ),
                  shiny::tags$button(
                    class = "emp-sb__tabtn", `data-tab` = "youth",
                    onclick = "switchEmpTab('youth')",
                    shiny::tags$i(class = "fas fa-user-graduate fa-fw"), " Youth & NEET"
                  ),
                  shiny::tags$button(
                    class = "emp-sb__tabtn", `data-tab` = "occupations",
                    onclick = "switchEmpTab('occupations')",
                    shiny::tags$i(class = "fas fa-briefcase fa-fw"), " Occupations"
                  ),
                  shiny::tags$button(
                    class = "emp-sb__tabtn", `data-tab` = "status",
                    onclick = "switchEmpTab('status')",
                    shiny::tags$i(class = "fas fa-id-badge fa-fw"), " Employment Status"
                  ),
                  shiny::tags$button(
                    class = "emp-sb__tabtn", `data-tab` = "education",
                    onclick = "switchEmpTab('education')",
                    shiny::tags$i(class = "fas fa-graduation-cap fa-fw"), " Education"
                  ),
                  shiny::tags$button(
                    class = "emp-sb__tabtn", `data-tab` = "agriculture",
                    onclick = "switchEmpTab('agriculture')",
                    shiny::tags$i(class = "fas fa-seedling fa-fw"), " Agriculture"
                  ),
                  shiny::tags$button(
                    class = "emp-sb__tabtn", `data-tab` = "sectors",
                    onclick = "switchEmpTab('sectors')",
                    shiny::tags$i(class = "fas fa-industry fa-fw"), " Economic Sectors"
                  )
                )
              ),

                # Hidden inputs for sector + tab switching
                shiny::div(style = "display:none",
                  shiny::textInput("active_sector", NULL, value = "finc"),
                  shiny::textInput("fi_tab", NULL, value = "overview"),
                  shiny::textInput("gov_tab", NULL, value = "overview"),
                  shiny::textInput("demo_tab", NULL, value = "overview"),
                  shiny::textInput("emp_tab", NULL, value = "overview"),
                  shiny::textInput("edu_tab", NULL, value = "overview")
                ),

              shiny::conditionalPanel("input.active_sector == 'edu'",
                shiny::div(class = "fi-sb__tabnav fi-sb__tabnav--edu",
                  shiny::tags$button(
                    class = "edu-sb__tabtn edu-sb__tabtn--active", `data-tab` = "overview",
                    onclick = "switchEduTab('overview')",
                    shiny::tags$i(class = "fas fa-tachometer-alt fa-fw"), " Overview"
                  ),
                  shiny::tags$button(
                    class = "edu-sb__tabtn", `data-tab` = "attainment",
                    onclick = "switchEduTab('attainment')",
                    shiny::tags$i(class = "fas fa-certificate fa-fw"), " Attainment"
                  ),
                  shiny::tags$button(
                    class = "edu-sb__tabtn", `data-tab` = "trend",
                    onclick = "switchEduTab('trend')",
                    shiny::tags$i(class = "fas fa-history fa-fw"), " Historical Trend"
                  ),
                  shiny::tags$button(
                    class = "edu-sb__tabtn", `data-tab` = "attendance",
                    onclick = "switchEduTab('attendance')",
                    shiny::tags$i(class = "fas fa-school fa-fw"), " School Attendance"
                  ),
                  shiny::tags$button(
                    class = "edu-sb__tabtn", `data-tab` = "literacy",
                    onclick = "switchEduTab('literacy')",
                    shiny::tags$i(class = "fas fa-book-open fa-fw"), " Literacy"
                  ),
                  shiny::tags$button(
                    class = "edu-sb__tabtn", `data-tab` = "digital",
                    onclick = "switchEduTab('digital')",
                    shiny::tags$i(class = "fas fa-laptop fa-fw"), " Digital Access"
                  ),
                  shiny::tags$button(
                    class = "edu-sb__tabtn", `data-tab` = "disability",
                    onclick = "switchEduTab('disability')",
                    shiny::tags$i(class = "fas fa-wheelchair fa-fw"), " Disability"
                  )
                )
              )

              ,shiny::conditionalPanel("input.active_sector == 'agri'",
                shiny::div(class = "fi-sb__tabnav fi-sb__tabnav--agri",
                  shiny::textInput("agri_tab", NULL, value = "overview"),
                  shiny::tags$button(
                    class = "agri-sb__tabtn agri-sb__tabtn--active", `data-tab` = "overview",
                    onclick = "switchAgriTab('overview')",
                    shiny::tags$i(class = "fas fa-tachometer-alt fa-fw"), " Overview"
                  ),
                  shiny::tags$button(
                    class = "agri-sb__tabtn", `data-tab` = "land",
                    onclick = "switchAgriTab('land')",
                    shiny::tags$i(class = "fas fa-map fa-fw"), " Land Access"
                  ),
                  shiny::tags$button(
                    class = "agri-sb__tabtn", `data-tab` = "extension",
                    onclick = "switchAgriTab('extension')",
                    shiny::tags$i(class = "fas fa-chalkboard-teacher fa-fw"), " Extension Services"
                  ),
                  shiny::tags$button(
                    class = "agri-sb__tabtn", `data-tab` = "inputs",
                    onclick = "switchAgriTab('inputs')",
                    shiny::tags$i(class = "fas fa-flask fa-fw"), " Inputs & Practices"
                  ),
                  shiny::tags$button(
                    class = "agri-sb__tabtn", `data-tab` = "workers",
                    onclick = "switchAgriTab('workers')",
                    shiny::tags$i(class = "fas fa-chart-line fa-fw"), " Workers Trend"
                  ),
                  shiny::tags$button(
                    class = "agri-sb__tabtn", `data-tab` = "livestock",
                    onclick = "switchAgriTab('livestock')",
                    shiny::tags$i(class = "fas fa-horse fa-fw"), " Livestock"
                  ),
                  shiny::tags$button(
                    class = "agri-sb__tabtn", `data-tab` = "programs",
                    onclick = "switchAgriTab('programs')",
                    shiny::tags$i(class = "fas fa-hand-holding-heart fa-fw"), " Programs"
                  )
                )
              )

              ), # end fi-sb

              # ━━━ RIGHT: FILTER BAR + BIG CHARTS ━━━━━━━━━━━━━━━━━━━━━━━
              shiny::div(class = "fi-main",
                shiny::conditionalPanel("input.active_sector == 'finc'",

                # ── All Filters bar (global + tab-specific) ───────────────
                shiny::div(class = "fi-gbar",
                  shiny::div(class = "fi-gbar__inner",

                    # Core location + age filters (always visible)
                    shiny::div(class = "fi-gbar__filters",
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-map-marker-alt fa-xs"),
                          " Province"
                        ),
                        shiny::selectInput("fi_province", NULL,
                          choices  = c("All Provinces" = "all",
                                       "Kigali", "South", "West", "North", "East"),
                          selected = "all", width = "135px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-map fa-xs"), " District"
                        ),
                        shiny::selectInput("fi_district", NULL,
                          choices  = c("All Districts" = "all"),
                          selected = "all", width = "145px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-city fa-xs"), " Area"
                        ),
                        shiny::selectInput("fi_area", NULL,
                          choices  = c("All Areas" = "all", "Urban", "Rural"),
                          selected = "all", width = "110px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item fi-gbar__item--slider",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-user-clock fa-xs"),
                          " Age Range"
                        ),
                        shiny::sliderInput("fi_age", NULL,
                          min = 16, max = 90, value = c(16, 90),
                          step = 1, width = "190px"
                        )
                      ),

                      # Tab-specific extra filters (appear in the same bar)
                      shiny::conditionalPanel("input.fi_tab == 'digital'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-layer-group fa-xs"),
                            " Age Groups"
                          ),
                          shiny::selectInput("fi_age_groups", NULL,
                            choices = c(
                              "16\u201324" = "16-24", "25\u201334" = "25-34",
                              "35\u201344" = "35-44", "45\u201354" = "45-54",
                              "55+"        = "55+"
                            ),
                            selected = c("16-24", "25-34", "35-44", "45-54", "55+"),
                            multiple = TRUE, width = "180px"
                          )
                        )
                      ),
                      shiny::conditionalPanel("input.fi_tab == 'wellbeing'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-highlighter fa-xs"),
                            " Highlight Trend"
                          ),
                          shiny::selectInput("fi_trend_hl", NULL,
                            choices  = c("All Trends" = "all",
                                         "Improved", "Worsened", "Unchanged"),
                            selected = "all", width = "140px"
                          )
                        )
                      )
                    ),

                    # Right side: count + reset
                    shiny::div(class = "fi-gbar__right",
                      shiny::div(class = "fi-gbar__count",
                        shiny::tags$i(class = "fas fa-users fa-xs"), " ",
                        shiny::textOutput("fi_sample_n", inline = TRUE)
                      ),
                      shiny::actionButton("fi_reset", "Reset",
                        class = "fi-gbar__reset"
                      )
                    )
                  )
                ),

                # ── Charts area (scrollable, large) ───────────────────────
                shiny::div(class = "fi-charts",

                  # ── Tab: Overview ─────────────────────────────────────
                  shiny::conditionalPanel("input.fi_tab == 'overview'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-tachometer-alt fa-xs"),
                        " Overview"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Gender Gap at a Glance"
                      )
                    ),
                    shiny::uiOutput("fi_kpis"),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-map fa-xs"),
                        " Formal Financial Inclusion by Province & Gender"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("fi_province_gender", height = "440px")
                      )
                    )
                  ),

                  # ── Tab: Financial Products ───────────────────────────
                  shiny::conditionalPanel("input.fi_tab == 'products'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-university fa-xs"),
                        " Financial Products"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Product Uptake by Gender"
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Which financial services do men and women use? (%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("fi_product_gender", height = "520px")
                      )
                    )
                  ),

                  # ── Tab: Savings & Credit ─────────────────────────────
                  shiny::conditionalPanel("input.fi_tab == 'savings'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-piggy-bank fa-xs"),
                        " Savings & Credit"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Savings Behavior & Credit Access"
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-pie fa-xs"),
                          " Savings Frequency by Gender"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("fi_savings_gender", height = "440px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-hand-holding-usd fa-xs"),
                          " Credit Access by Gender (%)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("fi_credit_gender", height = "440px")
                        )
                      )
                    )
                  ),

                  # ── Tab: Digital Finance ──────────────────────────────
                  shiny::conditionalPanel("input.fi_tab == 'digital'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-mobile-alt fa-xs"),
                        " Digital Finance"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Mobile Money & Age Profile"
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-wifi fa-xs"),
                          " Mobile Money Uptake \u2014 Urban vs Rural"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("fi_mm_area", height = "440px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                          " Financial Inclusion by Age Group"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("fi_age_gender", height = "440px")
                        )
                      )
                    )
                  ),

                  # ── Tab: Wellbeing ────────────────────────────────────
                  shiny::conditionalPanel("input.fi_tab == 'wellbeing'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-balance-scale fa-xs"),
                        " Financial Wellbeing"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Financial Control & Trends"
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-sliders-h fa-xs"),
                          " Degree of Financial Control by Gender"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("fi_control_gender", height = "440px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-arrow-trend-up fa-xs"),
                          " Financial Situation Trend (past 12 months)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("fi_trend_gender", height = "440px")
                        )
                      )
                    )
                  ),

                  # Data footnote
                  shiny::div(class = "fi-data-note",
                    shiny::tags$i(class = "fas fa-info-circle fa-xs"),
                    " FinScope Consumer Survey 2024, NISR. 13,994 respondents aged 16+. ",
                    "Unweighted proportions from microdata."
                  )

                ) # end fi-charts

              ), # end conditionalPanel finc

              shiny::conditionalPanel("input.active_sector == 'gov'",

                # ── Governance filters ───────────────────────────────
                shiny::div(class = "fi-gbar",
                  shiny::div(class = "fi-gbar__inner",
                    shiny::div(class = "fi-gbar__filters",
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-calendar-alt fa-xs"),
                          " Years"
                        ),
                        shiny::sliderInput("gov_year", NULL,
                          min = 2003, max = 2024, value = c(2003, 2024),
                          step = 1, sep = "", width = "210px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-user fa-xs"),
                          " Gender"
                        ),
                        shiny::checkboxGroupInput("gov_sex", NULL,
                          choices = c("Female" = "Female", "Male" = "Male"),
                          selected = c("Female", "Male"),
                          inline = TRUE, width = "160px"
                        )
                      ),
                      shiny::conditionalPanel("input.gov_tab == 'judiciary'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-landmark fa-xs"),
                            " Institution"
                          ),
                          shiny::selectInput("gov_entity", NULL,
                            choices = c("All" = "all"),
                            selected = "all",
                            width = "220px"
                          )
                        )
                      ),
                      shiny::conditionalPanel("input.gov_tab == 'local'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-user-tie fa-xs"),
                            " Position"
                          ),
                          shiny::selectInput("gov_position", NULL,
                            choices = c("All" = "all"),
                            selected = "all",
                            width = "240px"
                          )
                        )
                      )
                    ),
                    shiny::div(class = "fi-gbar__right",
                      shiny::div(class = "fi-gbar__count",
                        shiny::tags$i(class = "fas fa-users fa-xs"), " ",
                        shiny::textOutput("gov_sample_n", inline = TRUE)
                      ),
                      shiny::actionButton("gov_reset", "Reset",
                        class = "fi-gbar__reset"
                      )
                    )
                  )
                ),

                # ── Charts area (Governance) ────────────────────────
                shiny::div(class = "fi-charts",

                  # Overview — KPI snapshot only (charts live in dedicated tabs)
                  shiny::conditionalPanel("input.gov_tab == 'overview'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-balance-scale fa-xs"),
                        " Governance Overview"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Gender balance at a glance")
                    ),
                    shiny::uiOutput("gov_overview_kpis"),
                    shiny::div(class = "fi-data-note gov-overview-viz-hint",
                      shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                      shiny::tags$strong(" Visualisations: "),
                      "Open ",
                      shiny::tags$strong("Cabinet"), ", ",
                      shiny::tags$strong("Parliament"), ", ",
                      shiny::tags$strong("Prosecutors"), ", ",
                      shiny::tags$strong("Judiciary"), ", or ",
                      shiny::tags$strong("Local leaders"),
                      " in the sidebar for the full time series and charts."
                    )
                  ),

                  # Cabinet / ministers
                  shiny::conditionalPanel("input.gov_tab == 'ministers'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-user-tie fa-xs"),
                        " Executive"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Gender equality in ministerial roles")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-users fa-xs"),
                        " Share of ministerial roles by sex (%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("gov_ministers_gender", height = "440px")
                      )
                    )
                  ),

                  # Parliament / Senate
                  shiny::conditionalPanel("input.gov_tab == 'parliament'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-landmark fa-xs"),
                        " Legislature"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Women’s share of Senate and Parliament seats")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-landmark fa-xs"),
                        " Seat share by sex (%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("gov_parliament_gender", height = "440px")
                      )
                    )
                  ),

                  # National prosecutors
                  shiny::conditionalPanel("input.gov_tab == 'prosecutors'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-gavel fa-xs"),
                        " Prosecution service"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "National prosecutors by sex")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-gavel fa-xs"),
                        " Prosecutor posts — female vs male share (%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("gov_prosecutors_gender", height = "440px")
                      )
                    )
                  ),

                  # Judiciary & related institutions
                  shiny::conditionalPanel("input.gov_tab == 'judiciary'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-scale-balanced fa-xs"),
                        " Judiciary & institutions"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Representation across institutions")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-landmark fa-xs"),
                        " Women’s and men’s shares by institution (%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("gov_judiciary_gender", height = "460px")
                      )
                    )
                  ),

                  # Local government
                  shiny::conditionalPanel("input.gov_tab == 'local'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-city fa-xs"),
                        " Local Government"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Local leaders by position and year")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                        " Share of posts by sex — trends across survey years"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("gov_local_heatmap", height = "520px")
                      ),
                      shiny::div(class = "fi-data-note gov-local-chart-note",
                        shiny::tags$i(class = "fas fa-info-circle fa-xs"),
                        shiny::tags$strong(" How to read: "),
                        "With ",
                        shiny::tags$strong("Position = All"),
                        ", each line is ",
                        shiny::tags$strong("women’s share"),
                        " of posts in that role (men’s share is the remainder to 100%). ",
                        "Choose a single position to compare ",
                        shiny::tags$strong("Male vs Female"),
                        " lines for that role."
                      )
                    )
                  )

                ) # end gov fi-charts
              ), # end conditionalPanel gov

              # ───────────────────────────────────────────────────────────
              # Demography sector
              # ───────────────────────────────────────────────────────────
              shiny::conditionalPanel("input.active_sector == 'demo'",
                # ── All Filters bar (Demography) ──────────────────────────
                shiny::div(class = "fi-gbar",
                  shiny::div(class = "fi-gbar__inner",
                    shiny::div(class = "fi-gbar__filters",
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-map-marker-alt fa-xs"),
                          " Province"
                        ),
                        shiny::selectInput("demo_province", NULL,
                          choices  = c("All Provinces" = "all",
                                       "City of Kigali" = "City of Kigali",
                                       "Southern Province" = "Southern Province",
                                       "Western Province" = "Western Province",
                                       "Northern Province" = "Northern Province",
                                       "Eastern Province" = "Eastern Province"),
                          selected = "all", width = "135px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-map fa-xs"), " District"
                        ),
                        shiny::selectInput("demo_district", NULL,
                          choices  = c("All Districts" = "all"),
                          selected = "all", width = "145px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-city fa-xs"), " Residence"
                        ),
                        shiny::selectInput("demo_residence", NULL,
                          choices = c("Rwanda (All)" = "Rwanda", "Urban" = "Urban", "Rural" = "Rural"),
                          selected = "Rwanda", width = "140px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-user fa-xs"), " Gender"
                        ),
                        shiny::checkboxGroupInput("demo_sex", NULL,
                          choices  = c("Female" = "Female", "Male" = "Male"),
                          selected = c("Female", "Male"),
                          inline = TRUE, width = "155px"
                        )
                      ),
                      shiny::conditionalPanel("input.demo_tab == 'population' || input.demo_tab == 'overview'",
                        shiny::div(class = "fi-gbar__item fi-gbar__item--slider",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-calendar-alt fa-xs"),
                            " Years"
                          ),
                          shiny::sliderInput("demo_years", NULL,
                            min = 1978, max = 2022, value = c(1978, 2022),
                            step = 1, sep = "", width = "240px"
                          )
                        )
                      ),

                      shiny::conditionalPanel("input.demo_tab == 'age'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-layer-group fa-xs"),
                            " Age groups"
                          ),
                          shiny::selectInput("demo_age_groups", NULL,
                            choices = c(
                              "Total" = "Total",
                              "0-4" = "0-4", "5-9" = "5-9", "10-14" = "10-14",
                              "15-19" = "15-19", "20-24" = "20-24", "25-29" = "25-29",
                              "30-34" = "30-34", "35-39" = "35-39", "40-44" = "40-44",
                              "45-49" = "45-49", "50-54" = "50-54", "55-59" = "55-59",
                              "60-64" = "60-64", "65-69" = "65-69", "70-74" = "70-74",
                              "75-79" = "75-79", "80-84" = "80-84", "85+" = "85+"
                            ),
                            selected = c("0-4", "5-9", "10-14", "15-19", "20-24"),
                            multiple = TRUE, width = "250px"
                          )
                        )
                      ),

                      shiny::conditionalPanel("input.demo_tab == 'internet'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-wifi fa-xs"),
                            " Internet age band"
                          ),
                          shiny::selectInput("demo_internet_age_group", NULL,
                            choices = c(
                              "Population 10 years and above" = "Population 10 years and above",
                              "Population 16 years and above" = "Population 16 years and above"
                            ),
                            selected = "Population 10 years and above", width = "240px"
                          )
                        )
                      )
                      ,
                      shiny::conditionalPanel("input.demo_tab == 'education'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-school fa-xs"),
                            " Education residence"
                          ),
                          shiny::selectInput("demo_education_residence", NULL,
                            choices = c(
                              "Rwanda (All)" = "Rwanda",
                              "Urban"         = "Urban",
                              "Rural"         = "Rural"
                            ),
                            selected = "Rwanda", width = "200px"
                          )
                        )
                      ),
                      shiny::conditionalPanel("input.demo_tab == 'education'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-layer-group fa-xs"),
                            " Education levels"
                          ),
                          shiny::selectInput("demo_education_levels", NULL,
                            choices = c(
                              "Never attended School" = "Never attended School",
                              "Pre-nursery/ECD"       = "Pre-nursery/ECD",
                              "Nursery"               = "Nursery",
                              "Primary"              = "Primary",
                              "Lower secondary"      = "Lower secondary",
                              "Upper secondary"      = "Upper secondary",
                              "University"           = "University",
                              "INGOBOKA/Vocational"  = "INGOBOKA/Vocational",
                              "Not Stated"           = "Not Stated"
                            ),
                            selected = c(
                              "Never attended School",
                              "Pre-nursery/ECD",
                              "Nursery",
                              "Primary",
                              "Lower secondary",
                              "Upper secondary",
                              "University",
                              "INGOBOKA/Vocational",
                              "Not Stated"
                            ),
                            multiple = TRUE, width = "290px"
                          )
                        )
                      ),
                      shiny::conditionalPanel("input.demo_tab == 'intervention'",
                        shiny::div(class = "fi-gbar__item",
                          shiny::tags$label(class = "fi-gbar__lbl",
                            shiny::tags$i(class = "fas fa-sliders-h fa-xs"),
                            " Measure"
                          ),
                          shiny::selectInput("demo_t3_measure", NULL,
                            choices = c(
                              "Headcount" = "count",
                              "Share of population (%)" = "pct"
                            ),
                            selected = "pct", width = "220px"
                          )
                        )
                      )
                    ),

                    shiny::div(class = "fi-gbar__right",
                      shiny::div(class = "fi-gbar__count",
                        shiny::tags$i(class = "fas fa-users fa-xs"), " ",
                        shiny::textOutput("demo_sample_n", inline = TRUE)
                      ),
                      shiny::actionButton("demo_reset", "Reset",
                        class = "fi-gbar__reset"
                      )
                    )
                  )
                ),

                # ── Charts area (Demography) ───────────────────────────────
                shiny::div(class = "fi-charts",
                  # Overview
                  shiny::conditionalPanel("input.demo_tab == 'overview'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-tachometer-alt fa-xs"),
                        " Demography Overview"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Rwanda gender snapshot — population & key indicators")
                    ),
                    shiny::uiOutput("demo_overview_kpis"),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-users fa-xs"),
                          " Population by gender (Male & Female counts)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_pop_gender_overview", height = "420px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-user-clock fa-xs"),
                          " Elderly share (gender-disaggregated)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_elderly_share_overview", height = "420px")
                        )
                      )
                    )
                  ),

                  # Population tab
                  shiny::conditionalPanel("input.demo_tab == 'population'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-chart-column fa-xs"),
                        " Population"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Population growth & structure by gender")
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-users fa-xs"),
                          " Population counts by gender"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_pop_gender", height = "420px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-arrow-trend-up fa-xs"),
                          " Population change over years"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_pop_change", height = "420px")
                        )
                      )
                    )
                  ),

                  # Age structure tab
                  shiny::conditionalPanel("input.demo_tab == 'age'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-layer-group fa-xs"),
                        " Age Structure"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Age distribution by gender and residence")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-people-group fa-xs"),
                        " Age distribution (counts)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("demo_age_structure", height = "460px")
                      )
                    )
                  ),

                  # Internet tab
                  shiny::conditionalPanel("input.demo_tab == 'internet'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-wifi fa-xs"),
                        " Internet Use"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Internet usage by gender across provinces")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Internet users (counts)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("demo_internet_use", height = "470px")
                      )
                    )
                  ),

                  # Education tab
                  shiny::conditionalPanel("input.demo_tab == 'education'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-graduation-cap fa-xs"),
                        " Education Attendance"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Education attendance by gender and residence")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Male vs Female attendance share (%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("demo_education_attendance", height = "480px")
                      )
                    )
                  ),

                  # Elderly tab
                  shiny::conditionalPanel("input.demo_tab == 'elderly'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-user-clock fa-xs"),
                        " Elderly Share"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title", "Ageing indicators by location and gender")
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-map-marker-alt fa-xs"),
                        " Elderly share (percent)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("demo_elderly_share", height = "480px")
                      )
                    )
                  ),

                  # Programme age groups (policy / intervention targets)
                  shiny::conditionalPanel("input.demo_tab == 'intervention'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-bullseye fa-xs"),
                        " Population structure"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Programme age groups — national distribution by gender"
                      )
                    ),
                    shiny::uiOutput("demo_intervention_kpis"),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-column fa-xs"),
                          " Distribution by gender (selected measure)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_intervention_bars", height = "440px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-area fa-xs"),
                          " Difference in population share (female minus male, percentage points)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_intervention_gap", height = "440px")
                        )
                      )
                    )
                  ),

                  # School-age attendance 7–18
                  shiny::conditionalPanel("input.demo_tab == 'school718'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-school fa-xs"),
                        " School participation"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Attendance among children aged 7–18 — by residence and gender"
                      )
                    ),
                    shiny::uiOutput("demo_school718_kpis"),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-th-large fa-xs"),
                        " National, urban, and rural profiles — use chart tools to zoom or pan"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("demo_school718_facets", height = "480px")
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-water fa-xs"),
                        " Full distribution of attendance status — selected residence (sums to 100%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("demo_school718_stacked", height = "420px")
                      )
                    )
                  ),

                  # Adolescent schooling 13–18
                  shiny::conditionalPanel("input.demo_tab == 'school1318'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-book-open fa-xs"),
                        " Adolescent education"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Share of adolescents aged 13–18 currently in school — by province, district, and gender"
                      )
                    ),
                    shiny::uiOutput("demo_school1318_kpis"),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-bars fa-xs"),
                          " Enrolment rate by area — male and female"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_school1318_bars", height = "480px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-arrows-alt-h fa-xs"),
                          " Gender disparity (female minus male, percentage points)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_school1318_gap", height = "480px")
                        )
                      )
                    )
                  ),

                  # Youth population share
                  shiny::conditionalPanel("input.demo_tab == 'youth'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-person-running fa-xs"),
                        " Demographic composition"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Youth as a share of the population — provincial and district comparison by gender"
                      )
                    ),
                    shiny::uiOutput("demo_youth_kpis"),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-bars fa-xs"),
                          " Youth share by area — male and female"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_youth_bars", height = "480px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-arrows-alt-h fa-xs"),
                          " Gender disparity (female minus male, percentage points)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("demo_youth_gap", height = "480px")
                        )
                      )
                    )
                  )
                )
              ) # end conditionalPanel demo

              # ─────────────────────────────────────────────────────────────
              # Employment sector
              # ─────────────────────────────────────────────────────────────
              ,shiny::conditionalPanel("input.active_sector == 'emp'",

                # ── Filter bar ──────────────────────────────────────────────
                shiny::div(class = "fi-gbar",
                  shiny::div(class = "fi-gbar__inner",
                    shiny::div(class = "fi-gbar__filters",

                      shiny::div(class = "fi-gbar__item fi-gbar__item--slider",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-calendar-alt fa-xs"), " Year Range"
                        ),
                        shiny::sliderInput("emp_year", NULL,
                          min = 2019L, max = 2025L, value = c(2019L, 2025L),
                          step = 1L, sep = "", width = "200px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-calendar-check fa-xs"), " Quarter"
                        ),
                        shiny::selectInput("emp_quarter", NULL,
                          choices  = c("Annual average" = "all", "Q1", "Q2", "Q3", "Q4"),
                          selected = "all", width = "140px"
                        )
                      ),
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-venus-mars fa-xs"), " Sex"
                        ),
                        shiny::checkboxGroupInput("emp_sex", NULL,
                          choices  = c("Female", "Male"),
                          selected = c("Female", "Male"),
                          inline   = TRUE
                        )
                      )
                    ),
                    shiny::div(class = "fi-gbar__count",
                      shiny::uiOutput("emp_sample_n")
                    )
                  )
                ),

                # ── Charts area ─────────────────────────────────────────────
                shiny::div(class = "fi-charts",

                  # ── OVERVIEW tab ─────────────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'overview'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-briefcase fa-xs"), " Rwanda LFS"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Employment overview — gender at a glance"
                      )
                    ),
                    shiny::uiOutput("emp_overview_kpis"),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                          " Labour Force Participation Rate by sex (quarterly trend)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_lfpr_trend", height = "340px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                          " Unemployment Rate by sex (quarterly trend)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_unemp_trend", height = "340px")
                        )
                      )
                    )
                  ),

                  # ── LABOUR FORCE tab ─────────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'labourforce'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-chart-line fa-xs"), " Labour Market"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Labour Force indicators — Male vs Female quarterly trends"
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-users fa-xs"),
                          " Labour Force Participation Rate (%)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_lf_lfpr", height = "360px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-hard-hat fa-xs"),
                          " Employment-to-Population Ratio (%)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_lf_emppop", height = "360px")
                        )
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-user-times fa-xs"),
                          " Unemployment Rate — LU1 (%)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_lf_unemp", height = "360px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                          " Monthly earnings at main job (Rwf median)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_lf_earnings", height = "360px")
                        )
                      )
                    )
                  ),

                  # ── YOUTH tab ─────────────────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'youth'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-user-graduate fa-xs"), " Youth Labour Market"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Youth (16-30) vs Adult (31-64) — key indicators"
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                          " Labour Force Participation Rate — Youth vs Adult"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_youth_lfpr", height = "360px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-user-times fa-xs"),
                          " Unemployment Rate — Youth vs Adult"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_youth_unemp", height = "360px")
                        )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-exclamation-circle fa-xs"),
                        " NEET Rate — Youth not in Employment, Education or Training (%)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("emp_youth_neet", height = "340px")
                      ),
                      shiny::div(class = "fi-data-note",
                        shiny::tags$i(class = "fas fa-info-circle fa-xs"),
                        " NEET rate shown for Youth (16-30 years) across all available quarters."
                      )
                    )
                  ),

                  # ── OCCUPATIONS tab ───────────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'occupations'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-briefcase fa-xs"), " Occupation Structure"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Employed population by occupation group — Male vs Female"
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-bars fa-xs"),
                        " Share of employed by occupation — averaged over selected period"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("emp_occupations_bar", height = "480px")
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-arrows-alt-h fa-xs"),
                        " Gender gap by occupation (Female % minus Male %, percentage points)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("emp_occupations_gap", height = "380px")
                      )
                    )
                  ),

                  # ── STATUS tab ────────────────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'status'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-id-badge fa-xs"), " Employment Relationship"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Status in employment — Male vs Female"
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                          " Share by employment status — period average"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_status_bar", height = "420px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                          " Wage employment (Employee %) trend — Male vs Female"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_status_wage_trend", height = "420px")
                        )
                      )
                    )
                  ),

                  # ── EDUCATION tab ─────────────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'education'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-graduation-cap fa-xs"), " Human Capital"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Educational attainment of employed — Male vs Female"
                      )
                    ),
                    shiny::div(class = "fi-charts-grid",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                          " Education distribution — % of employed by sex"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_edu_bar", height = "420px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                          " University-educated employed (%) — trend by sex"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("emp_edu_uni_trend", height = "420px")
                        )
                      )
                    )
                  ),

                  # ── AGRICULTURE tab ───────────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'agriculture'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-seedling fa-xs"), " Agricultural Employment"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Share of workers in agriculture — Male vs Female"
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                        " Agriculture share of total workforce (%) — quarterly trend by sex"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("emp_agri_trend", height = "420px")
                      ),
                      shiny::div(class = "fi-data-note",
                        shiny::tags$i(class = "fas fa-info-circle fa-xs"),
                        " Women consistently have a higher share of employment in agriculture than men."
                      )
                    )
                  ),

                  # ── ECONOMIC SECTORS tab ──────────────────────────────────
                  shiny::conditionalPanel("input.emp_tab == 'sectors'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-industry fa-xs"), " Sectoral Distribution"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Economic activity of employed — Male vs Female"
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-bars fa-xs"),
                        " % of employed in each sector — Male vs Female (period average)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("emp_sectors_bar", height = "540px")
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-arrows-alt-h fa-xs"),
                        " Gender gap by sector (Female % minus Male %, percentage points)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("emp_sectors_gap", height = "380px")
                      )
                    )
                  )

                ) # end emp fi-charts
              ) # end conditionalPanel emp

              # ── EDUCATION DASHBOARD ──────────────────────────────────────────
              ,shiny::conditionalPanel("input.active_sector == 'edu'",
                shiny::div(class = "fi-charts",

                  # ── OVERVIEW tab ────────────────────────────────────────────
                  shiny::conditionalPanel("input.edu_tab == 'overview'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-tachometer-alt fa-xs"), " Overview"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Education Gender Dashboard \u00b7 Rwanda PHC 2022"
                      )
                    ),
                    shiny::uiOutput("edu_overview_kpis"),
                    shiny::div(class = "fi-chart-row",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-graduation-cap fa-xs"),
                          " Literacy rate (%) by sex \u00b7 National, Urban, Rural"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_literacy_overview", height = "320px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-school fa-xs"),
                          " School attendance rate (%) by age group \u00b7 Male vs Female"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_attendance_overview", height = "320px")
                        )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Education level attained (%) \u00b7 Male vs Female (Rwanda)"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_attainment_overview", height = "380px")
                      )
                    )
                  ),

                  # ── ATTAINMENT tab ──────────────────────────────────────────
                  shiny::conditionalPanel("input.edu_tab == 'attainment'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-certificate fa-xs"), " Education Attainment"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Highest level of education attained \u00b7 by sex & area"
                      )
                    ),
                    shiny::div(class = "fi-gbar",
                      shiny::div(class = "fi-gbar__inner",
                      shiny::div(class = "fi-gbar__filters",
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-map-marker-alt fa-xs"), " Area"
                        ),
                        shiny::selectInput("edu_area", NULL,
                          choices  = c("Rwanda", "Urban", "Rural"),
                          selected = "Rwanda", width = "140px"
                        )
                      )
                      )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Education level (%) \u00b7 Male vs Female"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_attainment_bar", height = "400px")
                      )
                    ),
                    shiny::div(class = "fi-chart-row",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-pie fa-xs"),
                          " Education distribution \u00b7 Male (donut)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_attainment_donut_m", height = "340px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-pie fa-xs"),
                          " Education distribution \u00b7 Female (donut)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_attainment_donut_f", height = "340px")
                        )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-arrows-alt-h fa-xs"),
                        " Gender gap (Female % minus Male %) by education level"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_attainment_gap", height = "340px")
                      )
                    )
                  ),

                  # ── HISTORICAL TREND tab ────────────────────────────────────
                  shiny::conditionalPanel("input.edu_tab == 'trend'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-history fa-xs"), " Historical Trend"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Education attainment progress 1978 \u2013 2022 \u00b7 by sex"
                      )
                    ),
                    shiny::div(class = "fi-gbar",
                      shiny::div(class = "fi-gbar__inner",
                      shiny::div(class = "fi-gbar__filters",
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-layer-group fa-xs"), " Level"
                        ),
                        shiny::selectInput("edu_trend_level", NULL,
                          choices  = c("No Education","Primary","Post-Primary","Secondary","University"),
                          selected = "No Education", width = "180px"
                        )
                      )
                      )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                        " % of population at selected level \u00b7 1978-2022 \u00b7 Male vs Female"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_trend_line", height = "420px")
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-th fa-xs"),
                        " All levels \u00b7 stacked area by sex over census years"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_trend_heatmap", height = "400px")
                      )
                    )
                  ),

                  # ── SCHOOL ATTENDANCE tab ───────────────────────────────────
                  shiny::conditionalPanel("input.edu_tab == 'attendance'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-school fa-xs"), " School Attendance"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "School-age population attendance \u00b7 by sex, age group & area"
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Current school attendance rate (%) by age group \u00b7 Male vs Female"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_attendance_agegroup", height = "380px")
                      )
                    ),
                    shiny::div(class = "fi-chart-row",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                          " Age 3-5 (pre-primary) attendance \u00b7 National/Urban/Rural"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_attendance_preprimary", height = "340px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                          " Age 6-17 attendance status \u00b7 Male vs Female"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_attendance_6to17", height = "340px")
                        )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Attendance status (3-17 yrs) breakdown \u00b7 Male vs Female by area"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_attendance_status", height = "380px")
                      )
                    )
                  ),

                  # ── LITERACY tab ────────────────────────────────────────────
                  shiny::conditionalPanel("input.edu_tab == 'literacy'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-book-open fa-xs"), " Literacy"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Language literacy rates \u00b7 by sex, age group & area"
                      )
                    ),
                    shiny::div(class = "fi-chart-row",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                          " Literacy rate (%) by area \u00b7 Male vs Female"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_literacy_area", height = "320px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-balance-scale fa-xs"),
                          " Literacy gender gap (Female \u2212 Male pp)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_literacy_gap_area", height = "320px")
                        )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-area fa-xs"),
                        " Literacy rate (%) by age group \u00b7 Male vs Female"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_literacy_age", height = "420px")
                      ),
                      shiny::div(class = "fi-data-note",
                        shiny::tags$i(class = "fas fa-info-circle fa-xs"),
                        " Older cohorts show the largest gender literacy gaps, reflecting historical inequalities in access to education."
                      )
                    )
                  ),

                  # ── DIGITAL ACCESS tab ──────────────────────────────────────
                  shiny::conditionalPanel("input.edu_tab == 'digital'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-laptop fa-xs"), " Digital Access"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "ICT literacy & mobile access \u00b7 by sex & province"
                      )
                    ),
                    shiny::div(class = "fi-gbar",
                      shiny::div(class = "fi-gbar__inner",
                      shiny::div(class = "fi-gbar__filters",
                      shiny::div(class = "fi-gbar__item",
                        shiny::tags$label(class = "fi-gbar__lbl",
                          shiny::tags$i(class = "fas fa-users fa-xs"), " Age group"
                        ),
                        shiny::selectInput("edu_ict_age", NULL,
                          choices = c("10+","16+","21+"), selected = "10+", width = "90px"
                        )
                      )
                      )
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-map fa-xs"),
                        " ICT literacy (%) by province \u00b7 Male vs Female"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_ict_province", height = "420px")
                      )
                    ),
                    shiny::div(class = "fi-chart-row",
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-mobile-alt fa-xs"),
                          " Mobile phone type by sex (Rwanda)"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_mobile_sex", height = "360px")
                        )
                      ),
                      shiny::div(class = "fi-chart-card",
                        shiny::div(class = "fi-chart-title",
                          shiny::tags$i(class = "fas fa-city fa-xs"),
                          " ICT literacy Urban vs Rural \u00b7 Male vs Female"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("edu_ict_urban_rural", height = "360px")
                        )
                      )
                    )
                  ),

                  # ── DISABILITY tab ──────────────────────────────────────────
                  shiny::conditionalPanel("input.edu_tab == 'disability'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-wheelchair fa-xs"), " Disability & Education"
                      ),
                      shiny::tags$h3(class = "fi-tab-hdr__title",
                        "Education attainment by disability status \u00b7 by sex"
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-chart-bar fa-xs"),
                        " Education level (%) \u00b7 With vs Without disability by sex"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_disability_bar", height = "440px")
                      )
                    ),
                    shiny::div(class = "fi-chart-card fi-chart-card--full",
                      shiny::div(class = "fi-chart-title",
                        shiny::tags$i(class = "fas fa-arrows-alt-h fa-xs"),
                        " Never attended school (%) \u00b7 With vs Without disability \u00b7 by sex"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("edu_disability_noedu", height = "340px")
                      )
                    )
                  )

                ) # end edu fi-charts
              ) # end conditionalPanel edu

              # ━━━ AGRICULTURE SECTOR ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
              ,shiny::conditionalPanel("input.active_sector == 'agri'",
                shiny::div(class = "fi-charts",

                  # ── OVERVIEW tab ────────────────────────────────────────────
                  shiny::conditionalPanel("input.agri_tab == 'overview'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-tachometer-alt fa-xs"), " Overview"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__title",
                        "Gender & Agriculture \u2014 Key Indicators"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__sub",
                        "Rwanda Agricultural Household Survey (AHS) 2020 \u00b7 PHC 2022"
                      )
                    ),
                    shiny::uiOutput("agri_overview_kpis"),
                    shiny::fluidRow(
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-home fa-xs"),
                          "\u00a0 Agricultural Households by Sex of Head"
                        ),
                        plotly::plotlyOutput("agri_hh_bar", height = "300px")
                      )),
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-chart-pie fa-xs"),
                          "\u00a0 Share of Agricultural HH (Male vs Female Headed)"
                        ),
                        plotly::plotlyOutput("agri_hh_donut", height = "300px")
                      ))
                    ),
                    shiny::fluidRow(
                      shiny::column(12, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-chart-line fa-xs"),
                          "\u00a0 Agricultural Workers Trend 2017\u20132022 (% of working-age population)"
                        ),
                        plotly::plotlyOutput("agri_workers_overview", height = "320px")
                      ))
                    )
                  ),

                  # ── LAND ACCESS tab ─────────────────────────────────────────
                  shiny::conditionalPanel("input.agri_tab == 'land'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-map fa-xs"), " Land Access"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__title",
                        "Land Ownership, Access & Rights by Gender"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__sub",
                        "NLA \u00b7 AHS 2020"
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-map fa-xs"),
                          "\u00a0 Land Ownership by Category (2018 vs 2021)"
                        ),
                        plotly::plotlyOutput("agri_land_ownership", height = "300px")
                      )),
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-key fa-xs"),
                          "\u00a0 Land Rights by Sex (Access & Right to Sell)"
                        ),
                        plotly::plotlyOutput("agri_land_rights", height = "300px")
                      ))
                    ),
                    shiny::fluidRow(
                      shiny::column(12, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-tractor fa-xs"),
                          "\u00a0 Agricultural Land Access by Type & Sex of HH Head"
                        ),
                        plotly::plotlyOutput("agri_land_access", height = "320px")
                      ))
                    )
                  ),

                  # ── EXTENSION SERVICES tab ──────────────────────────────────
                  shiny::conditionalPanel("input.agri_tab == 'extension'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-chalkboard-teacher fa-xs"),
                        " Extension Services"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__title",
                        "Agricultural Extension Services Received by Gender"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__sub",
                        "AHS 2020 \u00b7 % of agricultural households supported"
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-list fa-xs"),
                          "\u00a0 Extension Coverage by Service Type \u2014 Female vs Male HH"
                        ),
                        plotly::plotlyOutput("agri_extension_bar", height = "420px")
                      ))
                    ),
                    shiny::fluidRow(
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-percentage fa-xs"),
                          "\u00a0 Total Reach (% of all agricultural HH)"
                        ),
                        plotly::plotlyOutput("agri_extension_total", height = "280px")
                      )),
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-venus-mars fa-xs"),
                          "\u00a0 Gender Gap in Extension (F \u2212 M, pp)"
                        ),
                        plotly::plotlyOutput("agri_extension_gap", height = "280px")
                      ))
                    )
                  ),

                  # ── INPUTS & PRACTICES tab ─────────────────────────────────
                  shiny::conditionalPanel("input.agri_tab == 'inputs'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-flask fa-xs"), " Inputs & Practices"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__title",
                        "Agricultural Inputs & Community Group Participation"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__sub",
                        "AHS 2020 \u00b7 % of agricultural households"
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-seedling fa-xs"),
                          "\u00a0 Agricultural Inputs by Province & Sex of HH Head"
                        ),
                        shiny::selectInput("agri_input_type", NULL,
                          choices = c(
                            "Improved Seeds"     = "improved_seeds",
                            "Organic Fertilizer" = "organic_fert",
                            "Inorganic Fertilizer" = "inorganic_fert",
                            "Pesticides"         = "pesticides"
                          ), width = "260px"
                        ),
                        plotly::plotlyOutput("agri_inputs_chart", height = "340px")
                      ))
                    ),
                    shiny::fluidRow(
                      shiny::column(12, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-users fa-xs"),
                          "\u00a0 Community Group Membership by Sex of HH Head"
                        ),
                        plotly::plotlyOutput("agri_community_groups", height = "300px")
                      ))
                    )
                  ),

                  # ── WORKERS TREND tab ───────────────────────────────────────
                  shiny::conditionalPanel("input.agri_tab == 'workers'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-chart-line fa-xs"), " Workers Trend"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__title",
                        "Agricultural Workers as % of Working-Age Population (2017\u20132022)"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__sub",
                        "RLFS datasets 2017\u20132022 \u00b7 16 years and above"
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-filter fa-xs"),
                          "\u00a0 Worker Type:"
                        ),
                        shiny::selectInput("agri_worker_type", NULL,
                          choices = c(
                            "All: Market-oriented + Subsistence" = "Market-oriented + Subsistence",
                            "Market-oriented Only"               = "Market-oriented",
                            "Subsistence Only"                   = "Subsistence"
                          ), width = "360px"
                        ),
                        plotly::plotlyOutput("agri_workers_trend", height = "360px")
                      ))
                    ),
                    shiny::fluidRow(
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-venus fa-xs"),
                          "\u00a0 Female Agricultural Workers by Type (2017\u20132022)"
                        ),
                        plotly::plotlyOutput("agri_workers_female", height = "280px")
                      )),
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-mars fa-xs"),
                          "\u00a0 Male Agricultural Workers by Type (2017\u20132022)"
                        ),
                        plotly::plotlyOutput("agri_workers_male", height = "280px")
                      ))
                    )
                  ),

                  # ── LIVESTOCK tab ───────────────────────────────────────────
                  shiny::conditionalPanel("input.agri_tab == 'livestock'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-horse fa-xs"), " Livestock"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__title",
                        "Livestock Ownership by Type & Sex of HH Head"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__sub",
                        "AHS 2020 \u00b7 % of households raising each livestock type"
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-list fa-xs"),
                          "\u00a0 Livestock Ownership Rates: Male vs Female Headed HH"
                        ),
                        plotly::plotlyOutput("agri_livestock_bar", height = "360px")
                      ))
                    ),
                    shiny::fluidRow(
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-venus-mars fa-xs"),
                          "\u00a0 Gender Gap in Livestock Ownership (Male \u2212 Female HH, pp)"
                        ),
                        plotly::plotlyOutput("agri_livestock_gap", height = "300px")
                      )),
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-chart-pie fa-xs"),
                          "\u00a0 Livestock Portfolio Composition (% share per sex)"
                        ),
                        plotly::plotlyOutput("agri_livestock_donut", height = "300px")
                      ))
                    )
                  ),

                  # ── PROGRAMS tab ────────────────────────────────────────────
                  shiny::conditionalPanel("input.agri_tab == 'programs'",
                    shiny::div(class = "fi-tab-hdr",
                      shiny::tags$span(class = "fi-tab-hdr__eyebrow",
                        shiny::tags$i(class = "fas fa-hand-holding-heart fa-xs"), " Programs"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__title",
                        "Girinka Programme \u2014 Gender Participation & Outcomes"
                      ),
                      shiny::tags$span(class = "fi-tab-hdr__sub",
                        "AHS 2020 \u00b7 % of agricultural households"
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-award fa-xs"),
                          "\u00a0 Girinka Beneficiaries & Retention by Sex"
                        ),
                        plotly::plotlyOutput("agri_girinka_bar", height = "300px")
                      )),
                      shiny::column(6, shiny::div(class = "dash-chart-card",
                        shiny::div(class = "dash-chart-title",
                          shiny::tags$i(class = "fas fa-building fa-xs"),
                          "\u00a0 Provider Type Distribution (Govt vs NGO/Company)"
                        ),
                        plotly::plotlyOutput("agri_girinka_provider", height = "300px")
                      ))
                    )
                  )

                ) # end agri fi-charts
              ) # end conditionalPanel agri

              ) # end fi-main

            ) # end fi-dash
          ) # end vizc-panel__content
        ) # end vizc-panel (fi-fullscreen-panel)

      ) # end vizc-page
    )   # end Dashboard tabPanel

    # ╔═══════════════════════════════════════════════════════════════════════╗
    # ║                  DATA CATALOGUE TAB                                   ║
    # ╚═══════════════════════════════════════════════════════════════════════╝
    ,shiny::tabPanel(
      title = shiny::tagList(
        shiny::tags$i(class = "fas fa-book-open"), " Data Catalogue"
      ),
      value = "datahub",

      shiny::div(class = "dh-page",

        # ── HERO BANNER ────────────────────────────────────────────────────
        shiny::div(class = "dh-hero",
          shiny::div(class = "dh-hero__inner",
            shiny::div(class = "dh-hero__badge",
              shiny::tags$i(class = "fas fa-book-open"), " Self-Service Data Portal"
            ),
            shiny::tags$h1(class = "dh-hero__title",
              "Rwanda Gender Data Catalogue"
            ),
            shiny::tags$p(class = "dh-hero__sub",
              "A curated catalogue of gender-disaggregated datasets across all sectors of Rwanda.",
              shiny::tags$br(),
              "Select a sector \u2192 choose a dataset \u2192 pick your variables \u2192 filter \u2192 preview \u2192 download."
            ),
            shiny::div(class = "dh-hero__stats",
              shiny::div(class = "dh-hero__stat",
                shiny::tags$span(class = "dh-hero__stat-n", "6"),
                shiny::tags$span(class = "dh-hero__stat-l", "Sectors")
              ),
              shiny::div(class = "dh-hero__stat",
                shiny::tags$span(class = "dh-hero__stat-n", "45+"),
                shiny::tags$span(class = "dh-hero__stat-l", "Datasets")
              ),
              shiny::div(class = "dh-hero__stat",
                shiny::tags$span(class = "dh-hero__stat-n", "3"),
                shiny::tags$span(class = "dh-hero__stat-l", "Export Formats")
              ),
              shiny::div(class = "dh-hero__stat",
                shiny::tags$span(class = "dh-hero__stat-n", "100%"),
                shiny::tags$span(class = "dh-hero__stat-l", "Free Access")
              )
            )
          )
        ),

        # ── MAIN BODY ──────────────────────────────────────────────────────
        shiny::div(class = "dh-body",

          # ── TOP: FULL-WIDTH PAGE WIZARD ───────────────────────────────────
          shiny::div(class = "dh-wizard-panel",

            # Progress track
            shiny::uiOutput("dh_progress"),

            # Current page content
            shiny::div(class = "dh-wizard-body",
              shiny::uiOutput("dh_wizard_page")
            ),

            # Back / Next navigation bar
            shiny::uiOutput("dh_nav_btns"),

            # Hidden: sector value written by dhSelectSector() JS
            shiny::div(style = "display:none;",
              shiny::textInput("dh_sector", NULL, value = "")
            )

          ), # end dh-wizard-panel

          # ── BOTTOM: FULL-WIDTH PREVIEW ────────────────────────────────────
          shiny::div(class = "dh-preview-panel",

            shiny::uiOutput("dh_empty_state"),
            shiny::uiOutput("dh_status_bar"),
            shiny::uiOutput("dh_info_card"),
            shiny::uiOutput("dh_download_ui"),
            shiny::uiOutput("dh_preview_ui")

          ) # end dh-preview-panel
        ) # end dh-body
      ) # end dh-page
    ) # end Data Hub tabPanel

  ) # end navbarPage
) # end tagList
