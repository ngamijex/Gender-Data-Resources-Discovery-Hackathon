# ── R/helpers.R ───────────────────────────────────────────────────────────────
# Reusable HTML component builders and the plotly chart theme.
# All class names here correspond to rules in www/styles.css.

# ── Plotly chart theme ─────────────────────────────────────────────────────────
gddp_theme <- function(p, xlab = NULL, ylab = NULL) {
  p |>
    plotly::layout(
      paper_bgcolor = CLR["bg_page"],
      plot_bgcolor  = CLR["bg_page"],
      font  = list(family = "DM Sans", color = CLR["ink_500"], size = 12),
      xaxis = list(
        title     = list(text = xlab, font = list(size = 11, color = CLR["ink_500"])),
        gridcolor = CLR["bg_inset"],
        linecolor = CLR["ink_100"],
        tickfont  = list(size = 11, color = CLR["ink_500"]),
        zeroline  = FALSE
      ),
      yaxis = list(
        title     = list(text = ylab, font = list(size = 11, color = CLR["ink_500"])),
        gridcolor = CLR["bg_inset"],
        linecolor = CLR["ink_100"],
        tickfont  = list(size = 11, color = CLR["ink_500"]),
        zeroline  = FALSE
      ),
      legend = list(
        font    = list(family = "DM Sans", size = 11, color = CLR["ink_500"]),
        bgcolor = "transparent",
        bordercolor = "transparent"
      ),
      margin = list(l = 50, r = 20, t = 30, b = 50)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# ── Collection type tag ────────────────────────────────────────────────────────
collection_tag <- function(col) {
  mod <- switch(col,
    "DHS"            = "ctag--dhs",
    "EICV"           = "ctag--eicv",
    "Census"         = "ctag--census",
    "LFS"            = "ctag--lfs",
    "Agriculture"    = "ctag--agri",
    "FinScope"       = "ctag--gold",
    "Food Security"  = "ctag--food",
    ""
  )
  shiny::tags$span(class = paste("ctag", mod), col)
}

# ── Quality status pill ────────────────────────────────────────────────────────
quality_pill <- function(status) {
  mod <- switch(status,
    "Complete"     = "qpill--ok",
    "Minor Issues" = "qpill--warn",
    "Incomplete"   = "qpill--bad",
    ""
  )
  shiny::tags$span(class = paste("qpill", mod), status)
}

# ── Gender relevance fill-bar ──────────────────────────────────────────────────
gender_bar_html <- function(score) {
  pct   <- paste0(score * 10, "%")
  color <- if (score >= 7) CLR["primary"] else if (score >= 4) CLR["accent"] else CLR["ink_300"]
  shiny::tags$div(class = "gsbar",
    shiny::tags$div(class = "gsbar__track",
      shiny::tags$div(class = "gsbar__fill",
        style = paste0("width:", pct, ";background:", color))
    ),
    shiny::tags$span(class = "gsbar__label", paste0(score, "/10"))
  )
}

# ── Google-style search result item ───────────────────────────────────────────
build_search_result <- function(s, query = "") {

  yr       <- if (!is.na(s$year)) as.character(s$year) else "\u2014"
  abstract <- tidyr::replace_na(s$abstract, "No abstract available for this study.")

  # Truncate to 280 chars for the snippet
  snippet <- if (nchar(abstract) > 280) paste0(substr(abstract, 1, 280), "\u2026") else abstract

  # Highlight query terms (words > 2 chars)
  if (nchar(trimws(query)) > 0) {
    terms <- strsplit(trimws(query), "\\s+")[[1]]
    terms <- unique(terms[nchar(terms) > 2])
    for (term in terms) {
      snippet <- gsub(
        paste0("(?i)(", gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", term), ")"),
        "<mark class='sep-hl'>\\1</mark>",
        snippet, perl = TRUE
      )
    }
  }

  n_res <- if (!is.na(s$n_resources) && s$n_resources > 0)
    paste0(s$n_resources, " resource", if (s$n_resources > 1) "s" else "")
  else "No resources"

  # Chip modifiers
  g_cls <- if (s$gender_score >= 7) "sep-chip--g-hi" else
           if (s$gender_score >= 4) "sep-chip--g-mid" else "sep-chip--g-lo"
  a_cls <- if (s$access_clean == "Public") "sep-chip--public" else "sep-chip--licensed"
  q_cls <- switch(s$quality_status,
    "Complete"     = "sep-chip--complete",
    "Minor Issues" = "sep-chip--warn",
    "sep-chip--bad"
  )

  shiny::div(class = "sep-result",

    # Green URL path (Google-style)
    shiny::div(class = "sep-result__path",
      shiny::tags$i(class = "fas fa-database fa-xs"),
      shiny::HTML(paste0(
        " microdata.statistics.gov.rw",
        " <span class='sep-result__sep'>\u203a</span> catalog",
        " <span class='sep-result__sep'>\u203a</span> ",
        gsub("<", "&lt;", tidyr::replace_na(s$collection, "survey"))
      ))
    ),

    # Title — action link that triggers the modal
    shiny::actionLink(
      inputId = paste0("detail_", s$study_id),
      label   = s$title,
      class   = "sep-result__title"
    ),

    # Chips row: year · series · gender score · access · quality
    shiny::div(class = "sep-result__chips",
      shiny::tags$span(class = "sep-chip sep-chip--year", yr),
      shiny::tags$span(class = "sep-chip sep-chip--series",
        tidyr::replace_na(s$collection, "Other")),
      shiny::tags$span(class = paste("sep-chip", g_cls),
        paste0("Gender: ", s$gender_score, "/10")),
      shiny::tags$span(class = paste("sep-chip", a_cls), s$access_clean),
      shiny::tags$span(class = paste("sep-chip", q_cls), s$quality_status)
    ),

    # Abstract snippet with highlighted terms
    shiny::div(class = "sep-result__snippet", shiny::HTML(snippet)),

    # Footer: resource count + NISR external link
    shiny::div(class = "sep-result__footer",
      shiny::tags$span(class = "sep-result__res",
        shiny::tags$i(class = "fas fa-file-alt fa-xs"), " ", n_res
      ),
      if (!is.na(s$url) && nchar(s$url) > 4)
        shiny::tags$a(
          href   = s$url,
          target = "_blank",
          class  = "sep-result__src",
          shiny::tags$i(class = "fas fa-external-link-alt fa-xs"),
          " View on NISR Microdata Catalog"
        )
    )
  )
}

# ── Study result card ──────────────────────────────────────────────────────────
build_study_card <- function(s) {
  yr    <- if (!is.na(s$year)) as.character(s$year) else "\u2014"
  n_res <- if (!is.na(s$n_resources)) s$n_resources else 0L
  url   <- if (!is.na(s$url) && nchar(s$url) > 4) s$url else NA_character_

  shiny::tags$div(class = "scard",
    # Top row: badges + quality pill
    shiny::tags$div(class = "scard__top",
      shiny::tags$div(class = "scard__badges",
        collection_tag(s$collection),
        shiny::tags$span(class = "ctag ctag--year", yr)
      ),
      quality_pill(s$quality_status)
    ),
    # Title
    shiny::tags$h3(class = "scard__title", s$title),
    # Abstract excerpt
    shiny::tags$p(class = "scard__abstract", s$abstract_card),
    # Meta row
    shiny::tags$div(class = "scard__meta",
      shiny::tags$span(class = "smeta-item",
        shiny::tags$i(class = "fas fa-database fa-xs"),
        paste0("\u00a0", n_res, " resource", if (n_res != 1) "s")
      ),
      shiny::tags$span(class = "smeta-item",
        shiny::tags$i(class = "fas fa-venus fa-xs"),
        paste0("\u00a0Gender: ", s$gender_score, "/10")
      )
    ),
    # Footer: gender bar + action buttons
    shiny::tags$div(class = "scard__footer",
      gender_bar_html(s$gender_score),
      shiny::tags$div(class = "scard__actions",
        if (!is.na(url))
          shiny::tags$a(
            href   = url,
            target = "_blank",
            class  = "btn btn--outline btn--sm",
            shiny::tags$i(class = "fas fa-external-link-alt fa-xs"),
            " Source"
          ),
        shiny::actionButton(
          inputId = paste0("detail_", s$study_id),
          label   = shiny::tagList(
            shiny::tags$i(class = "fas fa-info-circle fa-xs"), " Details"
          ),
          class = "btn btn--primary btn--sm"
        )
      )
    )
  )
}
