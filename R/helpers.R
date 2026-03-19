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
