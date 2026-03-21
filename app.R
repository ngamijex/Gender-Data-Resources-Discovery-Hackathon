library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(plotly)
library(httr)
library(jsonlite)

source("R/constants.R")
source("R/reference_data.R")
source("R/data_load.R")
source("R/search.R")
source("R/helpers.R")
source("R/ai_overview.R")
source("R/finscope_data.R")
source("R/governance_data.R")
source("R/demography_data.R")
source("R/ui.R")
source("R/server.R")

# Build search corpus once after all modules are loaded
.catalog_corpus_cache <- tryCatch(
  build_search_corpus(catalog, resources),
  error = function(e) {
    message("[GDDP] Search corpus build failed: ", e$message, " — search will use fallback.")
    NULL
  }
)
if (!is.null(.catalog_corpus_cache)) {
  message("[GDDP] Search corpus ready: ", nrow(.catalog_corpus_cache),
          " studies | ", ncol(.catalog_corpus_cache), " columns indexed")
} else {
  message("[GDDP] Running without prebuilt corpus — simple search active.")
}

# ── Load FinScope microdata once at startup ───────────────────────────────────
.finscope_data <- tryCatch(
  load_finscope_data(),
  error = function(e) {
    message("[FI] Startup load failed: ", e$message)
    NULL
  }
)
if (!is.null(.finscope_data)) {
  message("[FI] FinScope ready: ", nrow(.finscope_data), " respondents")
} else {
  message("[FI] FinScope data unavailable — Financial Inclusion dashboard will show a notice.")
}

# ── Load Governance dashboard tables once at startup ─────────────────────────
.governance_data <- tryCatch(
  load_governance_data(),
  error = function(e) {
    message("[GOV] Startup load failed: ", e$message)
    NULL
  }
)
if (!is.null(.governance_data)) {
  message("[GOV] Governance ready.")
} else {
  message("[GOV] Governance data unavailable — Governance dashboard will show a notice.")
}

# ── Load Demography dashboard tables once at startup ──────────────────────────
.demography_data <- tryCatch(
  load_demography_data(),
  error = function(e) {
    message("[DEMO] Startup load failed: ", e$message)
    NULL
  }
)
if (!is.null(.demography_data)) {
  message("[DEMO] Demography ready.")
} else {
  message("[DEMO] Demography data unavailable — Demography dashboard will show a notice.")
}

shinyApp(ui = ui, server = server)
