# ── app.R ─────────────────────────────────────────────────────────────────────
# GDDP — Gender Data Discovery Platform
# Entry point: loads packages, sources all modules, launches the Shiny app.
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(plotly)

source("R/constants.R")        # CLR color vector
source("R/reference_data.R")   # curated Rwanda gender indicator tables
source("R/data_load.R")        # load_catalog() + global catalog / resources
source("R/helpers.R")          # gddp_theme(), collection_tag(), build_study_card(), …
source("R/ui.R")               # ui object
source("R/server.R")           # server function

shinyApp(ui = ui, server = server)
