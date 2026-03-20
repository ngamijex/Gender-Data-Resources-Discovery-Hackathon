

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

shinyApp(ui = ui, server = server)
