

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(plotly)

source("R/constants.R")       
source("R/reference_data.R")   
source("R/data_load.R")        
source("R/helpers.R")          
source("R/ui.R")               
source("R/server.R")           

shinyApp(ui = ui, server = server)
