
rm(list = ls())

# for shiny
library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)

# dataprep
library(stringr)
library(magrittr)
library(tidyr)
library(purrr)
library(readr)
library(scales)
library(lubridate)
library(tibble)

# data depiction
library(highcharter)
library(igraph)
library(RColorBrewer)
library(reactable)
library(DT)
library(visNetwork)

# for API
library(rvest)
library(httr)
library(jsonlite)
#library(GET)

library(dplyr)


# Read Manual Data =============================================================

source("./functions/read_MANUALDATA.R")

MANUALDATA <- read_MANUALDATA("./MANUALDATA/")

# update historicalDATA ========================================================

source("./functions/update_historicalDATA.R")

update_historicalDATA(datapath = "./historicalDATA/")

# read historicalDATA ==========================================================

source("./functions/read_historicalDATA.R")
historicalDATA <- read_historicalDATA(datapath = "./historicalDATA/")

# create Auto Data =============================================================

source("./functions/create_PORTFOLIODATA.R")
PORTFOLIODATA <- create_PORTFOLIODATA(manualdata = MANUALDATA, historicaldata = historicalDATA)


################################################################################
# load modules for shiny app ###################################################
source("./finanzkonten_mod.R")
source("./girokonten_mod.R")
#source("./depots_mod.R")
#source("./wallets_mod.R")
#source("./entnahme_mod.R")
source("./upload_mod.R")


################################################################################
# UI ###########################################################################

ui <- page_fluid(
  tabsetPanel(
    tabPanel("Übersicht", uebersichtUI("uebersicht", MANUALDATA, PORTFOLIODATA)),
    tabPanel("Girokonten", girokontenUI("girokonten", MANUALDATA)),
    tabPanel("Upload", uploadUI("upload", MANUALDATA))
  )
)



################################################################################
# SERVER #######################################################################
# Define the server logic for the main app
server <- function(input, output, session) {
  
  uebersichtServer("uebersicht", MANUALDATA, PORTFOLIODATA)
  girokontenServer("girokonten", MANUALDATA)
  uploadServer("upload", MANUALDATA)
  
}


shinyApp(ui, server)
