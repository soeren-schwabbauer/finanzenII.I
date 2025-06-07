
rm(list = ls())

# for shiny
library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
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
library(plotly)

# for API
library(rvest)
library(httr)
library(jsonlite)
#library(GET)

library(dplyr)


# Read Manual Data =============================================================

source("./functions/read_MANUALDATA.R")
MANUALDATA <- read_MANUALDATA()


# update historicalDATA ========================================================

source("./functions/update_historicalDATA.R")
update_historicalDATA(datapath = "./historicalDATA/")


# read historicalDATA ==========================================================

source("./functions/read_historicalDATA.R")
historicalDATA <- read_historicalDATA(datapath = "./historicalDATA/")


# create Auto Data =============================================================

source("./functions/create_PORTFOLIODATA.R")
PORTFOLIODATA <- create_PORTFOLIODATA(manualdata = MANUALDATA, historicaldata = historicalDATA)

INFLATIONDATA <- read.csv("./inflation.csv")
INFLATIONDATA$datum <- as.Date(paste0("01-", gsub("\\.", "", INFLATIONDATA$datum)), format = "%d-%b%y")

################################################################################
# load modules for shiny app ###################################################
source("./uebersicht_mod.R")
source("./giro_mod.R")
source("./depot_mod.R")
source("./konten_mod.R")
source("./wallet_mod.R")
source("./gruppen_mod.R")


################################################################################
# UI ###########################################################################

ui <- page_navbar(
  title = "Finanzen",
  # ⬇️ HEAD-Inhalte gehören hier rein
  header = tags$head(
    includeCSS("www/card-reveal-full-screen.css")
  ),
  
  # ⬇️ Das hier sind gültige Navigations-Panels
  nav_panel(
    title = "Übersicht",
    uebersichtUI("uebersicht", MANUALDATA, PORTFOLIODATA)
  ),
  nav_panel(
    title = "Giro",
    giroUI("giro", MANUALDATA)
  ),
  nav_panel(
    title = "Depot",
    depotUI("depot", PORTFOLIODATA)
  ),
  nav_panel(
    title = "Wallet",
    walletUI("wallet", PORTFOLIODATA)
  ),
  nav_panel(
    title = "Daten",
    fluidRow(
      column(width = 12,
             kontenUI("konten", MANUALDATA)
      ),
      column(width = 12,
             gruppenUI("gruppen")
      )
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item("x"),
    nav_item("y")
  )
)




################################################################################
# SERVER #######################################################################
# Define the server logic for the main app
server <- function(input, output, session) {
  
  uebersichtServer("uebersicht", MANUALDATA, PORTFOLIODATA, INFLATIONDATA)
  giroServer("giro", MANUALDATA)
  depotServer("depot", PORTFOLIODATA)
  walletServer("wallet", PORTFOLIODATA)
  kontenServer("konten", MANUALDATA)
  gruppenServer("gruppen")

}


shinyApp(ui, server)
