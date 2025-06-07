
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

# for API
library(rvest)
library(httr)
library(jsonlite)
#library(GET)

library(dplyr)


# Read Manual Data =============================================================

source("./functions/readdata_manual.R")
data_manual <- readdata_maual()


# read Historical Data =========================================================

# ziel ist aus den Preisen den wert des protfolios/sparbriefs, etc. zu berechnen
source("./functions/readdata_historical.R")

data_depot <- readdata_historical(datapath = "./data/depot/")
data_sparbrief <- readdata_historical(datapath = "./data/sparbrief/")
data_wallet <- readdata_historical(datapath = "./data/wallet/")

data_historical <- list(depot = data_depot, sparbrief = data_sparbrief, wallet = data_wallet)

source("./functions/createdata_portfolio.R")
data_portfolio <- createdata_portfolio(manualdata = data_manual, historicaldata = data_historical)



INFLATIONDATA <- read.csv("./data/inflation.csv")
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
    uebersichtUI("uebersicht", data_manual, data_portfolio)
  ),
  nav_panel(
    title = "Giro",
    giroUI("giro", data_manual)
  ),
  nav_panel(
    title = "Depot",
    depotUI("depot", data_portfolio)
  ),
  nav_panel(
    title = "Wallet",
    walletUI("wallet", data_portfolio)
  ),
  nav_panel(
    title = "Daten",
    fluidRow(
      column(width = 12,
             kontenUI("konten", data_manual)
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
  
  uebersichtServer("uebersicht", data_manual, data_portfolio, INFLATIONDATA)
  giroServer("giro", data_manual)
  depotServer("depot", data_portfolio)
  walletServer("wallet", data_portfolio)
  kontenServer("konten", data_manual)
  gruppenServer("gruppen")

}


shinyApp(ui, server)
