
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


# read manual data =============================================================

source("./functions/readdata_manual.R")
data_manual <- readdata_maual()


# load historical price data ===================================================

source("./functions/load_price_data.R")
source("./functions/createdata_portfolio.R")

data_depot <- bind_rows(
  load_price_data("IE00B5BMR087", type = "depot"),
  load_price_data("IE00BK5BQT80", type = "depot"),
  load_price_data("LU0908500753", type = "depot")
)

data_wallet <- load_price_data("BTC", type = "wallet")

data_sparbrief <- load_price_data(id = "Sparbrief_ING_231207", type = "sparbrief")

data_historical <- list(depot = data_depot, sparbrief = data_sparbrief, wallet = data_wallet)

# portfolio data has information on amount and price, and computes the saldo
data_portfolio <- createdata_portfolio(manualdata = data_manual, historicaldata = data_historical)


# add inflation data ===========================================================

data_inflation <- read.csv("./data/inflation.csv", comment.char = "#")
data_inflation$datum <- as.Date(paste0("01-", gsub("\\.", "", data_inflation$datum)), format = "%d-%b%y")

################################################################################
# load modules for shiny app ###################################################

source("./modules/uebersicht.R")

source("./modules/giro.R")

source("./modules/konten.R")
source("./modules/gruppen.R")


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
  
  uebersichtServer("uebersicht", data_manual, data_portfolio, data_inflation)
  giroServer("giro", data_manual)
  kontenServer("konten", data_manual)
  gruppenServer("gruppen")

}


shinyApp(ui, server)
