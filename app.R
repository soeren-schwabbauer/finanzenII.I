
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

# scrape latest wallet & stock data ============================================

#source("./functions/updatedata_wallet.R")
#source("./functions/updatedata_depot.R")


# read manual data =============================================================

source("./functions/readdata_manual.R")
data_manual <- readdata_maual()


# read historical hata =========================================================

# ziel ist aus den Preisen den anzahl der aktien/sparbriefs, etc. den gesamtwert je 
# depot / sparbrief zu berechnen

source("./functions/readdata_historical.R")

#data_depot <- readdata_historical(datapath = "./data/depot/")
data_depot <- bind_rows(
  read.csv("https://raw.githubusercontent.com/soeren-schwabbauer/finanzen/main/data/depot/IE00B5BMR087.csv") %>% mutate(id = "IE00B5BMR087"),
  read.csv("https://raw.githubusercontent.com/soeren-schwabbauer/finanzen/main/data/depot/IE00BK5BQT80.csv") %>% mutate(id = "IE00BK5BQT80"),
  read.csv("https://raw.githubusercontent.com/soeren-schwabbauer/finanzen/main/data/depot/LU0908500753.csv") %>% mutate(id = "LU0908500753")
) %>%
  
  mutate(datum = as.Date(datum)) %>%
  group_by(id) %>%
  complete(datum = seq.Date(min(datum), Sys.Date(), by = "day")) %>%
  fill(preis, .direction = "down") %>%
  ungroup()

#data_wallet <- readdata_historical(datapath = "./data/wallet/")
data_wallet <- bind_rows(
  read.csv("https://raw.githubusercontent.com/soeren-schwabbauer/finanzen/main/data/wallet/BTC.csv") %>% mutate(id = "BTC")
) %>%
  
  mutate(datum = as.Date(datum)) %>%
  group_by(id) %>%
  complete(datum = seq.Date(min(datum), Sys.Date(), by = "day")) %>%
  fill(preis, .direction = "down") %>%
  ungroup()

data_sparbrief <- readdata_historical(datapath = "./data/sparbrief/")



data_historical <- list(depot = data_depot, sparbrief = data_sparbrief, wallet = data_wallet)

source("./functions/createdata_portfolio.R")
data_portfolio <- createdata_portfolio(manualdata = data_manual, historicaldata = data_historical)



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
