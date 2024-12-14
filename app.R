
rm(list = ls())

# for shiny
library(shiny)
library(bslib)
library(shinyWidgets)

# dataprep
library(stringr)
library(magrittr)
library(tidyr)
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
library(httr)
library(jsonlite)
#library(GET)

library(dplyr)

# function to overwrite data with comments
source("./functions/overwrite_DATA.R")

# define datapath
DATAPATH <<- "./DATA/"


################################################################################
# load modules for shiny app ###################################################
source("./finanzkonten_mod.R")
source("./girokonten_mod.R")
source("./depots_mod.R")
source("./wallets_mod.R")
source("./upload.R")


################################################################################
# LOAD DATA ####################################################################

# scrape API data & load into csv DATA -----------------------------------------

source("./functions/live_BIPGATWWXXX.R")



# from static csv files --------------------------------------------------------
source("./functions/csv_RESIDUALBANKS.R")

finanzkonto <- load_csv(DATAPATH)

uebersicht <- finanzkonto
girokonten <- Filter(function(x) x$konto %in% c("GIRO", "EXTRAKONTO", "GUTHABEN"), finanzkonto)
depots     <- Filter(function(x) x$konto %in% c("DEPOT"), finanzkonto)
wallets    <- Filter(function(x) x$konto %in% c("WALLET"), finanzkonto)



# create dataframe with IDs ---------------------------------------------------- 

bank_konto <- lapply(finanzkonto, function(x) {
  data.frame(bank = x$bank, konto = x$konto, display_name = x$display_name, ID = x$name)
})

bank_konto <- bind_rows(bank_konto) 


################################################################################
# UI ###########################################################################

ui <- page_fluid(
  tabsetPanel(
    tabPanel("Übersicht", uebersichtUI("uebersicht", uebersicht, bank_konto)),
    tabPanel("Girokonten", girokontenUI("girokonten", girokonten)),
    tabPanel("Depots", depotsUI("depots", depots)),
    tabPanel("Wallets", walletsUI("wallets", wallets)),
    tabPanel("Upload", uploadUI("upload", finanzkonto))
  )
)



################################################################################
# SERVER #######################################################################
# Define the server logic for the main app
server <- function(input, output, session) {
  
  uebersichtServer("uebersicht", uebersicht, bank_konto)
  girokontenServer("girokonten", girokonten)
  depotsServer("depots", depots)
  walletsServer("wallets", wallets)
  uploadServer("upload", finanzkonto)
  
}


shinyApp(ui, server)
