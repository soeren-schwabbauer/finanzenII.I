
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
library(rvest)
library(httr)
library(jsonlite)
#library(GET)

library(dplyr)

# function to overwrite data with comments
source("./functions/overwrite_DATA.R")
# function to read data into list
source("./functions/listread_DATA.R")

# define global variabales
DATAPATH <<- "./DATA/"
FINANZKONTEN_ID <<- list.files(DATAPATH)[!str_detect(list.files(DATAPATH), "gruppen.csv")] # used in listread_DATA to get all files


################################################################################
# load modules for shiny app ###################################################
source("./finanzkonten_mod.R")
source("./girokonten_mod.R")
source("./depots_mod.R")
source("./wallets_mod.R")
source("./upload_mod.R")


################################################################################
# LIVE DATA ####################################################################

# scrape API data & load into csv DATA -----------------------------------------
source("./functions/live_BIPGATWWXXX.R")

# update live prices for depots ------------------------------------------------
source("./functions/live_depots_prices.R")


################################################################################
# LOAD DATA ####################################################################

# from static csv files --------------------------------------------------------

finanzkonto <- listread_DATA(DATAPATH)
girokonten  <- listread_DATA(DATAPATH, filter_typID = c("GIR", "EXT", "GTH"))
depots      <- listread_DATA(DATAPATH, filter_typID = "DEP")
wallets     <- listread_DATA(DATAPATH, filter_typID = "WAL") 





# create dataframe with IDs ---------------------------------------------------- 

bank_konto <- lapply(finanzkonto, function(x) {
  data.frame(bank = x$bank, konto = x$konto, display_name = x$display_name, ID = x$name)
})

bank_konto <- bind_rows(bank_konto) 


################################################################################
# UI ###########################################################################

ui <- page_fluid(
  tabsetPanel(
    tabPanel("Übersicht", uebersichtUI("uebersicht", finanzkonto, bank_konto)),
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
  
  uebersichtServer("uebersicht", finanzkonto, bank_konto)
  girokontenServer("girokonten", girokonten)
  depotsServer("depots", depots)
  walletsServer("wallets", wallets)
  uploadServer("upload", finanzkonto)
  
}


shinyApp(ui, server)
