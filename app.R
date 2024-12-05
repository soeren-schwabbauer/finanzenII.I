library(shiny)
library(bslib)
library(highcharter)
library(stringr)
library(magrittr)
library(tidyr)
library(igraph)
library(reactable)
library(DT)
library(visNetwork)
library(shinyWidgets)
library(readr)
library(scales)
library(lubridate)
library(RColorBrewer)
library(tibble)

library(dplyr)

source("./finanzkonten_mod.R")
source("./auswertunggiro_mod.R")
source("./upload.R")

################################################################################
# LOAD DATA ####################################################################

# finanzkonten file names - are IDentifier for each konto
filenames_csv <- list.files(path = "./DATA/")
filenames_csv <- filenames_csv[filenames_csv != "gruppen.csv"]

# funciton to load csv files
load_finanzkonten <- function(finanzkonto) {
  file_path <- paste0("./DATA/", finanzkonto)
  if (!file.exists(file_path)) {
    message("File does not exist:", file_path)
    return(NULL)  # Return NULL if the file does not exist
  }
  read.csv(file_path, comment.char = "#")
}

# Initialize the finanzkonto list (this will store all accounts)
bank_konto <- data.frame()
finanzkonto <- list()

# Loop through each account and load the data dynamically
for(i in filenames_csv) {
  
  info  <- readLines(paste0("./DATA/", i))
  bank  <- trimws(gsub(".*:", "", info[1]))
  konto <- trimws(gsub(".*:", "", info[2]))
  display_name  <- paste0(bank, " - ", konto)
  
  tryCatch({
    # Load the data for the current ID
    account_data <- load_finanzkonten(i)
    ID <- gsub("*.csv", "", i)
    # Only add to finanzkonto if the data is not NULL
    if (!is.null(account_data)) {
      finanzkonto[[ID]] <- list(
        name = ID,  
        display_name = display_name,
        data = account_data
      )
    } else {
      message(paste("Data is NULL for", ID))
    }
  }, error = function(e) {
    message(paste("Error loading data for", ID, ":", e$message))
  })
  #options(show.error.messages = TRUE)
  
  konto <- data.frame(bank = bank, konto = konto, name = display_name, ID = ID)
  bank_konto <- bind_rows(konto, bank_konto)
  
}


################################################################################
# UI ###########################################################################

ui <- page_fluid(
  tabsetPanel(
    tabPanel("Finanzkonten", 
             finanzkontenUI("finanzkonten", finanzkonto, bank_konto)  # Call the UI function from the module
    ),
    tabPanel("Girokonten - Auswertung",
             auswertunggiroUI("auswertunggiro", finanzkonto)),
    tabPanel("Upload",
             uploadUI("upload", finanzkonto))
  )
)



################################################################################
# SERVER #######################################################################
# Define the server logic for the main app
server <- function(input, output, session) {
  finanzkontenServer("finanzkonten", finanzkonto, bank_konto)  # Call the server function from the module
  auswertunggiroServer("auswertunggiro", finanzkonto)
  uploadServer("upload", finanzkonto)
}


shinyApp(ui, server)
