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
# KONTEN #######################################################################
# Create the bank and konto combinations


# Create the dataframe using tribble with bank, konto, name, and ID columns
bank_konto <- tribble(
  ~bank,           ~konto,        ~name,                    ~ID,
  "ING",           "GIRO",        "ING - GIRO",             "INGDDEFFXXX_GIR",
  "ING",           "DEPOT",       "ING - DEPOT",            "INGDDEFFXXX_DEP",
  "ING",           "SPARBRIEF",   "ING - SPARBRIEF",        "INGDDEFFXXX_SPB",
  "ING",           "EXTRAKONTO",  "ING - EXTRAKONTO",       "INGDDEFFXXX_EXT",
  "TRADEREPUBLIC", "GIRO",        "TRADEREPUBLIC - GIRO",   "TRBKDEBBXXX_GIR",
  "TRADEREPUBLIC", "DEPOT",       "TRADEREPUBLIC - DEPOT",  "TRBKDEBBXXX_DEP",
  "FLATEX",        "DEPOT",       "FLATEX - DEPOT",         "BIWBDE33XXX_DEP",
  "FLATEX",        "GUTHABEN",    "FLATEX - GUTHABEN",      "BIWBDE33XXX_GTH"
)


wertpapiere <- c("iShares Core S&P 500"    = "IE00B5BMR087",
                 "Amundi Stoxx Europe 600" = "LU0908500753")

wertpapiere_df <- data.frame(
  ISIN = wertpapiere,
  Name = names(wertpapiere)
)

# Function to load financial account data

# Initialize the finanzkonto list (this will store all accounts)
finanzkonto <- list()

load_finanzkonten <- function(finanzkonto) {
  file_path <- paste0("./DATA/", finanzkonto, ".csv")
  if (!file.exists(file_path)) {
    message("File does not exist:", file_path)
    return(NULL)  # Return NULL if the file does not exist
  }
  read.csv(file_path)
}

# Loop through each account and load the data dynamically
for(i in 1:nrow(bank_konto)) {
  ID <- pull(bank_konto[i, "ID"])
  #options(show.error.messages = FALSE)
  tryCatch({
    # Load the data for the current ID
    account_data <- load_finanzkonten(ID)
    
    # Only add to finanzkonto if the data is not NULL
    if (!is.null(account_data)) {
      finanzkonto[[ID]] <- list(
        name = ID,  
        display_name = pull(bank_konto[i, "name"]),
        data = account_data
      )
    } else {
      message(paste("Data is NULL for", ID))
    }
  }, error = function(e) {
    message(paste("Error loading data for", ID, ":", e$message))
  })
  #options(show.error.messages = TRUE)
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
