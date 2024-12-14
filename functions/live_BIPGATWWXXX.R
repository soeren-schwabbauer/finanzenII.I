# Documentation: https://developers.bitpanda.com/#list-fiat-wallets

# File liest Daten aus & überschreibt die BIPGATWWXXX_GTH/WAL.csv in ./DATA
# File liest Daten aus & überschreibt die BIPGATWWXXX_GTH/DEP.csv in ./DATA

# HEADER =======================================================================
bank         <- "BITPANDA"
api_bitpanda <- read_lines(paste0("./apikeys/", bank))
base_url     <- "https://api.bitpanda.com/v1/"


# function to read data for bitpanda ===========================================
read_data <- function(url, api_key = api_bitpanda) {
  # Set headers
  headers <- c("X-Api-Key" = api_key)
  
  # Make GET request
  response <- GET(url, add_headers(.headers = headers))
  
  # Check the status code
  if (status_code(response) == 200) {
    # Parse and display the response
    data <- fromJSON(content(response, "text"), flatten = TRUE)
  } else {
    # Handle errors
    stop("Error: ", status_code(response), " - ", content(response, "text"))
  }
  
  return(data)
}



# BITPANDA GUTHABEN auslesen ===================================================

# base paramters -----------------------
konto <- "GUTHABEN"
ID <- "BIPGATWWXXX_GTH"
api_link <- "fiatwallets/transactions"
guthaben_old <- read.csv(paste0(DATAPATH, ID, ".csv"), comment.char = "#") 

# define url ---------------------------
url <- paste0(base_url, api_link)

tryCatch({
  
  # Read Guthaben ----------------------
  guthaben_neu <- read_data(url)$data
  
  # Guthaben in Format bringen ---------
  guthaben_neu <- guthaben_neu %>% mutate(DATUM = gsub("T.*", "", attributes.time.date_iso8601),
                                          GEGENSEITE = case_when(attributes.type == "buy" ~ "BIPGATWWXXX_WAL",
                                                                 attributes.type == "deposit" ~ "INGDDEFFXXX_GIR"),
                                          VERWENDUNGSZWECK = attributes.trade.attributes.cryptocoin_symbol,
                                          BETRAG = case_when(attributes.in_or_out == "incoming" ~ as.numeric(attributes.amount),
                                                             attributes.in_or_out == "outgoing" ~ as.numeric(attributes.amount) * (-1)),
                                          BETRAG_EDITED = BETRAG,
                                          GRUND_EDITED = "",
                                          ID = as.integer(attributes.time.unix)) %>%
    
                                   select(DATUM, GEGENSEITE, VERWENDUNGSZWECK, BETRAG, BETRAG_EDITED, GRUND_EDITED, ID) %>%
                                   # Compute SALDO with lead function
                                   mutate(
                                     SALDO = BETRAG + if_else(is.na(lead(BETRAG)), 0, lead(BETRAG)),
                                     .before = ID
                                   ) %>%
                                   # Filter out existing data
                                   filter(!ID %in% guthaben_old$ID) %>%
                                   # Join existing data
                                   bind_rows(guthaben_old)
  
  # Overwrite data ---------------------
  overwrite_DATA(data_neu = guthaben_neu, bank = bank, konto = konto, filename = ID)
  
  message("Data processing and overwriting successful!")
  
}, error = function(e) {
  # Error handling ---------------------
  message("An error occurred: ", e$message)
})



# BITPANDA WALLET auslesen =====================================================

# base parameters ----------------------
konto    <- "WALLET"
ID       <- "BIPGATWWXXX_WAL"
api_link <- "wallets"

message("Live updating ", bank, "-", konto)

# define urls --------------------------
url_wallet <- paste0(base_url, api_link)
url_prices <- paste0(base_url, "ticker")

tryCatch({
  
  # Read wallet data -------------------
  wallet_neu <- read_data(url_wallet)$data 
  
  # Edit wallet data -------------------
  wallet_neu <- wallet_neu %>% mutate(COIN  = attributes.cryptocoin_symbol,
                                      ANZAHL = as.numeric(attributes.balance)) %>%
                               select(COIN, ANZAHL)
  
  # Preis für jeden Coin ---------------
  coins <- wallet_neu %>% distinct(COIN) %>%
                          # live preis für jeden Coin in wallet ermitteln
                          mutate(PREIS = as.numeric(read_data(url_prices)[[i]][["EUR"]])) 
  
  # Wert für jeden Coint ---------------
  wallet_neu <- wallet_neu %>% left_join(coins, by = "COIN") %>% 
                               # Gesamtwert von jeder Position berechnen
                               mutate(GESAMTWERT = round(PREIS * ANZAHL, 2)) %>%
                               select(-PREIS)
  
  # Wallet neu überschreiben -----------
  overwrite_DATA(data_neu = wallet_neu, bank = bank, konto = konto, filename = ID)
  
  message("Data processing and overwriting successful!")
  
}, error = function(e) {
  # Error handling ---------------------
  message("An error occurred: ", e$message)
  # data is not overwritten
  
})