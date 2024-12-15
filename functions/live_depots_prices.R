
# Live Preise der Depots scrapen
message("Live updating Stock Prices")

# depots laden -------------------------
depots <- listread_DATA(DATAPATH, filter_typID = "DEP")

# get prices for ISINs -----------------
# alles isins in dataframe
isins <- lapply(depots, function(x) x[["data"]] %>% distinct(ISIN)) %>% bind_rows() %>% distinct()

tryCatch({
  # Initialize an empty dataframe
  PREISE <- data.frame()
  
  # Loop through ISINs and scrape prices
  for (i in isins$ISIN) {
    wp <- read_html(paste0("https://www.wienerborse.at/marktdaten/exchange-traded-funds/preisdaten/?ISIN=", i))
    price <- as.numeric(gsub(",", ".", rvest::html_table(wp)[[1]][["Preis"]][1]))
    data <- data.frame(ISIN = i, PREIS = price)
    PREISE <- bind_rows(PREISE, data)
  }
  
  # Overwrite data with new info
  lapply(depots, function(x) {
    bank <- x[["bank"]]
    konto <- x[["konto"]]
    id <- x[["name"]]
    
    depot_neu <- x[["data"]] %>% 
      left_join(PREISE, by = "ISIN") %>%
      mutate(GESAMTWERT = round(ANZAHL * PREIS, 2)) %>%
      select(-PREIS)
    
    overwrite_DATA(bank, konto, id, depot_neu)
  })
  
  message("Stock Prices Updated")
  
}, error = function(e) {
  message(paste("Error in processing:", e$message))
  message("Depot Gesamtwerte wurden NICHT aktualisiert.")
})


