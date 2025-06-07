
message("Updating Wallet Data")
  
# Liste nur Dateien mit "BTC" im Namen
files <- list.files("./data/wallet", full.names = TRUE)
files <- files[!str_detect(files, "info")]

for (i in files) {
  
  olddata <- read.csv(i) %>%
    mutate(datum = as.Date(datum),
           preis = as.numeric(preis)) %>%
    # letzte Zeile weglassen, um ggf. alten Preis zu ersetzen
    slice(-1)
  
  maxdate_olddata <- max(olddata$datum)
  
  if (maxdate_olddata < Sys.Date()) {
    
    wp <- "https://btcdirect.eu/de-at/bitcoin-kurs"
    
    tryCatch({
      
      tables <- read_html(wp) %>% html_table()
      
      newdata <- tables[[1]] %>%
        mutate(datum = as.Date(Datum, "%m/%d/%y"),
               preis = as.numeric(gsub("€|,", "", Preis))) %>%
        select(datum, preis)
      
      newdata <- newdata %>% filter(datum > maxdate_olddata)
      
      updated_history <- bind_rows(newdata, olddata)
      
      write.csv(updated_history, i, row.names = FALSE)
      
      message("successfully updated ", i)
      
    }, error = function(e) {
      message("ERROR in ", i, ": ", e$message)
    })
    
  } else {
    message(i, " is already UPTODATE")
  }
}
