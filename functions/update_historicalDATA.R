

update_historicalDATA <- function(datapath = "./historicalDATA/") {
    
  message("... updating historcalData")
  
  files <- list.files(datapath, full.names = TRUE)
  files <- files[!str_detect(files, "SPARBRIEF")]
  
  for(i in files){
    
    olddata <- read.csv(i) %>%
      mutate(DATUM = as.Date(DATUM),
             price = as.numeric(price)) %>%
      # enusre to have the most recent price
      slice(-1)
    
    maxdate_olddata <- max(olddata$DATUM)

    if(maxdate_olddata < Sys.Date()) {
    
      if(str_detect(i, "BTC"))          wp <- "https://btcdirect.eu/de-at/bitcoin-kurs"
      if(str_detect(i, "IE00BK5BQT80")) wp <- "https://www.investing.com/etfs/vanguard-ftse-all-world-ucits-acc-historical-data?cid=1148060"
      if(str_detect(i, "IE00B5BMR087")) wp <- "https://www.investing.com/etfs/cs-etf-(ie)-on-s-p-500-historical-data?cid=45844"
      if(str_detect(i, "LU0908500753")) wp <- "https://www.investing.com/etfs/lyxor-stoxx-europe-600-dr-c-historical-data?cid=1156753"
      
      tryCatch({
        # Reading the HTML and extracting tables
        if(str_detect(i, "BTC")) {
          
          tables <- read_html(wp) %>% html_table()
          newdata <- tables[[1]] %>%
            mutate(DATUM = as.Date(Datum, "%m/%d/%y"),
                   price = as.numeric(gsub("€|,", "", Preis))) %>%
            select(DATUM, price)
        } else {
          
          tables <- read_html(wp) %>% html_table()
          newdata <- tables[[2]] %>%
            mutate(DATUM = as.Date(Date, format = "%b %d, %Y")) %>%
            mutate(price = as.numeric(Price)) %>%
            select(DATUM, price) 
        }
        
        newdata <- newdata %>% filter(DATUM > maxdate_olddata)
        
        # Updating the history and saving to CSV
        updated_history <- bind_rows(newdata, olddata)
        write.csv(updated_history, i, row.names = FALSE)
        
        # Success message (optional)
        message("sucessfully updated ", i)
        
      }, error = function(e) {
        # Error handling
        message("ERROR in ", i, ": ", e$message)
      })
      
      
    } else {
      
      message(i, " is already UPTODATE")
      
    }
    
  }
  
}


