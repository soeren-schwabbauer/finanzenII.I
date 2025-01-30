

update_historicalDATA <- function(datapath) {
    
  message("... updating historcalData")
  
  files <- list.files(datapath, full.names = TRUE)
  
  # no history for bitcoin
  files <- files[!str_detect(files, "BTC")]
  
  for(i in files){
    
    olddata <- read.csv(i) %>%
      mutate(DATUM = as.Date(DATUM),
      openingprice = as.numeric(openingprice)) 
    
    maxdate_olddata <- max(olddata$DATUM)
    
    if(maxdate_olddata < Sys.Date()) {
      
      #if(str_detect(i, "BTC"))          wp <- "https://www.investing.com/crypto/bitcoin/btc-eur-historical-data"
      if(str_detect(i, "IE00BK5BQT80")) wp <- "https://www.investing.com/etfs/vanguard-ftse-all-world-ucits-acc-historical-data?cid=1148060"
      if(str_detect(i, "IE00B5BMR087")) wp <- "https://www.investing.com/etfs/cs-etf-(ie)-on-s-p-500-historical-data?cid=45844"
      if(str_detect(i, "LU0908500753")) wp <- "https://www.investing.com/etfs/lyxor-stoxx-europe-600-dr-c-historical-data?cid=1156753"
      
      tryCatch({
        # Reading the HTML and extracting tables
        tables <- read_html(wp) %>% html_table()
        
        # Selecting the second table
        history <- tables[[2]]
        
        # Data transformation and filtering
        newdata <- history %>%
          mutate(DATUM = as.Date(Date, format = "%b %d, %Y")) %>%
          mutate(openingprice = as.numeric(Open)) %>%
          select(DATUM, openingprice) %>%
          filter(DATUM > maxdate_olddata)
        
        # Updating the history and saving to CSV
        updated_history <- bind_rows(newdata, olddata)
        write.csv(updated_history, i, row.names = FALSE)
        
        # Success message (optional)
        message(i, " was sucessfully UPDATED")
        
      }, error = function(e) {
        # Error handling
        message("ERROR in ", i, ": ", e$message)
      })
      
      
    } else {
      
      message(i, " is already UPTODATE")
      
    }
    
  }
  
}

