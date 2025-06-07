
# einlesen =====================================================================

read_historicalDATA <- function(datapath) {

  message("... reading historicalDATA")
  
  files <- list.files(datapath, full.names = TRUE)
  historicaldata <- list()
  
  for(i in files) {
    
    filename <- gsub(".*\\/\\/", "", i)
    ID <- gsub("_.*", "", filename)
    
     data <- read.csv(i) %>%
      
      mutate(DATUM = as.Date(DATUM),
             price = as.numeric(price),
             ID = ID) %>%
      arrange(DATUM) 
    
     # every date needs a closingprice
    historicaldata[[ID]] <- 
      
      data.frame(DATUM = seq.Date(from = min(data$DATUM),
                                 to   = Sys.Date(),
                                 by   = "day")) %>%
      
      left_join(data, by = "DATUM") %>%
      fill(price, ID, .direction = "down")
  }
  
  # all historical data
  historicalDATA <- bind_rows(historicaldata)
  
  return(historicalDATA)

}

