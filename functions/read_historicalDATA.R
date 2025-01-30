
# einlesen =====================================================================

read_historicalDATA <- function(datapath) {

  message("... loading historicalDATA")
  
  files <- list.files(datapath, full.names = TRUE)
  historicaldata <- list()
  
  for(i in files) {
    
    filename <- gsub(".*\\/\\/", "", i)
    ID <- gsub("_.*", "", filename)
    
     data <- read.csv(i) %>%
      
      mutate(DATUM = as.Date(DATUM),
             openingprice = as.numeric(openingprice),
             ID = ID) %>%
      arrange(DATUM) 
    
     # every date needs a closingprice
    historicaldata[[ID]] <- 
      
      data.frame(DATUM = seq.Date(from = min(data$DATUM),
                                 to   = Sys.Date(),
                                 by   = "day")) %>%
      
      left_join(data, by = "DATUM") %>%
      fill(openingprice, ID, .direction = "down")
  }
  
  # all historical data
  historicalDATA <- bind_rows(historicaldata)
  
  return(historicalDATA)

}

