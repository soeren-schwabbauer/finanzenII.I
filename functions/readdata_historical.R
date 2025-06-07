
# einlesen =====================================================================

readdata_historical <- function(datapath) {

  
  message("... reading ", datapath)
  
  files <- list.files(datapath, full.names = TRUE)
  files <- files[files != paste0(datapath, "/_info.txt")]
  historicaldata <- list()
  
  for(i in files) {
    
    filename <- gsub(".*\\/\\/", "", i)
    id <- gsub("_.*", "", filename)
    id <- gsub("\\..*", "", filename)
    
     data <- read.csv(i) %>%
      
      mutate(datum = as.Date(datum),
             preis = as.numeric(preis),
             id = id) %>%
      arrange(datum) 
    
     # every date needs a closingprice
    historicaldata[[id]] <- 
      
      data.frame(datum = seq.Date(from = min(data$datum),
                                 to   = Sys.Date(),
                                 by   = "day")) %>%
      
      left_join(data, by = "datum") %>%
      fill(preis, id, .direction = "down")
  }
  
  # all historical data
  historicalDATA <- bind_rows(historicaldata)
  
  return(historicalDATA)

}

