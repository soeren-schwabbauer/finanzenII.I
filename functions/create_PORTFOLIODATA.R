
create_PORTFOLIODATA <- function(manualdata, historicaldata) {
  
  message("... creating PORTFOLIODATA")
  
  data <- 
    
    data.frame(bind_rows(manualdata)$data) %>%
    
    mutate(name = GEGENSEITE) %>%
    select(name, DATUM, VERWENDUNGSZWECK, BETRAG) %>%
    # filter wertpapierkäufe/cryptokäufe
    filter(str_detect("ETFHANDEL|AKTIENHANDEL|CRYPTOHANDEL|SPARBRIEFHANDEL", VERWENDUNGSZWECK)) %>%
    
    separate_wider_delim(VERWENDUNGSZWECK, delim = "|", names = c("form", "ID", "ANZAHL")) %>%
    mutate(form = gsub("HANDEL", "", form)) %>%
    # format variables
    mutate(BETRAG = -BETRAG) %>%
    
    # get cumulated number & plain saldo (without rendite)
    arrange(DATUM) %>%
    group_by(name, ID) %>%
    mutate(ANZAHL = cumsum(ANZAHL),
           SALDO_raw = cumsum(BETRAG)) %>%
    ungroup() %>%
    select(name, form, DATUM, ID, ANZAHL, SALDO_raw)
  
  
  data_ts <-
    
    expand.grid(ID = unique(data$ID),
                name = unique(data$name),
                DATUM = seq.Date(from = as.Date("2021-11-01"),
                                 to = Sys.Date(),
                                 by = "day")) %>%
    arrange(ID, name, DATUM) %>%
    
    # add opening price for each ID
    left_join(historicaldata, by = c("ID", "DATUM")) %>%
    
    # add info on purchases
    left_join(data, by = c("DATUM", "ID", "name")) %>%
    
    # assign each saldo
    group_by(name, ID) %>%
    fill(ANZAHL, SALDO_raw, form, .direction = "down") %>%
    ungroup() %>%
    # filter NAs for performance
    filter(!is.na(ANZAHL)) %>%
    
    # saldo mit rendite
    mutate(SALDO_rendite = ANZAHL * price) 
    
  
  data_list <- split(data_ts, list(data_ts$name)) 
  

  data_list <- lapply(data_list, function(df) {
    
    df <- as.data.frame(df)  # Convert tibble to data.frame
    name = unique(df$name)

    # HARDCODE für angezeigte names
    if(name == "BIPGATWWXXX_WAL") {
      display_name = "BITPANDA - WALLET"
    } else if(name == "INGDDEFFXXX_DEP") {
      display_name = "ING - DEPOT"
    } else if(name == "BIWBDE33XXX_DEP") {
      display_name = "FLATEX - DEPOT"
    } else if(name == "INGDDEFFXXX_SPB"){
      display_name = "ING - SPARBRIEF"
    } else if(name == "TRBKDEBBXXX_DEP") {
      display_name = "TRADEREPUBLIC - DEPOT"
    }
    
    konto = gsub(".* ", "", display_name)
    bank  = gsub(" .*", "", display_name)
    
    list(
      name = name,  # Extract a unique name
      display_name = display_name,
      bank = bank,
      konto = konto,
      data = df
    )
  })
  
  return(data_list)
  
}
