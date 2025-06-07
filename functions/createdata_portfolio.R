
createdata_portfolio <- function(manualdata, historicaldata) {
  
  message("... creating PORTFOLIODATA")
  historicaldata <- bind_rows(historicaldata)
  
  data <- 
    
    data.frame(bind_rows(manualdata)$data) %>%
    
    mutate(name = gegenseite) %>%
    select(name, datum, verwendungszweck, betrag) %>%
    # filter wertpapierkäufe/cryptokäufe
    filter(str_detect("ETFHANDEL|AKTIENHANDEL|CRYPTOHANDEL|SPARBRIEFHANDEL", verwendungszweck)) %>%
    
    separate_wider_delim(verwendungszweck, delim = "|", names = c("form", "id", "anzahl")) %>%
    mutate(form = gsub("HANDEL", "", form)) %>%
    # format variables
    mutate(betrag = -betrag) %>%
    
    # get cumulated number & plain saldo (without rendite)
    arrange(datum) %>%
    group_by(name, id) %>%
    mutate(anzahl = cumsum(anzahl),
           saldo_raw = cumsum(betrag)) %>%
    ungroup() %>%
    select(name, form, datum, id, anzahl, saldo_raw)
  
  
  data_ts <-
    
    expand.grid(id = unique(data$id),
                name = unique(data$name),
                datum = seq.Date(from = as.Date("2021-11-01"),
                                 to = Sys.Date(),
                                 by = "day")) %>%
    arrange(id, name, datum) %>%
    
    # add opening price for each id
    left_join(historicaldata, by = c("id", "datum")) %>%
    
    # add info on purchases
    left_join(data, by = c("datum", "id", "name")) %>%
    
    # assign each saldo
    group_by(name, id) %>%
    fill(anzahl, saldo_raw, form, .direction = "down") %>%
    ungroup() %>%
    # filter NAs for performance
    filter(!is.na(anzahl)) %>%
    
    # saldo mit rendite
    mutate(saldo_rendite = anzahl * preis) 
    
  
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
