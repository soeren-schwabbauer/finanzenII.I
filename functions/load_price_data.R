library(dplyr)
library(tidyr)
library(readr)

load_price_data <- function(id, type = c("depot", "wallet", "sparbrief")) {
  type <- match.arg(type)
  
  url <- paste0(
    "https://raw.githubusercontent.com/soeren-schwabbauer/finanzen-prices/main/data/",
    type, "/", id, ".csv"
  )
  
  read_csv(url, show_col_types = FALSE) %>%
    mutate(id = id, datum = as.Date(datum)) %>%
    group_by(id) %>%
    complete(datum = seq.Date(min(datum), Sys.Date(), by = "day")) %>%
    fill(preis, .direction = "down") %>%
    ungroup()
}
