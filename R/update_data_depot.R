library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(xml2)

message("Updating ETF data...")

# update nur an wochentagen nötig
is_weekday <- !weekdays(Sys.Date()) %in% c("Saturday", "Sunday")

# Liste aller CSV-Dateien im ETF-Ordner
files <- list.files("./data/depot", full.names = TRUE)

for (i in files) {
  
  # Alte Daten einlesen
  olddata <- read_csv(i, show_col_types = FALSE) %>%
    mutate(datum = as.Date(datum),
           preis = as.numeric(preis))
  
  # Wenn heute kein Wochenende ist, oberste Zeile ausschließen
  # für aktuellsten wert
  if (is_weekday) {
    olddata <- olddata %>% slice(-1)
  }
  
  maxdate_olddata <- max(olddata$datum)
  # Nur aktualisieren, wenn neuere datum nicht 
  if (is_weekday) {
    
    # URL anhand der Dateinamen erkennen
    if (str_detect(i, "IE00BK5BQT80")) wp <- "https://www.investing.com/etfs/vanguard-ftse-all-world-ucits-acc-historical-data?cid=1148060"
    if (str_detect(i, "IE00B5BMR087")) wp <- "https://www.investing.com/etfs/cs-etf-(ie)-on-s-p-500-historical-data?cid=45844"
    if (str_detect(i, "LU0908500753")) wp <- "https://www.investing.com/etfs/lyxor-stoxx-europe-600-dr-c-historical-data?cid=1156753"
    
    tryCatch({
      # HTML laden und Tabellen extrahieren
      tables <- read_html(wp) %>% html_table()
      
      newdata <- tables[[2]] %>%
        mutate(datum = as.Date(Date, format = "%b %d, %Y")) %>%
        mutate(preis = as.numeric(gsub(",", "", Price))) %>%
        select(datum, preis) %>%
        filter(datum > maxdate_olddata)
      
      # Daten kombinieren und speichern
      updated_history <- bind_rows(newdata, olddata)
      write_csv(updated_history, i)
      
      message("✅ Updated: ", basename(i))
      
    }, error = function(e) {
      message("❌ Error in ", basename(i), ": ", e$message)
    })
    
  } else {
    message("⏩ Already up-to-date: ", basename(i))
  }
}
