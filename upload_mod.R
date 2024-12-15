
upload_newdata <- function(path_newdata, konto, olddata) {
  
  # Upload for ING Extrakonto and ING Girokonto 
  if(konto %in% c("INGDDEFFXXX_GIR", "INGDDEFFXXX_EXT")) {
   
    # Read and process the uploaded CSV file
    newdata <- readr::read_csv2(path_newdata, skip = 14, col_names = FALSE)
    
    # format & rename columns
    newdata$DATUM <- as.Date(newdata$X1, format = "%d.%m.%Y")
    newdata$GEGENSEITE <- newdata$X3
    newdata$VERWENDUNGSZWECK <- newdata$X5
    newdata$BETRAG <- newdata$X8
    newdata$BETRAG_EDITED = newdata$BETRAG
    newdata$GRUND_EDITED = ""
    newdata$SALDO = newdata$X6
    
    # remove original columns
    newdata <- newdata[,-c(1:9)]
    
    # avoid doubled rows
    newdata <- dplyr::filter(newdata,
                             DATUM >= max(as.Date(olddata$DATUM)) & 
                               !(DATUM %in% as.Date(olddata$DATUM) & 
                                   VERWENDUNGSZWECK %in% olddata$VERWENDUNGSZWECK))
  }
  
  # Upload for ING Depot
  if(konto == "INGDDEFFXXX_DEP") {
    
      # load new depotuebersicht
      newdata <- read.csv(path_newdata, sep = ";", dec = ",", skip = 0, header = FALSE)
      
      # extract date from 1strow-1stcol
      date <- stringr::str_extract(newdata[1,1], "\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b")
      date <- as.Date(date, format = "%d.%m.%Y")
      
      # subset depotuebersicht based on desired columns - rename cols
      newdata <- newdata[5:(nrow(newdata)-1),c(1,3,7,13,15)]
      colnames(newdata) <- c("ISIN", "ANZAHL", "EINSTANDSWERT", "GESAMTWERT", "ERTRAG")
      
      # date as dataframe nrow is the number of stocks - add to dataframe
      date <- data.frame(DATUM = rep(date, nrow(newdata)))
      newdata <- cbind(date, newdata)
      
      # transform variables
      newdata %<>% mutate(ANZAHL = as.numeric(gsub(",", ".", gsub("\\.", "", ANZAHL))),
                          EINSTANDSWERT = as.numeric(gsub(",", ".", gsub("\\.", "", EINSTANDSWERT))),
                          GESAMTWERT = as.numeric(gsub(",", ".", gsub("\\.", "", GESAMTWERT))),
                          ERTRAG = as.numeric(gsub(",", ".", gsub("\\.", "", ERTRAG))))
      
  }
  
  # Upload for Flatex Depot
  if(konto == "BIWBDE33XXX_DEP") {
    
    newdata <- read_csv2(path_newdata, col_names = FALSE) 
    
    newdata %<>% slice(-1) %>%
      
      mutate(DATUM = as.Date(X10, format = "%d.%m.%Y"),
             ISIN = X2,
             ANZAHL = as.numeric(gsub(",", ".", X4)),
             EINSTANDSWERT = as.numeric(gsub(",", ".", X20)),
             GESAMTWERT = as.numeric(gsub(",", ".", X18)),
             ERTRAG = as.numeric(gsub(",", ".", X22))) %>%
      select(-contains("X"))
    
  }
  
  if(konto == "BIWBDE33XXX_GTH") {
    
    newdata <- read_csv2(path_newdata, col_names = FALSE) 
    
    newdata %<>% slice(-1) %>%
      mutate(DATUM = as.Date(X1, format = "%d.%m.%Y"),
             GEGENSEITE = X4,
             VERWENDUNGSZWECK = X5,
             BETRAG = as.numeric(gsub(",", ".", X7)),
             SALDO = 0) %>%
      select(-contains("X")) %>%
      mutate(GEGENSEITE = case_when(GEGENSEITE == "DE76500105175427745862" ~ "INGDDEFFXXX_DEP",
                                    .default = GEGENSEITE))
  }
  
  return(newdata)
    
}




uploadUI <- function(id, finanzkonto) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, 
             br(),
             accordion(
               accordion_panel(
                 title = "Upload Optionen",
                 icon = bsicons::bs_icon("upload"),
                 selectInput(ns("select_konto"), "Konto auswählen",
                             choices = c("ING - GIRO" = "INGDDEFFXXX_GIR", 
                                         "ING - EXTRAKONTO" = "INGDDEFFXXX_EXT", 
                                         "ING - DEPOT" = "INGDDEFFXXX_DEP",
                                         "FLATEX - DEPOT" = "BIWBDE33XXX_DEP",
                                         "FLATEX - GUTHABEN" = "BIWBDE33XXX_GTH"),
                             selected = 1),
                 fileInput(ns("upload_file"), "Kontoauszug auswählen"),
                 actionButton(ns("upload_confirm"), "Hochladen",
                              icon = icon("upload"))
               )
             )
      ),
      
      column(8, 
             br(),
             accordion(
               accordion_panel(
                 title = "Upload Preview",
                 icon = bsicons::bs_icon("table"),
                 DTOutput(ns("newdata_preview"))
                 )
             )
      ),
      
      column(4, 
             br(),
             accordion(
               accordion_panel(
                 title = "Upload Info",
                 icon = bsicons::bs_icon("info"),
                 "1. Konto auswählen", br(),
                 "2. Kontoauszug hochladen", br(),
                 "3. Hochladen", br(),
                 
                 "wichtig:", br(),
                 "- kein Upload für TradeRepublik",br(),
                 "- kein Upload für Sparbrief",br(),
                 "- Korrekturen händisch in csv file vornehmen"
               )
             )
      )
    )
  )
}



uploadServer <- function(id, finanzkonto) {
  moduleServer(id, function(input, output, session) {
    
    # Store old data from the selected account
    olddata <- reactive({ finanzkonto[[input$select_konto]]$data })
    bank <- reactive({ finanzkonto[[input$select_konto]]$bank })
    konto <- reactive({ finanzkonto[[input$select_konto]]$konto })
    # Process new data after file upload
    newdata <- reactive({
      file <- input$upload_file
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a CSV file"))
      
      upload_newdata(
        path_newdata = file$datapath, 
        konto = input$select_konto,
        olddata = olddata()
      )
    })
    
    # Render a preview of the new data
    output$newdata_preview <- renderDT({
      datatable(newdata(),
                rownames = FALSE,
                editable = FALSE,
                filter = 'top',
                options = list(pageLength = -1, dom = 't')
      )
    })
    
    # Event for confirming upload
    observeEvent(input$upload_confirm, {
      cat("\n", intToUtf8(0x2139), "Upload Button wurde gedrückt")
      if (str_detect(input$select_konto, "GIR") |
          str_detect(input$select_konto, "GTH") |
          str_detect(input$select_konto, "EXT")) {
        # Append new data for "GIR" type account
        x <- rbind(newdata(), olddata())
        cat("\n", intToUtf8(0x2139), "Extrakonto, Girokonto, Guthaben angefügt")
      } else if (str_detect(input$select_konto, "DEP")) {
          x <- newdata()
          cat("\n", intToUtf8(0x2139), "Depot hochgeladen")
      }
        
        # Define file path
        file_name <- paste0("./DATA/", input$select_konto, ".csv")
        
        # Write the comment lines first
        comment_bank <- paste0("# bank:  ", bank())
        comment_konto <- paste0("# konto: ", konto())
        
        # Write the comments (if not already in the file)
        write_lines(comment_bank, file_name)  # Write the bank comment
        write_lines(comment_konto, file_name, append = TRUE)  # Write the konto comment
        
        # Check if file exists and is not empty
        # Write the column names as the second line (if the file doesn't exist or is empty)
        write_lines(paste(colnames(x), collapse = ","), file_name, append = TRUE)
        
        # Append the actual data using write_csv (without column names)
        write_csv(x, file_name, append = TRUE)
        
        #write.csv(x, file = paste0("./DATA/", input$select_konto, ".csv"), row.names = FALSE)
        cat("\n", intToUtf8(0x2139), "Neues File überschrieben")
        
        
      # Show confirmation notification
      showNotification("File uploaded successfully!", type = "message", duration = 3)
      
    })
    
  })
}