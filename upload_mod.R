
# load write_MANUALDATA()
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
    
    # eigene Konten umbenennen                           # Edits fürs Girokonto
    if(konto == "INGDDEFFXXX_GIR") {
      newdata <- newdata %>% mutate(GEGENSEITE = case_when(GEGENSEITE == "TradeRepublic - Girokonto" ~ "TRBKDEBBXXX_GIR",
                                                           GEGENSEITE == "TradeRepublic - Girokonto" ~ "TRBKDEBBXXX_GIR",
                                                           GEGENSEITE == "Soeren Schwabbauer" & 
                                                             VERWENDUNGSZWECK == "" ~ "TRBKDEBBXXX_GIR",
                                                           GEGENSEITE == "Flatex - Guthaben"        ~ "BIWBDE33XXX_GTH",
                                                           str_detect("Sparplan ISIN", VERWENDUNGSZWECK) &
                                                             GEGENSEITE == "Soeren Schwabbauer"     ~ "INGDDEFFXXX_EXT",
                                                           .default = GEGENSEITE))
    } else if(konto == "INGDDEFFXXX_EXT") {
      # entweder ist es eine abrechnung (kommt von depot) oder gegenseite ist girokonto
      newdata <- newdata %>% mutate(GEGENSEITE = case_when(str_detect(VERWENDUNGSZWECK, "WP-ABRECHNUNG") ~ "INGDDEFFXXX_DEP",
                                                           .default = "INGDDEFFXXX_GIR"))
    }
                                                         
                              
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
                             choices = c("ING - GIRO"        = "INGDDEFFXXX_GIR", 
                                         "ING - EXTRAKONTO"  = "INGDDEFFXXX_EXT", 
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
                 "- kein Upload für Depots",br(),
                 "- kein Upload für Bitpanda",br(),
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
      #validate(need(ext == "csv", "Please upload a CSV file"))
      
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
        
      
      write_MANUALDATA <- function(bank, konto, filename, data_neu, datapath = MANUALDATA_PATH){
        
        message("Overwriting DATA for ", bank, "-", konto)
        
        filepath <- paste0(datapath, filename, ".csv")
        
        # Write the comment lines first
        comment_bank <- paste0("# bank:  ", bank)
        comment_konto <- paste0("# konto: ", konto)
        
        # Write the comments (if not already in the file)
        write_lines(comment_bank, filepath)  # Write the bank comment
        write_lines(comment_konto, filepath, append = TRUE)  # Write the konto comment
        write_lines(paste(colnames(data_neu), collapse = ","), filepath, append = TRUE)
        
        # Append the actual data using write_csv (without column names)
        write_csv(data_neu, filepath, append = TRUE)
        
        message("Overwriting sucessfull")
        
      }
      
      
      write_MANUALDATA(bank = bank(), konto = konto(), filename = input$select_konto,
                       data_neu = x)
      

        
      # Show confirmation notification
      showNotification("File uploaded successfully!", type = "message", duration = 3)
      
    })
    
  })
}