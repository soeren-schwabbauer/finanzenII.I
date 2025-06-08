kontenUI <- function(id, finanzkonto) {
  ns <- NS(id)
  
  accordion_panels <- lapply(finanzkonto, function(account) {
    account_id <- account$name
    bank <- account$bank
    saldo <- format(account$data$saldo[1], big.mark = ".", decimal.mark = ",")
    
    accordion_panel(
      title = paste0(account$display_name, " | ", saldo, "€"),
      icon = img(src = paste0("icons/", bank, ".png"), style = "width: 20px; height: 20px;"),
      fluidRow(
        column(4, actionButton(ns(paste0("add_row_", account_id)), label = "Eintrag hinzufügen", icon = icon("plus"))),
        column(8, uiOutput(ns(paste0("save_btn_", account_id)))),
        column(12,
               br(),
               DTOutput(ns(paste0("table_", account_id)))
        )
      )
    )
  })
  
  tagList(
    fluidRow(
      column(12,
             accordion(
               do.call(tagList, accordion_panels),
               open = FALSE, multiple = FALSE
             )
      )
    )
  )
}


kontenServer <- function(id, finanzkonto) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_reactive <- reactiveValues()
    changed_flags <- reactiveValues()
    
    lapply(finanzkonto, function(account) {
      account_id <- account$name
      data_reactive[[account_id]] <- account$data
      changed_flags[[account_id]] <- FALSE
      
      # Tabelle rendern (editierbar)
      output[[paste0("table_", account_id)]] <- renderDT({
        datatable(
          data_reactive[[account_id]],
          editable = TRUE,
          selection = "none",
          filter = 'top',
          options = list(  pageLength = 5, autoWidth = TRUE)
        )
      }, server = TRUE)
      
      # Beobachte Zelländerungen
      observeEvent(input[[paste0("table_", account_id, "_cell_edit")]], {
        info <- input[[paste0("table_", account_id, "_cell_edit")]]
        df <- data_reactive[[account_id]]
        row <- info$row
        col <- info$col
        value <- info$value
        
        if (!is.null(row) && !is.null(col)) {
          df[row, col] <- DT::coerceValue(value, df[row, col])
          data_reactive[[account_id]] <- df
          changed_flags[[account_id]] <- TRUE
        }
      })
      
      # "Speichern" und "Löschen" Buttons anzeigen
      output[[paste0("save_btn_", account_id)]] <- renderUI({
        df <- data_reactive[[account_id]]
        
        tagList(
          if (isTRUE(changed_flags[[account_id]])) {
            actionButton(ns(paste0("save_", account_id)), "📅 Speichern")
          },
          if (nrow(df) > 0) {
            actionButton(ns(paste0("delete_last_", account_id)), "🗑️ Letzte Zeile löschen")
          }
        )
      })
      
      # Zeile hinzufügen: Modal anzeigen
      observeEvent(input[[paste0("add_row_", account_id)]], {
        df <- data_reactive[[account_id]]
        showModal(modalDialog(
          title = paste0("Neue Zeile für ", account$display_name),
          dateInput(ns("new_Datum"), "Datum", value = Sys.Date(), format = "yyyy-mm-dd"),
          selectizeInput(
            ns("new_Gegenseite"),
            "Gegenseite",
            choices = unique(df$gegenseite),
            selected = "",
            options = list(create = TRUE)
          ),
          selectizeInput(
            ns("new_Verwendungszweck"),
            "Verwendungszweck",
            choices = unique(na.omit(df$verwendungszweck)),
            selected = "",
            options = list(create = TRUE)
          ),
          numericInput(ns("new_Betrag"), "Betrag", value = 0),
          numericInput(ns("new_Betrag_edited"), "Betrag (korrigiert)", value = 0),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton(ns(paste0("confirm_add_", account_id)), "Hinzufügen")
          )
        ))
      })
      
      # Neue Zeile einfügen
      observeEvent(input[[paste0("confirm_add_", account_id)]], {
        removeModal()
        df <- data_reactive[[account_id]]
        
        vorheriger_saldo <- if (nrow(df) >= 1 && "saldo" %in% names(df)) {
          suppressWarnings(as.numeric(df[1, "saldo"]))
        } else {
          0
        }
        
        neuer_betrag <- input$new_Betrag
        neuer_saldo <- vorheriger_saldo + neuer_betrag
        
        new_row <- data.frame(
          datum = as.Date(input$new_Datum),
          gegenseite = input$new_Gegenseite,
          verwendungszweck = input$new_Verwendungszweck,
          betrag = neuer_betrag,
          betrag_edited = input$new_Betrag_edited,
          saldo = neuer_saldo,
          stringsAsFactors = FALSE
        )
        
        df <- rbind(new_row, df)
        data_reactive[[account_id]] <- df
        changed_flags[[account_id]] <- TRUE
      })
      
      # Letzte Zeile löschen
      observeEvent(input[[paste0("delete_last_", account_id)]], {
        df <- data_reactive[[account_id]]
        if (nrow(df) > 0) {
          df <- df[-1, ]  # erste Zeile löschen (neueste steht oben)
          data_reactive[[account_id]] <- df
          changed_flags[[account_id]] <- TRUE
        }
      })
      
      # Speichern
      observeEvent(input[[paste0("save_", account_id)]], {
        df <- data_reactive[[account_id]]
        file_path <- file.path("MANUALDATA", paste0(account_id, ".csv"))
        
        comment_lines <- c(
          paste0("# bank: ", account$bank),
          paste0("# konto: ", account$konto)
        )
        
        con <- file(file_path, open = "wt")
        writeLines(comment_lines, con)
        write.table(df, con, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
        close(con)
        
        changed_flags[[account_id]] <- FALSE
        showNotification(paste0("Tabelle für ", account$display_name, " gespeichert!"), type = "message")
      })
    })
  })
}
