gruppenUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    br(),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Gruppen",
        icon = bsicons::bs_icon("collection"),
         fluidRow(
           column(4,
                  actionButton(ns("add_row"), label = "Eintrag hinzufügen", icon = icon("plus"))
           ),
           column(8,
                  uiOutput(ns("save_ui"))
           )
         ),
         fluidRow(
           column(12,
                  br(),
                  DTOutput(ns("gruppen_table")),
                  br()
           )
         )
        )
    )
  )
}

gruppenServer <- function(id, gruppen_path = "./MANUALDATA/gruppen.csv", on_save_callback = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_reactive <- reactiveValues()
    changed_flag <- reactiveVal(FALSE)
    
    # Load initial data
    data_reactive$df <- if (file.exists(gruppen_path)) {
      read.csv(gruppen_path, stringsAsFactors = FALSE)
    } else {
      data.frame(patterns = character(), name = character(), kategorie = character(), stringsAsFactors = FALSE)
    }
    
    # Render DataTable
    output$gruppen_table <- renderDT({
      datatable(
        data_reactive$df,
        editable = TRUE,
        selection = "none",
        rownames = FALSE
      )
    }, server = TRUE)
    
    # Modal for row input
    show_add_modal <- function() {
      existing_kategorien <- unique(data_reactive$df$kategorie)
      showModal(
        modalDialog(
          title = "Neue Gruppe hinzufügen",
          selectInput(ns("modal_kategorie"), "Kategorie", choices = existing_kategorien, selected = NULL),
          textInput(ns("modal_pattern"), "Pattern"),
          textInput(ns("modal_name"), "Name"),
          selectInput(ns("modal_spalte"), "Spalte", choices = c("GEGENSEITE", "VERWENDUNGSZWECK"), selected = "GEGENSEITE"),
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton(ns("confirm_add_row"), "Hinzufügen")
          ),
          easyClose = TRUE
        )
      )
    }
    
    observeEvent(input$add_row, {
      show_add_modal()
    })
    
    observeEvent(input$confirm_add_row, {
      req(input$modal_pattern, input$modal_name, input$modal_kategorie, input$modal_spalte)
      
      new_row <- data.frame(
        kategorie = input$modal_kategorie,
        name = input$modal_name,
        patterns = input$modal_pattern,
        spalte = input$modal_spalte,
        stringsAsFactors = FALSE
      )
      
      data_reactive$df <- bind_rows(data_reactive$df, new_row)
      changed_flag(TRUE)
      removeModal()
    })
    
    # Watch for edits
    observeEvent(input$gruppen_table_cell_edit, {
      info <- input$gruppen_table_cell_edit
      df <- data_reactive$df
      row <- info$row
      col <- info$col
      value <- info$value
      
      if (!is.null(row) && !is.null(col)) {
        df[row, col + 1] <- DT::coerceValue(value, df[row, col + 1])
        data_reactive$df <- df
        changed_flag(TRUE)
      }
    })
    
    # Render save button UI
    output$save_ui <- renderUI({
      if (isTRUE(changed_flag())) {
        actionButton(ns("save_changes"), "💾 Datensatz speichern")
      }
    })
    
    # Save logic
    observeEvent(input$save_changes, {
      write.csv(data_reactive$df, gruppen_path, row.names = FALSE)
      changed_flag(FALSE)
      if (!is.null(on_save_callback)) on_save_callback()
    })
  })
}