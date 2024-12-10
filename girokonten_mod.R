# Betrachten aller Konten (2) als Ganzen: Wie viel kommt rein (von Außerhalb) und 
# wie viel geht raus. Dabei ist es egal, um welches Konto es sich handelt.
# Verwendet bisher nur Girokonten von ING und Trade republik.
# Idealerweise aufzeigen, wie viel in Sparplan geht und wie viel zu Flatex geht.

# Verwendet BETRAG_EDITED . Diese Variable kombiniert etwa Rückzahlungen aus dem 
# Onlineshopping, 

# ==============================================================================
# FUNKTIONEN ===================================================================
# ==============================================================================

# Nettobilanz ------------------------------------------------------------------
nettobilanz <- function(info) {
  
  data <- info$data
  
  # aggregate data ---------------------
  plot_data <- data %>%
    group_by(year_month) %>%
    summarise(sum = sum(BETRAG_EDITED, na.rm = TRUE), .groups = "drop") %>%
    mutate(typ = case_when(sum < 0 ~ "Defizit", .default = "Überschuss"))
  
  # create plot ------------------------
  hc <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = plot_data$year_month, title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = "Betrag (€)")) %>%
    hc_add_series(
      name = "Total",
      data = plot_data$sum,
      colorByPoint = TRUE,  # Color each point based on its value
      dataLabels = list(enabled = TRUE, format = "{point.y:.2f} €"),
      showInLegend = F
    ) %>%
    hc_plotOptions(column = list(
      dataLabels = list(enabled = TRUE, format = "{point.y:.2f} €"),
      pointPadding = 0.2,
      groupPadding = 0.1
    )) %>%
    hc_colors(ifelse(plot_data$sum < 0, "red", "darkgreen")) %>%
    hc_tooltip(shared = TRUE, valueDecimals = 2, valueSuffix = "€")
  
  return(hc)
}

# Verlauf ----------------------------------------------------------------------
verlauf <- function(info) {
  
  data <- info$data
  
  if(info$art == "Verhältnis") {
    sum_einnahmen_monthly <- data %>% 
      filter(typ == "Einnahme") %>%
      group_by(year_month) %>%
      summarise(sum_einnahmen_month = sum(BETRAG_EDITED))
  }
  
  # subset nach einnahmen & ausgaben
  if(info$art == "Einnahmen") plot_data <- data %>% filter(BETRAG_EDITED > 0) 
  if(info$art %in% c("Ausgaben", "Verhältnis")) plot_data <- data %>% filter(BETRAG_EDITED < 0)
  
  plot_data %<>%
    group_by(year_month, kategorie = as.character(kategorie)) %>%
    summarise(sum = abs(sum(BETRAG_EDITED, na.rm = TRUE)), .groups = "drop") %>%
    complete(year_month, kategorie, fill = list(sum = 0)) %>%  
    mutate(year_month = as.character(year_month))
  
  # summe pro monat --------------------
  # für ausgaben/einnahmen
  if(info$art == "Verhältnis") {
    plot_data %<>% left_join(sum_einnahmen_monthly, by = "year_month") %>%
      mutate(sum = round(sum/sum_einnahmen_month*100,2))
  }
  
  # smooth als mittelwert --------------
  if(info$smooth == "Mittelwert") {
    plot_data %<>%
      group_by(kategorie) %>% 
      mutate(sum = mean(sum)) %>%
      ungroup()
  }
  
  # 3 month rolling --------------------
  if(info$smooth == "3-Monats Mittel") {
    plot_data %<>%
      group_by(kategorie) %>%
      mutate(sum = zoo::rollapply(sum, 3, mean,align='right', fill=NA)) %>%
      ungroup()
  }
  
  # order data -------------------------
  # so largest is at the bottom of the graph
  order <- plot_data %>% 
    group_by(kategorie) %>%
    summarise(sum = sum(sum, na.rm = TRUE)) %>%
    arrange(sum) %>%
    rowid_to_column("order") %>%
    select(-sum)
  plot_data %<>% left_join(order, by = "kategorie") %>%
    arrange(order)
  
  # Calculate the total value ----------
  # for each year_month
  total_data <- plot_data %>%
    group_by(year_month) %>%
    summarise(total = round(sum(sum, na.rm = TRUE),0), .groups = "drop") 
  
  # create verlauf plot ----------------
  hc <- highchart() %>%
    hc_chart(type = "area") %>%
    hc_xAxis(categories = unique(plot_data$year_month), title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = if(info$art == "Verhältnis") "Ausgaben in % von Einnahmen" else if(info$art == "Ausgaben") "Ausgaben (€)" else if(info$art == "Einnahmen") "Einnahmen (€)")) %>%
    hc_plotOptions(area = list(stacking = "normal", lineColor = "black", lineWidth = 1)) %>%
    hc_title(text = NULL) %>%
    hc_add_series_list(
      lapply(unique(plot_data$kategorie), function(cat) {
        list(
          name = cat,
          data = plot_data %>% 
            filter(kategorie == cat) %>%
            arrange(year_month) %>%
            pull(sum)
        )
      })
    ) %>%
    # Add total as a separate series for labels
    hc_add_series(
      name = "Total",
      type = "line", # Use a line to avoid additional stacking
      data = total_data$total,
      color = "transparent", # Make the line invisible
      dataLabels = list(
        enabled = TRUE,
        format = if(info$art == "Verhältnis") "{y}%" else "€{y}", # Format the label
        style = list(fontWeight = "bold", color = "black") # Customize label style
      ),
      marker = list(enabled = FALSE), # Hide line markers
      enableMouseTracking = FALSE # Disable interactivity for the total series
    ) %>%
    hc_legend(enabled = TRUE) %>%
    hc_tooltip(shared = TRUE, 
               valueDecimals = 2,
               valueSuffix = if(info$art == "Verhältnis") "%" else "€")
  
  return(hc)
}


# Sankey diagramm --------------------------------------------------------------
sankey <- function(info) {
  
  data <- info$data
  
  # Step 1: Prepare the data for income flows
  income_flows <- data %>%
    filter(BETRAG_EDITED > 0) %>%
    group_by(from = name, to = kategorie) %>%
    summarise(weight = sum(BETRAG_EDITED), .groups = 'drop') %>%
    # For ordering largest group to smallest
    ungroup() %>%
    group_by(to) %>%
    mutate(sum_to = sum(weight)) %>%
    arrange(-sum_to, to) %>%
    select(-sum_to)
  
  # Step 3: Aggregate income by category
  income_aggregation <- data %>%
    filter(BETRAG_EDITED > 0) %>%
    group_by(from = kategorie) %>%
    summarise(weight = sum(BETRAG_EDITED), .groups = 'drop') %>%
    ungroup() %>%
    mutate(to = "Einkommen", .after = from) %>%
    # Order
    arrange(-weight)
  
  # Step 4: Aggregate expenses by category
  expense_aggregation <- data %>%
    filter(BETRAG_EDITED < 0) %>%
    group_by(to = kategorie) %>%
    summarise(weight = sum(-BETRAG_EDITED), .groups = 'drop') %>%
    ungroup() %>%
    mutate(from = "Ausgaben", .before = to) %>% # Aggregate to a single "Ausgaben" node
    # Arrange
    arrange(desc(weight))
  
  # Adjust for savings or deficit
  if (sum(income_aggregation$weight) > sum(expense_aggregation$weight)) {
    sparen <- sum(income_aggregation$weight) - sum(expense_aggregation$weight)
    sparen <- data.frame(from = "Einkommen", to = "Überschuss", weight = sparen)
    ausgaben <- data.frame(from = "Einkommen", to = "Ausgaben", weight = sum(expense_aggregation$weight))
    umschlag <- bind_rows(ausgaben, sparen)
  } else if (sum(income_aggregation$weight) < sum(expense_aggregation$weight)) {
    defizit <- sum(expense_aggregation$weight) - sum(income_aggregation$weight)
    defizit <- data.frame(from = "Defizit", to = "Ausgaben", weight = defizit)
    einnahmen <- data.frame(from = "Einkommen", to = "Ausgaben", weight = sum(income_aggregation$weight))
    umschlag <- bind_rows(einnahmen, defizit)
    income_flows <- bind_rows(income_flows, data.frame(from = "x", to = "y", weight = 0))
    income_aggregation <- bind_rows(income_aggregation, data.frame(from  = "y", to = "Defizit", weight = 0))
  }
  
  # Step 5: Combine all parts for the Sankey diagram
  sankey_data <- bind_rows(
    income_flows,
    income_aggregation,
    umschlag,
    expense_aggregation,
  )
  
  # Werte für einen Durchschnittsmonat
  if(info$show == "Durchschnittsmonat") {
    days <- difftime(info$filter_date[2], info$filter_date[1], units = "days")
    months <- interval(info$filter_date[1], info$filter_date[2]) %/% months(1) + 1 
    cat("\nNumber of days selected:", days)
    cat("\n= Number of months:", days/30)
    cat("\nNumber of months by forumla:", months)
    sankey_data %<>% 
      mutate(weight = round(weight / months, 2))
  }
  
  # Step 6: Create the Sankey diagram using highcharter
  hc <- highchart() %>%
    hc_chart(type = "sankey") %>%
    hc_title(text = NULL) %>%
    hc_add_series(data = sankey_data %>%
                    mutate(from = as.character(from), to = as.character(to)),
                  type = "sankey",
                  dataLabels = list(enabled = TRUE))
  
  return(hc)
}

tabelle <- function(info) {
  
  data <- info$data
  
  data %>%
    select(all_of(info$auswahl), DATUM, GEGENSEITE, VERWENDUNGSZWECK, BETRAG_EDITED) %>%
    reactable(
      groupBy = c(info$auswahl),
      columns = list(
        BETRAG_EDITED = colDef(aggregate = "sum", format = colFormat(currency = "EUR"))
      )
    )
}
        

# ==============================================================================
# UI ===========================================================================
# ==============================================================================

girokontenUI <- function(id, girokonten) {
  
  ns <- NS(id)  # Namespace function to scope the module
  
  accordion_panels <- lapply(girokonten, function(account) {
    account_id <- account$name  # Unique name for each account, used for output ID
    bank <- account$bank
    saldo <- format(account$data$SALDO[1], big.mark = ".", decimal.mark = ",")
    accordion_panel(
      title = paste0(account$display_name, " | ", saldo, "€"),  # Display name for the panel title
      icon = img(src = paste0("icons/", bank, ".png"), style = "width: 20px; height: 20px;"),  # Custom icon with inline style
      fluidRow(
        column(12,
               DTOutput(ns(account_id))  # Dynamic output ID for each account
        )
      )
    )
  })
  
  tagList(
    fluidRow(

      h1(""), h1(""),
      column(12, 
             accordion(
               # Insert all dynamic account accordion panels here
               do.call(tagList, accordion_panels),
               open = FALSE  # Default open section
             )
      ),
      
      h1(""), tags$head(tags$style(HTML("hr {border-top: 10px solid #111111;}"))), h1(""),

      column(12,
             accordion(
               accordion_panel(
                 title = "Auswahl filtern",
                 icon = bsicons::bs_icon("filter"),
                 fluidRow(
                   column(3,
                          dateRangeInput(ns("filter_date"), "Datum auswählen",
                                         start = floor_date(Sys.Date(), unit = "month") %m-% years(1),
                                         end = Sys.Date(),
                                         max = Sys.Date())
                   ),
                   column(4, 
                          selectInput(ns("filter_kategorien_einnahmen"),
                                      "Kategorien in Einnahmen",
                                      choices = NULL, selected = NULL,
                                      multiple = TRUE, width = "100%")),
                   column(5,
                          selectInput(ns("filter_kategorien_ausgaben"),
                                      "Kategorien in Ausgaben",
                                      choices = NULL, selected = NULL,
                                      multiple = TRUE, width = "100%"))
                 )
               ),
               
               accordion_panel(
                 title = "Nettobilanz",
                 icon = bsicons::bs_icon("bar-chart"),
                 highchartOutput(ns("nettobilanz_plot"))  # Render Ausgaben plot here
               ),
               
               accordion_panel(
                 title = "Verlauf", 
                 icon = bsicons::bs_icon("graph-up"),
                 fluidRow(
                   column(6, 
                          radioButtons(ns("verlauf_art"), "Verlauf für:",
                                       choices = c("Einnahmen", "Ausgaben", "Verhältnis"),
                                       inline = TRUE)),
                   column(6,
                          radioButtons(ns("verlauf_smooth"), "Werte anzeigen als:",
                                       choices = c("Total", "Mittelwert", "3-Monats Mittel"),
                                       inline = TRUE))
                 ),
                 highchartOutput(ns("verlauf_plot"))  # Render Einnahmen plot here
               ),
               
               accordion_panel(
                 title = "Sankey",
                 icon = bsicons::bs_icon("diagram-3"),
                 radioButtons(ns("sankey_values"), "Werte anzeigen als:",
                              choices = c("Summe", "Durchschnittsmonat"),
                              inline = TRUE),
                 highchartOutput(ns("sankey_plot"))
               ),
               accordion_panel(
                 title = "Übersicht - Tabelle",
                 icon = bsicons::bs_icon("table"),
                 fluidRow(
                   column(6, selectInput(ns("reactable_auswahl"), "Auswahl",
                                         choices = c("Jahr" = "year",
                                                     "Monat" = "year_month",
                                                     "Einnahme/Ausgabe" = "typ",
                                                     "Kategorie" = "kategorie",
                                                     "Name" = "name"),
                                         multiple = TRUE,
                                         width = "100%"))
                 ),
                 reactableOutput(ns("tabelle_reactable"))  # Render Ausgaben plot here
               ),
               id = "acc"
             )
      )
    )
  )
}


# ==============================================================================
# SERVER =======================================================================
# ==============================================================================

girokontenServer <- function(id, girokonten) {
  moduleServer(id, function(input, output, session) {
    
    # prepare data for page ----------------------------------------------------
    # Loop over each account in the finanzkonto list
    lapply(girokonten, function(account) {
      account_id <- account$name  # Assuming 'name' is the account ID
      bank <- account$bank
      konto <- account$konto
      
      # Render the editable DataTable for each account
      output[[account_id]] <- renderDT({
        req(account$data)  # Ensure account data is available
        DT::datatable(
          account$data,
          rownames = FALSE,
          filter = 'top',
          options = list(pageLength = -1, dom = 't'),
          editable = TRUE  # Enable cell editing
        )
      })
      
      # Capture the edits and save to the respective CSV file dynamically
      observeEvent(input[[paste0(account_id, '_cell_edit')]], {
        info <- input[[paste0(account_id, '_cell_edit')]]
        
        # Update the account's data with the edited value
        edited_data <- account$data
        edited_data[info$row, info$col + 1] <- info$value  # Update the correct cell
        
        # Define file path
        file_name <- paste0("./DATA/", account_id, ".csv")
        
        # Write the comment lines first
        comment_bank <- paste0("# bank:  ", bank)
        comment_konto <- paste0("# konto: ", konto)
        
        # Write the comments (if not already in the file)
        write_lines(comment_bank, file_name)  # Write the bank comment
        write_lines(comment_konto, file_name, append = TRUE)  # Write the konto comment
        
        # Check if file exists and is not empty
          # Write the column names as the second line (if the file doesn't exist or is empty)
          write_lines(paste(colnames(edited_data), collapse = ","), file_name, append = TRUE)
        
        # Append the actual data using write_csv (without column names)
        write_csv(edited_data, file_name, append = TRUE)
      })
    })

    
    # komplette daten
    GIR_allekategorien <- reactive({
      
      # Girokonten sind konten mit GIR oder GTH im Namen
      GIR <- bind_rows(lapply(girokonten, function(list) list[["data"]])) %>%
        
        mutate(DATUM = as.Date(DATUM)) %>%
        # filter date aus auswahl - zuerst, um speed zu erhöhen für join der kategorien
        filter(DATUM >= input$filter_date[1] & DATUM <= input$filter_date[2])  %>%
        
        # eigene Konten ausschließen
        filter(!str_detect(GEGENSEITE, "_GIR"),
               !str_detect(GEGENSEITE, "_GTH"),
               !str_detect(GEGENSEITE, "_EXT")) %>%
        filter(BETRAG_EDITED != 0) %>%
        
        mutate(monat_jahr = format(DATUM, "%Y-%m")) %>%
        
        mutate(year = format(DATUM, "%Y")) %>%
        mutate(year_month = format(DATUM, "%Y-%m")) %>%
        # eigene klasse für aktien, damit nicht angezeit wird, in einnahmen, bzw ausgaben, aber in 
        mutate(typ = factor(case_when(BETRAG_EDITED < 0 ~ "Ausgabe", 
                                      .default = "Einnahme")))
      
      # load gruppen for matching ------
      gruppen <- read.csv("./DATA/gruppen.csv")  %>%
        filter(kategorie != "") %>%
        mutate(patterns = tolower(patterns))
      patterns <- gruppen$patterns
      
      # add Gruppen --------------------
      GIR %<>% 
        rowwise() %>%
        mutate(
          lower_gegenseite = tolower(GEGENSEITE),
          lower_verwendungszweck = tolower(VERWENDUNGSZWECK),
          patterns = case_when(
            nchar(lower_gegenseite) == 0 & nchar(lower_verwendungszweck) == 0 ~ NA_character_,
            TRUE ~ {
              valid_patterns <- patterns[patterns != ""]
              matched_pattern <- valid_patterns[sapply(valid_patterns, function(pattern) {
                str_detect(lower_gegenseite, pattern) | str_detect(lower_verwendungszweck, pattern)
              })]
              if (length(matched_pattern) > 0) matched_pattern[1] else NA_character_
            }
          )
        ) %>%
        ungroup() %>%
        left_join(gruppen %>% distinct(patterns, kategorie, name), by = "patterns") %>%
        mutate(
          kategorie = ifelse(is.na(kategorie), "sonstige", kategorie),
          name = ifelse(is.na(name), GEGENSEITE, name)
        ) %>%
        select(-lower_gegenseite, -lower_verwendungszweck, -patterns) %>%
        mutate(kategorie = factor(kategorie),
               name = factor(name))
      
      return(GIR)
      
    })
    
    # Kategorien aus GIR ---------------
    reactive_choices_einnahmen <- reactive({
      GIR_allekategorien() %>% filter(BETRAG_EDITED > 0) %>%
        pull(kategorie) %>% unique()
    })
    
    reactive_choices_ausgaben <- reactive({
      GIR_allekategorien() %>% filter(BETRAG_EDITED < 0) %>%
        pull(kategorie) %>% unique()
    })
    
    # Filter dynamisch updaten ---------
    observe({
      # für kategorien - einnahmen
      updateSelectInput(session,
                        "filter_kategorien_einnahmen",
                        choices = reactive_choices_einnahmen(),
                        selected = reactive_choices_einnahmen()) # Preserve existing selection
    })
    
    observe({
      # für kategorien - ausgaben
      updateSelectInput(session,
                        "filter_kategorien_ausgaben",
                        choices = reactive_choices_ausgaben(),
                        # in defaul auswahl etf& sparbrief ausschließen
                        selected = reactive_choices_ausgaben()[!reactive_choices_ausgaben() %in% c("ETF", "Sparbrief", "Crypto")]) # Preserve existing selection
    })
    
    GIR <- reactive({
      # filter kategorien von auswahl
      GIR_allekategorien() %>% filter(kategorie %in% c(input$filter_kategorien_einnahmen,
                                                       input$filter_kategorien_ausgaben))
    })
    
    
    # Elements -----------------------------------------------------------------
    
    # Nettobilanz ----------------------
    output$nettobilanz_plot <- renderHighchart({
      req(GIR())
      nettobilanz_info <- list(
        data = GIR()
      )
      nettobilanz(nettobilanz_info)
    })
    
    
    # Verlauf --------------------------
    output$verlauf_plot <- renderHighchart({
      req(GIR())
      info_verlauf <- list(
        data = GIR(),
        art = input$verlauf_art, # Einnahme/Ausgabe/Verhältnis
        smooth = input$verlauf_smooth
      )
      verlauf(info = info_verlauf)
    })
    
    
    # Sankey Diagramm -------------------
    output$sankey_plot <- renderHighchart({
      req(GIR())
      cat("\nSankey anzeigen als:", input$sankey_values)
      # list to pass input info into function
      sankey_input <- list(
        data = GIR(),
        show = input$sankey_values, 
        filter_date = input$filter_date # zur berechnung von durchschnittsmonat
      )
      sankey(info = sankey_input)
    })
    
    
    # Reactable ------------------------
    output$tabelle_reactable <- renderReactable({
      req(GIR())
      tabelle_input <- list(
        data = GIR(),
        auswahl = input$reactable_auswahl
      )
      tabelle(tabelle_input)
    })
    
  })
}