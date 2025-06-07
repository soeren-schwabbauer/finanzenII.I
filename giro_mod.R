# Betrachten aller Konten (2) als Ganzen: Wie viel kommt rein (von Außerhalb) und 
# wie viel geht raus. Dabei ist es egal, um welches Konto es sich handelt.
# Verwendet bisher nur Girokonten von ING und Trade republik.
# Idealerweise aufzeigen, wie viel in Sparplan geht und wie viel zu Flatex geht.

# Verwendet betrag_edited . Diese Variable kombiniert etwa Rückzahlungen aus dem 
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
    summarise(sum = sum(betrag_edited, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate(typ = case_when(sum < 0 ~ "Defizit", .default = "Überschuss")) %>%
    mutate(kategorie = "") 
  
  if(info$smooth == "Mittelwert") {
    plot_data %<>%
      group_by(kategorie) %>% 
      mutate(sum = mean(sum)) %>%
      ungroup()
  }
  
  if(str_detect(info$smooth, "Monats Mittel")) {
    monthavg <- as.numeric(gsub("-.*", "", info$smooth))
    plot_data %<>%
      group_by(kategorie) %>%
      mutate(sum = zoo::rollapply(sum, monthavg, mean,align='right', fill=NA)) %>%
      ungroup()
  }
  
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
      summarise(sum_einnahmen_month = sum(betrag_edited))
  }
  
  # subset nach einnahmen & ausgaben
  if(info$art == "Einnahmen") plot_data <- data %>% filter(betrag_edited > 0) 
  if(info$art %in% c("Ausgaben", "Verhältnis")) plot_data <- data %>% filter(betrag_edited < 0)
  
  plot_data %<>%
    group_by(year_month, kategorie = as.character(kategorie)) %>%
    summarise(sum = abs(sum(betrag_edited, na.rm = TRUE)), .groups = "drop") %>%
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
  if(str_detect(info$smooth, "Monats Mittel")) {
    monthavg <- as.numeric(gsub("-.*", "", info$smooth))
    plot_data %<>%
      group_by(kategorie) %>%
      mutate(sum = zoo::rollapply(sum, monthavg, mean,align='right', fill=NA)) %>%
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
    filter(betrag_edited > 0) %>%
    group_by(from = name, to = kategorie) %>%
    summarise(weight = sum(betrag_edited), .groups = 'drop') %>%
    # For ordering largest group to smallest
    ungroup() %>%
    group_by(to) %>%
    mutate(sum_to = sum(weight)) %>%
    arrange(-sum_to, to) %>%
    select(-sum_to)
  
  # Step 3: Aggregate income by category
  income_aggregation <- data %>%
    filter(betrag_edited > 0) %>%
    group_by(from = kategorie) %>%
    summarise(weight = sum(betrag_edited), .groups = 'drop') %>%
    ungroup() %>%
    mutate(to = "Einkommen", .after = from) %>%
    # Order
    arrange(-weight)
  
  # Step 4: Aggregate expenses by category
  expense_aggregation <- data %>%
    filter(betrag_edited < 0) %>%
    group_by(to = kategorie) %>%
    summarise(weight = sum(-betrag_edited), .groups = 'drop') %>%
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
    select(all_of(info$auswahl), datum, gegenseite, verwendungszweck, betrag_edited) %>%
    reactable(
      groupBy = c(info$auswahl),
      columns = list(
        betrag_edited = colDef(aggregate = "sum", format = colFormat(currency = "EUR"))
      )
    )
}
        

# ==============================================================================
# UI ===========================================================================
# ==============================================================================

giroUI <- function(id, girokonten) {
  ns <- NS(id)
  
  tagList(
    
    navset_card_underline(
      full_screen = TRUE,
      # Tabs with underline style
      nav_panel(
        "Nettobilanz",
        icon = bsicons::bs_icon("bar-chart"),
        highchartOutput(ns("nettobilanz_plot"))
      ),
      nav_panel(
        "Einnahmen",
        icon = bsicons::bs_icon("graph-up"),
        highchartOutput(ns("einnahmen_plot"))
      ),
      
      nav_panel(
        "Ausgaben",
        icon = bsicons::bs_icon("graph-down"),
        highchartOutput(ns("ausgaben_plot"))
      ),
      
      nav_panel(
        "Ausgaben / Einnahmen",
        icon = bsicons::bs_icon("percent"),
        highchartOutput(ns("verhältnis_plot"))
      ),
        
      nav_panel(
        "Sankey",
        icon = bsicons::bs_icon("diagram-3"),
        div(
          class = "card-reveal-full-screen",
          card_body(
            radioButtons(ns("sankey_values"), "Werte anzeigen als:",
                         choices = c("Summe", "Durchschnittsmonat"),
                         inline = TRUE)
          )
        ),
          highchartOutput(ns("sankey_plot"))
      ),
      
      nav_panel(
        "Tabelle",
        icon = bsicons::bs_icon("table"),
        div(
          class = "card-reveal-full-screen",
          card_body(
            fluidRow(
              column(6, selectInput(ns("reactable_auswahl"), "Auswahl",
                                    choices = c("Jahr" = "year",
                                                "Monat" = "year_month",
                                                "Einnahme/Ausgabe" = "typ",
                                                "Kategorie" = "kategorie",
                                                "Name" = "name"),
                                    selected = c("year_month", "kategorie"),
                                    multiple = TRUE,
                                    width = "100%"))
            )
          )
        ),
        reactableOutput(ns("tabelle_reactable"))
      ),
      
      nav_panel(
        "",
        icon = bs_icon("gear"),
        fluidRow(
          column(3,
                 dateRangeInput(ns("filter_date"), "Zeitraum auswählen",
                                start = floor_date(Sys.Date(), unit = "month") %m-% months(12),
                                end = Sys.Date(),
                                max = Sys.Date())),
          column(4, 
                 selectInput(ns("filter_kategorien_einnahmen"),
                             "Kategorien in Einnahmen",
                             choices = NULL, selected = NULL,
                             multiple = TRUE, width = "100%")),
          column(4,
                 selectInput(ns("filter_kategorien_ausgaben"),
                             "Kategorien in Ausgaben",
                             choices = NULL, selected = NULL,
                             multiple = TRUE, width = "100%")),
          column(1, 
                 actionButton(ns("refresh_data"), label = "", icon = icon("repeat")))
        ),
        fluidRow(
          column(12, radioButtons(ns("verlauf_smooth"), "Werte anzeigen als:",
                                  choices = c("Total", "Mittelwert", "3-Monats Mittel", "6-Monats Mittel", "12-Monats Mittel"),
                                  inline = TRUE))
        )
      )
    )
  )
}

# ==============================================================================
# SERVER =======================================================================
# ==============================================================================

giroServer <- function(id, girokonten) {
  moduleServer(id, function(input, output, session) {
    
    load_gruppen <- function() read.csv(paste0("./data/manual/", "gruppen.csv"))
                                        
    girokonten_rv <- reactiveVal(readdata_maual())
    gruppen_rv <- reactiveVal(load_gruppen())
    
    observeEvent(input$refresh_data, {
      new_data <- readdata_maual()
      girokonten_rv(new_data)
      
      gruppen_rv(new_gruppen)
    })
    
    
    # komplette daten
    GIR_allekategorien <- reactive({
      
      # Girokonten sind konten mit GIR oder GTH im Namen
      GIR <- bind_rows(lapply(girokonten_rv(), function(list) list[["data"]])) %>%
        
        mutate(datum = as.Date(datum)) %>%
        # filter date aus auswahl - zuerst, um speed zu erhöhen für join der kategorien
        filter(datum >= input$filter_date[1] & datum <= input$filter_date[2])  %>%
        
        # eigene Konten ausschließen
        filter(!str_detect(gegenseite, "_GIR"),
               !str_detect(gegenseite, "_GTH"),
               !str_detect(gegenseite, "_EXT")) %>%
        filter(betrag_edited != 0) %>%
        
        mutate(monat_jahr = format(datum, "%Y-%m")) %>%
        
        mutate(year = format(datum, "%Y")) %>%
        mutate(year_month = format(datum, "%Y-%m")) %>%
        # eigene klasse für aktien, damit nicht angezeit wird, in einnahmen, bzw ausgaben, aber in 
        mutate(typ = factor(case_when(betrag_edited < 0 ~ "Ausgabe", 
                                      .default = "Einnahme")))
      
      # load gruppen for matching ------
      gruppen <- gruppen_rv()  %>%
        filter(kategorie != "") %>%
        mutate(patterns = tolower(patterns))
      patterns <- gruppen$patterns
      
      # add Gruppen --------------------
      GIR %<>% 
        rowwise() %>%
        mutate(
          lower_gegenseite = tolower(gegenseite),
          lower_verwendungszweck = tolower(verwendungszweck),
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
          name = ifelse(is.na(name), gegenseite, name)
        ) %>%
        select(-lower_gegenseite, -lower_verwendungszweck, -patterns) %>%
        mutate(kategorie = factor(kategorie),
               name = factor(name))
      
      return(GIR)
      
    })
    
    # Kategorien aus GIR ---------------
    reactive_choices_einnahmen <- reactive({
      GIR_allekategorien() %>% filter(betrag_edited > 0) %>%
        pull(kategorie) %>% unique()
    })
    
    reactive_choices_ausgaben <- reactive({
      GIR_allekategorien() %>% filter(betrag_edited < 0) %>%
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
        data = GIR(),
        smooth = input$verlauf_smooth
      )
      nettobilanz(nettobilanz_info)
    })
    
    
    # Verlauf --------------------------
    output$einnahmen_plot <- renderHighchart({
      req(GIR())
      info_verlauf <- list(
        data = GIR(),
        art = "Einnahmen", # Einnahme/Ausgabe/Verhältnis
        smooth = input$verlauf_smooth
      )
      verlauf(info = info_verlauf)
    })
    
    output$ausgaben_plot <- renderHighchart({
      req(GIR())
      info_verlauf <- list(
        data = GIR(),
        art = "Ausgaben", # Einnahme/Ausgabe/Verhältnis
        smooth = input$verlauf_smooth
      )
      verlauf(info = info_verlauf)
    })
    
    output$verhältnis_plot <- renderHighchart({
      req(GIR())
      info_verlauf <- list(
        data = GIR(),
        art = "Verhältnis", # Einnahme/Ausgabe/Verhältnis
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
