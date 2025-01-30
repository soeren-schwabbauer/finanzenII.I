
# verwendet wird das kontosaldo, bzw. der gegenwärtige depotwert.
if(FALSE) {
  input <- list(
    grouping_var = "konto", #bank-insgesammt-display_name,
    display_values = "absolut", #prozent
    filter_daterange = c("2024-11-01", as.character(Sys.Date())),
    incl_redite = TRUE
  )
}
uebersichtUI <- function(id, finanzkonto, bank_konto) {
  
  ns <- NS(id)
  
  # Return the UI structure with all dynamically created accordion panels
  tagList(
    br(),
    accordion(
      multiple = TRUE,
      open = TRUE,
      accordion_panel(
        title = "Auswahl",
        icon = bsicons::bs_icon("gear"),
        open = TRUE,
        fluidRow(
          column(2,
                 radioButtons(ns("grouping_var"), "Gruppieren nach", 
                              choices = c("Insgesammt" = "insgesammt", 
                                          "Kontoart" = "konto",
                                          "Bank" = "bank",
                                          "Konto" = "display_name"),
                              selected = "konto",
                              inline = FALSE)),
          column(4,
                 selectInput(ns("subset_var"),
                             label = NULL,
                             choices = NULL,
                             multiple = TRUE)),
          column(3, 
                 radioButtons(ns("display_values"), "Werte in",
                              choices = c("Absolut (€)" = "absolut",
                                          "Prozent (%)" = "prozent"),
                              selected = "absolut",
                              inline = FALSE)
          ),
          column(3,
                 dateRangeInput(ns("filter_daterange"), 
                                "Zeitraum auswählen",
                                start = Sys.Date() - 365,
                                end = Sys.Date(),
                                min = "2021-10-28", 
                                max = Sys.Date()),
                  checkboxInput(ns("incl_redite"),
                                "Incl. Rendite",
                                value = TRUE)
          )
        ) # fluidRow
      ), # accordion_panel
      accordion_panel(
        title = "Saldo",
        icon = bsicons::bs_icon("graph-up-arrow"),
        open = TRUE,
        fluidRow(
          br(),
          highchartOutput(ns("uebersicht_plot"))
        )
      )
    ) # accordion
  )
}




uebersichtServer <- function(id, MANUALDATA, PORTFOLIODATA, bank_konto) {
  moduleServer(id, function(input, output, session) {
    
    
    data_fullts <- reactive({
      
      # MANUALDATA to GRID ===================================================
      manualdata_unlist <- bind_rows(MANUALDATA)
      
      basekonten <- data.frame(display_name = manualdata_unlist$display_name, manualdata_unlist$data) 
      
      data_basekonten <- basekonten %>%
        select(DATUM, display_name, SALDO) %>%
        mutate(DATUM = as.Date(DATUM)) %>%
        
        # doppelte salden vermeiden
        group_by(DATUM, display_name) %>%
        filter(SALDO == first(SALDO)) %>%
        ungroup() %>%
        arrange(display_name, DATUM)
      
      grid_basekonten <- 
        # empty gird for timeserie in depots
        expand.grid(display_name = unique(data_basekonten$display_name),
                    DATUM = seq.Date(from = as.Date("2021-11-01"),
                                     to = Sys.Date(),
                                     by = "day")) %>%
        arrange(display_name, DATUM) %>%
        
        left_join(data_basekonten, by = c("display_name", "DATUM")) %>%
        
        group_by(display_name) %>%
        fill(SALDO, .direction = "down")
      
      
      # PORTFOLIOS to GRID ===================================================
      
      portfolio_unlist <- bind_rows(PORTFOLIODATA)
      
      portfolio <- data.frame(display_name = portfolio_unlist$display_name, portfolio_unlist$data)
      
      data_portfolio <- portfolio %>% 
        mutate(SALDO = if(input$incl_redite == TRUE) SALDO_rendite else SALDO_raw)
      
      grid_portfolio <- 
        # empty gird for timeserie in depots
        expand.grid(display_name = unique(data_portfolio$display_name),
                    DATUM = seq.Date(from = as.Date("2021-11-01"),
                                     to = Sys.Date(),
                                     by = "day")) %>%
        arrange(display_name, DATUM) %>%
        
        left_join(data_portfolio, by = c("display_name", "DATUM")) 
      
      
      # alle konten verbinden ================================================
      # -> historische saldoentwicklung (DATUM | SALDO)
      grid_full <- bind_rows(grid_basekonten, grid_portfolio) %>%
        
        mutate(SALDO = case_when(is.na(SALDO) ~ 0, .default = SALDO))
      
      
      # datum filtern
      full <- grid_full %>% filter(DATUM >= input$filter_daterange[1],
                                   DATUM <= input$filter_daterange[2])
      
      # kontosaldo nach groupingvariable aufsummieren --------------------------
      if(input$grouping_var == "insgesammt") {
        full <- full %>% group_by(DATUM) %>%
          summarise(SALDO = sum(SALDO)) %>%
          ungroup() %>%
          mutate(grouped_var = "Total")
      } else {
        full <- full %>% 
          # bank & konto hinzufügen für grouping
          mutate(konto = gsub(".* - ", "", display_name)) %>%
          mutate(bank =  gsub(" - .*", "", display_name)) %>%
          group_by(DATUM, grouped_var = !!sym(input$grouping_var)) %>%
          summarise(SALDO = sum(SALDO)) %>%
          ungroup()
      }
      
      return(full)
      
    })
    
    
    observe({
        options <- data_fullts() %>% pull(grouped_var) %>% unique()
      updateSelectInput(session, "subset_var", 
                        label = paste0(input$grouping_var, " filtern"),
                        choices = options,
                        selected = options)
    })
    
    
    output$uebersicht_plot <- renderHighchart({

      full <- data_fullts() %>% filter(grouped_var %in% input$subset_var)
        
        # prozent pro tag berechnen ----------------------------------------------
        if(input$display_values == "prozent"){
          full <- full %>% group_by(DATUM) %>%
            mutate(total = sum(SALDO)) %>%
            ungroup() %>%
            mutate(SALDO = round(SALDO/total,4)*100)
          sign_tooltip <- "%"
        } else {
          sign_tooltip <- "€"
        }

                
        # compute for total sum line ---------------------------------------------
        sum_data <- full %>%
          group_by(DATUM) %>%
          summarize(SALDO_SUM = sum(SALDO))
        
        # plot -------------------------------------------------------------------
        highchart() %>%
          # Add the area chart for each display_name
          hc_chart(type = "area") %>%
          hc_xAxis(categories = unique(full$DATUM)) %>%
          
          hc_add_series_list(
            full %>%
              group_by(grouped_var) %>%
              summarize(data = list(SALDO)) %>%
              mutate(series = map2(grouped_var, data, ~ list(
                name = .x,
                data = .y,
                type = "area" # Keeps the area type for individual series
              ))) %>%
              pull(series)
          ) %>%
          
          # Add the line for the sum
          hc_add_series(name = "Total Sum",
                        data = sum_data$SALDO_SUM,
                        type = "line",
                        color = "black") %>%
          
          hc_plotOptions(area = list(stacking = "normal")) %>%
          
          hc_tooltip(
            shared = TRUE, 
            formatter = JS(
              paste0(
                "function() {
          var points = this.points || [];
          var totalSum = points.find(p => p.series.name === 'Total Sum')?.y || 0;
          var tooltip = '<b>' + this.x + ': ' + Highcharts.numberFormat(totalSum, 0, ',', '.') + '", sign_tooltip, "</b><br>';
            
          points = points.sort(function(a, b) { return b.y - a.y; });

          points.forEach(function(point) {
            if (point.series.name !== 'Total Sum') {
              tooltip += '<span style=\"color:' + point.color + '\">●</span> ' + 
                         point.series.name + ': ' + Highcharts.numberFormat(point.y, 0, ',', '.') + '", sign_tooltip, "<br>';
            }
          });

          return tooltip;
        }"
              )
            )
          ) %>%
          
          hc_title(text = "") %>%
          hc_xAxis(title = list(text = ""),
                   labels = list(enabled = FALSE)) %>%
          hc_yAxis(title = list(text = if(input$display_values == "prozent") "Anteil (%)" else "Saldo (€)"))
        
        
    }) # renderHighchart
        
  }) # moduleServer
  
} # uebersicht_server

