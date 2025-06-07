
# verwendet wird das kontosaldo, bzw. der gegenwärtige depotwert.
if(FALSE) {
  input <- list(
    grouping_var = "konto", #bank-insgesammt-display_name,
    display_values = "absolut", #prozent
    subset_var = c("DEPOT", "GIRO", "EXTRAKONTO", "WALLET"),
    filter_daterange = c("2024-11-01", as.character(Sys.Date())),
    incl_redite = TRUE,
    incl_inflation = FALSE
  )
}

uebersichtUI <- function(id, finanzkonto, bank_konto) {
  ns <- NS(id)
  
  tagList(
    
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Gesamtsaldo",
        value = uiOutput(ns("totalsaldo")),
        showcase = bs_icon("wallet2")
      ),
      value_box(
        "Saldoentwicklung",
        value = uiOutput(ns("saldoentwicklung")),
        showcase = bs_icon("piggy-bank-fill")
      ),
      value_box(
        "Road to 100k",
        value = uiOutput(ns("roadto100k")),
        showcase = bs_icon("trophy")
      )
    ),
    
    navset_card_underline(
      full_screen = TRUE,
      
      nav_panel(
        title = "Übersicht",
        card_body(
          highchartOutput(ns("uebersicht_plot"))
        )
      ),
      
      nav_panel(
        title = "Road to 100k",
        card_body(
          highchartOutput(ns("trend"))
        )
      ),
      
      nav_panel(
        title = "",
        icon = bsicons::bs_icon("gear"),
        
        card_body(
          fluidRow(
            column(3,
                   dateRangeInput(ns("filter_daterange"), 
                                  "Zeitraum auswählen",
                                  start = "2023-01-01",
                                  end = Sys.Date(),
                                  min = "2020-01-01", 
                                  max = Sys.Date()),
                   checkboxInput(ns("incl_redite"),
                                 "Incl. Rendite",
                                 value = TRUE),
                   checkboxInput(ns("incl_inflation"),
                                 "Incl. Inflation",
                                 value = FALSE)
            ),
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
            column(2, 
                   radioButtons(ns("display_values"), "Werte in",
                                choices = c("Absolut (€)" = "absolut",
                                            "Prozent (%)" = "prozent"),
                                selected = "absolut",
                                inline = FALSE)
            ),
            column(1,
                   actionButton(ns("refresh_data"), label = "", icon = icon("repeat"))      
            )
          )
        )
      )
    )
  )
}







uebersichtServer <- function(id, MANUALDATA, PORTFOLIODATA, INFLATIONDATA, bank_konto) {
  moduleServer(id, function(input, output, session) {
    
    data_fullts <- reactive({
      
      
      # MANUALDATA to GRID ===================================================
      manualdata_unlist <- bind_rows(MANUALDATA)
      
      basekonten <- data.frame(display_name = manualdata_unlist$display_name,
                               manualdata_unlist$data) 
      
      data_basekonten <- basekonten %>%
        select(DATUM, display_name, SALDO) %>%
        mutate(DATUM = as.Date(DATUM)) %>%
        # doppelte salden vermeiden
        group_by(DATUM, display_name) %>%
        # bisher von nach absteigendem datum sortiert
        slice(1) %>% #-> erstes saldo behalten (das letzte eingetragene saldo des tages)
        ungroup() %>%
        arrange(display_name, DATUM) 
      
      grid_basekonten <- 
        # empty grid for timeserie in depots
        expand.grid(display_name = unique(data_basekonten$display_name),
                    DATUM = seq.Date(from = as.Date("2020-01-01"),
                                     to = Sys.Date(),
                                     by = "day")) %>%
        arrange(display_name, DATUM) %>%
        left_join(data_basekonten, by = c("display_name", "DATUM")) %>%
        group_by(display_name) %>%
        fill(SALDO, .direction = "down") %>%
        ungroup()
      
      # PORTFOLIOS to GRID ===================================================
      portfolio_unlist <- bind_rows(PORTFOLIODATA)
      
      portfolio <- data.frame(display_name = portfolio_unlist$display_name,
                              portfolio_unlist$data)
      
      data_portfolio <- portfolio %>% 
        mutate(SALDO = if(input$incl_redite) SALDO_rendite else SALDO_raw)
      
      grid_portfolio <- 
        expand.grid(display_name = unique(data_portfolio$display_name),
                    DATUM = seq.Date(from = as.Date("2020-01-01"),
                                     to = Sys.Date(),
                                     by = "day")) %>%
        arrange(display_name, DATUM) %>%
        left_join(data_portfolio, by = c("display_name", "DATUM"))
      
      # alle konten verbinden ==================================================
      grid_full <- bind_rows(grid_basekonten, grid_portfolio) %>%
        mutate(SALDO = ifelse(is.na(SALDO), 0, SALDO))
      
      # datum filtern
      full <- grid_full %>% 
        filter(DATUM >= input$filter_daterange[1],
               DATUM <= input$filter_daterange[2])
      
      # kontosaldo nach groupingvariable aufsummieren ------------------------
      if (input$grouping_var == "insgesammt") {
        full <- full %>%
          group_by(DATUM) %>%
          summarise(SALDO = sum(SALDO), .groups = "drop") %>%
          mutate(grouped_var = "Total")
      } else {
        full <- full %>%
          mutate(konto = gsub(".* - ", "", display_name),
                 bank =  gsub(" - .*", "", display_name)) %>%
          group_by(DATUM, grouped_var = !!sym(input$grouping_var)) %>%
          summarise(SALDO = sum(SALDO), .groups = "drop")
      }
      
      # inflation bereinigen ===================================================
      
      if(isTRUE(input$incl_inflation)) {
        
        full <- full %>% mutate(monat = floor_date(DATUM, unit = "month"))
        
        # 2. VPI-Basis definieren (z.B. Januar 2020)
        basis_datum <- min(full$monat)
        vpi_basis <- INFLATIONDATA %>% filter(datum == basis_datum) %>% pull(preis) 
        
        # 3. Mergen mit monatlichen VPI-Daten
        full <- full %>% left_join(INFLATIONDATA, by = c("monat" = "datum")) %>% 
          tidyr::fill(preis, .direction = "down")  # oder 'updown' für beidseitig
        
        # 4. Realsaldo berechnen
        full <- full %>% mutate(SALDO = SALDO * (vpi_basis / preis))
        
      }
      
      full
    })
    
    # Observing changes in data_fullts(), but preserving user selection
    observe({
      new_options <- data_fullts() %>% pull(grouped_var) %>% unique()
      
      # Preserve the old selection if it is still valid
      old_selection <- isolate(input$subset_var)
      valid_selection <- intersect(old_selection, new_options)
      
      # If there is no valid old selection, fall back to all new options
      if (length(valid_selection) == 0) {
        valid_selection <- new_options
      }
      
      updateSelectInput(
        session = session,
        inputId = "subset_var",
        label = paste0(input$grouping_var, " filtern"),
        choices = new_options,
        selected = valid_selection
      )
    })
    
    output$uebersicht_plot <- renderHighchart({
      
      full <- data_fullts() %>% 
        filter(grouped_var %in% input$subset_var) %>%
        mutate(
          DATUM_TS = datetime_to_timestamp(as.POSIXct(DATUM))
        )
      
      # Prozent pro Tag berechnen
      if (input$display_values == "prozent") {
        full <- full %>%
          group_by(DATUM_TS) %>%
          mutate(total = sum(SALDO)) %>%
          ungroup() %>%
          mutate(SALDO = round(SALDO / total, 4) * 100)
        sign_tooltip <- "%"
      } else {
        sign_tooltip <- "€"
      }
      
      # Total sum für schwarze Linie
      sum_data <- full %>%
        group_by(DATUM_TS) %>%
        summarize(SALDO_SUM = sum(SALDO), .groups = "drop")
      
      # Highchart
      highchart() %>%
        hc_chart(type = "area") %>%
        hc_xAxis(type = "datetime", title = list(text = "Datum")) %>%
        hc_add_series_list(
          full %>%
            group_by(grouped_var) %>%
            summarize(
              data = list(map2(DATUM_TS, SALDO, ~ list(x = .x, y = .y))),
              .groups = "drop"
            ) %>%
            mutate(series = map2(
              grouped_var, data,
              ~ list(
                name = .x,
                data = .y,
                type = "area"
              )
            )) %>%
            pull(series)
        ) %>%
        hc_add_series(
          name = "Total Sum",
          data = map2(sum_data$DATUM_TS, sum_data$SALDO_SUM, ~ list(x = .x, y = .y)),
          type = "line",
          color = "black"
        ) %>%
        hc_plotOptions(area = list(stacking = "normal")) %>%
        hc_tooltip(
          shared = TRUE, 
          formatter = JS(
            paste0(
              "function() {
           var points = this.points || [];
           var totalSum = points.find(p => p.series.name === 'Total Sum')?.y || 0;
           var tooltip = '<b>' + Highcharts.dateFormat('%b %e, %Y', this.x) + 
             ': ' + Highcharts.numberFormat(totalSum, 0, ',', '.') + '", 
              sign_tooltip, 
              "</b><br>';
           points = points.sort(function(a, b) { return b.y - a.y; });
           points.forEach(function(point) {
             if (point.series.name !== 'Total Sum') {
               tooltip += '<span style=\"color:' + point.color + '\">●</span> ' + 
                          point.series.name + ': ' +
                          Highcharts.numberFormat(point.y, 0, ',', '.') + '", 
              sign_tooltip, 
              "<br>';
             }
           });
           return tooltip;
         }"
            )
          )
        ) %>%
        hc_title(text = "") %>%
        hc_yAxis(
          title = list(
            text = if (input$display_values == "prozent") "Anteil (%)" else "Saldo (€)"
          )
        )
      
    }) 
    
    full_agg <- reactive({
      
      data_fullts() %>%
        group_by(DATUM) %>%
        summarize(SALDO_SUM = sum(SALDO)) %>%
        ungroup()
      
    })
    
    
    output$roadto100k <- renderText({
      
      avg_per_day <- full_agg() %>%
        mutate(diff = SALDO_SUM - lag(SALDO_SUM)) %>%
        summarise(mean = mean(diff, na.rm = TRUE)) %>%
        pull()
      
      diffdays <- as.numeric(max(full_agg()$DATUM) - min(full_agg()$DATUM))
      
      total_heute <- full_agg() %>% filter(DATUM == max(DATUM)) %>% pull(SALDO_SUM)
      
      days <- round((100000 - total_heute) / avg_per_day,0)
      
      info <- paste0(format(Sys.Date() + days, "%d.%m.%Y"))
      return(info)
      
    })
    
    output$totalsaldo <- renderText({
      
      info <- full_agg() %>% filter(DATUM == max(DATUM)) %>% pull(SALDO_SUM)
      info <- comma(info, suffix = "€", big.mark = ".", decimal.mark = ",")
      
      return(info)
      
    })
    
    output$saldoentwicklung <- renderText({
      
      data <- full_agg()
      info <- slice(data, c(1, nrow(data))) %>% pull(SALDO_SUM) %>% diff()
      info <- comma(info, suffix = "€", big.mark = ".", decimal.mark = ",")
      
      return(info)
      
    })
    
    output$trend <- renderHighchart({
      
      data <- full_agg()
      # Zielwert
      zielwert <- 100000
      min_index <- 20  # Skip early noisy days
      
      # Funktion zur Prognose
      berechne_prognosedatum <- function(daten_bis_jetzt, zielwert) {
        tage <- as.numeric(daten_bis_jetzt$DATUM - min(daten_bis_jetzt$DATUM))
        saldo <- daten_bis_jetzt$SALDO_SUM
        
        modell <- lm(saldo ~ tage)
        prognose_tag <- (zielwert - coef(modell)[1]) / coef(modell)[2]
        prognose_datum <- min(daten_bis_jetzt$DATUM) + round(prognose_tag)
        
        return(prognose_datum)
      }
      
      # Prognose berechnen (nur für stabile Phase)
      prognosen <- sapply(1:nrow(data), function(i) {
        if (i >= min_index) {
          berechne_prognosedatum(data[1:i, ], zielwert)
        } else {
          NA
        }
      })
      
      # Daten mit Prognosedatum erweitern
      data$PROGNOSE_DATUM <- as.Date(prognosen, origin = "1970-01-01")
      
      # Gefilterter Datensatz für Darstellung
      plot_data <- data %>%
        filter(!is.na(PROGNOSE_DATUM)) %>%
        filter(DATUM >= input$filter_daterange[1] + 30)
      
      # highcharter Plot
      highchart() %>%
        hc_xAxis(type = "datetime", title = list(text = "Datum")) %>%
        hc_yAxis(type = "datetime", title = list(text = "errechnetes Datum")) %>%
        hc_add_series(
          data = list_parse2(
            data.frame(
              x = datetime_to_timestamp(plot_data$DATUM),
              y = datetime_to_timestamp(plot_data$PROGNOSE_DATUM)
            )
          ),
          type = "line",
          name = "Prognose-Datum",
          color = "#0072B2"
        ) %>%
        hc_tooltip(formatter = JS("
    function () {
      return '<b>Datum:</b> ' + Highcharts.dateFormat('%Y-%m-%d', this.x) + 
             '<br><b>Prognose:</b> ' + Highcharts.dateFormat('%Y-%m-%d', this.y);
    }
  "))
      
    })
  })
}
