
formatdigits <- function(digits = 0, prefix = "€") {
  
  round(digits) %>%
  format(big.mark = ".", scientific = FALSE, decimal.mark = ",") %>% 
  paste0(., prefix)
  
}

get_last_row_sum <- function(data, suffix = "€") {
  data %>%
    select(-DATUM) %>%
    slice_tail(n = 1) %>%
    rowSums() %>%
    formatdigits(suffix)
}


depotUI <- function(id, finanzkonto) {
  ns <- NS(id)
  
  tagList(
    
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Gesamtvolumen",
        value = uiOutput(ns("gesamtvolumen_value")),
        showcase = bs_icon("briefcase")
      ),
      value_box(
        "Entwicklung",
        value = uiOutput(ns("entwicklung_value")),
        showcase = bs_icon("graph-up-arrow")
      ),
      value_box(
        "Rendite",
        value = uiOutput(ns("rendite_value")),
        showcase = bs_icon("percent")
      )
    ),
    
    navset_card_underline(
      full_screen = TRUE,
      
      nav_panel(
        title = "Volumen",
        card_body(
        )
      ),
      
      nav_panel(
        title = "Entwicklung",
        card_body(
        )
      ),
      
      nav_panel(
        title = "Rendite",
        card_body(
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
                                 value = TRUE)
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



depotServer <- function(id, PORTFOLIODATA) {
  moduleServer(id, function(input, output, session) {
    
    # dataprep
    data_fullts <- reactive({
      
      data_fullts <- lapply(PORTFOLIODATA, function(x) bind_rows(x$data)) %>% bind_rows() %>%
        
        filter(ID != "SPARBRIEF", ID != "BTC")
      
      return(data_fullts)
      
    })
    
    output$gesamtvolumen_value <- renderText({
      data_fullts() %>%
        group_by(DATUM, ID) %>%
        summarise(sum = sum(SALDO_rendite), .groups = "drop") %>%
        pivot_wider(names_from = ID, values_from = sum) %>%
        get_last_row_sum()
    })
    
    output$entwicklung_value <- renderText({
      data_fullts() %>%
        mutate(rendite = SALDO_rendite - SALDO_raw) %>%
        group_by(DATUM, ID) %>%
        summarise(rendite = sum(rendite), .groups = "drop") %>%
        pivot_wider(names_from = ID, values_from = rendite) %>%
        get_last_row_sum()
    })
    
    output$rendite_value <- renderText({
      data_fullts() %>%
        group_by(DATUM) %>%
        summarise(SALDO_rendite = sum(SALDO_rendite),
                  SALDO_raw = sum(SALDO_raw), .groups = "drop") %>%
        mutate(rendite = (SALDO_rendite / SALDO_raw - 1) * 100) %>%
        select(DATUM, rendite) %>%
        get_last_row_sum(suffix = "%")
      
    })

    
    
    # ==== Saldo Area Chart (funktionierend, dynamisch) ====
    output$saldo_treemap <- renderPlotly({
      data <- data_fullts() %>%
        group_by(DATUM, ID) %>%
        summarise(SALDO_rendite = sum(SALDO_rendite), .groups = "drop")
      
      plot_ly(data, x = ~DATUM, y = ~SALDO_rendite, color = ~ID,
              type = 'scatter', mode = 'lines', stackgroup = 'one',
              fill = 'tonexty', line = list(color = 'black')) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(visible = TRUE, showgrid = FALSE, title = FALSE),
          yaxis = list(visible = FALSE, showgrid = FALSE),
          hovermode = "x",
          margin = list(t = 0, r = 0, l = 0, b = 0),
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        )
    })
    
    # ==== Entwicklung Chart (dynamisch, funktioniert) ====
    output$entwicklung_chart <- renderPlotly({
      data <- data_fullts() %>%
        mutate(rendite = SALDO_rendite - SALDO_raw) %>%
        group_by(DATUM, ID) %>%
        summarise(rendite = sum(rendite), .groups = "drop")
      
      plot_ly(data, x = ~DATUM, y = ~rendite, color = ~ID,
              type = 'scatter', mode = 'lines', stackgroup = 'one',
              fill = 'tonexty', line = list(color = 'black')) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(visible = TRUE, showgrid = FALSE, title = FALSE),
          yaxis = list(visible = FALSE, showgrid = FALSE),
          hovermode = "x",
          margin = list(t = 0, r = 0, l = 0, b = 0),
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        )
    })
    
    # ==== Rendite % Chart (funktionierend) ====
    output$renditeperc_plot <- renderPlotly({
      data <- data_fullts() %>%
        group_by(DATUM, ID) %>%
        summarise(SALDO_rendite = sum(SALDO_rendite),
                  SALDO_raw = sum(SALDO_raw), .groups = "drop") %>%
        mutate(rendite = (SALDO_rendite / SALDO_raw - 1) * 100)
      
      plot_ly(data, x = ~DATUM, y = ~rendite, color = ~ID,
              type = 'scatter', mode = 'lines', fill = "tozeroy",
              alpha = 0.2) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(visible = TRUE, showgrid = FALSE, title = FALSE),
          yaxis = list(visible = FALSE, showgrid = FALSE),
          hovermode = "x+y",
          margin = list(t = 0, r = 0, l = 0, b = 0),
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        )
    })

  })
}


