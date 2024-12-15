
# verwendet wird das kontosaldo, bzw. der gegenwärtige depotwert.

uebersichtUI <- function(id, finanzkonto, bank_konto) {
  
  ns <- NS(id) 

  # Return the UI structure with all dynamically created accordion panels
  tagList(
    fluidRow(
      column(12,
             br(),
             fluidRow(
               column(6,
                      radioButtons(ns("group_treemap"), "Gruppieren nach", 
                                   choices = c("Insgesammt" = "insgesammt", 
                                               "Kontoart" = "konto",
                                               "Bank" = "bank",
                                               "Konto" = "display_name"),
                                   inline = TRUE)),
               column(6, 
                      radioButtons(ns("treemap_values"), "Werte in",
                                   choices = c("Absolut", "Prozent"),
                                   inline = TRUE)),
             ),
             highchartOutput(ns("treemap_finanzkonten"))
      )
    )
  )
}


uebersichtServer <- function(id, finanzkonto, bank_konto) {
  moduleServer(id, function(input, output, session) {
    
    output$treemap_finanzkonten <- renderHighchart({
      
      data <- lapply(finanzkonto, function(list) list[["data"]])
      lookup <- c("TOTAL" = "SALDO", "TOTAL" = "GESAMTWERT")
    
      total <- lapply(names(data), 
                      function(name) {
                        df <- data[[name]]
                        if(str_detect(name, "DEP|WAL")) df %<>% summarise(GESAMTWERT = sum(GESAMTWERT))
                        df %<>% 
                          slice(1) %>% # first row
                          select_if(names(.) %in% c("SALDO", "GESAMTWERT")) %>%
                          rename(any_of(lookup)) %>%
                          mutate(TOTAL = as.numeric(TOTAL)) %>%
                          mutate(ID = name)  # Assign the list element name to ID
                      }
      ) %>% bind_rows()
    
    
      # Prepare the data
      treemap_data <- left_join(total, bank_konto, by = "ID") 
      
      if(input$group_treemap != "insgesammt") treemap_data %<>% group_by(!!sym(input$group_treemap))
      treemap_data %<>% summarise(TOTAL = sum(TOTAL))
      if(input$group_treemap == "insgesammt") {
        treemap_data %<>% mutate(parent = "Vermögen", name = "Vermögen", value = TOTAL)
      } else {treemap_data %<>% 
          mutate(parent = !!sym(input$group_treemap),
                 name = !!sym(input$group_treemap)) %>%
          select(parent, value = TOTAL, name)
      }
      
      # treemap values as percent
      if(input$treemap_values == "Prozent") {
        total = sum(treemap_data$value)
        treemap_data %<>% mutate(value = round(value / total*100, 2))
      }
    
      # Generate a color palette for all unique banks
      color_palette <- brewer.pal(length(unique(treemap_data$parent)), "Set3")
      
      # Assign colors to each bank
      treemap_data$color <- factor(treemap_data$parent, levels = unique(treemap_data$parent)) 
      treemap_data$color <- color_palette[treemap_data$color]
      
      # Create the treemap plot
      hc <- highchart() %>%
        hc_chart(type = "treemap") %>%
        hc_add_series(
          name = "Bank Accounts",
          data = treemap_data,
          allowDrillToNode = TRUE,
          colorByPoint = TRUE,
          colors = treemap_data$color
        ) %>%
        hc_title(text = NULL) 
      
      if (input$treemap_values == "Prozent") {
        hc <- hc %>% hc_tooltip(pointFormat = "{point.value}%")
      } else {
        hc <- hc %>% hc_tooltip(pointFormat = "{point.value}€")
      }
      
      return(hc)
      
    })
  })
}

