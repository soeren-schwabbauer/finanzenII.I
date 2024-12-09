depotsUI <- function(id, depots) {
  ns <- NS(id)
  
  accordion_panels <- lapply(depots, function(account) {
    account_id <- account$name  # Unique name for each account, used for output ID
    bank <- account$bank
    saldo <- format(sum(account$data$GESAMTWERT), big.mark = ".", decimal.mark = ",")
    
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
      
      br(), br()
      
    )
  )
}

depotsServer <- function(id, depots) {
  moduleServer(id, function(input, output, session) {
      
    lapply(depots, function(account) {
      account_id <- account$name  # Unique ID for each account
      account_data <- account$data  # Data for the account
      
      # Dynamically create a renderDT for each account's data table
      output[[account_id]] <- renderDT({
        req(account_data)  # Ensure the data is available
        DT::datatable(
          account_data,
          rownames = FALSE,
          filter = 'top',
          options = list(pageLength = -1, dom = 't')
        )
      })
    })
    
    }
  )
}