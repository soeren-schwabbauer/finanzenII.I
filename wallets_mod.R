
walletsUI <- function(id, wallets) {
  ns <- NS(id)
  
  accordion_panels <- lapply(wallets, function(account) {
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

walletsServer <- function(id, wallets) {
  moduleServer(id, function(input, output, session) {
    
    lapply(wallets, function(account) {
      account_id <- account$name  # Unique ID for each account
      account_data <- account$data  # Data for the account
      
      # Dynamically add icons for the COIN column
      if ("COIN" %in% colnames(account_data)) {
        account_data$COIN <- sapply(account_data$COIN, function(coin) {
          icon_path <- paste0("www/icons_wallets/", coin, ".png")  # Assume file name matches coin value
          cat(file.exists(icon_path))
          if (file.exists(icon_path)) {
            paste0('<img src= "icons_wallets/', coin, '.png" style="height:40px;width:40px;"></img>')
          } else {
            coin  # Fallback to showing the coin value if the file doesn't exist
          }
        })
      }
      
      # Dynamically create a renderDT for each account's data table
      output[[account_id]] <- renderDT({
        req(account_data)  # Ensure the data is available
        DT::datatable(
          account_data,
          rownames = FALSE,
          filter = 'top',
          escape = FALSE,  # Allows HTML rendering
          options = list(pageLength = -1, dom = 't')
        )
      })
    })

    
  }
  )
}