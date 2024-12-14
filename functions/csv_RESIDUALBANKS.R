# Function loads static csv files 

load_csv <- function(datapath) {

  DATAPATH <- datapath
  
  # finanzkonten file names - are IDentifier for each konto
  filenames_csv <- list.files(path = DATAPATH)
  filenames_csv <- filenames_csv[filenames_csv != "gruppen.csv"]
  
  # funciton to load csv files
  load_finanzkonten <- function(finanzkonto_file) {
    filepath <- paste0(DATAPATH, finanzkonto_file)
    if (!file.exists(filepath)) {
      message("File does not exist:", filepath)
      return(NULL)  # Return NULL if the file does not exist
    }
    read.csv(filepath, comment.char = "#")
  }
  
  # Initialize the finanzkonto list (this will store all accounts)
  bank_konto <- data.frame()
  finanzkonto <- list()
  
  # Loop through each account and load the data dynamically
  for(i in filenames_csv) {
    
    # read header from csv files
    info  <- readLines(paste0(DATAPATH, i))
    bank  <- trimws(gsub(".*:", "", info[1]))
    konto <- trimws(gsub(".*:", "", info[2]))
    display_name  <- paste0(bank, " - ", konto)
    # ID is filename, wihtout ending
    ID <- gsub("*.csv", "", i)
    
    tryCatch({
      # Load the data for the current ID
      account_data <- load_finanzkonten(finanzkonto_file = i)
      
      if (!is.null(account_data)) { # Only add to finanzkonto if the data is not NULL
        finanzkonto[[ID]] <- list(
          name = ID,  
          display_name = display_name,
          bank = bank,
          konto = konto,
          data = account_data
        )
      } else message(paste("Data is NULL for", ID))
      
    }, error = function(e) { # if loading for data failed
      message(paste("Error loading data for", ID, ":", e$message))
    })
    
  
    
  }
  
  return(finanzkonto)

}
