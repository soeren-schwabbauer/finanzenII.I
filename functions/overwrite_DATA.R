
overwrite_DATA <- function(bank, konto, filename, data_neu, datapath = DATAPATH){

  message("Overwriting DATA for ", bank, "-", konto)

  filepath <- paste0(datapath, filename, ".csv")

  # Write the comment lines first
  comment_bank <- paste0("# bank:  ", bank)
  comment_konto <- paste0("# konto: ", konto)

  # Write the comments (if not already in the file)
  write_lines(comment_bank, filepath)  # Write the bank comment
  write_lines(comment_konto, filepath, append = TRUE)  # Write the konto comment
  write_lines(paste(colnames(data_neu), collapse = ","), filepath, append = TRUE)
  
  # Append the actual data using write_csv (without column names)
  write_csv(data_neu, filepath, append = TRUE)
  
  message("Overwriting sucessfull")
  
}
