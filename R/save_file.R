#' @title Save files for different types of files and for development work
#'
#' @description Save files for different types of files and for development work.
#'
#' @param dataset The object that you want to save.
#' @param filename A \code{character} string with name of output file.
#' @param outfolder A \code{character} string with name of destination folder.
#' This can be "base_file", "tde" or "output".
#' @param type A \code{character} string with name of file extension. 
#' This can be csv, rds or xlsx
#' @param dev A \code{character} string. It add a "dev_version" bit to the filename
#' to be able to identify development work
#' #' @param overwrite A \code{logical} value. To allow overwriting or not of files
#'
#' @export


save_file <- function(dataset, filename, out_folder = c("base_file", "output", "tde", "open_data"), 
                      type = c("csv", "rds", "xlsx"), dev = F, overwrite) {
  
  dataset <- dataset # brings data object
  
  dash <- case_when(type == "xlsx" ~ "-", T ~ "_")
  
  # Creating dev version of files if required
  if (dev == FALSE) {
    if (out_folder == "tde") {
      filepath <- paste0(data_folder, pub_day, "/", out_folder, "/",
                         filename, ".", type)
    } else {
      filepath <- paste0(data_folder, pub_day, "/", out_folder, "/",
                         pub_day, dash, filename, ".", type)
    }
    
  } else if (dev == TRUE) {
    filepath <- paste0(data_folder, pub_day,  "/", out_folder, "/",
                       pub_day, dash, filename, "_dev_version.", type)
  }
  
  if (overwrite == F && file.exists(filepath)) {
    print("The file already exists, please change the parameter overwrite to = TRUE",
          "if you want to overwrite it")
  } else {
  
  # Saving file depending on type
  if (type == "csv") {
    readr::write_csv(dataset, filepath)
  } else if (type == "rds") {
    saveRDS(dataset, filepath)
    
  } else if (type == "xlsx" & out_folder != "tde") {
    openxlsx::saveWorkbook(dataset, filepath, overwrite = TRUE)
    
  } else if (type == "xlsx" & out_folder == "tde") {
    
    openxlsx::write.xlsx(dataset, filepath, sheetName = filename)
  }
  
  }
}

