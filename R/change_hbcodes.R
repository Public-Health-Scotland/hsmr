#' @title Change health boards codes from 2019 version to the 2014 one.
#'
#' @description # Tableau uses 2014 codes, but code produces 2019. Changing from
#' 2019 to 2014 for the TDE files
#'
#' @param dataset The object that you want to save.
#' @param version_to If you want to go 14 to 19 or 19 to 14.
#' @param code_cols Character vector of names for the columns in dataset that
#' contain codes to change. Optional - will default to "location" and "hb".
#'
#' @export

change_hbcodes <- function(dataset, version_to,
                           code_cols = c("location", "hb")) {

  if (version_to == "14") { # From 2019 to 2014

      dataset %>%
        mutate(across(all_of(code_cols),
                      ~recode(.,
                              'S08000029' = "S08000018",
                              'S08000030' = "S08000027",
                              'S08000031' = "S08000021",
                              'S08000032' = "S08000023")))

  } else if (version_to == "19") { # From 2014 to 2019

      dataset %>%
        mutate(across(all_of(code_cols),
                      ~recode(.,
                              "S08000018" = 'S08000029',
                              "S08000027" = 'S08000030',
                              "S08000021" = 'S08000031',
                              "S08000023" = 'S08000032')))

  }
}

