#' @title Create text for Funnel Plot outliers
#'
#' @description
#'
#' The publication summary documents for the HSMR publication has main points
#' relating to outliers on the funnel plot. The \code{funnel_text} function
#' identifies outlying hospitals and returns the text for the publication
#' document.
#'
#'
#' @param smr_data Should the final\code{tibble} output from the \code{smr_data}
#' function.
#' @param indicator indicates whether the function produces text for hospitals
#' sitting above the upper control limit or hospitals sitting below the lower
#' control limit.
#'
#'
#' @examples
#' funnel_text(smr_data, indicator = "above")
#' funnel_text(smr_data, indicator = "below")
#'
#' @export
funnel_text <- function(smr_data, indicator = c("above", "below","both")){

  match.arg(indicator)

  ### 1 - Error Handling ----
  if(!tibble::is_tibble(smr_data)){

    stop(paste0("The smr_data argument provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("period", "deaths", "pred", "pats", "smr", "crd_rate",
          "location_type", "location", "hb", "location_name",
          "completeness_date", "period_label", "death_scot", "pred_scot",
          "pats_scot", "smr_scot") %in% names(smr_data))){

    stop(paste0("smr_data object must be objected returned from smr_data()",
                " function."))

  }
  
  # TEXT WARNING LIMIT SCENARIOS TO BE NESTED WITHIN
  # CONTROL LIMIT SCENARIOS
  
  # Scenario 1 (applies to both "above" and "below" arguments)
  #  = no hospitals exceeding upper or lower wl
  # ~~~~~~~~~~
  
  scen1_output_1_wl <- ""
  
  scen1_output_2_wl <- ""
  
  if(indicator %in% c("above","both")){
    
    #Upper control limit filter
    smr_data_ucl <- smr_data |> 
      dplyr::filter(period == 3 & smr > ucl)

    n_hosps_ucl <- nrow(smr_data_ucl)
    
    #Upper warning limit filter
    smr_data_uwl <- smr_data |> 
      dplyr::filter(period == 3 & smr > uwl)
    
    n_hosps_uwl <- nrow(smr_data_uwl)
    
      
    # UPPER WARNING LIMIT SCENARIOS ----
    # Scenario 2 = 1 hospital exceeding upper wl
    # ~~~~~~~~~~

      scen2_output_1_wl <- paste0("The standardised mortality ratio for ",
                            paste0(smr_data_uwl$location_name, " (",
                                   janitor::round_half_up(smr_data_uwl$smr, 2), ") ",
                                   collapse = ", "), "is above the upper warning limit.")
      
      scen2_output_2_wl <- paste0("There was one hospital above the ",
                            "upper warning limit, which is two standard ",
                            "deviations above the Scottish average: ",
                            paste0(smr_data_uwl$location_name, " (",
                                   janitor::round_half_up(smr_data_uwl$smr, 2), ")",
                                   collapse = ", "), ".")
    
    # Scenario 3 = more than one hospital exceeding upper wl
    # ~~~~~~~~~~
    
      scen3_output_1_wl <- paste0("There were ", n_hosps_uwl, " with a standardised mortality ratio ",
                         "exceeding the upper warning limit: ",
                         paste0(smr_data_uwl$location_name, " (",
                                      janitor::round_half_up(smr_data_uwl$smr, 2),
                                      ")", collapse = ", "), ".")
      
      scen3_output_2_wl <- paste0("There were ", n_hosps_uwl, " hospitals above the upper ",
                         "warning limit, which is two standard deviations ",
                         "above the Scottish average: ",
                         paste0(smr_data_uwl$location_name, " (",
                                janitor::round_half_up(smr_data_uwl$smr, 2),
                                ")", collapse = ", "), ".")

    if(n_hosps_ucl == 0){

      output_1_cl <- paste0("* For the period ",
                       hsmr::yr(end_date), " no hospitals had a significantly higher ",
                       "standardised mortality ratio than the national average.")

      output_2_cl <- paste0("This chart shows that there were no hospitals above the ",
                         "upper control limit, which is three standard ",
                         "deviations above the Scottish average. ")
      
      if(n_hosps_uwl == 0){ #Scenario 1 - no hosps over UWL
        
        output_1_wl <- scen1_output_1_wl
        output_2_wl <- scen1_output_2_wl
        
      }else if (n_hosps_uwl == 1){ #Scenario 2 - 1 hosp over UWL
        
        output_1_wl <- scen2_output_1_wl
        output_2_wl <- scen2_output_2_wl
        
      } else if (n_hosps_uwl > 1){ #Scenario 3 - more than 1 hosp over UWL
        
        output_1_wl <- scen3_output_1_wl
        output_2_wl <- scen3_output_2_wl
        
      }
      
      output_1 <- paste(output_1_cl, output_1_wl)
      output_2 <- paste(output_2_cl, output_2_wl)
      output_uwl <- output_2_wl
      output_ucl <- output_2_cl
    }
      

    if(n_hosps_ucl == 1){

      output_1_cl <- paste0("* For the period ",
                       hsmr::yr(end_date), " one hospital had a significantly higher ",
                       "standardised mortality ratio  than the national average",
                       ": ", paste0(smr_data_ucl$location_name, " (",
                                    janitor::round_half_up(smr_data_ucl$smr, 2), ")",
                                            collapse = ", "), ".")

      output_2_cl <- paste0("This chart shows that there was one hospital above the ",
                         "upper control limit, which is three standard ",
                         "deviations above the Scottish average: ",
                         paste0(smr_data_ucl$location_name, " (",
                                janitor::round_half_up(smr_data_ucl$smr, 2), ")",
                                collapse = ", "), ".")
      
      if(n_hosps_uwl == 0){ #Scenario 1 - no hosps over UWL
        
        output_1_wl <- scen1_output_1_wl
        output_2_wl <- scen1_output_2_wl
        
      }else if (n_hosps_uwl == 1){ #Scenario 2 - 1 hosp over UWL
        
        output_1_wl <- scen2_output_1_wl
        output_2_wl <- scen2_output_2_wl
        
      } else if (n_hosps_uwl > 1){ #Scenario 3 - more than 1 hosp over UWL
        
        output_1_wl <- scen3_output_1_wl
        output_2_wl <- scen3_output_2_wl
        
      }
      
      output_1 <- paste(output_1_cl, output_1_wl)
      output_2 <- paste(output_2_cl, output_2_wl)
      output_uwl <- output_2_wl
      output_ucl <- output_2_cl

    }
      
    if(n_hosps_ucl > 1){

      output_1_cl <- paste0("* For the period ", numbers_to_words(n_hosps_ucl), " had a significantly higher ",
                       "standardised mortality ratio ",
                       hsmr::yr(end_date), " than the national average",
                       ": ", paste0(smr_data_ucl$location_name, " (",
                                    janitor::round_half_up(smr_data_ucl$smr, 2),
                                    ")", collapse = ", "), ".")

      output_2_cl <- paste0("This chart shows there were ", numbers_to_words(n_hosps_ucl), " hospitals above the upper ",
                         "control limit, which is three standard deviations ",
                         "above the Scottish average: ",
                         paste0(smr_data_ucl$location_name, " (",
                                janitor::round_half_up(smr_data_ucl$smr, 2),
                                ")", collapse = ", "), ".")
      
      if(n_hosps_uwl == 0){ #Scenario 1 - no hosps over UWL
        
        output_1_wl <- scen1_output_1_wl
        output_2_wl <- scen1_output_2_wl
        
      }else if (n_hosps_uwl == 1){ #Scenario 2 - 1 hosp over UWL
        
        output_1_wl <- scen2_output_1_wl
        output_2_wl <- scen2_output_2_wl
        
      } else if (n_hosps_uwl > 1){ #Scenario 3 - more than 1 hosp over UWL
        
        output_1_wl <- scen3_output_1_wl
        output_2_wl <- scen3_output_2_wl
        
      }
      
      output_1 <- paste(output_1_cl, output_1_wl)
      output_2 <- paste(output_2_cl, output_2_wl)
      output_uwl <- output_2_wl
      output_ucl <- output_2_cl

    }

  }

  if(indicator %in% c("below","both")){

    #Lower control limit filter
    smr_data_lcl <- smr_data |> 
      dplyr::filter(period == 3 & smr < lcl)

    n_hosps_lcl <- nrow(smr_data_lcl)
    
    #Lower warning limit filter
    smr_data_lwl <- smr_data |> 
      dplyr::filter(period == 3 & smr < lwl)
    
    n_hosps_lwl <- nrow(smr_data_lwl)
    
    # LOWER WARNING LIMITS SCENARIOS ----
    # Scenario 4 = 1 hospital exceeding lower wl
    # ~~~~~~~~~~
    
    scen4_output_1_wl <- paste0("The standardised mortality ratio for ",
                                paste0(smr_data_lwl$location_name, " (",
                                       janitor::round_half_up(smr_data_lwl$smr, 2), ") ",
                                       collapse = ", "), "is below the lower warning limit.")
    
    scen4_output_2_wl <- paste0("There was one hospital below the ",
                                "lower warning limit, which is two standard ",
                                "deviations below the Scottish average: ",
                                paste0(smr_data_lwl$location_name, " (",
                                       janitor::round_half_up(smr_data_lwl$smr, 2), ")",
                                       collapse = ", "), ".")
    
    # Scenario 5 = more than one hospital exceeding lower wl
    # ~~~~~~~~~~
    
    scen5_output_1_wl <- paste0("There were ", numbers_to_words(n_hosps_lwl), " with a standardised mortality ratio ",
                                "below the lower warning limit: ",
                                paste0(smr_data_lwl$location_name, " (",
                                       janitor::round_half_up(smr_data_lwl$smr, 2),
                                       ")", collapse = ", "), ".")
    
    scen5_output_2_wl <- paste0("There were ",numbers_to_words(n_hosps_lwl), " hospitals below the lower ",
                                "warning limit, which is two standard deviations ",
                                "below the Scottish average: ",
                                paste0(smr_data_lwl$location_name, " (",
                                       janitor::round_half_up(smr_data_lwl$smr, 2),
                                       ")", collapse = ", "), ".")
    

    if(n_hosps_lcl == 0){

      output_1_cl <- paste0("* For the period ",
                       hsmr::yr(end_date), " no hospitals had a significantly lower ",
                       "standardised mortality ratio than the national average.")

      output_2_cl <- paste0("There were no hospitals below ",
                         "the lower control limit, which is three standard ",
                         "deviations below the Scottish average. ")
      
      if(n_hosps_lwl == 0){ #Scenario 1 - no hosps below UWL
        
        output_1_wl <- scen1_output_1_wl
        output_2_wl <- scen1_output_2_wl
        
      }else if (n_hosps_lwl == 1){ #Scenario 4 - 1 hosp under UWL
        
        output_1_wl <- scen4_output_1_wl
        output_2_wl <- scen4_output_2_wl
        
      } else if (n_hosps_lwl > 1){ #Scenario 5 - more than 1 hosp over UWL
        
        output_1_wl <- scen5_output_1_wl
        output_2_wl <- scen5_output_2_wl
        
      }
      
      output_1 <- paste(output_1_cl, output_1_wl)
      output_2 <- paste(output_2_cl, output_2_wl)
      output_lwl <- output_2_wl
      output_lcl <- output_2_cl

    }
    
    if(n_hosps_lcl == 1){

      output_1_cl <- paste0("* For the period ",
                       hsmr::yr(end_date), " one hospital had a significantly lower ",
                       "standardised mortality ratio than the national average",
                       ": ", paste0(smr_data_lcl$location_name, " (",
                                    janitor::round_half_up(smr_data_lcl$smr, 2), ")",
                                    collapse = ", "), ".")

      output_2_cl <- paste0("There was one hospital below the",
                         " lower control limit, which is three standard ",
                         "deviations below the Scottish average: ",
                         paste0(smr_data_lcl$location_name, " (",
                                janitor::round_half_up(smr_data_lcl$smr, 2), ")",
                                collapse = ", "), ".")
      
      if(n_hosps_lwl == 0){ #Scenario 1 - no hosps below UWL
        
        output_1_wl <- scen1_output_1_wl
        output_2_wl <- scen1_output_2_wl
        
      }else if (n_hosps_lwl == 1){ #Scenario 4 - 1 hosp under UWL
        
        output_1_wl <- scen4_output_1_wl
        output_2_wl <- scen4_output_2_wl
        
      } else if (n_hosps_lwl > 1){ #Scenario 5 - more than 1 hosp over UWL
        
        output_1_wl <- scen5_output_1_wl
        output_2_wl <- scen5_output_2_wl
        
      }
      
      output_1 <- paste(output_1_cl, output_1_wl)
      output_2 <- paste(output_2_cl, output_2_wl)
      output_lwl <- output_2_wl
      output_lcl <- output_2_cl

    }

    if(n_hosps_lcl > 1){

      output_1_cl <- paste0("* For the period ",
                       hsmr::yr(end_date),numbers_to_words(n_hosps_lcl), " had a significantly lower ",
                       "standardised mortality ratio "," than the national average",
                       ": ", paste0(smr_data_lcl$location_name, " (",
                                    janitor::round_half_up(smr_data_lcl$smr, 2), ")",
                                    collapse = ", "), ".")

      output_2_cl <- paste0("There were ", numbers_to_words(n_hosps_lcl),
                         " hospitals below the lower ",
                         "control limit, which is three standard deviations ",
                         "below the Scottish average: ",
                         paste0(smr_data_lcl$location_name, " (",
                                janitor::round_half_up(smr_data_lcl$smr, 2),
                                ")", collapse = ", "), ".")
      
      if(n_hosps_lwl == 0){ #Scenario 1 - no hosps below UWL
        
        output_1_wl <- scen1_output_1_wl
        output_2_wl <- scen1_output_2_wl
        
      }else if (n_hosps_lwl == 1){ #Scenario 4 - 1 hosp under UWL
        
        output_1_wl <- scen4_output_1_wl
        output_2_wl <- scen4_output_2_wl
        
      } else if (n_hosps_lwl > 1){ #Scenario 5 - more than 1 hosp over UWL
        
        output_1_wl <- scen5_output_1_wl
        output_2_wl <- scen5_output_2_wl
        
      }
      
      output_1 <- paste(output_1_cl, output_1_wl)
      output_2 <- paste(output_2_cl, output_2_wl)
      output_lwl <- output_2_wl
      output_lcl <- output_2_cl

    }
    
  }
  
  
  output_3 <- paste(output_ucl,output_lcl, output_uwl, output_lwl)

  return(c(output_1, output_2,output_3))
}
