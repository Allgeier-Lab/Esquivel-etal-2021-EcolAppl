################################################
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
################################################

#### Biomass ####

calc_biomass_sa <- function(default, changed, verbose = TRUE) {
  
  # get name of changed to get changed parameters
  names_parameters <- stringr::str_replace_all(string = names(changed), 
                                               pattern = "[:digit:]",
                                               replacement = "") %>% 
    stringr::str_sub(end = -2)
  
  # get length of input for printing
  n_default <- length(default)
  
  n_changed <- length(changed)
  
  # loop through all input data
  biomass_default <- purrr::map_dfr(seq_along(default), function(i) {
    
    if (verbose) {
      
      message("\r> Progress (default): ", i, "/", n_default, appendLF = FALSE)
      
    }
    
    # get mean of biomass
    temp_ag_default <- tryCatch(expr = mean(default[[i]]$seafloor$ag_biomass, 
                                           na.rm = TRUE), 
                                error = function(e) NA)
    
    temp_bg_default <- tryCatch(expr = mean(default[[i]]$seafloor$bg_biomass, 
                                            na.rm = TRUE), 
                                error = function(e) NA)
    
    c(ag = temp_ag_default, bg = temp_bg_default)
    
  })
  
  # get mean values of ag and bg
  ag_default <- mean(biomass_default$ag, na.rm = TRUE)
  
  bg_default <- mean(biomass_default$bg, na.rm = TRUE)
  
  if (verbose) {message("")}
  
  # loop through all input data
  biomass_changed <- purrr::map_dfr(seq_along(changed), function(i) {
    
    if (verbose) {
      
      message("\r> Progress (default): ", i, "/", n_changed, appendLF = FALSE)
      
    }
    
    # get mean of biomass
    temp_ag_changed <- tryCatch(expr = mean(changed[[i]]$seafloor$ag_biomass, 
                                            na.rm = TRUE), 
                                error = function(e) NA)
    
    temp_bg_changed <- tryCatch(expr = mean(changed[[i]]$seafloor$bg_biomass, 
                                            na.rm = TRUE), 
                                error = function(e) NA)
    
    # combine in one tibble and calc relative diff
    tibble::tibble(ag_changed = temp_ag_changed, 
                   bg_changed = temp_bg_changed) %>% 
      dplyr::mutate(diff_ag = (ag_changed - ag_default) / ag_default, 
                    diff_bg = (bg_changed - bg_default) / bg_default)
    
  })
  
  if (verbose) {message("")}
  
  # get mean of each parameter repetition
  biomass_changed <- dplyr::mutate(biomass_changed, parameter = names_parameters) %>% 
    dplyr::group_by(parameter) %>% 
    dplyr::summarise(diff_ag = mean(diff_ag), 
                     diff_bg = mean(diff_bg)) %>%
    dplyr::ungroup()
  
  return(biomass_changed)
}

#### Fishpop dimension ####

calc_dim_sa <- function(default, changed, 
                        verbose = TRUE) {
  
  # get name of changed to get changed parameters
  names_parameters <- stringr::str_replace_all(string = names(changed),
                                               pattern = "[:digit:]",
                                               replacement = "") %>% 
    stringr::str_sub(end = -2)
  
  # get length of input for printing
  n_default <- length(default)
  
  n_changed <- length(changed)
  
  # loop through all input data
  dim_default <- purrr::map_dfr(seq_along(default), function(i) {
    
    if (verbose) {
      
      message("\r> Progress (default): ", i, "/", n_default, appendLF = FALSE)
      
    }
    
    # get mean of mortality
    temp_length_default <- tryCatch(expr = mean(default[[i]]$fishpop$length), 
                                    error = function(e) NA)
    
    temp_weight_default <- tryCatch(expr = mean(default[[i]]$fishpop$weight),
                                    error = function(e) NA)
    
    c(length = temp_length_default, weight = temp_weight_default)
    
  })
  
  # get mean values of ag and bg
  length_default <- mean(dim_default$length, na.rm = TRUE)
  
  weight_default <- mean(dim_default$weight, na.rm = TRUE)
  
  if (verbose) {message("")}
  
  # loop through all input data
  dim_changed <- purrr::map_dfr(seq_along(changed), function(i) {
    
    if (verbose) {
      
      message("\r> Progress (default): ", i, "/", n_changed, appendLF = FALSE)
      
    }
    
    # get mean mortality
    temp_length_changed <- tryCatch(expr = mean(changed[[i]]$fishpop$length), 
                                    error = function(e) NA)
    
    temp_weight_changed <- tryCatch(expr = mean(changed[[i]]$fishpop$weight), 
                                    error = function(e) NA)
    
    # combine in one tibble and calc relative diff
    tibble::tibble(length_changed = temp_length_changed, 
                   weight_changed = temp_weight_changed) %>% 
      dplyr::mutate(diff_length = (length_changed - length_default) / length_default, 
                     diff_weight = (weight_changed - weight_default) / weight_default)
  })
  
  if (verbose) {message("")}
  
  # get mean of each parameter repetition
  dim_changed <- dplyr::mutate(dim_changed, parameter = names_parameters) %>% 
    dplyr::group_by(parameter) %>% 
    dplyr::summarise(diff_length = mean(diff_length), 
                     diff_weight = mean(diff_weight)) %>%
    dplyr::ungroup()
  
  return(dim_changed)
}
