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
    stringr::str_sub(end = -2) %>% 
    tibble::tibble(id = 1:length(.), parameter = .)
  
  # get length of input for printing
  n_default <- length(default)
  
  n_changed <- length(changed)
  
  # loop through all input data
  biomass_default <- purrr::map_dfr(seq_along(default), function(i) {
    
    if (verbose) {
      
      message("\r> Progress (default): ", i, "/", n_default, appendLF = FALSE)
      
    }
    
    dplyr::filter(default[[i]]$seafloor, timestep == max(timestep)) %>% 
      dplyr::select(x, y, ag_biomass, bg_biomass, ag_production, bg_production) %>% 
      tidyr::pivot_longer(-c(x, y)) %>% 
      dplyr::group_by(name) %>% 
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")}) %>% 
    dplyr::group_by(name) %>% 
    dplyr::summarise(value = mean(value), .groups = "drop")
  
  if (verbose) {message("")}
  
  # loop through all input data
  biomass_changed <- purrr::map_dfr(seq_along(changed), function(i) {
    
    if (verbose) {
      
      message("\r> Progress (changed): ", i, "/", n_changed, appendLF = FALSE)
      
    }
    
    tryCatch(expr = {
      dplyr::filter(changed[[i]]$seafloor, timestep == max(timestep)) %>% 
        dplyr::select(x, y, ag_biomass, bg_biomass, ag_production, bg_production) %>% 
        tidyr::pivot_longer(-c(x, y)) %>% 
        dplyr::group_by(name) %>% 
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
        dplyr::left_join(biomass_default, by = "name", suffix = c(".changed" , ".default")) %>% 
        dplyr::mutate(diff_value = (value.changed - value.default) / value.default * 100)
    }, error = function(e) tibble::tibble(name = c("ag_biomass", "bg_biomass", "ag_production", "bg_production"),
                                          value.changed = rep(as.numeric(NA), times = 4),  
                                          value.default = rep(as.numeric(NA), times = 4),
                                          diff_value = rep(as.numeric(NA), times = 4)))
  }, .id = "id") %>% 
    dplyr::mutate(id = as.integer(id))
  
  if (verbose) {message("")}
  
  # get mean of each parameter repetition
  biomass_changed <- dplyr::left_join(x = biomass_changed, y = names_parameters, by = "id") %>% 
    dplyr::group_by(name, parameter) %>% 
    dplyr::summarise(diff_mean = mean(diff_value), 
                     diff_sd = sd(diff_value), .groups = "drop")
  
  return(biomass_changed)
}
