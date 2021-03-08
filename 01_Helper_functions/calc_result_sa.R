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
        dplyr::mutate(diff_value = (value.changed - value.default) / value.default)
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
                     diff_sd = mean(diff_value), .groups = "drop")
  
  return(biomass_changed)
}

# #### Fishpop dimension ####
# 
# calc_dim_sa <- function(default, changed, 
#                         verbose = TRUE) {
#   
#   # get name of changed to get changed parameters
#   names_parameters <- stringr::str_replace_all(string = names(changed),
#                                                pattern = "[:digit:]",
#                                                replacement = "") %>% 
#     stringr::str_sub(end = -2)
#   
#   # get length of input for printing
#   n_default <- length(default)
#   
#   n_changed <- length(changed)
#   
#   # loop through all input data
#   dim_default <- purrr::map_dfr(seq_along(default), function(i) {
#     
#     if (verbose) {
#       
#       message("\r> Progress (default): ", i, "/", n_default, appendLF = FALSE)
#       
#     }
#     
#     # get mean of mortality
#     temp_length_default <- tryCatch(expr = mean(default[[i]]$fishpop$length), 
#                                     error = function(e) NA)
#     
#     temp_weight_default <- tryCatch(expr = mean(default[[i]]$fishpop$weight),
#                                     error = function(e) NA)
#     
#     c(length = temp_length_default, weight = temp_weight_default)
#     
#   })
#   
#   # get mean values of ag and bg
#   length_default <- mean(dim_default$length, na.rm = TRUE)
#   
#   weight_default <- mean(dim_default$weight, na.rm = TRUE)
#   
#   if (verbose) {message("")}
#   
#   # loop through all input data
#   dim_changed <- purrr::map_dfr(seq_along(changed), function(i) {
#     
#     if (verbose) {
#       
#       message("\r> Progress (changed): ", i, "/", n_changed, appendLF = FALSE)
#       
#     }
#     
#     # get mean mortality
#     temp_length_changed <- tryCatch(expr = mean(changed[[i]]$fishpop$length), 
#                                     error = function(e) NA)
#     
#     temp_weight_changed <- tryCatch(expr = mean(changed[[i]]$fishpop$weight), 
#                                     error = function(e) NA)
#     
#     # combine in one tibble and calc relative diff
#     tibble::tibble(length_changed = temp_length_changed, 
#                    weight_changed = temp_weight_changed) %>% 
#       dplyr::mutate(diff_length = (length_changed - length_default) / length_default, 
#                      diff_weight = (weight_changed - weight_default) / weight_default)
#   })
#   
#   if (verbose) {message("")}
#   
#   # get mean of each parameter repetition
#   dim_changed <- dplyr::mutate(dim_changed, parameter = names_parameters) %>% 
#     dplyr::group_by(parameter) %>% 
#     dplyr::summarise(diff_length = mean(diff_length), 
#                      diff_weight = mean(diff_weight)) %>%
#     dplyr::ungroup()
#   
#   return(dim_changed)
# }
