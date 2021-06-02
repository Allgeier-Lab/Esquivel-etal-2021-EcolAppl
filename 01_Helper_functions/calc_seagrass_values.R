##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# Helper functions to calculate seagrass total and mean values # 

calc_total_biomass <- function(x, i = NULL) {
  
  if (is.null(i)) i = max(x$timestep)
  
  result <- dplyr::filter(x, timestep == i) %>% 
    dplyr::select(ag_biomass, bg_biomass) %>% 
    tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                        names_to = "part", values_to = "biomass") %>% 
    dplyr::group_by(part) %>% 
    dplyr::summarise(value = sum(biomass, na.rm = TRUE), .groups = "drop") 
  
  return(result)
  
}

calc_total_production <- function(x, i = NULL) {
  
  if (is.null(i)) i = max(x$timestep)
  
  result <- dplyr::filter(x, timestep == max(timestep)) %>% 
    dplyr::select(ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(part) %>% 
    dplyr::summarise(value = sum(production, na.rm = TRUE), .groups = "drop") 
  
  return(result)
  
}

calc_dist_biomass <- function(x, class_width = 1, i = NULL) {
  
  if (is.null(i)) i = max(x$timestep)
  
  result <- dplyr::filter(x, timestep == i, reef == 0) %>% 
    dplyr::mutate(dist = sqrt(x ^ 2 + y ^ 2), 
                  dist_class = cut(dist, 
                                   breaks = seq(from = 0, to = max(dist) + class_width,
                                                by = class_width), ordered_result = TRUE)) %>% 
    dplyr::select(dist_class, ag_biomass, bg_biomass) %>% 
    tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                        names_to = "part", values_to = "biomass") %>% 
    dplyr::group_by(dist_class, part) %>% 
    dplyr::summarise(value = mean(biomass, na.rm = TRUE), .groups = "drop") 
  
  return(result)
  
}

calc_dist_production <- function(x, class_width = 1, i = NULL) {
  
  if (is.null(i)) i = max(x$timestep)
  
  result <- dplyr::filter(x, timestep == i, reef == 0) %>% 
    dplyr::mutate(dist = sqrt(x ^ 2 + y ^ 2), 
                  dist_class = cut(dist, 
                                   breaks = seq(from = 0, to = max(dist) + class_width,
                                                by = class_width), ordered_result = TRUE)) %>% 
    dplyr::select(dist_class, ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(dist_class, part) %>% 
    dplyr::summarise(value = mean(production, na.rm = TRUE), .groups = "drop") 
  
  return(result)
  
}

log_response <- function(data, indices) {
  
  x <- data[indices, ] 
  
  f <- mean(log(x$attr)) - mean(log(x$rand))
  
  # f <- (exp(f) - 1) * 100
  
  return(f)
} 

calc_rel_diff_dist <- function(x, breaks, i = NULL) {
  
  if (is.null(i)) i = max(x$rand$max_i)
  
  rand <- magrittr::extract2(x, "rand") %>%
    magrittr::extract2("seafloor")
  
  attr <- magrittr::extract2(x, "attr") %>%
    magrittr::extract2("seafloor")
  
  total <- dplyr::bind_rows(rand = rand, attr = attr, .id = "type") %>% 
    dplyr::filter(timestep == i, reef == 0) %>% 
    dplyr::mutate(dist = sqrt(x ^ 2 + y ^ 2),
                  class_dist = cut(dist, breaks = breaks, 
                                   right = TRUE, ordered_result = TRUE)) %>% 
    dplyr::select(type, class_dist, ag_biomass, ag_production, bg_biomass, bg_production) %>% 
    tidyr::pivot_longer(-c(type, class_dist)) %>% 
    dplyr::group_by(type, class_dist, name) %>% 
    dplyr::summarise(value = mean(value), .groups = "drop") %>% 
    tidyr::pivot_wider(names_from = type, values_from = value) %>% 
    dplyr::mutate(value = (attr - rand) / rand * 100) %>% 
    dplyr::select(-c(rand, attr))

return(total)
  
}

calc_total_excretion <- function(x, i = NULL) {
  
  if (is.null(i)) i = max(x$timestep)
  
  dplyr::filter(x, timestep == i) %>%
    dplyr::select(excretion) %>%
    tidyr::pivot_longer(cols = excretion,
                        names_to = "measure", values_to = "value") %>%
    dplyr::group_by(measure) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
}
