##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# Helper functions to calculate seagrass total and mean values # 

calc_total_biomass <- function(x, norm = FALSE) {
  
  result <- dplyr::filter(x, timestep == max(timestep)) %>% 
    dplyr::select(ag_biomass, bg_biomass) %>% 
    tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                        names_to = "part", values_to = "biomass") %>% 
    dplyr::group_by(part) %>% 
    dplyr::summarise(value = sum(biomass, na.rm = TRUE), .groups = "drop") 
  
  if (norm) {
    
    excretion <- dplyr::filter(x, timestep == max(timestep)) %>%
      dplyr::pull(excretion) %>% 
      sum()
    
    result <- dplyr::mutate(result, value = value / excretion)
    
  }
  
  return(result)
  
}

calc_inc_biomass <- function(x, norm = FALSE) {
  
  result <- dplyr::group_by(x, timestep) %>% 
    dplyr::summarise(ag_biomass = sum(ag_biomass, na.rm = TRUE), 
                     bg_biomass = sum(bg_biomass, na.rm = TRUE), .groups = "drop") %>% 
    tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                        names_to = "part", values_to = "biomass") %>% 
    dplyr::group_by(part) %>% 
    dplyr::mutate(value = biomass - dplyr::lag(biomass)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(timestep == max(timestep)) %>% 
    dplyr::select(-c(timestep, biomass))
  
  if (norm) {
    
    excretion <- dplyr::filter(x, timestep == max(timestep)) %>%
      dplyr::pull(excretion) %>% sum()
    
    result <- dplyr::mutate(result, value = value / excretion)
    
  }
  
  return(result)
  
}

calc_total_production <- function(x, norm = FALSE) {
  
  result <- dplyr::filter(x, timestep == max(timestep)) %>% 
    dplyr::select(ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(part) %>% 
    dplyr::summarise(value = sum(production, na.rm = TRUE), .groups = "drop") 
  
  if (norm) {
    
    excretion <- dplyr::filter(x, timestep == max(timestep)) %>%
      dplyr::pull(excretion) %>% 
      sum()
    
    result <- dplyr::mutate(result, value = value / excretion)
    
  }
  
  return(result)
  
}

calc_dist_biomass <- function(x, norm = FALSE, clss_width = 1) {
  
  result <- dplyr::filter(x, timestep == max(timestep), reef == 0) %>% 
    dplyr::select(reef_dist, ag_biomass, bg_biomass) %>% 
    dplyr::mutate(dist_clss = cut(reef_dist,
                                  breaks = seq(from = 0, 
                                               to = max(reef_dist) + clss_width, 
                                               by = clss_width))) %>% 
    tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                        names_to = "part", values_to = "biomass") %>% 
    dplyr::group_by(dist_clss, part) %>% 
    dplyr::summarise(value = mean(biomass, na.rm = TRUE), .groups = "drop") 
  
  if (norm) {
    
    excretion <- dplyr::filter(x, timestep == max(timestep)) %>%
      dplyr::pull(excretion) %>% sum()
    
    result <- dplyr::mutate(result, value = value / excretion)
    
  }
  
  return(result)
  
}

calc_dist_production <- function(x, norm = FALSE, clss_width = 1) {
  
  result <- dplyr::filter(x, timestep == max(timestep), reef == 0) %>% 
    dplyr::select(reef_dist, ag_production, bg_production) %>% 
    dplyr::mutate(dist_clss = cut(reef_dist,
                                  breaks = seq(from = 0, 
                                               to = max(reef_dist) + clss_width, 
                                               by = clss_width), 
                                  ordered_result = TRUE)) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(dist_clss, part) %>% 
    dplyr::summarise(value = mean(production, na.rm = TRUE), .groups = "drop") 
  
  if (norm) {
    
    excretion <- dplyr::filter(x, timestep == max(timestep)) %>%
      dplyr::pull(excretion) %>% sum()
    
    result <- dplyr::mutate(result, value = value / excretion)
    
  }
  
  return(result)
  
}

log_response <- function(data, indices) {
  
  x <- data[indices, ] 
  
  f <- mean(log(x$attr)) - mean(log(x$rand))
  
  # f <- (exp(f) - 1) * 100
  
  return(f)
} 

calc_rel_diff_dist <- function(x, breaks) {
  
  rand <- magrittr::extract2(x, "rand") %>%
    magrittr::extract2("seafloor")
  
  attr <- magrittr::extract2(x, "attr") %>%
    magrittr::extract2("seafloor")
  
  total <- dplyr::bind_rows(rand = rand, attr = attr, .id = "type") %>% 
    dplyr::filter(timestep == max(timestep), reef == 0) %>% 
    dplyr::select(type, reef_dist, ag_biomass, ag_production, bg_biomass, bg_production) %>% 
    tidyr::pivot_longer(-c(type, reef_dist)) %>% 
    dplyr::mutate(class_dist = cut(reef_dist, breaks = breaks, 
                                   right = TRUE, ordered_result = TRUE)) %>%
    dplyr::filter(!is.na(class_dist)) %>% 
    dplyr::group_by(type, class_dist, name) %>% 
    dplyr::summarise(value = mean(value), .groups = "drop") %>% 
    tidyr::pivot_wider(names_from = type, values_from = value) %>% 
    dplyr::mutate(value = (attr - rand) / rand * 100) %>% 
    dplyr::select(-c(rand, attr))

return(total)
}
