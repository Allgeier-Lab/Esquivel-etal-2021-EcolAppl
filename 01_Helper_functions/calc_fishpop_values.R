##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# Helper functions to calculate fish population total and mean values # 

calc_fish_size <- function(x) {
  
  dplyr::select(x, timestep, length, weight) %>% 
    tidyr::pivot_longer(cols = c(length, weight), 
                        names_to = "measure", values_to = "value") %>% 
    dplyr::group_by(timestep, measure) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") 
  
}

calc_fish_excretion <- function(x, n_cells) {
  
  dplyr::select(x, timestep, consumption, excretion) %>% 
    tidyr::pivot_longer(cols = c(consumption, excretion), 
                        names_to = "measure", values_to = "value") %>% 
    dplyr::group_by(timestep, measure) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::group_by(measure) %>% 
    dplyr::mutate(value = (value - dplyr::lag(value, default = 0)) / n_cells) %>%
    dplyr::ungroup()
  
}

calc_fish_mortality <- function(x) {
  
  # get number of fish 
  n <- max(x$id)
  
  dplyr::select(x, timestep, died_consumption, died_background) %>% 
    tidyr::pivot_longer(cols = c(died_consumption, died_background), 
                        names_to = "mortality", values_to = "value") %>% 
    dplyr::group_by(timestep, mortality) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::group_by(mortality) %>% 
    dplyr::mutate(value = (value - dplyr::lag(value, default = 0)) / n) %>%
    dplyr::ungroup()
  
}
