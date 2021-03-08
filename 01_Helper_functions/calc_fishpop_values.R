##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# Helper functions to calculate fish population total and mean values # 

calc_total_excretion <- function(x) {
  
  dplyr::filter(x, timestep == max(timestep)) %>%
    dplyr::select(excretion) %>%
    tidyr::pivot_longer(cols = excretion,
                        names_to = "measure", values_to = "value") %>%
    dplyr::group_by(measure) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
}

calc_mort <- function(x) {
  
  dplyr::filter(x, timestep == max(timestep)) %>%
    dplyr::select(died_consumption, died_background) %>%
    tidyr::pivot_longer(cols = c(died_consumption, died_background),
                        names_to = "measure", values_to = "value") %>%
    dplyr::group_by(measure) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
}

# divide  by 100 x 100 cells to get value/sqm
calc_fish_size <- function(x) {
  
  dplyr::select(x, timestep, length, weight) %>% 
    tidyr::pivot_longer(cols = c(length, weight), 
                        names_to = "measure", values_to = "value") %>% 
    dplyr::group_by(timestep, measure) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE) / 10000, .groups = "drop") 
  
}
