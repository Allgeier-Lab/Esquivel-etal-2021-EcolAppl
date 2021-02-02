calculate_fish_excretion <- function(x) {
  
  dplyr::select(x, timestep, consumption, excretion) %>% 
    tidyr::pivot_longer(cols = c(consumption, excretion), 
                        names_to = "measure", values_to = "value") %>% 
    dplyr::group_by(timestep, measure) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 
  
}
