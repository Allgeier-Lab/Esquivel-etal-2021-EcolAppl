calculate_fish_size <- function(x) {
  
  dplyr::select(x, timestep, length, weight) %>% 
    tidyr::pivot_longer(cols = c(length, weight), 
                        names_to = "measure", values_to = "value") %>% 
    dplyr::group_by(timestep, measure) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 
  
}
