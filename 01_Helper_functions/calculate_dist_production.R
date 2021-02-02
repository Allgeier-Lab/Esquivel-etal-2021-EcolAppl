calculate_dist_production <- function(x) {
  
  dplyr::filter(x, reef == 0, timestep == max(timestep)) %>% 
    dplyr::select(timestep, reef_dist, ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(timestep, reef_dist, part) %>% 
    dplyr::summarise(production = mean(production, na.rm = TRUE), .groups = "drop") 
  
}
