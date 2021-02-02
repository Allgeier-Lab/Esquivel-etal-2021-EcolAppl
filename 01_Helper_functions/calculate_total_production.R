calculate_total_production <- function(x) {
  
    dplyr::select(x, timestep, ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(timestep, part) %>% 
    dplyr::summarise(production = sum(production, na.rm = TRUE), .groups = "drop") 

}
