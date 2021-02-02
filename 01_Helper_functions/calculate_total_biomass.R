calculate_total_biomass <- function(x) {
  
  dplyr::select(x, timestep, ag_biomass, bg_biomass) %>% 
    tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                        names_to = "part", values_to = "biomass") %>% 
    dplyr::group_by(timestep, part) %>% 
    dplyr::summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop") 
  
}
