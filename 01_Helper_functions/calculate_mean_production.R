calculate_mean_production <- function(x, n_cells) {
  
  dplyr::select(x, timestep, ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(timestep, part) %>% 
    dplyr::summarise(production = sum(production), .groups = "drop") %>% 
    dplyr::group_by(part) %>% 
    dplyr::mutate(production = (production - dplyr::lag(production, default = 0)) / n_cells) %>%
    dplyr::ungroup()
  
}
