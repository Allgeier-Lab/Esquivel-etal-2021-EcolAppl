remove_burnin <- function(x) {
  
  # get burn_in timestep
  burn_itr <- dplyr::filter(x, burn_in == "yes") %>% 
    dplyr::pull(timestep) %>% 
    max()
  
  # get cumulative production at burn_in
  burn_production <- dplyr::filter(x, timestep == burn_itr) %>% 
    dplyr::select(ag_production, bg_production)
  
  # remove production before burn_in
  dplyr::filter(x, timestep == max(timestep)) %>% 
    dplyr::mutate(ag_production = ag_production - burn_production$ag_production, 
                  bg_production = bg_production - burn_production$bg_production) %>% 
    dplyr::select(ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(part) %>% 
    dplyr::summarise(production = sum(production, na.rm = TRUE), .groups = "drop")
  
}
