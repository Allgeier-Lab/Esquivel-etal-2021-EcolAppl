##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# Helper functions to calculate seagrass total and mean values # 

calc_total_biomass <- function(x) {
  
  dplyr::select(x, timestep, ag_biomass, bg_biomass) %>% 
    tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                        names_to = "part", values_to = "biomass") %>% 
    dplyr::group_by(timestep, part) %>% 
    dplyr::summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop") 
  
}

calc_total_production <- function(x) {
  
  dplyr::select(x, timestep, ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(timestep, part) %>% 
    dplyr::summarise(production = sum(production, na.rm = TRUE), .groups = "drop") 
  
}

calc_mean_production <- function(x, n_cells) {
  
  dplyr::select(x, timestep, ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(timestep, part) %>% 
    dplyr::summarise(production = sum(production), .groups = "drop") %>% 
    dplyr::group_by(part) %>% 
    dplyr::mutate(production = (production - dplyr::lag(production, default = 0)) / n_cells) %>%
    dplyr::ungroup()
  
}

calc_dist_production <- function(x) {
  
  dplyr::filter(x, reef == 0, timestep == max(timestep)) %>% 
    dplyr::select(timestep, reef_dist, ag_production, bg_production) %>% 
    tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                        names_to = "part", values_to = "production") %>% 
    dplyr::group_by(timestep, reef_dist, part) %>% 
    dplyr::summarise(production = mean(production, na.rm = TRUE), .groups = "drop") 
  
}

# remove_burnin <- function(x) {
#   
#   # get burn_in timestep
#   burn_itr <- dplyr::filter(x, burn_in == "yes") %>% 
#     dplyr::pull(timestep) %>% 
#     max()
#   
#   # get cumulative production at burn_in
#   burn_production <- dplyr::filter(x, timestep == burn_itr) %>% 
#     dplyr::select(ag_production, bg_production)
#   
#   # remove production before burn_in
#   dplyr::filter(x, timestep == max(timestep)) %>% 
#     dplyr::mutate(ag_production = ag_production - burn_production$ag_production, 
#                   bg_production = bg_production - burn_production$bg_production) %>% 
#     dplyr::select(ag_production, bg_production) %>% 
#     tidyr::pivot_longer(cols = c(ag_production, bg_production), 
#                         names_to = "part", values_to = "production") %>% 
#     dplyr::group_by(part) %>% 
#     dplyr::summarise(production = sum(production, na.rm = TRUE), .groups = "drop")
#   
# }
