################################################
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
################################################

#### Biomass ####

calc_biomass_sobol <- function(x) {
  
  dplyr::filter(x$seafloor, timestep == max(timestep)) %>% 
    dplyr::select(x, y, ag_biomass, bg_biomass, ag_production, bg_production) %>% 
    tidyr::pivot_longer(-c(x, y)) %>% 
    dplyr::group_by(name) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::group_by(name) %>% 
    dplyr::summarise(value = mean(value), .groups = "drop")
  
}
