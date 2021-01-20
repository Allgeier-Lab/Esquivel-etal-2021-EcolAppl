##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

calc_biomass_dist_change <- function(result, by = 1) {
  
  seafloor_temp <- result$seafloor
  
  reef_dist_clss <- cut(seafloor_temp$reef_dist, include.lowest = TRUE, labels = FALSE,
                        breaks = seq(from = 0, to = max(seafloor_temp$reef_dist) + by, 
                                     by = by))
  
  seafloor_temp$reef_dist_clss <- reef_dist_clss
  
  biomass_dist <- dplyr::group_by(seafloor_temp, reef_dist_clss) %>% 
    dplyr::summarise(ag_biomass = mean(ag_biomass, na.rm = TRUE), 
                     bg_biomass = mean(bg_biomass, na.rm = TRUE)) %>% 
    dplyr::mutate(bg_change = (bg_biomass - dplyr::first(bg_biomass)) / dplyr::first(bg_biomass), 
                  ag_change = (ag_biomass - dplyr::first(ag_biomass)) / dplyr::first(ag_biomass)) %>% 
    dplyr::select(-ag_biomass, -bg_biomass)
  
  return(biomass_dist)
}
