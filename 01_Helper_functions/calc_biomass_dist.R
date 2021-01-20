##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

calc_biomass_dist <- function(result, by = 1) {
  
  # filter last timestep of model run (not really needed; already filtered)
  seafloor_temp <- dplyr::filter(result$seafloor, timestep == result$max_i)
  
  # classifiy distance into distance bins
  reef_dist_clss <- cut(seafloor_temp$reef_dist, include.lowest = TRUE, labels = FALSE,
                        breaks = seq(from = 0, to = max(seafloor_temp$reef_dist) + by, 
                                     by = by))
  
  # add classification to data
  seafloor_temp$reef_dist_clss <- reef_dist_clss
  
  # calculate mean distance at distance r
  biomass_dist <- dplyr::group_by(seafloor_temp, reef_dist_clss) %>% 
    dplyr::summarise(bg_biomass = mean(bg_biomass, na.rm = TRUE), 
                     ag_biomass = mean(ag_biomass, na.rm = TRUE))
  
  return(biomass_dist)
}
