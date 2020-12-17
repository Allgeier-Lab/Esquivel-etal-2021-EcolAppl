##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

calc_biomass_range <- function(result, by = 1) {
  
  # filter last timestep of model run (not really needed; already filtered)
  seafloor_temp <- dplyr::filter(result$seafloor, timestep == result$max_i)
  
  # classify distance values into bins
  reef_dist_clss <- cut(seafloor_temp$reef_dist, include.lowest = TRUE, labels = FALSE,
                        breaks = seq(from = 0, to = max(seafloor_temp$reef_dist) + by, 
                                     by = by))
  
  # add classification to data frame
  seafloor_temp$reef_dist_clss <- reef_dist_clss
  
  # calculate range at distance r
  biomass_range <- dplyr::group_by(seafloor_temp, reef_dist_clss) %>% 
    dplyr::summarise(bg_biomass = mean(bg_biomass, na.rm = TRUE)) %>% 
    dplyr::pull(bg_biomass) %>% 
    range(na.rm = TRUE)
  
  # standardize range by maximum
  rel_range <- (biomass_range[2] - biomass_range[1]) / biomass_range[2] * 100
  
  return(tibble(rel_range))
}
