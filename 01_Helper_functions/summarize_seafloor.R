##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

summarize_seafloor <- function(result) {
  
  # get seafloor data
  seafloor_temp <- result$seafloor
  
  # calculate mean values per timestep
  seafloor_sum <- dplyr::group_by(seafloor_temp, timestep) %>% 
    dplyr::summarize(bg_biomass = mean(bg_biomass, na.rm = TRUE), 
                     ag_biomass = mean(ag_biomass, na.rm = TRUE), 
                     nutrients_pool = mean(nutrients_pool, na.rm = TRUE), 
                     detritus_pool = mean(detritus_pool, na.rm = TRUE))
  
  return(seafloor_sum)
}
