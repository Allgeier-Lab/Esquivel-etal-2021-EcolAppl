##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

summarize_fishpop <- function(result) {
  
  # get data of fish population
  fishpop_temp <- result$fishpop
  
  # summarize for each timestep
  seafloor_sum <- dplyr::group_by(fishpop_temp, timestep) %>% 
    dplyr::summarize(length = mean(length, na.rm = TRUE), 
                     weight = mean(weight, na.rm = TRUE), 
                     died_consumption = mean(died_consumption, na.rm = TRUE), 
                     died_background = mean(died_background, na.rm = TRUE))
  
  return(seafloor_sum)
}
