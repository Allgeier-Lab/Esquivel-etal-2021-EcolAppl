##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# 1: bg and ag rage are smaller than threshold
# 2: bg range is smaller and ag range is larger than threshold
# 3: bg range is larger and ag range is smaller than threshold
# 4: bg range and ag range are larger than threshold

classify_range <- function(result, threshold = 0.25) {

  # get seafloor results of last timestep (not really needed; already filtered)
  seafloor_temp <- dplyr::filter(result$seafloor, timestep == max(timestep))
  
  # calculate mean bg/ag biomass of all cells
  bg_mean <- mean(seafloor_temp$bg_biomass, na.rm = TRUE)
  
  ag_mean <- mean(seafloor_temp$ag_biomass, na.rm = TRUE)
  
  # calculate max bg/ag biomass of all cells
  bg_max <- max(seafloor_temp$bg_biomass, na.rm = TRUE)
  
  ag_max <- max(seafloor_temp$ag_biomass, na.rm = TRUE)
  
  # calculate relative difference 
  bg_diff <- (bg_max - bg_mean) / bg_max
  
  ag_diff <- (ag_max - ag_mean) / ag_max
  
  # returns NA if no IF is TRUE (should not happen)
  classification <- NA
  
  if (bg_diff < threshold & ag_diff < threshold) {
    
    classification <- "bg-//ag-"
    
  }
  
  if (bg_diff < threshold & ag_diff >= threshold) {
    
    classification <- "bg-//ag+"
    
  }
  
  if (bg_diff >= threshold & ag_diff < threshold) {
    
    classification <- "bg+//ag-"
    
  }
  
  if (bg_diff >= threshold & ag_diff >= threshold) {
    
    classification <- "bg+//ag+"
    
  }
  
  return(classification)
}
