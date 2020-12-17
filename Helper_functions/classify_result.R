##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# 1: bg and ag are smaller than starting value
# 2: bg is smaller and ag is larger than starting value
# 3: bg is larger and ag is smaller than starting value
# 4: bg and ag are larger than starting value
# 5: no change

classify_result <- function(result) {

  # get seafloor results of last timestep (not really needed; already filtered)
  seafloor_temp <- dplyr::filter(result$seafloor, timestep == max(timestep))
  
  # calculate mean bg/ag biomass of all cells
  bg_temp <- max(seafloor_temp$bg_biomass, na.rm = TRUE)
  
  ag_temp <- max(seafloor_temp$ag_biomass, na.rm = TRUE)
  
  # returns NA if no IF is TRUE (should not happen)
  classification <- NA
  
  if (bg_temp < result$starting_values$bg_biomass & 
      ag_temp < result$starting_values$ag_biomass) {
    
    classification <- "bg-//ag-"
    
  }
  
  if (bg_temp < result$starting_values$bg_biomass & 
      ag_temp >= result$starting_values$ag_biomass) {
    
    classification <- "bg-//ag+"
    
  }
  
  if (bg_temp >= result$starting_values$bg_biomass & 
      ag_temp < result$starting_values$ag_biomass) {
    
    classification <- "bg+//ag-"
    
  }
  
  if (bg_temp >= result$starting_values$bg_biomass & 
      ag_temp >= result$starting_values$ag_biomass) {
    
    classification <- "bg+//ag+"
    
  }
  
  if (bg_temp == result$starting_values$bg_biomass & 
      ag_temp == result$starting_values$ag_biomass) {
    
    classification <- "bg=//ag="
    
  }
  
  return(classification)
}
