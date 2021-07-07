################################################
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
################################################

change_parameters <- function(x, change, defaults = NULL, return_list = TRUE) {
  
  # convert list to vector
  param_temp <- unlist(x)
  
  # loop through all parameters
  param_changed <- purrr::map(seq_along(param_temp), function(i) {
    
    # change current parameter
    param_temp[i] <- param_temp[i] + param_temp[i] * change 
    
    # check if some parameters needed to be added again
    if (!is.null(defaults)) {
      
      param_temp <- c(param_temp, defaults)
      
    }
    
    # convert to list again
    if (return_list) {
      
      param_temp <- as.list(param_temp)
      
    }
    
    return(param_temp)
    
  })
  
  return(param_changed)
  
}
