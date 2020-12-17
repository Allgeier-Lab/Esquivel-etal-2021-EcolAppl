################################################
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
################################################

change_parameters <- function(x, change, return_list = TRUE) {
  
  param_default <- unlist(x)
  
  param <- purrr::map(seq_along(param_default), function(i) {
    
    param_temp <- param_default
    
    param_temp[i] <- param_temp[i] + param_temp[i] * change 
    
    if (return_list) {
      
      param_temp <- as.list(param_temp)
      
    }
    
    return(param_temp)
  })
  
  return(param)
}
