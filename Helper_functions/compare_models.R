##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

compare_models <- function(x, id, col) {

  # read data  
  data <- purrr::map(x[id$id], readr::read_rds)
  
  # get label of col
  labels_temp <- id[[col]]

  # summarize results, add id col, reshape long format, remove dead detritus pool
  data <- purrr::map_dfr(data, function(i) {
    x <- arrR::summarize_results(i)
    return(x$seafloor)}, .id = "design_id") %>% 
    dplyr::mutate(design_id = dplyr::case_when(design_id == 1 ~ id$id[[1]], 
                                               design_id == 2 ~ id$id[[2]],
                                               design_id == 3 ~ id$id[[3]],
                                               design_id == 4 ~ id$id[[4]],
                                               design_id == 5 ~ id$id[[5]])) %>% 
    tidyr::pivot_longer(-c(design_id, timestep, summary)) %>% 
    dplyr::filter(name != "detritus_dead") %>% 
    dplyr::mutate(design_id = factor(design_id, labels = labels_temp), 
                  summary = factor(summary, levels = c("min", "mean", "max")),
                  name = factor(name, levels = c("ag_biomass", "bg_biomass", 
                                                 "nutrients_pool", "detritus_pool")))
  
  return(data)
}
