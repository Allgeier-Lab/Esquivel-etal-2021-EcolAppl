##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

source("Helper_functions/setup.R")

#### load data and parameters ####

parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

full_design_subset <- readr::read_rds(file = "Data/Modified/results_full_design/full_design_subset.rds")

model_runs <- paste0("Data/Modified/results_full_design/future_", 
                     full_design_subset$id, "_rand.rds") %>% 
  stringr::str_sort(numeric = TRUE) %>% 
  purrr::map(readr::read_rds)

# overwrite argument for saving
overwrite <- FALSE

#### Preprocess data ####

# add missing min/max biomass parameters
full_design_subset <- dplyr::mutate(full_design_subset, 
                                    bg_biomass_min = parameters$bg_biomass_min,
                                    ag_biomass_min = parameters$ag_biomass_min, 
                                    ag_biomass_max = parameters$ag_biomass_max)

#### Result ####

seafloor <- purrr::map2_dfr(model_runs, 1:length(model_runs), function(model_temp, i) {
  
  # get parameter values
  design_temp <- full_design_subset[i, ]
  
  # get seafloor results
  seafloor_temp <- dplyr::filter(model_temp$seafloor, timestep == max(timestep))
  
  # calculate mean biomass of all cells
  bg_mean <- mean(seafloor_temp$bg_biomass, na.rm = TRUE)
  
  ag_mean <- mean(seafloor_temp$ag_biomass, na.rm = TRUE)
  
  # calculate location within range
  bg_rel <- (1 - ((design_temp$bg_biomass_max -  bg_mean) /
    (design_temp$bg_biomass_max - design_temp$bg_biomass_min))) * 100
  
  ag_rel <- (1 -  ((design_temp$ag_biomass_max - ag_mean) /
    (design_temp$ag_biomass_max - design_temp$ag_biomass_min))) * 100

  c(id = design_temp$id, bg = bg_mean, bg_rel = bg_rel, ag = ag_mean, ag_rel = ag_rel)
})

seafloor <- dplyr::left_join(x = seafloor, y = full_design_subset, by = "id")
