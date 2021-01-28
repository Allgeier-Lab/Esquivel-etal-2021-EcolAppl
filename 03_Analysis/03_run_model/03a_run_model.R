##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

#-------------------#
# Purpose of Script #
#-------------------#
# Run model without fish population to find burn-in time

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting_values.csv", sep = ";")

#### Set arguments to run model ####

# repetitions <- 50

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 50

max_i <- (60 * 24 * 365 * years) / min_per_i

# save each m days
days <- 125

save_each <- (24 / (min_per_i / 60)) * days

# check if combination of max_i and save_each are possible
max_i %% save_each

# extent and grain of seafloor
extent <- c(50, 50)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# print progress
verbose <- TRUE

#### Setup seafloor with sequence of nutrients ### 

# No repetitions needed because no stochasticity included w/o fish
nutrients_pool <- seq(from = 0.25, to = 1.5, by = 0.25)

input_seafloor_list <- purrr::map(nutrients_pool, function(i){
  
  starting_values$nutrients_pool <- i
  
  arrR::setup_seafloor(extent = extent, grain = grain,
                       reefs = reef_matrix,
                       starting_values = starting_values,
                       verbose = verbose)
  })

#### Run model  ####

future::plan(future::multisession)

# run model
result_null <- future.apply::future_lapply(input_seafloor_list, function(i) {
  
  arrR::run_simulation(seafloor = i,
                       fishpop = NULL,
                       parameters = parameters,
                       reef_attraction = FALSE,
                       max_i = max_i, min_per_i = min_per_i,
                       save_each = save_each,
                       verbose = verbose) 
  }, future.seed = 42L)

#### Plot results ####

purrr::map(result_null, plot, summarize = TRUE, burn_in = FALSE)

# Burn_in = 50,000 should okay

#### Save results ####

suppoRt::save_rds(object = result_null, filename = "result_null.rds", 
                  path = "02_Data/02_Modified/03_run_model/", 
                  overwrite = FALSE)
