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
# Run the model w/o and with attractionpare results

#### Import libraries and data ####

# load packages #
source("Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "Data/Raw/starting_values.csv", sep = ";")

#### Set default arguments to run model ####

repetitions <- 5

# set minutes per iteration
min_per_i <- 120

# run model for n years
n <- 3

max_i <- (60 * 24 * 365 * n) / min_per_i

# save each m days
m <- 5

save_each <- (24 / (min_per_i / 60)) * m

max_i %% save_each

# extent and grain of seafloor
extent <- c(50, 50)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# add random noise to starting values
random <- 0

# use uniform distribution of starting fish population values
use_log <- FALSE

#### Run model ####

# no stochasticity for no fish
result_null <- purrr::map(1, function(i) {
  
  cat("> Progress: ", i , "/", 1, "\n")
  
  # create seafloor
  input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                         reefs = reef_matrix,
                                         starting_values = starting_values, 
                                         random = random, 
                                         verbose = FALSE)

  # run model
  arrR::run_simulation(seafloor = input_seafloor,
                       fishpop = NULL,
                       parameters = parameters,
                       reef_attraction = FALSE,
                       max_i = max_i, min_per_i = min_per_i,
                       save_each = save_each,
                       verbose = FALSE) 
  
})

result_rand <- purrr::map(1:repetitions, function(i) {
 
  cat("> Progress: ", i , "/", repetitions, "\n")
  
  # create seafloor
  input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                         reefs = reef_matrix,
                                         starting_values = starting_values, 
                                         random = random, 
                                         verbose = FALSE)
  
  # create population
  input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                       starting_values = starting_values,
                                       parameters = parameters,
                                       use_log = use_log, 
                                       verbose = FALSE)
  
  # run model
  arrR::run_simulation(seafloor = input_seafloor,
                       fishpop = input_fishpop,
                       parameters = parameters,
                       reef_attraction = FALSE,
                       max_i = max_i, min_per_i = min_per_i,
                       save_each = save_each,
                       verbose = FALSE) 
  
})

result_attr <- purrr::map(1:repetitions, function(i) {
  
  cat("> Progress: ", i , "/", repetitions, "\n")
  
  # create seafloor
  input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                         reefs = reef_matrix,
                                         starting_values = starting_values, 
                                         random = random, 
                                         verbose = FALSE)
  
  # create population
  input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                       starting_values = starting_values,
                                       parameters = parameters,
                                       use_log = use_log, 
                                       verbose = FALSE)
  
  # run model
  arrR::run_simulation(seafloor = input_seafloor,
                       fishpop = input_fishpop,
                       parameters = parameters,
                       reef_attraction = TRUE,
                       max_i = max_i, min_per_i = min_per_i,
                       save_each = save_each,
                       verbose = FALSE) 
  
})

#### Save results ####

overwrite <- FALSE

suppoRt::save_rds(object = result_null, filename = "result_null.rds", 
                  path = "Data/Modified/03_run_model/", overwrite = overwrite)

suppoRt::save_rds(object = result_rand, filename = "result_rand.rds", 
                  path = "Data/Modified/03_run_model/", overwrite = overwrite)

suppoRt::save_rds(object = result_attr, filename = "result_attr.rds", 
                  path = "Data/Modified/03_run_model/", overwrite = overwrite)
