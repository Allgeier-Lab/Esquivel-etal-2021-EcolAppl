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
# Run the model for one parameter setting and save more timesteps

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting-values.csv", sep = ";")

#### Change parameters ####

parameters$seagrass_thres <- -1/4 # -1/4, -1/2, -3/4

#### Set run time ####

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 50
max_i <- (60 * 24 * 365 * years) / min_per_i

# run seagrass once each day
days <- 1
seagrass_each <- (24 / (min_per_i / 60)) * days

# save each n days
days <- 25
save_each <- (24 / (min_per_i / 60)) * days

# check if combination of max_i and save_each are possible
max_i %% save_each

#### Setup seafloor and stuff #### 

# dimensions and grain of seafloor
dimensions <- c(100, 100)

grain <- c(1, 1)

# create reef
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                ncol = 2, byrow = TRUE)

# get stable values
stable_values <- arrR::get_stable_values(starting_values = starting_values,
                                         parameters = parameters)

# set stable values
starting_values$detritus_pool <- stable_values$detritus_pool

starting_values$nutrients_pool <- stable_values$nutrients_pool

starting_values$pop_n <- 6

# setup seafloor
input_seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain, reefs = reefs, 
                                       starting_values = starting_values)

# setup fish
input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                     starting_values = starting_values,
                                     parameters = parameters)

#### Run model  ####

# run model
result_rand <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = parameters, movement = "rand",
                                    max_i = max_i, min_per_i = min_per_i,
                                    seagrass_each = seagrass_each, save_each = save_each) 

# run model
result_attr <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = parameters, movement = "attr",
                                    max_i = max_i, min_per_i = min_per_i,
                                    seagrass_each = seagrass_each, save_each = save_each) 
    
#### Save results ####

model_runs_timesteps <- list(rand = result_rand, attr = result_attr)

filename <- (parameters$seagrass_thres * 100) %>% 
  paste0("model-runs-timesteps_", ., "_", parameters$seagrass_slope) %>% 
  stringr::str_replace(pattern = "\\.", replacement = "") %>% 
  paste0(".rds")

suppoRt::save_rds(object = model_runs_timesteps, filename = filename,
                  path = "02_Data/02_Modified/02_run_model/",
                  overwrite = FALSE)
