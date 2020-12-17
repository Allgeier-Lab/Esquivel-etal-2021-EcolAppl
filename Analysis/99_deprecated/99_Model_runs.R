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

#### Change some values ####

# # nutrients starting value
# starting_values$nutrients_pool <- 15

# # biomass values
# starting_values$bg_biomass <- 325.7
# starting_values$ag_biomass <- 55.7

# # detritus and nutrients
# starting_values$detritus_pool <- 0
# starting_values$nutrients_pool <- 150

# # biomass max
# parameters$bg_biomass_max <- 160
# parameters$ag_biomass_max <- 159.5

# # detritus ratios
# parameters$detritus_ratio <- 0.1
# parameters$detritus_decomposition <- 0.01

starting_values$pop_n <- 50

# set minutes per iteration
min_per_i <- 120

# check if all parameters are present
check_parameters(starting_values = starting_values, parameters = parameters)

#### Create input data ####

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# reef_matrix <- matrix(data = c(0, 0), ncol = 2, byrow = TRUE)
# reef_matrix <- NULL

# create seafloor
input_seafloor <- setup_seafloor(extent = c(50, 50), grain = c(1, 1),
                                 reefs = reef_matrix,
                                 starting_values = starting_values, random = 0.01)

# plot(input_seafloor[[c("ag_biomass", "bg_biomass", "nutrients_pool", "detritus_pool")]])

# create population
input_fishpop <- setup_fishpop(seafloor = input_seafloor,
                               starting_values = starting_values,
                               parameters = parameters, 
                               use_log = FALSE)

# create dataframe without fish
input_fishpop_null <- input_fishpop[-c(1:starting_values$pop_n), ]

#### Run full model ####

# run model for n years
n <- 3

max_i <- (60 * 24 * 365 * n) / min_per_i

# save every m days
m <- 5

save_each <- (24 / (min_per_i / 60)) * m

# run model

result_nofish <- run_simulation(seafloor = input_seafloor,
                                fishpop = input_fishpop_null,
                                parameters = parameters,
                                reef_attraction = FALSE,
                                max_i = max_i, min_per_i = min_per_i, 
                                save_each = save_each,
                                verbose = TRUE)

result_randmove <- run_simulation(seafloor = input_seafloor,
                                  fishpop = input_fishpop,
                                  parameters = parameters,
                                  reef_attraction = FALSE,
                                  max_i = max_i, min_per_i = min_per_i, 
                                  save_each = save_each,
                                  verbose = TRUE)

result_attrmove <- run_simulation(seafloor = input_seafloor,
                                  fishpop = input_fishpop,
                                  parameters = parameters,
                                  reef_attraction = TRUE,
                                  max_i = max_i, min_per_i = min_per_i, 
                                  save_each = save_each,
                                  verbose = TRUE)

#### Save results ####

overwrite <- FALSE

suppoRt::save_rds(object = result_nofish, 
                  filename = "Data/Modified/result_nofish.rds", 
                  overwrite = overwrite)

suppoRt::save_rds(object = result_randmove, 
                  filename = "Data/Modified/result_randmove.rds", 
                  overwrite = overwrite)

suppoRt::save_rds(object = result_attrmove, 
                  filename = "Data/Modified/result_attrmove.rds", 
                  overwrite = overwrite)
