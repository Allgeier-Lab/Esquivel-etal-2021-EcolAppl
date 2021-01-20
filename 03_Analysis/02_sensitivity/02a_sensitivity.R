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
# Local, one-at-a-time sensitivity analysis of all parameters and biomass and 
# fishpop dimensions as output

#### load data and parameters ####

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/change_parameters.R")

parameters_def <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting_values.csv", sep = ";")

#### Set default arguments to run SA ####

# number of model run repetitions
repetitions <- 25

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 20

max_i <- (60 * 24 * 365 * years) / min_per_i

# save only once; will be filtered for last timestep anyways
days <- 20

save_each <- max_i / 1 # (24 / (min_per_i / 60)) * days

burn_in <- max_i * 0.35

# extent and grain of seafloor
extent <- c(50, 50)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# use uniform distribution of starting fish population values
use_log <- TRUE

# set attraction to reef argument
reef_attraction <- TRUE

# create vector with names
parameters_names <- paste(rep(x = names(parameters_def), each = repetitions), 
                          1:repetitions, sep = "_")

default_names <- paste("default", 1:repetitions, sep = "_")

#### Setup future plan ####

# login node -> cluster nodes -> core
login <- tweak(remote, workers = "greatlakes.arc-ts.umich.edu", user = "mhessel")

sbatch <- tweak(batchtools_slurm, template = "future_slurm.tmpl",
                resources = list(job_name = "local_sa",
                                 log_file = "local_sa.log",
                                 walltime = "02:00:00", # walltime <hh:mm:ss>
                                 mem_cpu  = "7G")) # memory per core in mb

plan(list(
  login,
  sbatch,
  sequential
))

#### Run default parameters #### 

globals_def <- list(starting_values = starting_values, parameters_def = parameters_def, 
                    extent = extent, grain = grain,
                    reef_matrix = reef_matrix,
                    use_log = use_log, reef_attraction = reef_attraction,
                    max_i = max_i, min_per_i = min_per_i, save_each = save_each, 
                    burn_in = burn_in)

results_default %<-% future.apply::future_lapply(1:repetitions, FUN = function(i) {
  
  result %<-% {
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values,
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters_def,
                                         use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters_def,
                                        reef_attraction = reef_attraction,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each, burn_in = burn_in,
                                        verbose = FALSE)
    
    # return only last  timestep
    result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    file_name <- paste0("/home/mhessel/results/local_default_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_def, future.seed = 42)

#### Change parameters #### 

# change parameters by 5%
parameters_inc_5 <- change_parameters(parameters_def, change = 0.05) %>% 
  rep(each = repetitions) %>% 
  purrr::set_names(parameters_names)

parameters_dec_5 <- change_parameters(parameters_def, change = -0.05) %>% 
  rep(each = repetitions) %>% 
  purrr::set_names(parameters_names)

# change parameters by 10%
parameters_inc_10 <- change_parameters(parameters_def, change = 0.1) %>% 
  rep(each = repetitions) %>% 
  purrr::set_names(parameters_names)

parameters_dec_10 <- change_parameters(parameters_def, change = -0.1) %>% 
  rep(each = repetitions) %>% 
  purrr::set_names(parameters_names)

#### Run changed parameters ####

# increased by 5%
globals_inc_5 <- list(starting_values = starting_values, parameters_inc_5 = parameters_inc_5,
                      extent = extent, grain = grain,
                      reef_matrix = reef_matrix,
                      use_log = use_log, reef_attraction = reef_attraction,
                      max_i = max_i, min_per_i = min_per_i, save_each = save_each, 
                      burn_in = burn_in)

results_inc_5 %<-% future.apply::future_lapply(seq_along(parameters_inc_5), FUN = function(i) {
  
  result %<-% {
    
    parameters_temp <- parameters_inc_5[[i]]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values, 
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters_temp,
                                         use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters_temp,
                                        reef_attraction = reef_attraction,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each, burn_in = burn_in,
                                        verbose = FALSE)
    
    # return only last  timestep
    result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    file_name <- paste0("/home/mhessel/results/local_inc_5_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_inc_5, future.seed = 42)

# decreased by 5%
globals_dec_5 <- list(starting_values = starting_values, parameters_dec_5 = parameters_dec_5,
                      extent = extent, grain = grain,
                      reef_matrix = reef_matrix,
                      use_log = use_log, reef_attraction = reef_attraction,
                      max_i = max_i, min_per_i = min_per_i, save_each = save_each, 
                      burn_in = burn_in)

results_dec_5 %<-% future.apply::future_lapply(seq_along(parameters_dec_5), FUN = function(i) {
  
  result %<-% {
    
    parameters_temp <- parameters_dec_5[[i]]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values,
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters_temp,
                                         use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters_temp,
                                        reef_attraction = reef_attraction,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each, burn_in = burn_in,
                                        verbose = FALSE)
    
    # return only last  timestep
    result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    file_name <- paste0("/home/mhessel/results/local_dec_5_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_dec_5, future.seed = 42)

# increased by 10%
globals_inc_10 <- list(starting_values = starting_values, parameters_inc_10 = parameters_inc_10,
                       extent = extent, grain = grain,
                       reef_matrix = reef_matrix,
                       use_log = use_log, reef_attraction = reef_attraction,
                       max_i = max_i, min_per_i = min_per_i, save_each = save_each, 
                       burn_in = burn_in)


results_inc_10 %<-% future.apply::future_lapply(seq_along(parameters_inc_10), FUN = function(i) {
  
  result %<-% {
    
    parameters_temp <- parameters_inc_10[[i]]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values, 
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters_temp,
                                         use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters_temp,
                                        reef_attraction = reef_attraction,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each, burn_in = burn_in,
                                        verbose = FALSE)
    
    # return only last  timestep
    result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    file_name <- paste0("/home/mhessel/results/local_inc_10_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_inc_10, future.seed = 42)

# decreased by 10%
globals_dec_10 <- list(starting_values = starting_values, parameters_dec_10 = parameters_dec_10,
                       extent = extent, grain = grain,
                       reef_matrix = reef_matrix,
                       use_log = use_log, reef_attraction = reef_attraction,
                       max_i = max_i, min_per_i = min_per_i, save_each = save_each, 
                       burn_in = burn_in)

results_dec_10 %<-% future.apply::future_lapply(seq_along(parameters_dec_10), FUN = function(i) {
  
  result %<-% {
    
    parameters_temp <- parameters_dec_10[[i]]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values, 
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters_temp,
                                         use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters_temp,
                                        reef_attraction = reef_attraction,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each, burn_in = burn_in,
                                        verbose = FALSE)
    
    # return only last  timestep
    result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    file_name <- paste0("/home/mhessel/results/local_dec_10_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_dec_10, future.seed = 42)

# Get data from HPC /home/mhessel/results/
