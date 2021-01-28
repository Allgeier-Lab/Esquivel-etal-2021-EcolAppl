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
# Run the model for different fish populations w/o and with attraction

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

# # change bg_thres parameter
# parameters$bg_thres <- 2/3

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting_values.csv", sep = ";")

#### Set arguments to run model ####

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 50

max_i <- (60 * 24 * 365 * years) / min_per_i

# save each m days
days <- 125

save_each <- max_i / 100 # (24 / (min_per_i / 60)) * days

# check if combination of max_i and save_each are possible
max_i %% save_each

# set burn_in based on 03a_run_model
burn_in <- 50000

# extent and grain of seafloor
extent <- c(50, 50)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# use starting log distribution of size
use_log <- TRUE

# print progress
verbose <- FALSE

#### Setup seafloor and sequence of fish population #### 

# set repetitions
repetitions <- 10

# sequence nutrients pool sequence
nutrients_pool <- seq(from = 0.25, to = 1.5, by = 0.25)

# sequence of fish population
pop_n <- seq(from = 5, to = 105, by = 20)

# get all combinations
sim_experiment <- expand.grid(nutrients_pool = nutrients_pool,
                              pop_n = pop_n)

# repeat all combinations and combine to final data.frame
sim_experiment <- dplyr::bind_cols(nutrients_pool = rep(sim_experiment$nutrients_pool, 
                                                        each = repetitions), 
                                   pop_n = rep(sim_experiment$pop_n, 
                                               each = repetitions))

#### Setup future plan ####

# login node -> cluster nodes -> core
login <- tweak(remote, workers = "greatlakes.arc-ts.umich.edu", user = "mhessel")

sbatch <- tweak(batchtools_slurm, template = "future_slurm.tmpl",
                resources = list(job_name = "run_model",
                                 log_file = "run_model.log",
                                 walltime = "06:00:00", # walltime <hh:mm:ss>
                                 mem_cpu  = "7G")) # memory per core in mb

plan(list(
  login,
  sbatch,
  sequential
))

# plan(list(
#   sequential,
#   sequential,
#   sequential
# ))

globals_model <- list(sim_experiment = sim_experiment, 
                      starting_values = starting_values, parameters = parameters,
                      extent = extent, grain = grain,
                      reef_matrix = reef_matrix, use_log = use_log,
                      max_i = max_i, burn_in = burn_in, min_per_i = min_per_i, 
                      save_each = save_each)

#### Run model  ####

# run model with random movement
result_rand %<-% future.apply::future_lapply(1:nrow(sim_experiment), FUN = function(i) {
  
  result %<-% {
    
    # change values
    starting_values$nutrients_pool <- sim_experiment[[i, "nutrients_pool"]]
    
    starting_values$pop_n <- sim_experiment[[i, "pop_n"]]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values,
                                           verbose = FALSE)
    
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor, 
                                         starting_values = starting_values, 
                                         parameters = parameters, use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters,
                                        reef_attraction = FALSE,
                                        max_i = max_i, burn_in = burn_in, 
                                        min_per_i = min_per_i,
                                        save_each = save_each,
                                        verbose = FALSE) 
    
    # create filename
    file_name <- paste0("/home/mhessel/results/result_rand_", i, ".rds")
    # file_name <- paste0("~/Downloads/results/result_rand_", i, ".rds")

    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)

    # only return string
    file_name
    
  }
}, future.globals = globals_model, future.seed = 42L)

# run model with attracted movement
result_attr %<-% future.apply::future_lapply(1:nrow(sim_experiment), FUN = function(i) {
  
  result %<-% {
    
    # change values
    starting_values$nutrients_pool <- sim_experiment[[i, "nutrients_pool"]]
    
    starting_values$pop_n <- sim_experiment[[i, "pop_n"]]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values,
                                           verbose = FALSE)
    
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor, 
                                         starting_values = starting_values, 
                                         parameters = parameters, use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters,
                                        reef_attraction = TRUE,
                                        max_i = max_i, burn_in = burn_in, 
                                        min_per_i = min_per_i,
                                        save_each = save_each,
                                        verbose = FALSE) 
    
    # create filename
    file_name <- paste0("/home/mhessel/results/result_attr_", i, ".rds")
    # file_name <- paste0("~/Downloads/results/result_attr_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_model, future.seed = 42L)

#### Save results ####

# Get results from HPC /home/mhessel/results/

# result_rand <- list.files(path = "~/Downloads/results/",
#                           pattern = "^result_rand", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(readr::read_rds)
# 
# suppoRt::save_rds(object = result_rand, filename = "result_rand.rds",
#                   path = "02_Data/02_Modified/03_run_model/", overwrite = FALSE)
# 
# result_attr <- list.files(path = "~/Downloads/results/",
#                           pattern = "^result_attr_", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(readr::read_rds)

suppoRt::save_rds(object = result_attr, filename = "result_attr.rds",
                  path = "02_Data/02_Modified/03_run_model/", overwrite = FALSE)

suppoRt::save_rds(object = sim_experiment, filename = "sim_experiment.rds", 
                  path = "02_Data/02_Modified/03_run_model/", 
                  overwrite = FALSE)
