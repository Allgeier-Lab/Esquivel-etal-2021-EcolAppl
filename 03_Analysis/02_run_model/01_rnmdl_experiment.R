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

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting-values.csv", sep = ";")

#### Set arguments to run model ####

parameters$seagrass_thres <- -1/4 # -1/4, -1/2, -3/4

arrR::plot_allocation(parameters = parameters)

#### Set run time ####

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 50

max_i <- (60 * 24 * 365 * years) / min_per_i

# run seagrass once each day
days <- 1

seagrass_each <- (24 / (min_per_i / 60)) * days

# save only final step
save_each <- max_i

# check if combination of max_i and save_each are possible
max_i %% save_each

#### Setup seafloor and stuff #### 

# dimensions and grain of seafloor
dimensions <- c(100, 100)

grain <- c(1, 1)

# create reef
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                ncol = 2, byrow = TRUE)

#### Setup experiment #### 

# set repetitions
repetitions <- 25

# sequence nutrients pool sequence
starting_biomass <- c(1/4, 1/2, 3/4)

# sequence of fish population
pop_n <- c(1, 2, 4, 8, 16, 32)

# get all combinations
sim_experiment <- expand.grid(starting_biomass = starting_biomass,
                              pop_n = pop_n)

# repeat all combinations and combine to final data.frame
sim_experiment <- dplyr::bind_cols(starting_biomass = rep(sim_experiment$starting_biomass, 
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
                      dimensions = dimensions, grain = grain, reefs = reefs,
                      max_i = max_i, min_per_i = min_per_i, seagrass_each = seagrass_each,
                      save_each = save_each)

#### Run model  ####

# run model with random and attracted movement
result_total %<-% future.apply::future_lapply(1:nrow(sim_experiment), FUN = function(i) {
  
  result %<-% {
    
    # get pop_n value
    starting_values$pop_n <- sim_experiment[[i, "pop_n"]]
    
    # calc starting ag biomass
    starting_values$ag_biomass <- parameters$ag_biomass_min +
      (parameters$ag_biomass_max - parameters$ag_biomass_min) * 
      sim_experiment[[i, "starting_biomass"]]
    
    # calc starting bg biomass
    starting_values$bg_biomass <- parameters$bg_biomass_min +
      (parameters$bg_biomass_max - parameters$bg_biomass_min) * 
      sim_experiment[[i, "starting_biomass"]]
    
    # get stable values
    stable_values <- arrR::get_stable_values(starting_values = starting_values,
                                             parameters = parameters)
    
    # set stable values
    starting_values$detritus_pool <- stable_values$detritus_pool
    
    starting_values$nutrients_pool <- stable_values$nutrients_pool
    
    # setup seafloor
    input_seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain, reefs = reefs, 
                                           starting_values = starting_values, 
                                           verbose = FALSE)
    
    # setup fish
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters,
                                         verbose = FALSE)
    
    # run model
    result_rand <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                        parameters = parameters, movement = "rand",
                                        max_i = max_i, min_per_i = min_per_i,
                                        seagrass_each = seagrass_each,
                                        save_each = save_each, verbose = FALSE) 
    
    # run model
    result_attr <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                        parameters = parameters, movement = "attr",
                                        max_i = max_i, min_per_i = min_per_i,
                                        seagrass_each = seagrass_each,
                                        save_each = save_each, verbose = FALSE) 
    
    result_temp <- list(rand = result_rand, attr = result_attr)
    
    # create filename
    file_name <- paste0("/home/mhessel/results/mdlrn_", i, ".rds")
    # file_name <- paste0("~/Downloads/results/mdlrn_", i, ".rds")

    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)

    # only return string
    file_name
    
  }
}, future.globals = globals_model, future.seed = TRUE)

#### Save results ####

# Get results from HPC /home/mhessel/results/
present_id <- list.files(path = "~/Downloads/results",
                         full.names = TRUE, pattern = "^mdlrn_*") %>%
  stringr::str_sort(numeric = TRUE) %>%
  purrr::map_int(function(i) stringr::str_split(i, pattern = "_",
                                                simplify = TRUE) %>%
               magrittr::extract(2) %>%
               stringr::str_sub(end = -5) %>%
               as.integer())

sim_id <- 1:nrow(sim_experiment)

(missing_id <- which(!sim_id %in% present_id))

model_runs <- list.files(path = "~/Downloads/results",
                         full.names = TRUE, pattern = "^mdlrn_*") %>%
  stringr::str_sort(numeric = TRUE) %>%
  purrr::map(readr::read_rds)

filename <- (parameters$seagrass_thres * 100) %>% 
  paste0("model-runs_", ., "_", parameters$seagrass_slope) %>% 
  stringr::str_replace(pattern = "\\.", replacement = "") %>% 
  paste0(".rds")

suppoRt::save_rds(object = model_runs, filename = filename,
                  path = "02_Data/02_Modified/02_run_model/",
                  overwrite = FALSE)

suppoRt::save_rds(object = sim_experiment, filename = "sim_experiment.rds", 
                  path = "02_Data/02_Modified/02_run_model/", 
                  overwrite = FALSE)
