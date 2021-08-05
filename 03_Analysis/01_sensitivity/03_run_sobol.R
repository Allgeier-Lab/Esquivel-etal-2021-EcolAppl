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
# Sobol sensitivity analysis with subset of parameters that showed sensitivity in
# 02_results_local_SA.R

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting-values.csv", sep = ";")

#### Create parameter sets using Latin hyper cube ####

# [1] "ag_biomass_max" "ag_gamma" "bg_biomass_max" "pop_a" "pop_b"          
# [6] "pop_linf" "pop_n_body" "resp_intercept" "resp_slope" "resp_temp_low" 
# [11] resp_temp_optm" "seagrass_slough"

# ---- #

# [1] ag_biomass_max: data biomass pooled
# [2] ag_gamma: Layman et al. 2016

# [3] bg_biomass_max: data biomass pooled

# [4] pop_a: Fishbase
# [5] pop_b: Fishbase
# [6] pop_linf: Fishbase
# [7] pop_n_body: No reference

# [8] resp_intercept: Bioenergetics model Jake
# [9] resp_slope: Bioenergetics model Jake
# [10] resp_temp_low: Bioenergetics model Jake
# [11] resp_temp_optm: Bioenergetics model Jake

# [12] seagrass_slough

# sample parameters #
n <- 250

set.seed(42)

param_set_1 <- tgp::lhs(n = n, rect = matrix(data = c(
  
  144.7575, 193.01, # ag_biomass_max; (max - 25%) - max
  0.012015, 0.016685, # ag_gamma; Layman et al. 2016
  
  699.7699, 933.0266, # bg_biomass_max; (max - 25%) - max

  0.003493019, 0.04783051, # pop_a; mean+-sd
  2.813759, 3.099794, # pop_b; mean+-sd
  32.684, 50.952, # pop_linf; mean+-sd
  0.026991, 0.032989, # pop_n_body; +-10%

  0.00972, 0.01188, # resp_intercept; 10% 
  -0.22, -0.18, # resp_slope; +-10%
  1.89, 2.31, # resp_temp_low; +- 10%
  32.4, 39.6, # resp_temp_optm; +-10%
  
  9e-05, 0.00011), # seagrass_slough; +-10%
  ncol = 2, byrow = TRUE))

param_set_2 <- tgp::lhs(n = n, rect = matrix(data = c(
  
  144.7575, 193.01, # ag_biomass_max; (max - 25%) - max
  0.012015, 0.016685, # ag_gamma; Layman et al. 2016
  
  699.7699, 933.0266, # bg_biomass_max; (max - 25%) - max
  
  0.003493019, 0.04783051, # pop_a; mean+-sd
  2.813759, 3.099794, # pop_b; mean+-sd
  32.684, 50.952, # pop_linf; mean+-sd
  0.026991, 0.032989, # pop_n_body; +-10%
  
  0.00972, 0.01188, # resp_intercept; 10% 
  -0.22, -0.18, # resp_slope; +-10%
  1.89, 2.31, # resp_temp_low; +- 10%
  32.4, 39.6, # resp_temp_optm; +-10%
  
  9e-05, 0.00011), # seagrass_slough; +-10%
  ncol = 2, byrow = TRUE))

# create an instance of the class sobol 
model_sobol2007 <- sensitivity::sobol2007(model = NULL, 
                                          X1 = data.frame(param_set_1), 
                                          X2 = data.frame(param_set_2), 
                                          nboot = 1000) 

# get parameter combinations from sobol model
param_sampled <- purrr::map(seq_len(nrow(model_sobol2007$X)), 
                            function(i) as.numeric(model_sobol2007$X[i, ]))

#### Setup future #### 

# login node -> cluster nodes -> core
login <- tweak(remote, workers = "greatlakes.arc-ts.umich.edu", user = "mhessel")

sbatch <- tweak(batchtools_slurm, template = "future_slurm.tmpl",
                resources = list(job_name = "sobol_sa",
                                 log_file = "sobol_sa.log",
                                 walltime = "48:00:00", # walltime <hh:mm:ss>
                                 mem_cpu  = "7G")) # memory per core in mb

plan(list(
  login,
  sbatch,
  sequential
))

#### Set default arguments to run model ####

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 50

max_i <- (60 * 24 * 365 * years) / min_per_i

# run seagrass once each day
days <- 1

seagrass_each <- (24 / (min_per_i / 60)) * days

# only save once, filtered for max_i anyways
save_each <- max_i

# extent and grain of seafloor
dimensions <- c(100, 100)

grain <- c(1, 1)

# create reef
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                ncol = 2, byrow = TRUE)

#### Run on HPC ####

# create list with global parameters
globals_sobol <- list(starting_values = starting_values, parameters = parameters, param_sampled = param_sampled,
                      dimensions = dimensions, grain = grain, reefs = reefs, 
                      seagrass_each = seagrass_each, max_i = max_i, min_per_i = min_per_i, 
                      save_each = save_each)

# run on hcp
results_sobol %<-% future.apply::future_lapply(seq_along(param_sampled), FUN = function(i) {
  
  result %<-% {
    
    parameters$ag_biomass_max <- param_sampled[[i]][1]
    parameters$ag_gamma <- param_sampled[[i]][2]
    
    parameters$bg_biomass_max <- param_sampled[[i]][3]
    
    parameters$pop_a <- param_sampled[[i]][4]
    parameters$pop_b <- param_sampled[[i]][5]
    parameters$pop_linf <- param_sampled[[i]][6]
    parameters$pop_n_body <- param_sampled[[i]][7]
    
    parameters$resp_intercept <- param_sampled[[i]][8]
    parameters$resp_slope <- param_sampled[[i]][9]
    parameters$resp_temp_low <- param_sampled[[i]][10]
    parameters$resp_temp_optm <- param_sampled[[i]][11]
    
    parameters$seagrass_slough <- param_sampled[[i]][12]
    
    # get stable values
    stable_values <- arrR::get_stable_values(starting_values = starting_values,
                                             parameters = parameters, verbose = FALSE)
    
    # set stable values
    starting_values$detritus_pool <- stable_values$detritus_pool
    
    starting_values$nutrients_pool <- stable_values$nutrients_pool
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain, reefs = reefs,
                                           starting_values = starting_values,
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor, 
                                         starting_values = starting_values,
                                         parameters = parameters, verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                        parameters = parameters, movement = "attr",
                                        max_i = max_i, min_per_i = min_per_i,
                                        seagrass_each = seagrass_each,
                                        save_each = save_each, verbose = FALSE) 
    
    # return only last  timestep
    result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    file_name <- paste0("/home/mhessel/results/sobol_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_sobol, future.seed = TRUE)

#### Save results ####

# Get results from HPC /home/mhessel/results
present_id <- list.files(path = "~/Downloads/results",
                         full.names = TRUE, pattern = "^sobol_*") %>%
  stringr::str_sort(numeric = TRUE) %>%
  purrr::map_int(function(i) stringr::str_split(i, pattern = "_",
                                                simplify = TRUE) %>%
               magrittr::extract(, 2) %>%
               stringr::str_sub(end = -5) %>%
               as.integer())

sim_id <- 1:length(param_sampled)

(missing_id <- which(!sim_id %in% present_id))

# import data model runs
model_runs_sobol <- list.files(path = "~/Downloads/results/",
                               pattern = "^sobol_*", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE) %>%
  purrr::map(function(i) readr::read_rds(i))

# save objects
suppoRt::save_rds(object = model_sobol2007, filename = "model_sobol2007.rds", 
                  path = "02_Data/02_Modified/01_sensitivity/", 
                  overwrite = FALSE)

suppoRt::save_rds(object = model_runs_sobol, filename = "model_runs_sobol.rds",
                  path = "02_Data/02_Modified/01_sensitivity/",
                  overwrite = FALSE)
