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
# 02b_sensitivity

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting_values.csv", sep = ";")

#### Create parameter sets using Latin hyper cube ####

# para_thres
# ag_biomass_min" "ag_gamma" "bg_biomass_max" "bg_gamma" "bg_thres" "detritus_ratio"
# "pop_a" "pop_b" "pop_k" "pop_linf" "pop_mean_move" "pop_n_body" "pop_visibility" 
# "resp_intercept" "resp_slope" "resp_temp_low" "resp_temp_max" "resp_temp_optm"

# [1] ag_gamma: Layman et al. 2016
# [2] bg_biomass_max: Data biomass pooled
# [3] bg_gamma: No reference
# [4] bg_thres: No reference
# [5] detritus_ratio: No reference

# [6] pop_a: Fishbase
# [7] pop_b: Fishbase
# [8] pop_linf: Fishbase
# [9] pop_mean_move: No reference
# [10] pop_n_body: No reference
# [11] pop_visibility: No reference

# [12] resp_intercept: Bioenergetics model Jake
# [13] resp_slope: Bioenergetics model Jake
# [14] resp_temp_low: Bioenergetics model Jake
# [15] resp_temp_max: Bioenergetics model Jake
# [16] resp_temp_optm: Bioenergetics model Jake

# sample parameters #
n <- 250

set.seed(42)

param_set_1 <- tgp::lhs(n = n, rect = matrix(data = c(0.012015, 0.016685, # ag_gamma; Layman et al. 2016
                                                      638.0007, 933.0266, # bg_biomass_max; hi - max
                                                      0.007302,  0.008738, # bg_gamma; Layman et al. 2016
                                                      0.6, 0.7333334, # bg_thres; +- 10%
                                                      9e-05, 0.00011, # detritus_ratio; +- 10%
                                                      
                                                      0.003450665, 0.04899308, # pop_a; mean+-sd
                                                      2.807747, 3.102903, # pop_b; mean+-sd
                                                      32.684, 50.952, # pop_linf; mean+-sd
                                                      7.2, 8.8, # pop_mean_move; +- 10%
                                                      0.026991, 0.032989, # pop_n_body; +-10%
                                                      0.9, 1.1, # pop_visibility; +-10%
                                                      
                                                      0.00972, 0.01188, # resp_intercept; 10% 
                                                      -0.22, -0.18, # resp_slope; +-10%
                                                      1.89, 2.31, # resp_temp_low; +-10%
                                                      39.7, 45, # resp_temp_max; increase by 5°
                                                      32.4, 39.6), # resp_temp_optm; +-10%
                                                   ncol = 2, byrow = TRUE))

param_set_2 <- tgp::lhs(n = n, rect = matrix(data = c(0.012015, 0.016685, # ag_gamma; Layman et al. 2016
                                                      638.0007, 933.0266, # bg_biomass_max; hi - max
                                                      0.007302,  0.008738, # bg_gamma; Layman et al. 2016
                                                      0.6, 0.7333334, # bg_thres; +- 10%
                                                      9e-05, 0.00011, # detritus_ratio; +- 10%
                                                      
                                                      0.003450665, 0.04899308, # pop_a; mean+-sd
                                                      2.807747, 3.102903, # pop_b; mean+-sd
                                                      32.684, 50.952, # pop_linf; mean+-sd
                                                      7.2, 8.8, # pop_mean_move; +- 10%
                                                      0.026991, 0.032989, # pop_n_body; +-10%
                                                      0.9, 1.1, # pop_visibility; +-10%
                                                      
                                                      0.00972, 0.01188, # resp_intercept; 10% 
                                                      -0.22, -0.18, # resp_slope; +-10%
                                                      1.89, 2.31, # resp_temp_low; +-10%
                                                      39.7, 45, # resp_temp_max; increase by 5°
                                                      32.4, 39.6), # resp_temp_optm; +-10%
                                             ncol = 2, byrow = TRUE))

# create an instance of the class sobol 
model_sobol2007 <- sensitivity::sobol2007(model = NULL, 
                                          X1 = data.frame(param_set_1), 
                                          X2 = data.frame(param_set_2), 
                                          nboot = 10000) 

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
n <- 50

max_i <- (60 * 24 * 365 * n) / min_per_i

# only save once, filtered for max_i anyways
save_each <- max_i

# set burn_in time
burn_in <- 50000

# extent and grain of seafloor
extent <- c(100, 100)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# use uniform distribution of starting fish population values
use_log <- TRUE

# set attraction to reef argument
reef_attraction <- TRUE

#### Run on HPC ####

# create list with global parameters
globals_sobol <- list(starting_values = starting_values, parameters = parameters,
                      param_sampled = param_sampled,
                      extent = extent, grain = grain,
                      reef_matrix = reef_matrix,
                      use_log = use_log, reef_attraction = reef_attraction,
                      max_i = max_i, min_per_i = min_per_i, save_each = save_each)

# run on hcp
results_sobol %<-% future.apply::future_lapply(seq_along(param_sampled), FUN = function(i) {
  
  result %<-% {
    
    parameters$ag_gamma <- param_sampled[[i]][1]
    parameters$bg_biomass_max <- param_sampled[[i]][2]
    parameters$bg_gamma <- param_sampled[[i]][3]
    parameters$bg_thres <- param_sampled[[i]][4]
    parameters$detritus_ratio <- param_sampled[[i]][5]
    
    parameters$pop_a_grunt <- param_sampled[[i]][6]
    parameters$pop_b_grunt <- param_sampled[[i]][7]
    parameters$pop_linf_grunt <- param_sampled[[i]][8]
    parameters$pop_mean_move <- param_sampled[[i]][9]
    parameters$pop_n_body <- param_sampled[[i]][10]
    parameters$pop_visibility <- param_sampled[[i]][11]
    
    parameters$resp_intercept <- param_sampled[[i]][12]
    parameters$resp_slope <- param_sampled[[i]][13]
    parameters$resp_temp_low <- param_sampled[[i]][14]
    parameters$resp_temp_max <- param_sampled[[i]][15]
    parameters$resp_temp_optm <- param_sampled[[i]][16]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values, 
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters,
                                         use_log = use_log, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters,
                                        reef_attraction = reef_attraction,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each,
                                        verbose = FALSE)
    
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

overwrite <- FALSE

# present_id <- list.files(path = "~/Downloads/results",
#                          full.names = TRUE, pattern = "^sobol_*") %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map_int(function(i) stringr::str_split(i, pattern = "_",
#                                                 simplify = TRUE) %>%
#                magrittr::extract(, 2) %>%
#                stringr::str_sub(end = -5) %>%
#                as.integer())
# 
# sim_id <- 1:length(param_sampled)
# 
# (missing_id <- which(!sim_id %in% present_id))

# # import data model runs
# model_runs_sobol <- list.files(path = "~/Downloads/results/",
#                                pattern = "^sobol_*", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(function(i) readr::read_rds(i))
# 
# # save into one object
# suppoRt::save_rds(object = model_runs_sobol, filename = "model_runs_sobol.rds",
#                   path = "02_Data/02_Modified/02_sensitivity/",
#                   overwrite = overwrite)

suppoRt::save_rds(object = model_sobol2007, filename = "model_sobol2007.rds", 
                  path = "02_Data/02_Modified/02_sensitivity/", overwrite = overwrite)
