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

# Run model for different parameter values of bg_thres, detritus_ratio and 
# detritus_mineralization on HPC cluster using future. Results are currently 
# saved on the HPC and need to be downloaded separately (dodgy future connection)

source("Helper_functions/setup.R")

#### load data and parameters ####

parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

#### Preprocess and init data #### 

# set minutes per iteration
min_per_i <- 120

# run model for n years
n <- 3

max_i <- (60 * 24 * 365 * n) / min_per_i

save_each <- max_i / 90 # (24 / (min_per_i / 60)) * m

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

# set attraction to reef argument
reef_attraction <- FALSE # TRUE

#### Change some starting values #### 

# # biomass values
# starting_values$bg_biomass <- 434.5 # 50% of range
# 
# starting_values$ag_biomass <- 82.3 # 50% of range
# 
# starting_values$pop_n <- 25 # 50% of range

#### Setup parameter combinations ####

bg_thres <- c(3/5, 2/3, 3/4, 4/5, 9/10)

detritus_ratio <- c(0.0001, 0.001, 0.01, 0.05, 0.1)

detritus_mineralization <- c(0.0001, 0.001, 0.01, 0.05, 0.1)

# combine to full design
full_design <- tidyr::expand_grid(bg_thres, detritus_ratio, detritus_mineralization)

# check if all parameters are present
arrR::check_parameters(starting_values = starting_values, parameters = parameters)

#### Setup future plan ####

# login node -> cluster nodes -> core
login <- tweak(remote, workers = "greatlakes.arc-ts.umich.edu", user = "mhessel")

sbatch <- tweak(batchtools_slurm, template = "future_slurm.tmpl",
                resources = list(job_name = "explore_para",
                                 log_file = "explore_para.log",
                                 walltime = "00:30:00", # walltime <hh:mm:ss>
                                 mem_cpu  = "7G")) # memory per core in mb

plan(list(
  login,
  sbatch,
  sequential
))

#### Run model with different parameter options #### 

globals_nutrients <- list(starting_values = starting_values, parameters = parameters, 
                          extent = extent, grain = grain,
                          reef_matrix = reef_matrix, random = random, 
                          use_log = use_log, reef_attraction = reef_attraction,
                          max_i = max_i, min_per_i = min_per_i, save_each = save_each, 
                          full_design = full_design)

results_explore %<-% future.apply::future_lapply(1:nrow(full_design), FUN = function(i) {
  
  result %<-% {
    
    # replace values
    parameters$bg_thres <- full_design$bg_thres[i]
    
    parameters$detritus_ratio <- full_design$detritus_ratio[i]
    
    parameters$detritus_mineralization <- full_design$detritus_mineralization[i]
    
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
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters,
                                        reef_attraction = reef_attraction,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each,
                                        verbose = FALSE)
    
    # # return only last  timestep
    # result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    # create name to save result on HPC
    end_name <- ifelse(test = reef_attraction, yes = "_attr", no = "_rand")

    file_name <- paste0("/home/mhessel/results/explore_", i, end_name, ".rds")

    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)

    # only return string
    file_name
    
  }
}, future.globals = globals_nutrients, future.seed = 42)

#### Save results ####

# Get data from /home/mhessel/results/ on HPC

# save full_design for later results
suppoRt::save_rds(object = full_design, 
                  filename = "Data/Modified/01_explore_para_range/explore_design.rds", 
                  overwrite = FALSE)
