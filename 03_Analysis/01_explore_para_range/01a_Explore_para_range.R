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

source("01_Helper_functions/setup.R")

#### load data and parameters ####

parameters <- arrR::read_parameters("02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("02_Data/01_Raw/starting_values.csv", sep = ";")

#### Preprocess and init data #### 

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 20

max_i <- (60 * 24 * 365 * years) / min_per_i

# save each n days
days <- 20

save_each <- (24 / (min_per_i / 60)) * days
# save_each <- max_i / 90

# check of possible
max_i %% save_each

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

#### Change some starting values #### 

# starting_values$pop_n <- 50

#### Setup parameter combinations ####

bg_thres <- c(1/2, 3/5, 2/3, 3/4, 4/5)

detritus_ratio <- c(0.0001, 0.001, 0.01, 0.05, 0.1)

# combine to full design
full_design <- tidyr::expand_grid(bg_thres, detritus_ratio)

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
                          reef_matrix = reef_matrix,
                          max_i = max_i, min_per_i = min_per_i, save_each = save_each, 
                          full_design = full_design)

results_explore %<-% future.apply::future_lapply(1:nrow(full_design), FUN = function(i) {
  
  result %<-% {
    
    # replace values
    parameters$bg_thres <- full_design$bg_thres[i]
    
    parameters$detritus_ratio <- full_design$detritus_ratio[i]
    
    parameters$detritus_mineralization <- full_design$detritus_ratio[i]
    
    # create seafloor
    input_seafloor <- arrR::setup_seafloor(extent = c(50, 50), grain = c(1, 1),
                                           reefs = reef_matrix,
                                           starting_values = starting_values,
                                           verbose = FALSE)
    
    # create population
    input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters,
                                         use_log = TRUE,
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters,
                                        reef_attraction = TRUE,
                                        max_i = max_i, min_per_i = min_per_i,
                                        burn_in = max_i * 0.35,
                                        save_each = save_each,
                                        verbose = FALSE)
    
    # # return only last  timestep
    # result_temp <- arrR::filter_mdlrn(result_temp, timestep = max_i)
    
    file_name <- paste0("/home/mhessel/results/explore_run_", i, ".rds")

    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)

    # only return string
    file_name
    
  }
}, future.globals = globals_nutrients, future.seed = 42L)

#### Save results ####

# Get data from HPC /home/mhessel/results/ on HPC

# save full_design for later results
suppoRt::save_rds(object = full_design, 
                  filename = "02_Data/02_Modified/01_explore_para_range/explore_design.rds", 
                  overwrite = FALSE)
