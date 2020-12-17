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

# check if all parameters are present
arrR::check_parameters(starting_values = starting_values, parameters = parameters)

#### Preprocess and init data #### 

overwrite <- FALSE

# set minutes per iteration
min_per_i <- 120

# run model for n years
n <- 3

max_i <- (60 * 24 * 365 * n) / min_per_i

save_each <- save_each <- max_i / 90 # (24 / (min_per_i / 60)) * m

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
reef_attraction <- TRUE

# create vector with parameter possibilities
pop_n <- c(5, 10, 50, 100, 250)

nutrients_pool <- c(0.1, 0.25, 0.5, 0.75, 1.5)

bg_biomass_max <- parameters$bg_biomass_max * c(1/5, 2/5, 3/5, 4/5, 5/5)

detritus_ratio <- c(0.001, 0.01, 0.05, 0.1, 0.25)

detritus_mineralization <- c(0.001, 0.01, 0.05, 0.1, 0.25)

# combine to full design
full_design <- tidyr::expand_grid(pop_n, nutrients_pool, bg_biomass_max, 
                                  detritus_ratio, detritus_mineralization)

#### Setup future plan ####

# login node -> cluster nodes -> core
login <- tweak(remote, workers = "greatlakes.arc-ts.umich.edu", user = "mhessel")

sbatch <- tweak(batchtools_slurm, template = "future_slurm.tmpl",
                resources = list(job_name = "run_arrR",
                                 log_file = "run_arrR.log",
                                 walltime = "06:00:00", # walltime <hh:mm:ss>
                                 n_cpu = 1, # number of cores per job
                                 memory = 3072)) # memory per core in mb

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

result_full_design %<-% future.apply::future_lapply(1:nrow(full_design), FUN = function(i) {
  
  result %<-% {
    
    # replace values
    starting_values$pop_n <- full_design$pop_n[i]
    
    starting_values$nutrients_pool <- full_design$nutrients_pool[i]
    
    parameters$bg_biomass_max <- full_design$bg_biomass_max[i]
    
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
    
    result_temp <- arrR::filter_mdlrn(result_temp, timestep = c(0, max_i))
    
    end_name <- ifelse(test = reef_attraction, yes = "_attr", no = "_rand")
    
    file_name <- paste0("/home/mhessel/results_arrR/results_full_design/future_", i, end_name, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_nutrients, future.seed = 42)

#### Save results ####

# Get data from /home/mhessel/results_arrR/results_full_design/

# save full_design for later results
suppoRt::save_rds(object = full_design, 
                  filename = "Data/Modified/results_full_design/full_design.rds", 
                  overwrite = overwrite)
