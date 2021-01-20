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
# Run the model w/o and with attractionpare results

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/Raw/starting_values.csv", sep = ";")

# starting_values$bg_biomass <- parameters$bg_biomass_max
# starting_values$ag_biomass <- parameters$ag_biomass_max
# starting_values$nutrients_pool <- 5

# parameters$bg_thres <- 0.25
# parameters$detritus_ratio <- 0
# parameters$detritus_mineralization <- 0.001

#### Set default arguments to run model ####

repetitions <- 50

# set minutes per iteration
min_per_i <- 120

# run model for n years
n <- 3

max_i <- (60 * 24 * 365 * n) / min_per_i

# save each m days
m <- 5

save_each <- (24 / (min_per_i / 60)) * m

max_i %% save_each

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

#### Setup future ####

# login node -> cluster nodes -> core
login <- tweak(remote, workers = "greatlakes.arc-ts.umich.edu", user = "mhessel")

sbatch <- tweak(batchtools_slurm, template = "future_slurm.tmpl",
                resources = list(job_name = "run_model",
                                 log_file = "run_model.log",
                                 walltime = "00:30:00", # walltime <hh:mm:ss>
                                 mem_cpu  = "7G")) # memory per core in mb

plan(list(
  login,
  sbatch,
  sequential
))

globals_model <- list(starting_values = starting_values, parameters = parameters,
                      extent = extent, grain = grain,
                      reef_matrix = reef_matrix, random = random,
                      use_log = use_log,
                      max_i = max_i, min_per_i = min_per_i, save_each = save_each)

#### Run model ####

# no stochasticity for no fish
result_null <- purrr::map(1, function(i) {
  
  # create seafloor
  input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                         reefs = reef_matrix,
                                         starting_values = starting_values, 
                                         random = random, 
                                         verbose = FALSE)

  # run model
  arrR::run_simulation(seafloor = input_seafloor,
                       fishpop = NULL,
                       parameters = parameters,
                       reef_attraction = FALSE,
                       max_i = max_i, min_per_i = min_per_i,
                       save_each = save_each,
                       verbose = FALSE) 
  
})

result_rand %<-% future.apply::future_lapply(1:repetitions, FUN = function(i) {
  
  result %<-% {
 
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
                                         use_log = TRUE, 
                                         verbose = FALSE)
    
    # run model
    result_temp <- arrR::run_simulation(seafloor = input_seafloor,
                                        fishpop = input_fishpop,
                                        parameters = parameters,
                                        reef_attraction = FALSE,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each,
                                        verbose = FALSE) 
    
    file_name <- paste0("/home/mhessel/results/result_rand_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
  
  }
}, future.globals = globals_model, future.seed = 42)

result_attr %<-% future.apply::future_lapply(1:repetitions, function(i) {
  
  result %<-% { 
  
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
                                        reef_attraction = TRUE,
                                        max_i = max_i, min_per_i = min_per_i,
                                        save_each = save_each,
                                        verbose = FALSE) 
    
    file_name <- paste0("/home/mhessel/results/result_attr_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
  
  }
}, future.globals = globals_model, future.seed = 42)

#### Save results ####

# Get results from HPC /home/mhessel/results/

suppoRt::save_rds(object = result_null, filename = "result_null.rds", 
                  path = "Data/Modified/03_run_model/", overwrite = T)

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
# Increase number of fish

#### Import libraries and data ####

# load packages #
source("Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "Data/Raw/starting_values.csv", sep = ";")

#### Set default arguments to run model ####

# set minutes per iteration
min_per_i <- 120

# run model for n years
n <- 1

max_i <- (60 * 24 * 365 * n) / min_per_i

# save each m days
m <- 5

save_each <- (24 / (min_per_i / 60)) * m

max_i %% save_each

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

input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                       reefs = reef_matrix,
                                       starting_values = starting_values,
                                       random = random,
                                       verbose = FALSE)

number_fish <- c(seq(from = 0, to = 10), 25, 50)

result <- purrr::map(number_fish, function(i) {

  cat("> Progress: ", i, "/", length(number_fish), "\n")

  starting_values$pop_n <- i

  input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                       starting_values = starting_values,
                                       parameters = parameters,
                                       use_log = TRUE,
                                       verbose = FALSE)

  # run model
  arrR::run_simulation(seafloor = input_seafloor,
                       fishpop = input_fishpop,
                       parameters = parameters,
                       reef_attraction = FALSE,
                       max_i = max_i, min_per_i = min_per_i,
                       save_each = save_each,
                       verbose = FALSE)
})

result_summarized <- purrr::map_dfr(result, function(i) {

  summarized_temp <- arrR::summarize_mdlrn(i, summary = "mean")

  summarized_temp$seafloor

}, .id = "id")

result_summarized_lng <- dplyr::select(result_summarized, -detritus_dead) %>%
  tidyr::pivot_longer(-c(id, timestep, summary)) %>%
  dplyr::mutate(id = factor(id, levels = 1:length(number_fish),
                            labels = number_fish))

ggplot(data = result_summarized_lng) +
  geom_line(aes(x = timestep, y = value, col = id)) +
  facet_wrap(~name, scales = "free_y") +
  scale_color_viridis_d(name = "Number of fish") +
  theme_classic() +
  theme(legend.position = "bottom")
