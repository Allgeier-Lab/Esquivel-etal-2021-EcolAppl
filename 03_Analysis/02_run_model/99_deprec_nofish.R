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
# Run model without fish population to find burn-in time

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting-values.csv", sep = ";")

arrR::check_parameters(starting_values = starting_values, parameters = parameters)

#### Set arguments to run model ####

parameters$seagrass_thres <- 1/2 # -1/4, 1/4, 1/2, 3/4,

parameters$seagrass_slope <- 2 # 3 5

# set minutes per iteration
min_per_i <- 120

# # run seagrass once each day
# seagrass_each <- 12

# run model for n years
years <- 50

max_i <- (60 * 24 * 365 * years) / min_per_i

# save each m days
days <- 25

save_each <- (24 / (min_per_i / 60)) * days

# check if combination of max_i and save_each are possible
max_i %% save_each

# extent and grain of seafloor
extent <- c(100, 100)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# No repetitions needed because no stochasticity included w/o fish
starting_biomass <- c(1/4, 1/2, 3/4, 1)

#### Setup future plan ####

# login node -> cluster nodes -> core
login <- tweak(remote, workers = "greatlakes.arc-ts.umich.edu", user = "mhessel")

sbatch <- tweak(batchtools_slurm, template = "future_slurm.tmpl",
                resources = list(job_name = "nofish",
                                 log_file = "nofish.log",
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

# future::plan(future::multisession)

globals_model <- list(starting_biomass = starting_biomass, 
                      starting_values = starting_values, parameters = parameters,
                      extent = extent, grain = grain, reef_matrix = reef_matrix,
                      max_i = max_i, min_per_i = min_per_i, save_each = save_each)

#### Run model  ####

# run model
result_nofish <- future.apply::future_lapply(1:length(starting_biomass), function(i) {
  
  result %<-% {
    
    starting_values$ag_biomass <- parameters$ag_biomass_min + 
      (parameters$ag_biomass_max - parameters$ag_biomass_min) * starting_biomass[[i]]
    
    starting_values$bg_biomass <- parameters$bg_biomass_min + 
      (parameters$bg_biomass_max - parameters$bg_biomass_min) * starting_biomass[[i]]
    
    # get stable values
    stable_values <- arrR::get_stable_values(starting_values = starting_values,
                                             parameters = parameters)
    
    starting_values$nutrients_pool <- stable_values$nutrients_pool
    
    starting_values$detritus_pool <- stable_values$detritus_pool
    
    input <- arrR::setup_seafloor(extent = extent, grain = grain,
                                  reefs = reef_matrix, starting_values = starting_values,
                                  verbose = FALSE)
    
    result_temp <- arrR::run_simulation(seafloor = input, fishpop = NULL,
                                        parameters = parameters,
                                        max_i = max_i, min_per_i = min_per_i, 
                                        save_each = save_each, verbose = FALSE)
    
    # create filename
    file_name <- paste0("/home/mhessel/results/nofish_", i, ".rds")
    # file_name <- paste0("~/Downloads/results/mdlrn_", i, ".rds")
    
    # save result explicit in folder
    saveRDS(object = result_temp, file = file_name)
    
    # only return string
    file_name
    
  }
}, future.globals = globals_model, future.seed = TRUE)

#### Save results ####

# Get results from HPC /home/mhessel/results/
result_nofish <- list.files(path = "~/Downloads/results",
                         full.names = TRUE, pattern = "^nofish_*") %>%
  stringr::str_sort(numeric = TRUE) %>%
  purrr::map(readr::read_rds)

filename <- as.character(parameters$seagrass_thres) %>% 
  stringr::str_replace(pattern = "\\.", replacement = "") %>% 
  paste0("result-nofish_", ., "_", parameters$seagrass_slope, ".rds")

# save results
suppoRt::save_rds(object = result_nofish, filename = filename,
                  path = "02_Data/02_Modified/02_run_model/",
                  overwrite = FALSE)

# #### Analyse results ####
# 
# result_nofish <- readr::read_rds("02_Data/02_Modified/02_run_model/result-nofish_XX_2.rds")
# 
# # sequence nutrients pool sequence
# starting_biomass <- tibble::tibble(id = 1:4, starting = starting_biomass)
# 
# result_df <- purrr::map_dfr(result_nofish, function(x) {
#   
#   capacity_temp <- dplyr::filter(x$seafloor, timestep == max(timestep)) %>% 
#     dplyr::summarise(ag_biomass = mean(ag_biomass, na.rm = TRUE), 
#                      bg_biomass = mean(bg_biomass, na.rm = TRUE), 
#                      ag_capacity = (ag_biomass - parameters$ag_biomass_min) / 
#                        (parameters$ag_biomass_max - parameters$ag_biomass_min), 
#                      bg_capacity = (bg_biomass - parameters$bg_biomass_min) / 
#                        (parameters$bg_biomass_max - parameters$bg_biomass_min))}, .id = "id") %>% 
#   dplyr::mutate(id = as.numeric(id)) %>%
#   dplyr::left_join(y = starting_biomass, by = "id") %>% 
#   dplyr::mutate(ag_diff = ag_capacity - starting, 
#                 bg_diff = bg_capacity - starting)
# 
# purrr::map(result_nofish, plot, summarize = TRUE)
# 
# #### Compare thresholds #### 
# 
# # sequence nutrients pool sequence
# starting_biomass <- tibble::tibble(starting = 1:4, capacity = c(1/4, 1/2, 3/4, 1))
# 
# threshold <- tibble::tibble(threshold = 1:3, thres = c(1/4, 1/2, 3/4))
# 
# # load all data
# result_nofish_14 <- readr::read_rds("02_Data/02_Modified/02_run_model/result-nofish_14_2.rds")
# 
# result_nofish_12 <- readr::read_rds("02_Data/02_Modified/02_run_model/result-nofish_12_2.rds")
# 
# result_nofish_34 <- readr::read_rds("02_Data/02_Modified/02_run_model/result-nofish_34_2.rds")
# 
# result_nofish <- list(result_nofish_14, result_nofish_12, result_nofish_34)
# 
# rm(result_nofish_14, result_nofish_12, result_nofish_34)
# 
# result <- purrr::map_dfr(result_nofish, function(i) {
#   
#   purrr::map_dfr(i, function(j) {
#     
#     dplyr::select(j$seafloor, 
#                   timestep, ag_biomass, bg_biomass, 
#                   nutrients_pool, detritus_pool) %>% 
#       tidyr::pivot_longer(-timestep, names_to = "measure") %>% 
#       dplyr::group_by(timestep, measure) %>% 
#       dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")}, 
#     .id = "starting")}, 
#   .id = "threshold") %>% 
#   dplyr::mutate(starting = as.integer(starting), 
#                 threshold = as.integer(threshold)) %>% 
#   dplyr::left_join(starting_biomass, by = "starting") %>% 
#   dplyr::left_join(threshold, by = "threshold") %>% 
#   dplyr::mutate(capacity = factor(capacity, ordered = TRUE), 
#                 threshold = factor(threshold, ordered = TRUE))
# 
# ggplot(data = result) +
#   geom_line(aes(x = timestep, y = value, col = capacity)) + 
#   facet_wrap(. ~ measure + thres, scales = "free_y", ncol = 3) + 
#   theme_classic() + 
#   theme(legend.position = "bottom")
