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

# Check if pattern of parameter combination are similar for different number of 
# fish population

source("Helper_functions/setup.R")

source("Helper_functions/calc_biomass_dist.R")

#### load data and parameters ####

# load default starting values and parameters
parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

# get parameters and id of filtered combinations
result_dist_fltrd <- readr::read_rds("Data/Modified/01_explore_para_range/explore_dist_fltrd.rds")

#### set default arguments ####

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
reef_attraction <- TRUE # TRUE

#### Use filtered parameter values ####
i <- 6

parameters$bg_thres <- result_dist_fltrd$bg_thres[i]

parameters$detritus_ratio <- result_dist_fltrd$detritus_ratio[i]

parameters$detritus_mineralization <- result_dist_fltrd$detritus_mineralization[i]

fishpop_values <- data.frame(id = 1:5, pop_n = c(5, 10, 25, 50, 100))

model_runs_attr <- purrr::map(1:nrow(fishpop_values), function(j) {
  
  cat("> Progress: starting_values$pop_n <-", fishpop_values$pop_n[j], "\n")
  
  # reset starting pop n value
  starting_values$pop_n <- fishpop_values$pop_n[j]
  
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
  arrR::run_simulation(seafloor = input_seafloor,
                       fishpop = input_fishpop,
                       parameters = parameters,
                       reef_attraction = reef_attraction,
                       max_i = max_i, min_per_i = min_per_i,
                       save_each = save_each,
                       verbose = FALSE)
  
})

model_runs_attr_sum <- purrr::map_dfr(model_runs_attr, calc_biomass_dist, .id = "id") %>% 
  dplyr::mutate(id = as.integer(id)) %>% 
  dplyr::left_join(fishpop_values, by = "id") %>% 
  tidyr::pivot_longer(-c(id, pop_n, reef_dist_clss), names_to = "biomass") %>% 
  dplyr::mutate(id = forcats::as_factor(id), pop_n = forcats::as_factor(pop_n))

ggplot(data = model_runs_attr_sum) +
  geom_line(aes(x = reef_dist_clss, y = value, col = pop_n, group = pop_n)) +
  facet_wrap(~biomass, scales = "free_y", ncol = 1, nrow = 2) +
  guides(col = guide_legend(nrow = 2)) +
  scale_color_viridis_d(option = "D") +
  scale_x_continuous(breaks = seq(from = 1, to = 35, by = 3)) +
  labs(x = "Classified distance to reef [m] ", y = "Biomass dry [g]") +
  theme_classic() +
  theme(legend.position = "bottom")

