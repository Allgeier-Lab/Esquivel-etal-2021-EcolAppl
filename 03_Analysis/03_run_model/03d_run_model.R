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
# Plot results of interaction fishpop_n and nutrients

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

sim_experiment <- readr::read_rds("02_Data/02_Modified/03_run_model/sim_experiment.rds")

# # import all model runs for default and changed parameters
# result_rand <- list.files(path = "~/Downloads/results/",
#                           pattern = "^result_rand", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(readr::read_rds)
# 
# suppoRt::save_rds(object = result_rand, filename = "result_rand.rds", 
#                   path = "02_Data/02_Modified/03_run_model/", overwrite = FALSE)
# 
# # import all model runs for default and changed parameters
# result_attr <- list.files(path = "~/Downloads/results/",
#                           pattern = "^result_attr_", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(readr::read_rds)
# 
# suppoRt::save_rds(object = result_attr, filename = "result_attr.rds",
#                   path = "02_Data/02_Modified/03_run_model/", overwrite = FALSE)

result_rand <- readr::read_rds("02_Data/02_Modified/03_run_model/result_rand.rds")

result_attr <- readr::read_rds("02_Data/02_Modified/03_run_model/result_attr.rds")

#### Preprocess data #### 

sim_experiment <- dplyr::mutate(sim_experiment, id = 1:nrow(sim_experiment), 
                                .before = "nutrients_pool")

# get burn_in time (identical across all models)
burn_in <- result_rand[[1]]$burn_in

# combine both movement behaviors
seafloor_combined <- purrr::map_dfr(result_rand, "seafloor", 
                                    .id = "sim_id") %>% 
  dplyr::bind_rows(purrr::map_dfr(result_attr, "seafloor", 
                                  .id = "sim_id"), .id = "movement") %>% 
  dplyr::left_join(dplyr::bind_cols(sim_id = unique(.$sim_id), 
                                    pop_n = sim_experiment$pop_n, 
                                    nutrients = sim_experiment$nutrients_pool), 
                   by = "sim_id") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("1", "2"), 
                                  labels = c("Random movement", "Attracted movement")), 
                pop_n = factor(pop_n, ordered = TRUE)) %>% 
  tibble::as_tibble()

rm(result_rand, result_attr)

