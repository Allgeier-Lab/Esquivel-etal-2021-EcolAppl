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
# Plot biomass over time with filtered parameters that produced realistic 
# biomass over distance pattern

source("Helper_functions/setup.R")

source("Helper_functions/classify_result.R")

source("Helper_functions/summarize_seafloor.R")

source("Helper_functions/summarize_fishpop.R")

#### load data and parameters ####

# # load default starting values and parameters
# parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")
# 
# starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

# get parameters and id of filtered combinations
result_dist_fltrd <- readr::read_rds("Data/Modified/01_explore_para_range/explore_dist_fltrd.rds")

# # import data for random movement of last timestep
# model_runs_rand <- list.files(path = "Data/Modified/01_explore_para_range/", 
#                               pattern = "rand\\.rds$", full.names = TRUE) %>% 
#   stringr::str_sort(numeric = TRUE) %>% 
#   purrr::map(function(i) arrR::filter_mdlrn(readr::read_rds(i)))

model_runs_attr <- readr::read_rds(file = "Data/Modified/01_explore_para_range/model_runs_attr.rds")

# model_runs_attr <- list.files(path = "Data/Modified/01_explore_para_range/", 
#                               pattern = "attr\\.rds$", full.names = TRUE) %>% 
#   stringr::str_sort(numeric = TRUE) %>% 
#   magrittr::extract(result_dist_fltrd$id) %>% 
#   purrr::map(function(i) readr::read_rds(i)) %>% 
#   purrr::set_names(result_dist_fltrd$id)

# parameter combination 57 reduces ag a lot and only small effect in bg
result_dist_fltrd <- dplyr::filter(result_dist_fltrd, id != 57)

#### Calculate summarized seafloor values for each timestep ####

seafloor_sum <- purrr::map_dfr(model_runs_attr, summarize_seafloor, .id = "id") %>% 
  dplyr::mutate(id = as.integer(id))

seafloor_sum_lng <- tidyr::pivot_longer(seafloor_sum, -c(timestep, id)) %>% 
  dplyr::mutate(name = factor(name, levels = c("ag_biomass", "bg_biomass", 
                                               "nutrients_pool", "detritus_pool"), 
                              labels = c("Dry weight ag biomass [g/cell]", 
                                         "Dry weight bg biomass [g/cell]", 
                                         "Nutrients pool [g/cell]", "Detritus nutrients pool [g/cell]")), 
                id = factor(id))

gg_seafloor_sum <- ggplot(data = seafloor_sum_lng) + 
  geom_line(aes(x = timestep, y = value, col = id)) +
  facet_wrap(~name, nrow = 2, ncol = 2, scales = "free_y") + 
  scale_color_viridis_d(name = "Parameter\ncombination", option = "B") +
  labs(x = "Timestep in ticks", y = "Mean value seafloor cells") +
  theme_classic() + 
  theme(legend.position = "bottom")

#### Calculate summarized fish population values for each timestep ####
fishpop_sum <- purrr::map_dfr(model_runs_attr, summarize_fishpop, .id = "id") %>% 
  dplyr::mutate(id = as.integer(id))

fishpop_sum_lng <- tidyr::pivot_longer(fishpop_sum, -c(timestep, id)) %>% 
  dplyr::mutate(name = factor(name, levels = c("length", "weight", 
                                               "died_consumption", "died_background"), 
                              labels = c("Body length [cm]", 
                                         "Body weigth [g]", 
                                         "Mortality consumption [#]", 
                                         "Mortality background [#]")), 
                id = factor(id))

gg_fish_pop_sum <- ggplot(data = fishpop_sum_lng) + 
  geom_line(aes(x = timestep, y = value, col = id)) +
  facet_wrap(~name, nrow = 2, ncol = 2, scales = "free_y") + 
  scale_color_viridis_d(name = "Parameter\ncombination", option = "B") +
  labs(x = "Timestep in ticks", y = "Mean value seafloor cells") +
  theme_classic() + 
  theme(legend.position = "bottom")

#### Save results ####

overwrite <- FALSE

suppoRt::save_rds(object = result_dist_fltrd, filename = "explore_dist_fltrd.rds", 
                  path = "Data/Modified/01_explore_para_range/", overwrite = TRUE)

suppoRt::save_ggplot(plot = gg_seafloor_sum,
                     filename = "Figures/gg_explore_seafloor_fltrd_sum.png",
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwriteT)

suppoRt::save_ggplot(plot = gg_fish_pop_sum,
                     filename = "Figures/gg_explore_fishpop_fltrd_sum.png",
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)
