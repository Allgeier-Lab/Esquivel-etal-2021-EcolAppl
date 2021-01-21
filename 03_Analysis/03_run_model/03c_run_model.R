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
# Plot results if increasing fish population

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

# get all model runs with stable nutrients pool and incresing fish_pop
filter_nutr <- dplyr::filter(sim_experiment, nutrients_pool == 0.75) %>% 
  dplyr::pull(id)

# combine both movement behaviors
seafloor_combined <- purrr::map_dfr(result_rand[filter_nutr], "seafloor", 
                                    .id = "fish_pop") %>% 
  dplyr::bind_rows(purrr::map_dfr(result_attr[filter_nutr], "seafloor", 
                                  .id = "fish_pop"), .id = "movement") %>% 
  dplyr::left_join(dplyr::bind_cols(fish_pop = unique(.$fish_pop),
                                    pop_n = sim_experiment$pop_n[filter_nutr]), 
                   by = "fish_pop") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("1", "2"), 
                                  labels = c("Random movement", "Attracted movement")), 
                pop_n = factor(pop_n, ordered = TRUE)) %>% 
  tibble::as_tibble()

rm(result_rand, result_attr)

#### Cumulative biomass production over iteration ####

production_total <- dplyr::group_by(seafloor_combined, timestep, movement, pop_n) %>% 
  dplyr::summarise(ag_production = sum(ag_production),
                   ag_slough = sum(ag_slough), 
                   bg_production = sum(bg_production), 
                   bg_slough = sum(bg_slough)) %>%  
  tidyr::pivot_longer(-c(timestep, movement, pop_n)) %>% 
  dplyr::mutate(name = factor(name, levels = c("ag_production",  "ag_slough", 
                                               "bg_production",  "bg_slough"), 
                              labels = c("Aboveground production", "Aboveground slough",
                                         "Belowground production", "Belowground slough")))

# production_end <- dplyr::group_by(production_total, move, name) %>% 
#   dplyr::filter(timestep == max_i) %>% 
#   dplyr::mutate(value = round(value))

gg_production_total <- ggplot(data = production_total) + 
  geom_line(aes(x = timestep, y = value, col = pop_n, linetype = movement)) + 
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  # geom_label(data = production_end, aes(x = max_i, y = value, label = value, col = move),
  #            hjust = 1.25, show.legend = FALSE) +
  facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
  scale_color_viridis_d(name = "Fish population size") +
  scale_linetype_manual(name = "Movement behaviour", values = c(1, 2)) +
  labs(x = "Iteration", y = "Total cumulative biomass (dry) [g]") + 
  theme_classic()

#### Mean biomass production over iteration ####

production_mean <- dplyr::group_by(seafloor_combined, x, y, movement, pop_n) %>% 
  dplyr::mutate(ag_production = ag_production - dplyr::lag(ag_production),
                ag_slough = ag_slough - dplyr::lag(ag_slough),
                bg_production = bg_production - dplyr::lag(bg_production),
                bg_slough = bg_slough - dplyr::lag(bg_slough)) %>% 
  dplyr::group_by(movement, timestep, pop_n) %>% 
  dplyr::summarise(ag_production = mean(ag_production), 
                   ag_slough = mean(ag_slough), 
                   bg_production = mean(bg_production), 
                   bg_slough = mean(bg_slough)) %>% 
  tidyr::pivot_longer(-c(timestep, movement, pop_n)) %>%
  dplyr::mutate(name = factor(name, levels = c("ag_production", "ag_slough",
                                               "bg_production", "bg_slough"), 
                              labels = c("Aboveground production", "Aboveground slough", 
                                         "Belowground production", "Belowground slough"))) %>%
  tidyr::replace_na(replace = list(value = 0))

gg_production_mean <- ggplot(data = production_mean) + 
  geom_line(aes(x = timestep, y = value, col = pop_n, linetype = movement)) + 
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  # geom_label(data = total_itr_end, aes(x = timestep, y = value, label = value, col = move),
  #            hjust = 1.5, show.legend = FALSE) +
  facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
  scale_color_viridis_d(name = "Fish population size") +
  scale_linetype_manual(name = "Movement behaviour", values = c(1, 2)) +
  labs(x = "Timestep", y = " Mean biomass (dry) [g/cell]") + 
  theme_classic()

#### Total biomass production at distance #### 

production_dist <- dplyr::filter(seafloor_combined, reef != 1) %>% 
  dplyr::group_by(reef_dist, movement, pop_n) %>% 
  dplyr::summarise(ag_production = mean(ag_production),
                   ag_slough = mean(ag_slough),
                   bg_production = mean(bg_production),
                   bg_slough = mean(bg_slough)) %>%  
  tidyr::pivot_longer(-c(reef_dist, movement, pop_n)) %>% 
  dplyr::mutate(name = factor(name, levels = c("ag_production", "ag_slough", 
                                               "bg_production", "bg_slough"), 
                              labels = c("Aboveground production", "Aboveground slough",
                                         "Belowground production", "Belowground slough")))

# production_dist_end <- dplyr::group_by(production_dist, move, name) %>% 
#   dplyr::filter(reef_dist == min(reef_dist)) %>% 
#   dplyr::mutate(value = round(value))

gg_production_dist <- ggplot(data = production_dist) + 
  geom_line(aes(x = reef_dist, y = value, col = pop_n, linetype = movement)) + 
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  # geom_label(data = production_dist_end, aes(x = reef_dist, y = value, label = value, col = move),
  #            hjust = -0.5, show.legend = FALSE) +
  facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
  scale_color_viridis_d(name = "Fish population size") +
  scale_linetype_manual(name = "Movement behaviour", values = c(1, 2)) +
  labs(x = "Distance to reef [m]", y = "Mean biomass (dry) [g/cell]") + 
  theme_classic()


#### Save ggplots ####

overwrite <- FALSE

suppoRt::save_ggplot(plot = gg_production_total, filename = "gg_production_total.png", 
                     path = "04_Figures/03_simulation_experiment/", 
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_mean, filename = "gg_production_mean.png", 
                     path = "04_Figures/03_simulation_experiment/", 
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_dist, filename = "gg_production_dist.png", 
                     path = "04_Figures/03_simulation_experiment/", 
                     overwrite = overwrite)
