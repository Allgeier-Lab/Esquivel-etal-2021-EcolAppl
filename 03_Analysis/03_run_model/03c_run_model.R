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

source("01_Helper_functions/calculate_total_biomass.R")

source("01_Helper_functions/calculate_total_production.R")

source("01_Helper_functions/calculate_mean_production.R")

source("01_Helper_functions/calculate_dist_production.R")

sim_experiment <- readr::read_rds("02_Data/02_Modified/03_run_model/sim_experiment.rds")

#### Preprocess and load data #### 

# get all model runs for simulation experiment
sim_experiment <- dplyr::mutate(sim_experiment, id = 1:nrow(sim_experiment), 
                                .before = "nutrients_pool") %>% 
  dplyr::filter(nutrients_pool == 0.75) %>% 
  dplyr::mutate(id_sim_sub = as.character(1:nrow(.)), 
                .before = "nutrients_pool")

model_runs_rand <- list.files(path = "02_Data/02_Modified/03_run_model/result_23/",
                              pattern = "^result_rand", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE) %>%
  magrittr::extract(sim_experiment$id)

model_runs_attr <- list.files(path = "02_Data/02_Modified/03_run_model/result_23/",
                              pattern = "^result_attr", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE) %>%
  magrittr::extract(sim_experiment$id)

# set alpha for all plots
alpha <- 0.5

# to calculate to days
min_per_i <- 120

# set burn_in based on 03a_run_model
burn_in <- (50000 * min_per_i) / 60 / 24 / 365

# set base_size
base_size <- 8.5

#### Total biomass over time ####

biomass_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>% 
                                 magrittr::extract2("seafloor") %>% 
                                 calculate_total_biomass, .id = "id_sim")

biomass_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
                                 magrittr::extract2("seafloor") %>% 
                                 calculate_total_biomass, .id = "id_sim")

# combine both movement behaviors
biomass_combined <- dplyr::bind_rows(biomass_rand, biomass_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = c("id_sim" = "id_sim_sub")) %>% 
  dplyr::group_by(id_move, timestep, part, pop_n) %>% 
  dplyr::summarise(mean = mean(biomass), sd = sd(biomass), .groups = "drop") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365,
                part = factor(part, levels = c("ag_biomass", "bg_biomass"), 
                              labels = c("Aboveground biomass", "Belowground biomass")), 
                pop_n = factor(pop_n, ordered = TRUE))

#### Delta total biomass over time ####

biomass_combined_diff <- dplyr::mutate(biomass_combined, low = mean - sd, 
                                       high = mean + sd) %>% 
  dplyr::select(-sd) %>% 
  tidyr::pivot_wider(names_from = id_move, values_from = c(mean, low, high)) %>% 
  purrr::set_names(names(.) %>% 
                     stringr::str_replace(pattern = " ", replacement = "_") %>% 
                     stringr::str_to_lower()) %>% 
  dplyr::mutate(diff_low = ((low_attracted_movement - low_random_movement) / low_random_movement * 100),
                diff_mean = ((mean_attracted_movement - mean_random_movement) / mean_random_movement * 100), 
                diff_high = ((high_attracted_movement - high_random_movement) / high_random_movement * 100)) %>% 
  dplyr::select(-tidyselect::starts_with(c("low_", "mean_", "high_")))

# ylim <- abs(max(c(biomass_combined_diff$diff_low, 
#                   biomass_combined_diff$diff_mean, 
#                   biomass_combined_diff$diff_high)))

gg_biomass_combined_diff <- ggplot(data = biomass_combined_diff) + 
  geom_ribbon(aes(x = timestep, ymin = diff_low, ymax = diff_high, fill = pop_n), alpha = alpha) + 
  geom_line(aes(x = timestep, y = diff_mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  facet_wrap(~part, nrow = 2, ncol = 1, scales = "free_y") + 
  # scale_y_continuous(limits = c(-ylim, ylim)) +
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Delta, Sigma, Biomass[rand - attr], " (dry) [%]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Cumulative total production over time ####

production_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>% 
                                 magrittr::extract2("seafloor") %>% 
                                   calculate_total_production, .id = "id_sim")

production_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
                                 magrittr::extract2("seafloor") %>% 
                                   calculate_total_production, .id = "id_sim")

# combine both movement behaviors
production_combined <- dplyr::bind_rows(production_rand, production_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = c("id_sim" = "id_sim_sub")) %>% 
  dplyr::group_by(id_move, timestep, part, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365,
                part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")), 
                pop_n = factor(pop_n, ordered = TRUE))

#### Delta cumulative total production over time ####

production_combined_diff <- dplyr::mutate(production_combined, low = mean - sd, 
                                          high = mean + sd) %>% 
  dplyr::select(-sd) %>% 
  tidyr::pivot_wider(names_from = id_move, values_from = c(mean, low, high)) %>% 
  purrr::set_names(names(.) %>% 
                     stringr::str_replace(pattern = " ", replacement = "_") %>% 
                     stringr::str_to_lower()) %>% 
  dplyr::mutate(diff_low = ((low_attracted_movement - low_random_movement) / low_random_movement * 100),
                diff_mean = ((mean_attracted_movement - mean_random_movement) / mean_random_movement * 100), 
                diff_high = ((high_attracted_movement - high_random_movement) / high_random_movement * 100)) %>% 
  dplyr::select(-tidyselect::starts_with(c("low_", "mean_", "high_"))) %>% 
  tidyr::replace_na(list(diff_low = 0, diff_mean = 0, diff_high = 0))

# ylim <- abs(max(c(production_combined_diff$diff_low,
#                   production_combined_diff$diff_mean,
#                   production_combined_diff$diff_high)))

gg_production_combined_diff <- ggplot(data = production_combined_diff) +
  geom_ribbon(aes(x = timestep, ymin = diff_low, ymax = diff_high, fill = pop_n), alpha = alpha) +
  geom_line(aes(x = timestep, y = diff_mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  facet_wrap(~part, scales = "free_y", nrow = 2, ncol = 1) +
  # scale_y_continuous(limits = c(-ylim, ylim)) +
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Delta, Sigma, Prdocution[rand - attr], " (dry) [%]"))) + 
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom")

#### Mean production over time ####

# production_mean_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>% 
#                                          magrittr::extract2("seafloor") %>% 
#                                          calculate_mean_production(n_cells = 2500),
#                                        .id = "id_sim")
# 
# production_mean_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
#                                          magrittr::extract2("seafloor") %>% 
#                                          calculate_mean_production(n_cells = 2500),
#                                        .id = "id_sim")
# 
# production_mean_combined <- dplyr::bind_rows(production_mean_rand, production_mean_attr, 
#                                              .id = "id_move") %>% 
#   dplyr::left_join(sim_experiment, by = c("id_sim" = "id_sim_sub")) %>% 
#   dplyr::group_by(id_move, timestep, part, pop_n) %>% 
#   dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
#   dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
#                                  labels = c("Random movement", "Attracted movement")), 
#                 timestep = (timestep * min_per_i) / 60 / 24 / 365,
#                 part = factor(part, levels = c("ag_production", "bg_production"), 
#                               labels = c("Aboveground production", "Belowground production")), 
#                 pop_n = factor(pop_n, ordered = TRUE))
# 
# #### Delta mean production over time ####
# 
# production_mean_combined_diff <- dplyr::mutate(production_mean_combined, 
#                                                low = mean - sd, 
#                                                high = mean + sd) %>% 
#   dplyr::select(-sd) %>% 
#   tidyr::pivot_wider(names_from = id_move, values_from = c(mean, low, high)) %>% 
#   purrr::set_names(names(.) %>% 
#                      stringr::str_replace(pattern = " ", replacement = "_") %>% 
#                      stringr::str_to_lower()) %>% 
#   dplyr::mutate(diff_low = ((low_attracted_movement - low_random_movement) / low_random_movement * 100),
#                 diff_mean = ((mean_attracted_movement - mean_random_movement) / mean_random_movement * 100), 
#                 diff_high = ((high_attracted_movement - high_random_movement) / high_random_movement * 100)) %>% 
#   dplyr::select(-tidyselect::starts_with(c("low_", "mean_", "high_"))) %>% 
#   tidyr::replace_na(list(diff_low = 0, diff_mean = 0, diff_high = 0))
# 
# # ylim <- abs(max(c(production_mean_combined_diff$diff_low,
# #                   production_mean_combined_diff$diff_mean,
# #                   production_mean_combined_diff$diff_high)))
# 
# gg_production_mean_combined_diff <- ggplot(data = production_mean_combined_diff) + 
#   geom_ribbon(aes(x = timestep, ymin = diff_low, ymax = diff_high, fill = pop_n), alpha = alpha) +
#   geom_line(aes(x = timestep, y = diff_mean, col = pop_n)) + 
#   geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
#   facet_wrap(~part, scales = "free_y", nrow = 2, ncol = 1) + 
#   # scale_y_continuous(limits = c(-ylim, ylim)) +
#   scale_fill_viridis_d(name = "Fish population size") +
#   scale_color_viridis_d(name = "Fish population size") +
#   guides(colour = guide_legend(nrow = 1), 
#          fill = guide_legend(nrow = 1)) +
#   labs(x = "Years", y = expression(paste(Delta, mu, Production[rand - attr], " (dry) [%]"))) + 
#   theme_classic(base_size = base_size) + 
#   theme(legend.position = "bottom")

#### Mean production over distance ####

production_dist_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>% 
                                         magrittr::extract2("seafloor") %>% 
                                         calculate_dist_production(),
                                       .id = "id_sim")

production_dist_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
                                         magrittr::extract2("seafloor") %>% 
                                         calculate_dist_production(),
                                       .id = "id_sim")

production_dist_combined <- dplyr::bind_rows(production_dist_rand, production_dist_attr, 
                                             .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = c("id_sim" = "id_sim_sub")) %>% 
  dplyr::group_by(id_move, reef_dist, part, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")), 
                pop_n = factor(pop_n, ordered = TRUE))

#### Delta mean production over distance ####

production_dist_combined_diff <- dplyr::mutate(production_dist_combined, 
                                               low = mean - sd, 
                                               high = mean + sd) %>% 
  dplyr::select(-sd) %>% 
  tidyr::pivot_wider(names_from = id_move, values_from = c(mean, low, high)) %>% 
  purrr::set_names(names(.) %>% 
                     stringr::str_replace(pattern = " ", replacement = "_") %>% 
                     stringr::str_to_lower()) %>% 
  dplyr::mutate(diff_low = ((low_attracted_movement - low_random_movement) / low_random_movement * 100),
                diff_mean = ((mean_attracted_movement - mean_random_movement) / mean_random_movement * 100), 
                diff_high = ((high_attracted_movement - high_random_movement) / high_random_movement * 100)) %>% 
  dplyr::select(-tidyselect::starts_with(c("low_", "mean_", "high_")))

# ylim <- abs(max(c(production_dist_combined_diff$diff_low,
#                   production_dist_combined_diff$diff_mean,
#                   production_dist_combined_diff$diff_high)))

gg_production_dist_diff <- ggplot(data = production_dist_combined_diff) + 
  geom_ribbon(aes(x = reef_dist, ymin = diff_low, ymax = diff_high, fill = pop_n), alpha = alpha) +
  geom_line(aes(x = reef_dist, y = diff_mean, col = pop_n)) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  facet_wrap(~part, scales = "free_y", nrow = 2, ncol = 1) + 
  # scale_y_continuous(limits = c(-ylim, ylim)) +
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Distance to reef [m]", y = expression(paste(Delta, mu, Production[rand - attr], " (dry) [%]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Save ggplots ####

overwrite <- FALSE

width <- 170

height <- 120

dpi <- 300

units <- "mm"

suppoRt::save_ggplot(plot = gg_biomass_combined_diff, filename = "gg_biomass_combined_diff.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_combined_diff, filename = "gg_production_combined_diff.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

# suppoRt::save_ggplot(plot = gg_production_mean_combined_diff, filename = "gg_production_mean_combined_diff.png",
#                      path = "04_Figures/03_simulation_experiment/",
#                      width = width, height = height, dpi = dpi, units = units,
#                      overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_dist_diff, filename = "gg_production_dist_diff.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)
