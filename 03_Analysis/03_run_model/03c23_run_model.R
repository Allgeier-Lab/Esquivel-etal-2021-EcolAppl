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

# result_null <- readr::read_rds("02_Data/02_Modified/03_run_model/result_null.rds")
# 
# result_rand <- readr::read_rds("02_Data/02_Modified/03_run_model/result_rand.rds")
# 
# result_attr <- readr::read_rds("02_Data/02_Modified/03_run_model/result_attr.rds")

#### Preprocess data #### 

sim_experiment <- dplyr::mutate(sim_experiment, id = 1:nrow(sim_experiment), 
                                .before = "nutrients_pool")

# get all model runs with stable nutrients pool and incresing fish_pop
filter_nutr <- dplyr::filter(sim_experiment, nutrients_pool == 0.75) %>% 
  dplyr::group_by(nutrients_pool, pop_n) %>% 
  dplyr::pull(id)

# needed for later join
sim_experiment_sub <- dplyr::slice(sim_experiment, filter_nutr) %>% 
  dplyr::select(id, pop_n) %>% 
  dplyr::mutate(id_fish = factor(1:nrow(.)))

result_rand <- list.files(path = "~/Downloads/results_23/",
                          pattern = "^result_rand", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE) %>%
  magrittr::extract(filter_nutr) %>% 
  purrr::map_dfr(function(i) readr::read_rds(i) %>% magrittr::extract2("seafloor"), 
                 .id = "id_fish") %>% 
  tibble::as_tibble()

result_attr <- list.files(path = "~/Downloads/results_23/",
                          pattern = "^result_attr", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE) %>%
  magrittr::extract(filter_nutr) %>% 
  purrr::map_dfr(function(i) readr::read_rds(i) %>% magrittr::extract2("seafloor"), 
                 .id = "id_fish") %>% 
  tibble::as_tibble()

# combine both movement behaviors
seafloor_combined <- dplyr::bind_rows(result_rand, result_attr, .id = "id_move") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                id_fish = factor(as.integer(id_fish)))

# rm raw data to free memory
rm(result_rand, result_attr)

# set alpha for all plots
alpha <- 0.5

# to calculate to days
min_per_i <- 120

# set burn_in based on 03a_run_model
burn_in <- (50000 * min_per_i) / 60 / 24 / 365

base_size <- 8.5

# # set probs to summarize repetitions
# probs <- c(0.05, 0.5, 0.95)

#### Total biomass over time ####

biomass_total <- dplyr::select(seafloor_combined, 
                               id_move, id_fish, ag_biomass, bg_biomass, timestep) %>% 
  tidyr::pivot_longer(cols = c(ag_biomass, bg_biomass), 
                      names_to = "part", values_to = "biomass") %>% 
  dplyr::group_by(id_move, id_fish, timestep, part) %>% 
  dplyr::summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop") %>% 
  dplyr::left_join(sim_experiment_sub, by = "id_fish") %>% 
  dplyr::group_by(id_move, timestep, part, pop_n) %>% 
  dplyr::summarise(mean = mean(biomass), sd = sd(biomass), .groups = "drop") %>% 
  dplyr::mutate(part = factor(part, levels = c("ag_biomass", "bg_biomass"), 
                              labels = c("Aboveground biomass", "Belowground biomass")), 
                pop_n = factor(pop_n, ordered = TRUE), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365)

gg_biomass_total_ag <- dplyr::filter(biomass_total, part == "Aboveground biomass") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Sigma, Biomass[ag], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

gg_biomass_total_bg <- dplyr::filter(biomass_total, part == "Belowground biomass") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Sigma, Biomass[bg], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Delta total biomass over time ####

biomass_total_diff <- dplyr::mutate(biomass_total, 
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

# ylim <- abs(max(c(biomass_total_diff$diff_low, 
#                   biomass_total_diff$diff_mean, 
#                   biomass_total_diff$diff_high)))

gg_biomass_total_diff <- ggplot(data = biomass_total_diff) + 
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

production_total <- dplyr::select(seafloor_combined, 
                                  id_move, id_fish, ag_production, bg_production, timestep) %>% 
  tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                      names_to = "part", values_to = "production") %>% 
  dplyr::group_by(id_move, id_fish, timestep, part) %>% 
  dplyr::summarise(production = sum(production, na.rm = TRUE), .groups = "drop") %>% 
  dplyr::left_join(sim_experiment_sub, by = "id_fish") %>% 
  dplyr::group_by(id_move, timestep, part, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
  dplyr::mutate(part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")), 
                pop_n = factor(pop_n, ordered = TRUE), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365)

gg_production_total_ag <- dplyr::filter(production_total, part == "Aboveground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Sigma, Production[ag], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

gg_production_total_bg <- dplyr::filter(production_total, part == "Belowground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Sigma, Production[bg], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Delta cumulative total production over time ####

production_total_diff <- dplyr::mutate(production_total, 
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
  dplyr::select(-tidyselect::starts_with(c("low_", "mean_", "high_"))) %>% 
  tidyr::replace_na(list(diff_low = 0, diff_mean = 0, diff_high = 0))

# ylim <- abs(max(c(production_total_diff$diff_low,
#                   production_total_diff$diff_mean,
#                   production_total_diff$diff_high)))

gg_production_total_diff <- ggplot(data = production_total_diff) +
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

production_mean <- dplyr::select(seafloor_combined, 
                                 id_move, id_fish, ag_production, bg_production, timestep) %>% 
  tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                      names_to = "part", values_to = "production") %>% 
  dplyr::group_by(id_move, id_fish, timestep, part) %>% 
  dplyr::summarise(production = sum(production), .groups = "drop") %>% 
  dplyr::group_by(id_move, id_fish, part) %>% 
  dplyr::mutate(production = (production - dplyr::lag(production, default = 0)) / 2500) %>% # divide by ncell?
  dplyr::ungroup() %>% 
  dplyr::left_join(sim_experiment_sub, by = "id_fish") %>% 
  dplyr::group_by(id_move, timestep, part, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
  dplyr::mutate(part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")), 
                pop_n = factor(pop_n, ordered = TRUE), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365)

gg_production_mean_ag <- dplyr::filter(production_mean, part == "Aboveground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(mu, Production[ag], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

gg_production_mean_bg <- dplyr::filter(production_mean, part == "Belowground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(mu, Production[bg], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Delta mean production over time ####

production_mean_diff <- dplyr::mutate(production_mean, 
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
  dplyr::select(-tidyselect::starts_with(c("low_", "mean_", "high_"))) %>% 
  tidyr::replace_na(list(diff_low = 0, diff_mean = 0, diff_high = 0))

# ylim <- abs(max(c(production_mean_diff$diff_low,
#                   production_mean_diff$diff_mean,
#                   production_mean_diff$diff_high)))

gg_production_mean_diff <- ggplot(data = production_mean_diff) + 
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
  labs(x = "Years", y = expression(paste(Delta, mu, Production[rand - attr], " (dry) [%]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Mean production over distance ####

production_dist <- dplyr::filter(seafloor_combined, timestep == max(timestep), reef == 0) %>% 
  dplyr::select(id_move, id_fish, ag_production, bg_production, reef_dist) %>% 
  tidyr::pivot_longer(cols = c(ag_production, bg_production), 
                      names_to = "part", values_to = "production") %>% 
  dplyr::group_by(id_move, id_fish, reef_dist, part) %>% 
  dplyr::summarise(production = mean(production, na.rm = TRUE), .groups = "drop") %>% 
  dplyr::left_join(sim_experiment_sub, by = "id_fish") %>% 
  dplyr::group_by(id_move, reef_dist, part, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
  dplyr::mutate(part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")), 
                pop_n = factor(pop_n, ordered = TRUE))

gg_production_dist_ag <- dplyr::filter(production_dist, part == "Aboveground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = reef_dist, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = reef_dist, y = mean, col = pop_n)) +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Distance to reef [m]", y = expression(paste(mu, Production[ag], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

gg_production_dist_bg <- dplyr::filter(production_dist, part == "Belowground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = reef_dist, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = alpha) +
  geom_line(aes(x = reef_dist, y = mean, col = pop_n)) +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Distance to reef [m]", y = expression(paste(mu, Production[bg], " (dry) [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Delta mean production over distance ####

production_dist_diff <- dplyr::mutate(production_dist, 
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

# ylim <- abs(max(c(production_dist_diff$diff_low,
#                   production_dist_diff$diff_mean,
#                   production_dist_diff$diff_high)))

gg_production_dist_diff <- ggplot(data = production_dist_diff) + 
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

# biomass total # 

suppoRt::save_ggplot(plot = gg_biomass_total_ag, filename = "gg_biomass_total_ag.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_biomass_total_bg, filename = "gg_biomass_total_bg.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_biomass_total_diff, filename = "gg_biomass_total_diff.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

# production total #

suppoRt::save_ggplot(plot = gg_production_total_ag, filename = "gg_production_total_ag.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_total_bg, filename = "gg_production_total_bg.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_total_diff, filename = "gg_production_total_diff.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

# production mean #

suppoRt::save_ggplot(plot = gg_production_mean_ag, filename = "gg_production_mean_ag.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_mean_bg, filename = "gg_production_mean_bg.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_mean_diff, filename = "gg_production_mean_diff.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

# production dist #

suppoRt::save_ggplot(plot = gg_production_dist_ag, filename = "gg_production_dist_ag.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_dist_bg, filename = "gg_production_dist_bg.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_dist_diff, filename = "gg_production_dist_diff.png",
                     path = "04_Figures/03_simulation_experiment/23/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)
