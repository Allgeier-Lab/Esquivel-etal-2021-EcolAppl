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

# set burn_in based on 03a_run_model
burn_in <- 50000

# set probs to summarize repetitions
probs <- c(0.05, 0.5, 0.95)

# get all model runs with stable nutrients pool and incresing fish_pop
filter_nutr <- dplyr::filter(sim_experiment, nutrients_pool == 0.75) %>% 
  dplyr::group_by(nutrients_pool, pop_n) %>% 
  # dplyr::slice_head(n = 3) %>% # REMOVE! 
  dplyr::pull(id)

# needed for later join
sim_experiment_sub <- dplyr::slice(sim_experiment, filter_nutr) %>% 
  dplyr::select(id, pop_n) %>% 
  dplyr::mutate(id_fish = factor(1:nrow(.)))

result_rand <- list.files(path = "~/Downloads/results_34/",
                          pattern = "^result_rand", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE) %>%
  magrittr::extract(filter_nutr) %>% 
  purrr::map_dfr(function(i) readr::read_rds(i) %>% magrittr::extract2("seafloor"), 
                 .id = "id_fish") %>% 
  tibble::as_tibble()

result_attr <- list.files(path = "~/Downloads/results_34/",
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

#### Total biomass over iteration ####

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
                pop_n = factor(pop_n, ordered = TRUE))

ylim <- dplyr::filter(biomass_total, timestep >= burn_in) %>%
  dplyr::group_by(part) %>%
  dplyr::summarise(min = min(mean),
                   max = max(mean))

gg_biomass_total_ag <- dplyr::filter(biomass_total, part == "Aboveground biomass") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = 0.3) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  # geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  # coord_cartesian(ylim = c(ylim[[1, 2]], ylim[[1, 3]])) +
  labs(x = "Iteration", y = "Total ag biomass (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

gg_biomass_total_bg <- dplyr::filter(biomass_total, part == "Belowground biomass") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = 0.3) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  # geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  # coord_cartesian(ylim = c(ylim[[2, 2]], ylim[[2, 3]])) +
  labs(x = "Iteration", y = "Total bg biomass (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

#### Cumulative biomass production over iteration ####

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
                pop_n = factor(pop_n, ordered = TRUE))

ylim <- dplyr::filter(production_total, timestep >= burn_in) %>%
  dplyr::group_by(part) %>%
  dplyr::summarise(min = min(mean),
                   max = max(mean))

gg_production_total_ag <- dplyr::filter(production_total, part == "Aboveground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = 0.3) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  # geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  # coord_cartesian(ylim = c(ylim[[1, 2]], ylim[[1, 3]])) +
  labs(x = "Iteration", y = "Total cumulative ag production (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

gg_production_total_bg <- dplyr::filter(production_total, part == "Belowground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = 0.3) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  # geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  # coord_cartesian(ylim = c(ylim[[2, 2]], ylim[[2, 3]])) +
  labs(x = "Iteration", y = "Total cumulative bg production (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

#### Mean biomass production over iteration ####

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
                pop_n = factor(pop_n, ordered = TRUE))

ylim <- dplyr::filter(production_mean, timestep >= burn_in) %>%
  dplyr::group_by(part) %>%
  dplyr::summarise(min = min(mean), 
                   max = max(mean))

gg_production_mean_ag <- dplyr::filter(production_mean, part == "Aboveground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = 0.3) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  # geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  # coord_cartesian(ylim = c(ylim[[1, 2]], ylim[[1, 3]])) +
  labs(x = "Iteration", y = "Mean ag production (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

gg_production_mean_bg <- dplyr::filter(production_mean, part == "Belowground production") %>% 
  ggplot() + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), 
              alpha = 0.3) +
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  # geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_grid(~id_move) + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  # coord_cartesian(ylim = c(ylim[[2, 2]], ylim[[2, 3]])) +
  labs(x = "Iteration", y = "Mean bg production (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

# #### Total biomass production at distance #### 
# 
# production_dist <- dplyr::filter(seafloor_combined, 
#                                  reef != 1, timestep == max(timestep)) %>% 
#   dplyr::group_by(reef_dist, movement, pop_n) %>% 
#   dplyr::summarise(ag_production = mean(ag_production),
#                    ag_slough = mean(ag_slough),
#                    bg_production = mean(bg_production),
#                    bg_slough = mean(bg_slough)) %>%  
#   tidyr::pivot_longer(-c(reef_dist, movement, pop_n)) %>% 
#   dplyr::mutate(name = factor(name, levels = c("ag_production", "ag_slough", 
#                                                "bg_production", "bg_slough"), 
#                               labels = c("Aboveground production", "Aboveground slough",
#                                          "Belowground production", "Belowground slough")))
# 
# # production_dist_end <- dplyr::group_by(production_dist, move, name) %>% 
# #   dplyr::filter(reef_dist == min(reef_dist)) %>% 
# #   dplyr::mutate(value = round(value))
# 
# gg_production_dist <- ggplot(data = production_dist) + 
#   geom_line(aes(x = reef_dist, y = value, col = pop_n, linetype = movement)) + 
#   # geom_label(data = production_dist_end, aes(x = reef_dist, y = value, label = value, col = move),
#   #            hjust = -0.5, show.legend = FALSE) +
#   facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
#   scale_color_viridis_d(name = "Fish population size") +
#   scale_linetype_manual(name = "Movement behaviour", values = c(1, 2)) +
#   labs(x = "Distance to reef [m]", y = "Mean biomass (dry) [g/cell]") + 
#   theme_classic()

# #### Save ggplots ####
# 
# overwrite <- FALSE
# 
# suppoRt::save_ggplot(plot = gg_production_total, filename = "gg_production_total.png", 
#                      path = "04_Figures/03_simulation_experiment/", 
#                      overwrite = overwrite)
# 
# suppoRt::save_ggplot(plot = gg_production_mean, filename = "gg_production_mean.png", 
#                      path = "04_Figures/03_simulation_experiment/", 
#                      overwrite = overwrite)
# 
# suppoRt::save_ggplot(plot = gg_production_dist, filename = "gg_production_dist.png", 
#                      path = "04_Figures/03_simulation_experiment/", 
#                      overwrite = overwrite)
