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
# Plot results of model runs w/o and with attraction

#### Import libraries and data ####

# load packages #
source("Helper_functions/setup.R")

source("Helper_functions/calc_biomass_dist.R")

result_null <- readr::read_rds("Data/Modified/03_run_model/result_null.rds")

result_rand <- readr::read_rds("Data/Modified/03_run_model/result_rand.rds")

result_attr <- readr::read_rds("Data/Modified/03_run_model/result_attr.rds")

# # import all model runs for default and changed parameters
# result_rand <- list.files(path = "~/Downloads/results/",
#                           pattern = "^result_rand", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(readr::read_rds)
# 
# # import all model runs for default and changed parameters
# result_attr <- list.files(path = "~/Downloads/results/",
#                           pattern = "^result_attr_", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(readr::read_rds)

#### Get envelope of biomass at distance ####

dist_null_sum <- purrr::map_dfr(result_null, calc_biomass_dist, .id = "id") %>% 
  dplyr::group_by(reef_dist_clss) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass, na.rm = TRUE), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-reef_dist_clss) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "null")

dist_rand_sum <- purrr::map_dfr(result_rand, calc_biomass_dist, .id = "id") %>% 
  dplyr::group_by(reef_dist_clss) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-reef_dist_clss) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "rand")

dist_attr_sum <- purrr::map_dfr(result_attr, calc_biomass_dist, .id = "id") %>% 
  dplyr::group_by(reef_dist_clss) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-reef_dist_clss) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "attr")

dist_total_sum <- dplyr::bind_rows(dist_null_sum, dist_rand_sum, dist_attr_sum) %>% 
  dplyr::mutate(biomass = factor(biomass, levels = c("ag", "bg"), 
                                 labels = c("Aboveground biomass", "Belowground biomass")), 
                move = factor(move, levels = c("null", "rand", "attr"), 
                              labels = c("No fish", "Random movement", "Attracted movement"))) %>% 
  tidyr::replace_na(replace = list(sd = 0))

#### Biomass over time ####

time_null_sum <- purrr::map_dfr(result_null, 
                                function(i) summarize_mdlrn(i, summary = "mean")$seafloor, 
                                .id = "id") %>% 
  dplyr::group_by(timestep) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-timestep) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "null")

time_rand_sum <- purrr::map_dfr(result_rand, 
                                function(i) summarize_mdlrn(i, summary = "mean")$seafloor, 
                                .id = "id") %>% 
  dplyr::group_by(timestep) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-timestep) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "rand")

time_attr_sum <- purrr::map_dfr(result_attr, 
                                function(i) summarize_mdlrn(i, summary = "mean")$seafloor, 
                                .id = "id") %>% 
  dplyr::group_by(timestep) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-timestep) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "attr")

time_total_sum <- dplyr::bind_rows(time_null_sum, time_rand_sum, time_attr_sum) %>% 
  dplyr::mutate(biomass = factor(biomass, levels = c("ag", "bg"), 
                                 labels = c("Aboveground biomass", "Belowground biomass")), 
                move = factor(move, levels = c("null", "rand", "attr"), 
                              labels = c("No fish", "Random movement", "Attracted movement")), 
                timestep = (timestep * 120) / 60 / 24) %>% 
  tidyr::replace_na(replace = list(sd = 0))

#### Biomass map ####

map_null_sum <- purrr::map_dfr(result_null,function(i) filter_mdlrn(i)$seafloor, .id = "id") %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-c(x, y)) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "null")

map_rand_sum <- purrr::map_dfr(result_rand, function(i) filter_mdlrn(i)$seafloor, .id = "id") %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-c(x, y)) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "rand")

map_attr_sum <- purrr::map_dfr(result_attr, function(i) filter_mdlrn(i)$seafloor, .id = "id") %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(bg_mean = mean(bg_biomass),
                   bg_sd = sd(bg_biomass), 
                   ag_mean = mean(ag_biomass),
                   ag_sd = sd(ag_biomass)) %>% 
  tidyr::pivot_longer(-c(x, y)) %>% 
  tidyr::separate(name, sep = "_", into = c("biomass", "distr")) %>% 
  tidyr::pivot_wider(names_from = distr, values_from = value) %>% 
  dplyr::mutate(move = "attr")

map_total_sum <- dplyr::bind_rows(map_null_sum, map_rand_sum, map_attr_sum) %>% 
  dplyr::mutate(biomass = factor(biomass, levels = c("ag", "bg"), 
                                 labels = c("Aboveground biomass", "Belowground biomass")), 
                move = factor(move, levels = c("null", "rand", "attr"), 
                              labels = c("No fish", "Random movement", "Attracted movement"))) %>% 
  tidyr::replace_na(replace = list(sd = 0))


#### Create ggplot ####

ggplot_compare_dist <- ggplot(data = dist_total_sum) + 
  geom_ribbon(aes(x = reef_dist_clss, ymin = mean - sd, ymax = mean + sd, fill = move), alpha = 0.3) + 
  geom_line(aes(x = reef_dist_clss, y = mean, col = move)) +
  facet_wrap(~ biomass, scales = "free_y", nrow = 2) + 
  scale_color_viridis_d(name = "Movement") +
  scale_fill_viridis_d(name = "Movement") +
  scale_x_continuous(breaks = seq(from = 1, to = 34, by = 2), limits = c(1, 34)) +
  labs(x = "Distance to reef [m]", y = "Biomass dry [g/cell]") +
  theme_classic() + 
  theme(legend.position = "bottom")

ggplot_compare_time <- ggplot(data = time_total_sum) + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = move), alpha = 0.3) +
  geom_line(aes(x = timestep, y = mean, col = move)) +
  facet_wrap(~ biomass, scales = "free_y", nrow = 2) + 
  scale_color_viridis_d(name = "Movement") +
  scale_fill_viridis_d(name = "Movement") +
  scale_x_continuous(breaks = seq(from = 0, to = 1095, by = 120), limits = c(0, 1095)) +
  labs(x = "Days", y = "Biomass dry [g/cell]") +
  theme_classic() + 
  theme(legend.position = "bottom")

ggplot_compare_map <- ggplot(data = map_total_sum) + 
  geom_raster(aes(x = x, y = y, fill = mean)) +
  facet_wrap(~ biomass + move, nrow = 2) + 
  scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                       na.value = "#9B964A", name = "Biomass dry [g/cell]") +
  coord_equal() +
  theme_classic() + 
  theme(legend.position = "bottom")

#### Save ggplots ####

overwrite <- FALSE

# save plot
suppoRt::save_ggplot(plot = ggplot_compare_dist, 
                     filename = "ggplot_compare_dist.png", 
                     path = "Figures/",     
                     dpi = dpi,
                     width = height_full, height = width_full, units = units, 
                     overwrite = overwrite)

# save plot
suppoRt::save_ggplot(plot = ggplot_compare_time, 
                     filename = "ggplot_compare_time.png", 
                     path = "Figures/",     
                     dpi = dpi,
                     width = height_full, height = width_full, units = units, 
                     overwrite = overwrite)

# save plot
suppoRt::save_ggplot(plot = ggplot_compare_map, 
                     filename = "ggplot_compare_map.png", 
                     path = "Figures/",     
                     dpi = dpi,
                     width = height_full, height = width_full, units = units, 
                     overwrite = overwrite)
