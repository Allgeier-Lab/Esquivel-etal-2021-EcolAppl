source("Helper_functions/setup.R")

#### load data and parameters ####

parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

#### Change parameters ####

# starting_values$ag_biomass <- parameters$ag_biomass_min +
#   ((parameters$ag_biomass_max - parameters$ag_biomass_min) * 0.01)
# 
# starting_values$bg_biomass <- parameters$bg_biomass_min +
#   ((parameters$bg_biomass_max - parameters$bg_biomass_min) * 0.01)
# 
# starting_values$nutrients_pool <- 0.75
# 
# starting_values$detritus_pool <- 0.75
# 
# starting_values$pop_n <- 25
# 
# parameters$detritus_ratio <- 0.0001
# 
# parameters$detritus_mineralization <- 0.0001
# 
# parameters$bg_thres <- 2/3

check_parameters(starting_values = starting_values, parameters = parameters)

#### Preprocess and init data ####

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 20

max_i <- (60 * 24 * 365 * years) / min_per_i

# save each m days
days <- 20

save_each <- (24 / (min_per_i / 60)) * days

# check if n and m are possible
max_i %% save_each

# set burn-in time
burn_in <- 37500

# extent and grain of seafloor
extent <- c(50, 50)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# create seafloor
input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                       reefs = reef_matrix,
                                       starting_values = starting_values)

# create population
input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                     starting_values = starting_values,
                                     parameters = parameters,
                                     use_log = TRUE)

#### Run model ####

# run model
result_null <- arrR::run_simulation(seafloor = input_seafloor,
                                    fishpop = NULL,
                                    parameters = parameters,
                                    reef_attraction = FALSE,
                                    max_i = max_i, min_per_i = min_per_i,
                                    save_each = save_each, burn_in = burn_in)

result_rand <- arrR::run_simulation(seafloor = input_seafloor,
                                    fishpop = input_fishpop,
                                    parameters = parameters,
                                    reef_attraction = FALSE,
                                    max_i = max_i, min_per_i = min_per_i,
                                    save_each = save_each, burn_in = burn_in)

result_attr <- arrR::run_simulation(seafloor = input_seafloor,
                                    fishpop = input_fishpop,
                                    parameters = parameters,
                                    reef_attraction = TRUE,
                                    max_i = max_i, min_per_i = min_per_i,
                                    save_each = save_each, burn_in = burn_in)

#### Plot results ####

# limits_sum <- arrR::get_limits(result = list(result_null, result_rand, result_attr),
#                                timestep = 0:max_i)
# 
# plot(result_null, summarize = TRUE, limits = limits_sum)
# plot(result_rand, summarize = TRUE, limits = limits_sum)
# plot(result_attr, summarize = TRUE, limits = limits_sum)
# 
# limits_map <- arrR::get_limits(result = list(result_null, result_rand, result_attr))
# 
# plot(result_null, limits = limits_map)
# plot(result_rand, limits = limits_map)
# plot(result_attr, limits = limits_map)
# 
# plot(result_null, what = "fishpop", summarize = TRUE)
# plot(result_rand, what = "fishpop", summarize = TRUE)
# plot(result_attr, what = "fishpop", summarize = TRUE)

#### Combine generic plot ###

# # summarize results
# result_null_sum <- arrR::summarize_mdlrn(result_null)
# 
# result_rand_sum <- arrR::summarize_mdlrn(result_rand)
# 
# result_attr_sum <- arrR::summarize_mdlrn(result_attr)
# 
# result_total <- dplyr::bind_rows(null = result_null_sum$seafloor,
#                                  rand = result_rand_sum$seafloor,
#                                  attr = result_attr_sum$seafloor,
#                                  .id = "move") %>%
#   tidyr::pivot_longer(-c(move, timestep, summary, burn_in)) %>%
#   dplyr::mutate(summary = factor(summary, levels = c("min", "mean", "max")),
#                 name = factor(name, levels = c("ag_biomass", "bg_biomass",
#                                                "nutrients_pool", "detritus_pool")),
#                 move = factor(move, levels = c("null", "rand", "attr"),
#                               labels = c("No fish", "Random movement", "Attracted movement"))) %>%
#   dplyr::filter(name %in% c("ag_biomass", "bg_biomass"), burn_in == "no")
# 
# ggplot(data = result_total) +
#   geom_line(aes(x = timestep, y = value, col = move)) +
#   # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
#   facet_wrap(~name * summary) +
#   scale_color_viridis_d(name = "Movement") +
#   labs(x = "Timestep", y = "Dry weight biomass [g/cell]") +
#   theme_classic() +
#   theme(legend.position = "bottom")

#### Compare total biomass####

biomass_total <- dplyr::bind_rows(null = result_null$seafloor, 
                                  rand = result_rand$seafloor, 
                                  attr = result_attr$seafloor, 
                                  .id = "move") %>% 
  dplyr::filter(burn_in == "no") %>%
  dplyr::group_by(move, timestep) %>% 
  dplyr::summarise(ag_biomass = sum(ag_biomass, na.rm = TRUE), 
                   bg_biomass = sum(bg_biomass, na.rm = TRUE)) %>% 
  tidyr::pivot_longer(-c(move, timestep)) %>% 
  dplyr::mutate(name = factor(name, levels = c("ag_biomass", "bg_biomass"), 
                              labels = c("Aboveground biomass", "Belowground biomass")), 
                move = factor(move, levels = c("null", "rand", "attr"), 
                              labels = c("No fish", "Random movement", "Attracted movement")))

biomass_end <- dplyr::group_by(biomass_total, move, name) %>% 
  dplyr::filter(timestep == max_i) %>% 
  dplyr::mutate(value = round(value))

ggplot(data = biomass_total) + 
  geom_line(aes(x = timestep, y = value, col = move)) + 
  # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
  geom_label(data = biomass_end, aes(x = max_i, y = value, label = value, col = move),
             hjust = 1.25, show.legend = FALSE) +
  facet_wrap(~name, scales = "free_y", ncol = 1,  nrow = 2) + 
  scale_color_viridis_d(name = "Movement") +
  labs(x = "Timestep", y = "Biomass (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

#### Compare production over time ####

# Normalize by biomass_max * n(cells) --> same as modifier approach

production_total <- dplyr::bind_rows(null = result_null$seafloor, 
                                     rand = result_rand$seafloor, 
                                     attr = result_attr$seafloor, 
                                     .id = "move") %>% 
  dplyr::filter(burn_in == "no") %>%
  dplyr::group_by(move, timestep) %>% 
  dplyr::summarise(ag_production = sum(ag_production, na.rm = TRUE),
                   bg_production = sum(bg_production, na.rm = TRUE), 
                   ag_slough = sum(ag_slough, na.rm = TRUE), 
                   bg_slough = sum(bg_slough, na.rm = TRUE)) %>%  
  tidyr::pivot_longer(-c(move, timestep)) %>% 
  dplyr::mutate(name = factor(name, levels = c("ag_production",  "ag_slough", 
                                               "bg_production", "bg_slough"), 
                              labels = c("Aboveground production", "Aboveground slough",
                                         "Belowground production", "Belowground slough")), 
                move = factor(move, levels = c("null", "rand", "attr"), 
                              labels = c("No fish", "Random movement", "Attracted movement")))

production_end <- dplyr::group_by(production_total, move, name) %>% 
  dplyr::filter(timestep == max_i) %>% 
  dplyr::mutate(value = round(value))

ggplot(data = production_total) + 
  geom_line(aes(x = timestep, y = value, col = move)) + 
  # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
  geom_label(data = production_end, aes(x = max_i, y = value, label = value, col = move),
             hjust = 1.25, show.legend = FALSE) +
  facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
  scale_color_viridis_d(name = "Movement") +
  labs(x = "Timestep", y = "Cumulative biomass (dry) [g]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

#### Compare slough over time ####

# # Normalize by biomass_max * n(cells) --> same as modifier approach
# 
# detritus_total <- dplyr::bind_rows(null = result_null$seafloor, 
#                                    rand = result_rand$seafloor, 
#                                    attr = result_attr$seafloor, 
#                                    .id = "move") %>% 
#   dplyr::filter(burn_in == "no") %>%
#   dplyr::group_by(move, timestep) %>% 
#   dplyr::summarise(ag_slough = sum(ag_slough, na.rm = TRUE),
#                    bg_slough = sum(bg_slough, na.rm = TRUE), 
#                    total_slough = sum((ag_slough + bg_slough), na.rm = TRUE)) %>%  
#   tidyr::pivot_longer(-c(move, timestep)) %>% 
#   dplyr::mutate(name = factor(name, levels = c("ag_slough", "bg_slough", "total_slough"), 
#                               labels = c("Aboveground slough", "Belowground slough", "Total slough")), 
#                 move = factor(move, levels = c("null", "rand", "attr"), 
#                               labels = c("No fish", "Random movement", "Attracted movement")))
# 
# ggplot(data = detritus_total) + 
#   geom_line(aes(x = timestep, y = value, col = move)) + 
#   # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
#   facet_wrap(~name, scales = "free_y", ncol = 1) + 
#   scale_color_viridis_d(name = "Movement") +
#   labs(x = "Timestep", y = "Total slough dry weight biomass [g/cell]") + 
#   theme_classic() + 
#   theme(legend.position = "bottom")

#### Compare production over reef distance ####

production_dist <- dplyr::bind_rows(null = result_null$seafloor, 
                                    rand = result_rand$seafloor, 
                                    attr = result_attr$seafloor, 
                                    .id = "move") %>% 
  dplyr::filter(burn_in == "no") %>% 
  dplyr::group_by(move, reef_dist) %>% 
  dplyr::summarise(ag_production = mean(ag_production, na.rm = TRUE),
                   bg_production = mean(bg_production, na.rm = TRUE), 
                   ag_slough = mean(ag_slough, na.rm = TRUE),
                   bg_slough = mean(bg_slough, na.rm = TRUE)) %>%  
  tidyr::pivot_longer(-c(move, reef_dist)) %>% 
  dplyr::filter(reef_dist != 0) %>% 
  dplyr::mutate(name = factor(name, levels = c("ag_production", "ag_slough", 
                                               "bg_production", "bg_slough"), 
                              labels = c("Aboveground production", "Aboveground slough",
                                         "Belowground production", "Belowground slough")), 
                move = factor(move, levels = c("null", "rand", "attr"), 
                              labels = c("No fish", "Random movement", "Attracted movement")))

production_dist_end <- dplyr::group_by(production_dist, move, name) %>% 
  dplyr::filter(reef_dist == min(reef_dist)) %>% 
  dplyr::mutate(value = round(value))

ggplot(data = production_dist) + 
  geom_line(aes(x = reef_dist, y = value, col = move)) + 
  # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
  geom_label(data = production_dist_end, aes(x = reef_dist, y = value, label = value, col = move),
             hjust = -0.5, show.legend = FALSE) +
  facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
  scale_color_viridis_d(name = "Movement") +
  labs(x = "Distance to reef [m]", y = "Production biomass (dry) [g/cell]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

#### Compare uptake over time ####

# # Normalize by biomass_max * n(cells) --> same as modifier approach
# 
# uptake_total <- dplyr::bind_rows(null = result_null$seafloor, 
#                                  rand = result_rand$seafloor, 
#                                  attr = result_attr$seafloor, 
#                                  .id = "move") %>% 
#   dplyr::filter(burn_in == "no") %>%
#   dplyr::group_by(move, timestep) %>% 
#   dplyr::summarise(ag_uptake = sum(ag_uptake, na.rm = TRUE),
#                    bg_uptake = sum(bg_uptake, na.rm = TRUE), 
#                    total_uptake = sum((ag_uptake + bg_uptake), na.rm = TRUE)) %>%  
#   tidyr::pivot_longer(-c(move, timestep)) %>% 
#   dplyr::mutate(name = factor(name, levels = c("ag_uptake", "bg_uptake", "total_uptake"), 
#                               labels = c("Aboveground uptake", "Belowground uptake", "Total uptake")), 
#                 move = factor(move, levels = c("null", "rand", "attr"), 
#                               labels = c("No fish", "Random movement", "Attracted movement")))
# 
# ggplot(data = uptake_total) + 
#   geom_line(aes(x = timestep, y = value, col = move)) + 
#   # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
#   facet_wrap(~name, scales = "free_y", ncol = 1) + 
#   scale_color_viridis_d(name = "Movement") +
#   labs(x = "Timestep", y = "Total nutrient uptake [g/cell]") + 
#   theme_classic() + 
#   theme(legend.position = "bottom")

#### Non-cumulative production/slough ####

result_total_itr <- dplyr::bind_rows(null = result_null$seafloor,
                                     rand = result_rand$seafloor,
                                     attr = result_attr$seafloor,
                                     .id = "move") %>%
  dplyr::group_by(move, x, y) %>% 
  dplyr::mutate(ag_prod_itr = ag_production - dplyr::lag(ag_production),
                ag_slough_itr = ag_slough - dplyr::lag(ag_slough),
                bg_prod_itr = bg_production - dplyr::lag(bg_production),
                bg_slough_itr = bg_slough - dplyr::lag(bg_slough)) %>% 
  dplyr::filter(burn_in == "no", reef == 0) %>%
  dplyr::group_by(move, timestep) %>% 
  dplyr::summarise(ag_prod_itr = sum(ag_prod_itr), 
                   ag_slough_itr = sum(ag_slough_itr), 
                   bg_prod_itr = sum(bg_prod_itr), 
                   bg_slough_itr = sum(bg_slough_itr)) %>% 
  tidyr::pivot_longer(-c(move, timestep)) %>%
  dplyr::mutate(name = factor(name, levels = c("ag_prod_itr", "ag_slough_itr",
                                               "bg_prod_itr", "bg_slough_itr"), 
                              labels = c("Aboveground production", "Aboveground slough", 
                                         "Belowground production", "Belowground slough")),
                move = factor(move, levels = c("null", "rand", "attr"),
                              labels = c("No fish", "Random movement", "Attracted movement")))

total_itr_end <- dplyr::group_by(result_total_itr, move, name) %>% 
  dplyr::filter(timestep == max_i) %>% 
  dplyr::mutate(value = round(value))

ggplot(data = dplyr::filter(result_total_itr, !name %in% c("Total production", 
                                                           "Total slough"))) + 
  geom_line(aes(x = timestep, y = value, col = move)) + 
  # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
  geom_label(data = total_itr_end, aes(x = timestep, y = value, label = value, col = move),
             hjust = 1.5, show.legend = FALSE) +
  facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
  scale_color_viridis_d(name = "Movement") +
  labs(x = "Timestep", y = "Biomass (dry) [g/cell]") + 
  theme_classic() + 
  theme(legend.position = "bottom")

# ggplot(data = dplyr::filter(result_total_itr, name %in% c("Total production", 
#                                                           "Total slough"))) + 
#   geom_line(aes(x = timestep, y = value, col = move)) + 
#   # geom_vline(xintercept = max_i * burn_in, linetype = 3, col = "grey") +
#   facet_wrap(~name, scales = "free_y", ncol = 2, nrow = 2) + 
#   scale_color_viridis_d(name = "Movement") +
#   labs(x = "Timestep", y = "Mean dry weight biomass [g/cell]") + 
#   theme_classic() + 
#   theme(legend.position = "bottom")
