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
# Analyze results for increasing fish population and nutrients levels simultaneously

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/calc_seagrass_values.R")

sim_experiment <- readr::read_rds("02_Data/02_Modified/03_run_model/sim_experiment.rds")

#### Preprocess data #### 

model_runs_rand <- list.files(path = "02_Data/02_Modified/03_run_model/result_23/",
           pattern = "^result_rand", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE)

model_runs_attr <- list.files(path = "02_Data/02_Modified/03_run_model/result_23/",
                              pattern = "^result_attr", full.names = TRUE) %>%
  stringr::str_sort(numeric = TRUE)

sim_experiment <- dplyr::mutate(sim_experiment, 
                                id_sim = as.character(1:nrow(sim_experiment)), 
                                .before = "nutrients_pool")

# set alpha for all plots
alpha <- 0.5

# to calculate to days
min_per_i <- 120

# set burn_in based on 03a_run_model
burn_in <- (50000 * min_per_i) / 60 / 24 / 365

# set base_size
base_size <- 8.5

#### Calculate total biomass (remove burn_in production) ####

biomass_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>%
                                 magrittr::extract2("seafloor") %>% 
                                 calc_total_biomass(), .id = "id_sim")

biomass_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>%
                                 magrittr::extract2("seafloor") %>%  
                                 calc_total_biomass(), .id = "id_sim")

# combine both movement behaviors
biomass_combined <- dplyr::bind_rows(rand = biomass_rand, attr = biomass_attr, 
                                     .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = "id_sim") %>% 
  dplyr::filter(timestep == max(timestep)) %>% 
  dplyr::select(-c(timestep, id_sim)) %>%
  dplyr::mutate(id_move = factor(id_move, levels = c("rand", "attr")),  
                part = factor(part, levels = c("ag_biomass", "bg_biomass")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE),
                pop_n = factor(pop_n, ordered = TRUE), 
                id = 1:nrow(.))

# get mean and sd of repetitions
biomass_combined_sum <- dplyr::group_by(biomass_combined, id_move, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(biomass), sd = sd(biomass), .groups = "drop") %>%
  dplyr::mutate(low = mean - sd, high = mean + sd) %>% 
  dplyr::mutate(part = factor(part, labels = c("Aboveground biomass", 
                                               "Belowground biomass")), 
                id_move = factor(id_move, labels = c("No influence of AR", 
                                                     "Attracted towards AR")))

#### Calculate total production ####

production_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>%
                                    magrittr::extract2("seafloor") %>% 
                                    calc_total_production(), .id = "id_sim")

production_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>%
                                    magrittr::extract2("seafloor") %>%  
                                    calc_total_production(), .id = "id_sim")

# combine both movement behaviors
production_combined <- dplyr::bind_rows(rand = production_rand, attr = production_attr, 
                                        .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = "id_sim") %>% 
  dplyr::filter(timestep == max(timestep)) %>% 
  dplyr::select(-c(timestep, id_sim)) %>%
  dplyr::mutate(id_move = factor(id_move, levels = c("rand", "attr")), 
                part = factor(part, levels = c("ag_production", "bg_production")),
                nutrients_pool = factor(nutrients_pool, ordered = TRUE),
                pop_n = factor(pop_n, ordered = TRUE), 
                id = 1:nrow(.))

production_combined_sum <- dplyr::group_by(production_combined, id_move, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>%
  dplyr::mutate(low = mean - sd, high = mean + sd) %>% 
  dplyr::mutate(part = factor(part, labels = c("Aboveground production", 
                                               "Belowground production")), 
                id_move = factor(id_move, labels = c("No influence of AR", 
                                                     "Attracted towards AR")))

#### Normalize values ####

biomass_norm <- dplyr::group_by(biomass_combined, part) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(function(x) {
    biomass_norm <- predict(bestNormalize::bestNormalize(x$biomass, k = 10, r = 100))
    tibble::tibble(id = x$id, biomass_norm = biomass_norm)}) %>%
  dplyr::left_join(x = biomass_combined, y = ., by = "id")

production_norm <- dplyr::group_by(production_combined, part) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(function(x) {
    production_norm <- predict(bestNormalize::bestNormalize(x$production, k = 10, r = 100))
    tibble::tibble(id = x$id, production_norm = production_norm)}) %>%
  dplyr::left_join(x = production_combined, y = ., by = "id")

#### Run ANOVA ####

anova_biomass <- dplyr::group_by(biomass_norm, part) %>% 
  dplyr::group_split() %>% 
  purrr::map(function(i) stats::aov(formula = biomass_norm ~ id_move + pop_n * nutrients_pool,
                                    data = i))

anova_biomass_sum <- purrr::map(anova_biomass, summary)

anova_production <- dplyr::group_by(production_norm, part) %>% 
  dplyr::group_split() %>% 
  purrr::map(function(i) stats::aov(formula = production_norm ~ id_move + pop_n * nutrients_pool,
                                    data = i))

anova_production_sum <- purrr::map(anova_production, summary)

#### Delta treatment vs null #### 

gg_biomass_full_design_sum <- ggplot(biomass_combined_sum, 
                                     aes(x = pop_n, y = mean)) + 
  geom_line(aes(group = interaction(id_move, nutrients_pool), linetype = id_move), 
            col = "lightgrey") +
  geom_point(aes(col = nutrients_pool, shape = id_move), size = 1.5) +
  facet_wrap(~ part, scales = "free_y", ncol = 2) + 
  scale_color_manual(name = "Water column nutrients pool",
                     values = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")) +
  scale_shape_manual(name = "Hypothesis", values = c(1, 15)) +
  scale_linetype_manual(name = "Hypothesis", values = c(2, 1)) +
  labs(x = "Number of fish individuals", 
       y = expression(paste(Sigma, Biomass, " (dry) [g]"))) + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "bottom")

biomass_combined_diff <- dplyr::group_by(biomass_combined, id_move, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(biomass), sd = sd(biomass), .groups = "drop") %>%
  dplyr::mutate(low = mean - sd, high = mean + sd) %>% 
  tidyr::pivot_wider(-sd, names_from = id_move, values_from = c(low, mean, high)) %>% 
  purrr::set_names(names(.) %>% 
                     stringr::str_replace_all(pattern = " ", replacement = "_") %>% 
                     stringr::str_to_lower()) %>% 
  dplyr::mutate(low_abs = low_attr - low_rand,
                mean_abs = mean_attr - mean_rand, 
                high_abs = high_attr - high_rand, 
                low_rel = low_abs / low_rand * 100,
                mean_rel = mean_abs / mean_rand * 100, 
                high_rel = high_abs / high_rand * 100) %>% 
  dplyr::mutate(part = factor(part, labels = c("Aboveground biomass", 
                                               "Belowground biomass")))
  
gg_biomass_full_design_diff <- ggplot(biomass_combined_diff, 
                                      aes(x = pop_n, y = mean_rel)) + 
  geom_line(aes(group = nutrients_pool), alpha = 0.1) +
  geom_point(aes(col = nutrients_pool), size = 1.5) +
  geom_errorbar(aes(ymin = low_rel, ymax = high_rel, col = nutrients_pool), 
                width = 0.1, size = 0.1) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  facet_wrap(~ part, scales = "free_y", ncol = 2) + 
  scale_color_manual(name = "Water column nutrients pool", 
                     values = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")) +
  labs(x = "Number of fish individuals", 
       y = expression(paste(Delta, Sigma, Biomass[null - AR], " (dry) [%]"))) + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "bottom")

# ggplot(data = biomass_diff, aes(x = pop_n, y = mean_abs)) + 
#   geom_line(aes(group = nutrients_pool), alpha = 0.1) +
#   geom_point(aes(col = nutrients_pool), size = 1.5) +
#   geom_errorbar(aes(ymin = low_abs, ymax = high_abs, col = nutrients_pool), 
#                 width = 0.1, size = 0.1) +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
#   facet_wrap(~ part, scales = "free_y", nrow = 1) + 
#   scale_color_viridis_d(name = "Starting water column nutrients pool") +
#   labs(x = "Number of fish individuals", 
#        y = expression(paste(Delta, Sigma, Biomass[rand - attr], " (dry) [%]"))) + 
#   theme_classic(base_size = 10) + 
#   theme(legend.position = "bottom")

gg_production_full_design_sum <- ggplot(production_combined_sum, 
                                        aes(x = pop_n, y = mean)) + 
  geom_line(aes(group = interaction(id_move, nutrients_pool), linetype = id_move), 
            col = "lightgrey") +
  geom_point(aes(col = nutrients_pool, shape = id_move), size = 1.5) +
  facet_wrap(~ part, scales = "free_y", ncol = 2) + 
  scale_color_manual(name = "Water column nutrients pool",
                     values = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")) +
  scale_shape_manual(name = "Hypothesis", values = c(1, 15)) +
  scale_linetype_manual(name = "Hypothesis", values = c(2, 1)) +
  labs(x = "Number of fish individuals", 
       y = expression(paste(Sigma, Production, " (dry) [g]"))) + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "bottom")

production_diff <- dplyr::group_by(production_combined, id_move, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>%
  dplyr::mutate(low = mean - sd, high = mean + sd) %>% 
  tidyr::pivot_wider(-sd, names_from = id_move, values_from = c(low, mean, high)) %>% 
  purrr::set_names(names(.) %>% 
                     stringr::str_replace_all(pattern = " ", replacement = "_") %>% 
                     stringr::str_to_lower()) %>% 
  dplyr::mutate(low_abs = low_attr - low_rand,
                mean_abs = mean_attr - mean_rand, 
                high_abs = high_attr - high_rand, 
                low_rel = low_abs / low_rand * 100,
                mean_rel = mean_abs / mean_rand * 100, 
                high_rel = high_abs / high_rand * 100, 
                part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")))

gg_production_full_design_diff <- ggplot(data = production_diff, 
                                         aes(x = pop_n, y = mean_rel)) + 
  geom_line(aes(group = nutrients_pool), alpha = 0.1) +
  geom_point(aes(col = nutrients_pool), size = 1.5) +
  geom_errorbar(aes(ymin = low_rel, ymax = high_rel, col = nutrients_pool), 
                width = 0.1, size = 0.1) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  facet_wrap(~ part, scales = "free_y", ncol = 2) + 
  scale_color_viridis_d(name = "Water column nutrients pool") +
  labs(x = "Number of fish individuals", 
       y = expression(paste(Delta, Sigma, Production[null - AR], " (dry) [%]"))) + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "bottom")

# ggplot(data = production_diff, aes(x = pop_n, y = mean_abs)) + 
#   geom_line(aes(group = nutrients_pool), alpha = 0.1) +
#   geom_point(aes(col = nutrients_pool), size = 1.5) +
#   geom_errorbar(aes(ymin = low_abs, ymax = high_abs, col = nutrients_pool), 
#                 width = 0.1, size = 0.1) +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
#   facet_wrap(~ part, scales = "free_y", nrow = 1) + 
#   scale_color_viridis_d(name = "Starting water column nutrients pool") +
#   labs(x = "Number of fish individuals", 
#        y = expression(paste(Delta, Sigma, Production[rand - attr], " (dry) [%]"))) + 
#   theme_classic(base_size = 10) + 
#   theme(legend.position = "bottom")

#### Save ggplots ####

# set default arguments to save plots
width <- 210

height <- 297 * 0.45

dpi <- 300

units <- "mm"

overwrite <- FALSE

suppoRt::save_ggplot(plot = gg_biomass_full_design_sum, 
                     filename = "gg_biomass_full_design_sum.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_biomass_full_design_diff, 
                     filename = "gg_biomass_full_design_diff.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_full_design_sum , 
                     filename = "gg_production_full_design_sum.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_production_full_design_diff, 
                     filename = "gg_production_full_design_diff.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)
