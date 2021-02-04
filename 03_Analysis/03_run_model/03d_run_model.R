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
biomass_combined <- dplyr::bind_rows(biomass_rand, biomass_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = "id_sim") %>% 
  dplyr::filter(timestep == max(timestep)) %>% 
  dplyr::select(-c(timestep, id_sim)) %>%
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                part = factor(part, levels = c("ag_biomass", "bg_biomass")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE),
                pop_n = factor(pop_n, ordered = TRUE), 
                id = 1:nrow(.))

#### Calculate total production ####

production_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>%
                                    magrittr::extract2("seafloor") %>% 
                                    calc_total_production(), .id = "id_sim")

production_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>%
                                    magrittr::extract2("seafloor") %>%  
                                    calc_total_production(), .id = "id_sim")

# combine both movement behaviors
production_combined <- dplyr::bind_rows(production_rand, production_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = "id_sim") %>% 
  dplyr::filter(timestep == max(timestep)) %>% 
  dplyr::select(-c(timestep, id_sim)) %>%
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                part = factor(part, levels = c("ag_production", "bg_production")),
                nutrients_pool = factor(nutrients_pool, ordered = TRUE),
                pop_n = factor(pop_n, ordered = TRUE), 
                id = 1:nrow(.))

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

# #### Plot low vs high nutrients ####
# 
# # load random movement data
# result_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>%
#                                 magrittr::extract2("seafloor") %>% 
#                                 calc_total_production(), .id = "id_sim")
# 
# # load attracted movement data
# result_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
#                                 magrittr::extract2("seafloor") %>% 
#                                 calc_total_production(), .id = "id_sim")
# 
# # combine to one data.frame
# result_combined <- dplyr::bind_rows(result_rand, result_attr, .id = "id_move") %>% 
#   dplyr::left_join(sim_experiment, by = "id_sim") %>% 
#   dplyr::group_by(id_move, timestep, part, nutrients_pool, pop_n) %>% 
#   dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
#   dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
#                                  labels = c("Random movement", "Attracted movement")),
#                 part = factor(part, levels = c("ag_production", "bg_production"), 
#                               labels = c("Aboveground production", "Belowground production")), 
#                 nutrients_pool = factor(nutrients_pool, ordered = TRUE),
#                 pop_n = factor(pop_n, ordered = TRUE), 
#                 timestep = (timestep * min_per_i) / 60 / 24 / 365) %>% 
#   dplyr::mutate(low = mean - sd, high = mean + sd) %>% 
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
# gg_production_nutr <- ggplot(data = result_combined) +
#   # geom_ribbon(aes(x = timestep, ymin = diff_low, ymax = diff_high, fill = pop_n), alpha = alpha) +
#   geom_line(aes(x = timestep, y = diff_mean, col = pop_n)) +
#   geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
#   facet_wrap(~part + nutrients_pool, scales = "free_y", nrow = 2, ncol = 5) +
#   scale_fill_viridis_d(name = "Fish population size") +
#   scale_color_viridis_d(name = "Fish population size") +
#   guides(colour = guide_legend(nrow = 1), 
#          fill = guide_legend(nrow = 1)) +
#   labs(x = "Years", y = expression(paste(Delta, Sigma, Prdocution[rand - attr], " (dry) [%]"))) + 
#   theme_classic(base_size = base_size) +
#   theme(legend.position = "bottom")
# 
# 
# #### Save ggplots ####
# 
# # set default arguments to save plots
# width <- 297
# 
# height <- 120
# 
# dpi <- 300
# 
# units <- "mm"
# 
# suppoRt::save_ggplot(plot = gg_production_nutr, filename = "gg_production_nutr.png",
#                      path = "04_Figures/03_simulation_experiment/",
#                      width = width, height = height, dpi = dpi, units = units,
#                      overwrite = FALSE)
