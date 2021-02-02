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
# Calculate fish biomass and excrution

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/calculate_fish_size.R")

source("01_Helper_functions/calculate_fish_excretion.R")

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

#### Total size over time ####

size_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>% 
                                 magrittr::extract2("fishpop") %>% 
                                 calculate_fish_size, .id = "id_sim")

size_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
                                 magrittr::extract2("fishpop") %>% 
                                 calculate_fish_size, .id = "id_sim")

# combine both movement behaviors
size_combined <- dplyr::bind_rows(size_rand, size_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = c("id_sim" = "id_sim_sub")) %>% 
  dplyr::group_by(id_move, timestep, measure, pop_n) %>% 
  dplyr::summarise(mean = mean(value), sd = sd(value), .groups = "drop") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365,
                measure = factor(measure, levels = c("length", "weight"), 
                              labels = c("Fish length", "Fish weight")), 
                pop_n = factor(pop_n, ordered = TRUE))

gg_size_combined <- ggplot(data = size_combined) + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), alpha = alpha) + 
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_wrap(~measure + id_move, nrow = 2, ncol = 2, scales = "free_y") + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  coord_cartesian(xlim = c(burn_in, 50)) +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = "Fish length [cm] / Fish weight [g]") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Total excretion over time ####

excretion_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>% 
                                   magrittr::extract2("seafloor") %>% 
                                   calculate_fish_excretion, .id = "id_sim")

excretion_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
                                   magrittr::extract2("seafloor") %>% 
                                   calculate_fish_excretion, .id = "id_sim")

# combine both movement behaviors
excretion_combined <- dplyr::bind_rows(excretion_rand, excretion_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = c("id_sim" = "id_sim_sub")) %>% 
  dplyr::group_by(id_move, timestep, measure, pop_n) %>% 
  dplyr::summarise(mean = mean(value), sd = sd(value), .groups = "drop") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365,
                measure = factor(measure, levels = c("consumption", "excretion"), 
                                 labels = c("Fish consumption", "Fish excretion")), 
                pop_n = factor(pop_n, ordered = TRUE))

gg_excretion_combined <- ggplot(data = excretion_combined) + 
  geom_ribbon(aes(x = timestep, ymin = mean - sd, ymax = mean + sd, fill = pop_n), alpha = alpha) + 
  geom_line(aes(x = timestep, y = mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  facet_wrap(~measure + id_move, nrow = 2, ncol = 2, scales = "free_y") + 
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  coord_cartesian(xlim = c(burn_in, 50)) +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Sigma, " Nutrients [g]"))) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

#### Save ggplots ####

overwrite <- FALSE

width <- 170

height <- 120

dpi <- 300

units <- "mm"

suppoRt::save_ggplot(plot = gg_size_combined, filename = "gg_size_combined.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_excretion_combined, filename = "gg_excretion_combined.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height, dpi = dpi, units = units,
                     overwrite = overwrite)
