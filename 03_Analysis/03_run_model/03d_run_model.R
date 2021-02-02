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
# Analyse results for fishpop and nutrients levels

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/remove_burnin.R")

source("01_Helper_functions/calculate_total_production.R")

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

#### Calculate total production (remove burn_in production) ####

production_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>%
                                    magrittr::extract2("seafloor") %>% 
                                    remove_burnin(), .id = "id_sim")

production_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>%
                                    magrittr::extract2("seafloor") %>%  
                                    remove_burnin(), 
                                  .id = "id_sim")

# combine both movement behaviors
production_combined <- dplyr::bind_rows(production_rand, production_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = "id_sim") %>% 
  dplyr::select(-id_sim) %>%
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")), 
                part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE),
                pop_n = factor(pop_n, ordered = TRUE))

#### Run ANOVA ####

anova_model <- dplyr::group_by(production_combined, part) %>% 
  dplyr::group_split() %>% 
  purrr::map(function(i) stats::aov(formula = production ~ id_move + nutrients_pool * pop_n, 
                                    data = i) %>% summary())

#### Plot low vs high nutrients ####

# load random movement data
result_rand <- purrr::map_dfr(model_runs_rand, function(i) readr::read_rds(i) %>%
                                magrittr::extract2("seafloor") %>% 
                                calculate_total_production(), .id = "id_sim")

# load attracted movement data
result_attr <- purrr::map_dfr(model_runs_attr, function(i) readr::read_rds(i) %>% 
                                magrittr::extract2("seafloor") %>% 
                                calculate_total_production(), .id = "id_sim")

# combine to one data.frame
result_combined <- dplyr::bind_rows(result_rand, result_attr, .id = "id_move") %>% 
  dplyr::left_join(sim_experiment, by = "id_sim") %>% 
  dplyr::group_by(id_move, timestep, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(production), sd = sd(production), .groups = "drop") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("1", "2"), 
                                 labels = c("Random movement", "Attracted movement")),
                part = factor(part, levels = c("ag_production", "bg_production"), 
                              labels = c("Aboveground production", "Belowground production")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE),
                pop_n = factor(pop_n, ordered = TRUE), 
                timestep = (timestep * min_per_i) / 60 / 24 / 365) %>% 
  dplyr::mutate(low = mean - sd, high = mean + sd) %>% 
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

gg_production_total_sub_bg <- ggplot(data = result_combined) +
  # geom_ribbon(aes(x = timestep, ymin = diff_low, ymax = diff_high, fill = pop_n), alpha = alpha) +
  geom_line(aes(x = timestep, y = diff_mean, col = pop_n)) +
  geom_vline(xintercept = burn_in, linetype = 3, col = "grey") +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  facet_wrap(~part + nutrients_pool, scales = "free_y", nrow = 2, ncol = 5) +
  scale_fill_viridis_d(name = "Fish population size") +
  scale_color_viridis_d(name = "Fish population size") +
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) +
  labs(x = "Years", y = expression(paste(Delta, Sigma, Prdocution[rand - attr], " (dry) [%]"))) + 
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom")
