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

source("01_Helper_functions/calc_fishpop_values.R")

experiment <- readr::read_rds("02_Data/02_Modified/03_run_model/sim_experiment.rds")

model_runs <- readr::read_rds("02_Data/02_Modified/03_run_model/model_runs.rds")

#### Preprocess and load data #### 

# add row id to sim_experiment
experiment <- dplyr::mutate(experiment, id = as.character(1:nrow(experiment)), 
                            .before = "nutrients_pool")

#### Calculate total consumption and excretion ####

# calculate total excretion rand/attr
excretion_rand <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_excretion(), .id = "id")

excretion_attr <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_excretion(), .id = "id")

excretion_total <- dplyr::bind_rows(rand = excretion_rand, attr = excretion_attr, 
                                    .id = "movement") %>% 
  dplyr::left_join(experiment, by = "id") %>% 
  dplyr::group_by(movement, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

#### Calculate total mortality dimensions ####

# calculate total mortality
mortality_rand <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "rand") %>%
    magrittr::extract2("fishpop") %>% 
    calc_mort(), .id = "id")

mortality_attr <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "attr") %>%
    magrittr::extract2("fishpop") %>% 
    calc_mort(), .id = "id")

mortality_total <- dplyr::bind_rows(rand = mortality_rand, attr = mortality_attr, 
                                    .id = "movement") %>% 
  dplyr::left_join(experiment, by = "id") %>% 
  dplyr::group_by(movement, measure, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                measure  = factor(measure, levels = c("died_background", "died_consumption", "ttl")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

mortality_sum <- dplyr::bind_rows(rand = mortality_rand, attr = mortality_attr, 
                 .id = "movement") %>% 
  dplyr::group_by(movement, id) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop") %>% 
  dplyr::left_join(experiment, by = "id") %>% 
  dplyr::group_by(movement, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                measure = "ttl",
                measure  = factor(measure, levels = c("died_background", "died_consumption", "ttl")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

mortality_total <- dplyr::bind_rows(x = mortality_total, y = mortality_sum) %>% 
  dplyr::mutate(measure_n = paste(measure, pop_n, sep = "_"),
                measure_n = factor(measure_n, 
                                   levels = stringr::str_sort(unique(measure_n), 
                                                              numeric = TRUE)))

#### Calculate fish biomass ####

size_rand <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "rand") %>%
    magrittr::extract2("fishpop") %>% 
    calc_fish_size(), .id = "id")

size_attr <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "rand") %>%
    magrittr::extract2("fishpop") %>% 
    calc_fish_size(), .id = "id")

size_total <- dplyr::bind_rows(rand = size_rand, attr = size_attr, 
                               .id = "movement") %>% 
  dplyr::left_join(experiment, by = "id") %>% 
  dplyr::group_by(movement, measure, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                measure  = factor(measure, levels = c("length", "weight")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

#### Setup ggplots ####

# print 2 digits on y-axsis
scale_fun_a <- function(x) sprintf("%.4f", x)

scale_fun_b <- function(x) sprintf("%.0f", x)

# function to create 5 ticks on y axis
breaks_fun <- function(x) seq(from = 0, to = max(x), length.out = 5)

# font size
base_size <- 8

# margins
mar <- c(t = 0, r = 2, b = 0, l = 2)

# set position dodge
pd <- position_dodge(width = 0.25)

# point shape 
shape <- 20

pd <- position_dodge(width = 0.5)

#### ggplot total bimass ####

lab_pop_n <- as_labeller(c(`1` = "1 individuals", `2` = "2 individuals", 
                           `4` = "4 individuals", `8` = "8 individuals", 
                           `16` = "16 individuals", `32` = "32 individuals"))

gg_total_size <- ggplot(data = size_total) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = nutrients_pool, y = mean, fill = movement), width = 0.45, position = pd) + 
  geom_linerange(aes(x = nutrients_pool, ymin = mean - se, ymax = mean + se, 
                     col = movement, group = movement), position = pd, size = 0.5) +
  facet_wrap(. ~ pop_n, ncol = 6, labeller = lab_pop_n) + 
  guides(col = FALSE) +
  scale_y_continuous(labels = scale_fun_a, breaks = seq(0, 0.8, length.out = 5), 
                     limits = c(0, 0.8)) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("Random movement", "Attraction towards AR")) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20")) +
  labs(x = "", y = "Mean fish biomass [g/sqm]") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "none", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), 
         axis.text.x = element_blank(),
        plot.margin = margin(mar))

#### Create ggplot excretion ####

gg_total_excretion <- ggplot(data = excretion_total) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = nutrients_pool, y = mean, fill = movement), width = 0.45, position = pd) + 
  geom_linerange(aes(x = nutrients_pool, ymin = mean - se, ymax = mean + se, 
                     col = movement, group = movement), position = pd, size = 0.5) +
  facet_wrap(. ~ pop_n, ncol = 6) + 
  guides(col = FALSE) +
  scale_y_continuous(labels = scale_fun_b, breaks = seq(0, 95000, length.out = 5), 
                     limits = c(0, 95000)) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("Random movement", "Attraction towards AR")) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20")) +
  labs(x = "Starting nutrient pool [g/cell]", y = "Total fish excretion [g]") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", 
        strip.text = element_blank(), strip.background = element_blank(),
        plot.margin = margin(mar))

### ggplot mortality ####

# lab_measure_n <- as_labeller(c("died_background_1" = "Background mortality",
#                                "died_background_2" = "", "died_background_4" = "",
#                                "died_background_8" = "", "died_background_16" = "",
#                                "died_background_32" = "",
#                                "died_consumption_1" = "Consumption mortality",
#                                "died_consumption_2" = "", "died_consumption_4" = "",
#                                "died_consumption_8" = "", "died_consumption_16" = "",
#                                "died_consumption_32" = "",
#                                "ttl_1" = "Total mortality",
#                                "ttl_2" = "", "ttl_4" = "",
#                                "ttl_8" = "", "ttl_16" = "", "ttl_32" = ""))
# 
# gg_total_mortality <- ggplot(data = mortality_total) +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
#   geom_col(aes(x = nutrients_pool, y = mean, fill = movement), width = 0.45, position = pd) +
#   geom_linerange(aes(x = nutrients_pool, ymin = mean - se, ymax = mean + se,
#                      col = movement, group = movement), position = pd, size = 0.5) +
#   facet_wrap(. ~ measure_n + pop_n, ncol = 6,
#              labeller = labeller(measure_n = lab_measure_n, pop_n = lab_pop_n))  +
#   guides(col = FALSE) +
#   scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"),
#                     labels = c("No AR", "Attraction towards AR")) +
#   scale_color_manual(name = "", values = c("#46ACC8", "#B40F20")) +
#   labs(x = "Starting nutrient pool [g/cell]", y = "Mean mortality") +
#   theme_classic(base_size = 10) +
#   theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
#         strip.background = element_blank())

gg_biomass_excr <- cowplot::plot_grid(gg_total_size, gg_total_excretion,
                                      ncol = 1, nrow = 2)

### Save ggplot ####

# set defaults for plotting
overwrite <- FALSE

suppoRt::save_ggplot(plot = gg_biomass_excr, filename = "gg_biomass_excr.pdf",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = overwrite)

# suppoRt::save_ggplot(plot = gg_total_excretion, filename = "gg_total_excretion.png",
#                      path = "04_Figures/03_simulation_experiment/",
#                      width = width, height = height * 0.5, dpi = dpi, units = units,
#                      overwrite = overwrite)
# 
# suppoRt::save_ggplot(plot = gg_total_mortality, filename = "gg_total_mortality.png",
#                      path = "04_Figures/03_simulation_experiment/",
#                      width = width, height = height * 0.5, dpi = dpi, units = units,
#                      overwrite = overwrite)
# 
# suppoRt::save_ggplot(plot = gg_total_size, filename = "gg_total_size.png",
#                      path = "04_Figures/03_simulation_experiment/",
#                      width = width, height = height * 0.5, dpi = dpi, units = units,
#                      overwrite = overwrite)
