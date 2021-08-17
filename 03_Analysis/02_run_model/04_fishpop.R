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

sim_experiment <- readr::read_rds("02_Data/02_Modified/02_run_model/sim_experiment.rds")

model_runs <- readr::read_rds("02_Data/02_Modified/02_run_model/model-runs_-25_2.rds")

#### Preprocess and load data #### 

timestep <- 219000

# add row id to sim_experiment
sim_experiment <- dplyr::mutate(sim_experiment, 
                                id = 1:nrow(sim_experiment), .before = "starting_biomass", 
                                starting_biomass = starting_biomass * 100)

#### Calculate total consumption and excretion ####

# calculate total excretion rand/attr
excretion_rand <- purrr::map_dfr(model_runs, function(i) {
  magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_excretion(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

excretion_attr <- purrr::map_dfr(model_runs, function(i) {
  magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_excretion(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

# combine to one data.frame
excretion_combined <- dplyr::bind_rows(rand = excretion_rand, attr = excretion_attr, 
                                       .id = "movement") %>% 
  dplyr::left_join(sim_experiment, by = "id") %>% 
  dplyr::select(-measure) %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                starting_biomass = factor(starting_biomass, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

# summarize excretion
excretion_sum <- dplyr::group_by(excretion_combined, movement, starting_biomass, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop")

# calc t test between move for starting, pop_n
excretion_signif <- dplyr::group_by(excretion_combined, starting_biomass, pop_n) %>% 
  dplyr::group_split() %>% 
  purrr::map_dfr(function(i) {
    
    rand <- dplyr::filter(i, movement == "rand")
    attr <- dplyr::filter(i, movement == "attr")
    
    t_test <- t.test(x = rand$value, y = attr$value, alternative = "two.sided", 
                     conf.level = 0.95)
    
    tibble::tibble(starting_biomass = unique(i$starting_biomass), pop_n = unique(i$pop_n), 
                   p_value = t_test$p.value)}) %>% 
  dplyr::mutate(signif_lvl = dplyr::case_when(p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
                                              p_value < 0.05 ~ "*", TRUE ~ "")) %>% 
  dplyr::left_join(excretion_sum, by = c("starting_biomass", "pop_n")) %>% 
  dplyr::group_by(starting_biomass, pop_n) %>% 
  dplyr::filter(mean == max(mean))

#### Calculate fish biomass ####

size_rand <- purrr::map_dfr(model_runs, function(i) {
  magrittr::extract2(i, "rand") %>%
    magrittr::extract2("fishpop") %>% 
    calc_fish_size(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

size_attr <- purrr::map_dfr(model_runs, function(i) {
  magrittr::extract2(i, "attr") %>%
    magrittr::extract2("fishpop") %>% 
    calc_fish_size(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

# combine to one data.frame
size_combined <- dplyr::bind_rows(rand = size_rand, attr = size_attr, .id = "movement") %>% 
  dplyr::left_join(sim_experiment, by = "id") %>% 
  dplyr::filter(measure == "weight") %>% 
  dplyr::select(-measure) %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                starting_biomass = factor(starting_biomass, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

# summarize size
size_sum <- dplyr::group_by(size_combined, movement, starting_biomass, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop") 

# calc t_test
size_signif <- dplyr::group_by(size_combined, starting_biomass, pop_n) %>% 
  dplyr::group_split() %>% 
  purrr::map_dfr(function(i) {
    
    rand <- dplyr::filter(i, movement == "rand")
    attr <- dplyr::filter(i, movement == "attr")
    
    t_test <- t.test(x = rand$value, y = attr$value, alternative = "two.sided", 
                     conf.level = 0.95)
    
    tibble::tibble(starting_biomass = unique(i$starting_biomass), pop_n = unique(i$pop_n), 
                   p_value = t_test$p.value)}) %>% 
  dplyr::mutate(signif_lvl = dplyr::case_when(p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
                                              p_value < 0.05 ~ "*", TRUE ~ "")) %>% 
  dplyr::left_join(size_sum, by = c("starting_biomass", "pop_n")) %>% 
  dplyr::group_by(starting_biomass, pop_n) %>% 
  dplyr::filter(mean == max(mean))

#### Setup ggplots ####

lab_pop_n <- as_labeller(c(`1` = "1 individual", `2` = "2 individuals", 
                           `4` = "4 individuals", `8` = "8 individuals", 
                           `16` = "16 individuals", `32` = "32 individuals"))

lab_pop_n_empty <- as_labeller(c(`1` = "", `2` = "", 
                                 `4` = "", `8` = "", 
                                 `16` = "", `32` = ""))

# print 2 digits on y-axsis
scale_fun_a <- function(x) sprintf("%.2f", x)

scale_fun_b <- function(x) sprintf("%.1f", x)

# function to create 5 ticks on y axis
breaks_fun <- function(x) seq(from = 0, to = max(x), length.out = 5)

# margins
mar <- c(t = 0, r = 2, b = 0, l = 2)

# set position dodge
pd <- position_dodge(width = 0.5)

bar_width <- 0.45

base_size <- 10

size_line <- 0.25

size_label <- 5

#### ggplot total bimass ####

limits <- dplyr::mutate(size_sum, 
                        pop_n_class = dplyr::case_when(pop_n %in% c(1, 2, 4) ~ "low", 
                                                       pop_n %in% c(8, 16, 32) ~ "high"), 
                        pop_n_class = factor(pop_n_class, levels = c("low", "high"))) %>% 
  dplyr::group_by(pop_n_class) %>% 
  dplyr::summarise(value = max(c(mean - se, mean + se)), .groups = "drop") %>% 
  dplyr::mutate(value = ceiling(value / 0.25) * 0.25)

data_temp <- dplyr::filter(size_sum, pop_n %in% c(1, 2, 4))
signif_temp <- dplyr::filter(size_signif, pop_n %in% c(1, 2, 4))

gg_total_size_a <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = starting_biomass, y = mean, fill = movement), 
           width = bar_width, position = pd) + 
  geom_linerange(aes(x = starting_biomass, ymin = mean - se, ymax = mean + se, 
                     col = movement, group = movement), position = pd, size = size_line) +
  geom_text(data = signif_temp, size = size_label,
            aes(x = starting_biomass, y = mean + se, label = signif_lvl, col = movement)) +
  facet_wrap(. ~ pop_n, ncol = 3, labeller = lab_pop_n) + 
  scale_y_continuous(labels = scale_fun_a, breaks = seq(0, limits$value[1], length.out = 5),
                     limits = c(0, limits$value[1])) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("Random movement", "Attracted movement")) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20")) +
  guides(col = "none") +
  labs(x = "", y = "Total fish biomass [g]") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

data_temp <- dplyr::filter(size_sum, pop_n %in% c(8, 16, 32))
signif_temp <- dplyr::filter(size_signif, pop_n %in% c(8, 16, 32))

gg_total_size_b <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = starting_biomass, y = mean, fill = movement), 
           width = bar_width, position = pd) + 
  geom_linerange(aes(x = starting_biomass, ymin = mean - se, ymax = mean + se, 
                     col = movement, group = movement), position = pd, size = size_line) +
  geom_text(data = signif_temp, size = size_label,
            aes(x = starting_biomass, y = mean + se, label = signif_lvl, col = movement)) +
  facet_wrap(. ~ pop_n, ncol = 3, labeller = lab_pop_n) + 
  scale_y_continuous(labels = scale_fun_a, breaks = seq(0, limits$value[2], length.out = 5),
                     limits = c(0, limits$value[2])) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("Random movement", "Attracted movement")) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20")) +
  guides(col = "none") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

#### Create ggplot excretion ####

limits <- dplyr::mutate(excretion_sum, 
                        pop_n_class = dplyr::case_when(pop_n %in% c(1, 2, 4) ~ "low", 
                                                       pop_n %in% c(8, 16, 32) ~ "high"), 
                        pop_n_class = factor(pop_n_class, levels = c("low", "high"))) %>% 
  dplyr::group_by(pop_n_class) %>% 
  dplyr::summarise(value = max(c(mean - se, mean + se)), .groups = "drop") %>% 
  dplyr::mutate(value = ceiling(value / 0.25) * 0.25)

data_temp <- dplyr::filter(excretion_sum, pop_n %in% c(1, 2, 4))
signif_temp <- dplyr::filter(excretion_signif, pop_n %in% c(1, 2, 4))

gg_total_excretion_a <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = starting_biomass, y = mean, fill = movement), 
           width = bar_width, position = pd) + 
  geom_linerange(aes(x = starting_biomass, ymin = mean - se, ymax = mean + se, 
                     col = movement, group = movement), position = pd, size = size_line) +
  geom_text(data = signif_temp, size = size_label,
            aes(x = starting_biomass, y = mean + se, label = signif_lvl, col = movement)) +
  facet_wrap(. ~ pop_n, ncol = 3, labeller = lab_pop_n_empty) + 
  scale_y_continuous(labels = scale_fun_a, breaks = seq(0, limits$value[1], length.out = 5),
                     limits = c(0, limits$value[1])) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("Random movement", "Attracted movement")) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20")) +
  guides(col = "none") +
  labs(x = "", y = "Total nutrients excretion [g]") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

data_temp <- dplyr::filter(excretion_sum, pop_n %in% c(8, 16, 32))
signif_temp <- dplyr::filter(excretion_signif, pop_n %in% c(8, 16, 32))

gg_total_excretion_b <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = starting_biomass, y = mean, fill = movement), 
           width = bar_width, position = pd) + 
  geom_linerange(aes(x = starting_biomass, ymin = mean - se, ymax = mean + se, 
                     col = movement, group = movement), position = pd, size = size_line) +
  geom_text(data = signif_temp, size = size_label,
            aes(x = starting_biomass, y = mean + se, label = signif_lvl, col = movement)) +
  facet_wrap(. ~ pop_n, ncol = 3, labeller = lab_pop_n_empty) + 
  scale_y_continuous(labels = scale_fun_a, breaks = seq(0, limits$value[2], length.out = 5),
                     limits = c(0, limits$value[2])) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("Random movement", "Attracted movement")) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20")) +
  guides(col = "none") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

#### Create full figure ####

legend <- get_legend(gg_total_excretion_b)

gg_biomass_excr <- cowplot::plot_grid(gg_total_size_a + theme(legend.position = "none"),
                                      gg_total_size_b + theme(legend.position = "none"),
                                      gg_total_excretion_a + theme(legend.position = "none"),
                                      gg_total_excretion_b + theme(legend.position = "none"),
                                      ncol = 2, nrow = 2)

gg_biomass_excr <- plot_grid(gg_biomass_excr, legend,
                             ncol = 1, nrow = 2, rel_heights = c(1, 0.1)) + 
  draw_label(label = "Starting capacity biomass [%]", y = 0.095, size = base_size)

### Save ggplot ####

filename <- (model_runs[[1]]$rand$parameters$seagrass_thres * 100) %>% 
  paste0("gg-fish-excr_", ., "_", 
         model_runs[[1]]$rand$parameters$seagrass_slope) %>% 
  stringr::str_replace(pattern = "\\.", replacement = "") %>% 
  paste0(".pdf")

suppoRt::save_ggplot(plot = gg_biomass_excr, filename = filename,
                     path = "04_Figures/02_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = FALSE)
