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
# Analyze results for increasing fish population and nutrients levels

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/calc_seagrass_values.R")

experiment <- readr::read_rds("02_Data/02_Modified/03_run_model/sim_experiment.rds")

model_runs <- readr::read_rds("02_Data/02_Modified/03_run_model/model_runs.rds")

#### Preprocess data #### 

# add row id to sim_experiment
experiment <- dplyr::mutate(experiment, id = as.character(1:nrow(experiment)), 
                            .before = "nutrients_pool")

norm <- FALSE

#### Calculate total biomass increase ####

# calculate total biomass rand/attr
biomass_rand <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_inc_biomass(norm = norm), .id = "id")

biomass_attr <- purrr::map_dfr(model_runs, function(i) 
  magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_inc_biomass(norm = norm), .id = "id")

biomass_total <- dplyr::bind_rows(rand = biomass_rand, attr = biomass_attr, 
                                  .id = "movement") %>% 
  dplyr::left_join(experiment, by = "id") %>% 
  dplyr::group_by(movement, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                part  = factor(part, levels = c("ag_biomass", "bg_biomass", "ttl_biomass")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

biomass_sum <- dplyr::bind_rows(rand = biomass_rand, attr = biomass_attr, 
                                .id = "movement") %>% 
  dplyr::group_by(movement, id) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop") %>% 
  dplyr::left_join(experiment, by = "id") %>% 
  dplyr::group_by(movement, nutrients_pool, pop_n) %>% 
  dplyr::summarise(mean = mean(value), se = 1.96 * sd(value) / sqrt(dplyr::n()), 
                   .groups = "drop") %>% 
  dplyr::mutate(movement = factor(movement, levels = c("rand", "attr")), 
                part = "ttl_biomass",
                part  = factor(part, levels = c("ag_biomass", "bg_biomass", "ttl_biomass")), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE), 
                pop_n = factor(pop_n, ordered = TRUE))

biomass_total <- dplyr::bind_rows(x = biomass_total, y = biomass_sum) %>% 
  dplyr::mutate(part_n = paste(part, pop_n, sep = "_"),
                part_n = factor(part_n, 
                                levels = stringr::str_sort(unique(part_n), 
                                                           numeric = TRUE)))

#### Create ggplots ####

pd <- position_dodge(width = 0.5)

width_col <- 0.45

lab_pop_n <- as_labeller(c(`1` = "1 individuals", `2` = "2 individuals", 
                           `4` = "4 individuals", `8` = "8 individuals", 
                           `16` = "16 individuals"))

lab_part_n <- as_labeller(c("ag_biomass_1" = "Aboveground value", 
                            "ag_biomass_2" = "", "ag_biomass_4" = "", 
                            "ag_biomass_8" = "", "ag_biomass_16" = "", 
                            "bg_biomass_1" = "Belowground value", 
                            "bg_biomass_2" = "", "bg_biomass_4" = "", 
                            "bg_biomass_8" = "", "bg_biomass_16" = "", 
                            "ttl_biomass_1" = "Total value", 
                            "ttl_biomass_2" = "", "ttl_biomass_4" = "", 
                            "ttl_biomass_8" = "", "ttl_biomass_16" = ""))

# get absolute largest values
limits_full <- dplyr::group_by(biomass_total, part) %>% 
  dplyr::summarise(lim = max(abs(c(mean - se, mean + se))))

gg_full_ag <- ggplot(data = filter(biomass_total, part == "ag_biomass")) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = nutrients_pool, y = mean, fill = movement),
           position = pd, width = width_col) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 5, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(limits = c(-limits_full[[1, 2]], limits_full[[1, 2]])) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("No AR", "Attraction towards AR")) +
  guides(fill = FALSE) + 
  labs(x = "", y = "") + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank())

gg_full_bg <- ggplot(data = filter(biomass_total, part == "bg_biomass")) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = nutrients_pool, y = mean, fill = movement),
           position = pd, width = width_col) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 5, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(limits = c(0, limits_full[[2, 2]])) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("No AR", "Attraction towards AR")) +
  guides(fill = FALSE) +
  labs(x = "", y = "Total biomass increase") + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank())

gg_full_ttl <- ggplot(data = filter(biomass_total, part == "ttl_biomass")) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_col(aes(x = nutrients_pool, y = mean, fill = movement),
           position = pd, width = width_col) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 5, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(limits = c(0, limits_full[[3, 2]])) +
  scale_fill_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                    labels = c("No AR", "Attraction towards AR")) +
  guides(col = FALSE) + 
  labs(x = "Starting nutrient pool [g/cell]", y = "") + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank())

gg_full_design <- cowplot::plot_grid(gg_full_ag, gg_full_bg, gg_full_ttl, 
                                     ncol = 1, nrow = 3)

### Save ggplot ####

suppoRt::save_ggplot(plot = gg_full_design, filename = "gg_biomass_inc.png",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = FALSE)
