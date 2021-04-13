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
# Analyze results for increasing fish population and nutrients levels and distance

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/calc_seagrass_values.R")

experiment <- readr::read_rds("02_Data/02_Modified/03_run_model/sim_experiment.rds")

model_runs <- readr::read_rds("02_Data/02_Modified/03_run_model/model_runs.rds")

#### Preprocess data #### 

norm <- FALSE

clss_width <- 5

# add row id to sim_experiment
experiment <- dplyr::mutate(experiment, id = as.character(1:nrow(experiment)), 
                            .before = "nutrients_pool")

#### Calculate total biomass ####

# calculate total biomass rand/attr
biomass_rand <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_biomass(norm = norm, clss_width = clss_width), .id = "id")

biomass_attr <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_biomass(norm = norm, clss_width = clss_width), .id = "id")

# combine data.frames
biomass_wide <- dplyr::left_join(x = biomass_rand, y = biomass_attr, 
                                 by = c("id", "dist_clss", "part"), 
                                 suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(experiment, by = "id")

# calculate total bg/ag biomass
biomass_wide_ttl <- dplyr::group_by(biomass_wide, 
                                    id, dist_clss, nutrients_pool, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_biomass")

#### Calculate total production ####

# calculate total production rand/attr
production_rand <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_production(norm = norm, clss_width = clss_width), .id = "id")

production_attr <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_production(norm = norm, clss_width = clss_width), .id = "id")

# combine data.frames
production_wide <- dplyr::left_join(x = production_rand, y = production_attr, 
                                    by = c("id", "dist_clss", "part"), 
                                    suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(experiment, by = "id")

# calculate total bg/ag production
production_wide_ttl <- dplyr::group_by(production_wide, 
                                       id, dist_clss, nutrients_pool, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_production")

#### Response ratios after max_i ####

pb <- progress::progress_bar$new(total = 2520, width = 60,
                                 format = " Progress [:bar] :percent Remaining: :eta")

repetitions <- 1000

response_ratios <- dplyr::bind_rows(biomass = biomass_wide, 
                                    biomass_ttl = biomass_wide_ttl,
                                    production = production_wide, 
                                    production_ttl = production_wide_ttl) %>% 
  # dplyr::filter(pop_n %in% c(2, 8, 32)) %>% 
  dplyr::group_by(dist_clss, part, nutrients_pool, pop_n) %>% 
  dplyr::group_split() %>%  # length()
  purrr::map_dfr(function(i) {
    
    pb$tick()
    
    bootstrap <- boot::boot(data = tibble::tibble(rand = i$value.rand, 
                                                  attr = i$value.attr), 
                            statistic = log_response, R = repetitions)
    
    bootstrap_ci <- boot::boot.ci(bootstrap, type = "norm", conf = 0.95)
    
    tibble(part = unique(i$part), dist_clss = unique(i$dist_clss),
           pop_n = unique(i$pop_n), nutrients_pool = unique(i$nutrients_pool),
           mean = mean(bootstrap$t[, 1]), 
           lo = bootstrap_ci$normal[2], 
           hi = bootstrap_ci$normal[3])}) %>% 
  tidyr::separate(col = part, into = c("part", "measure"), sep = "_") %>% 
  dplyr::mutate(part = factor(part, levels = c("ag", "bg", "ttl")), 
                measure = factor(measure, levels = c("biomass", "production")), 
                pop_n = factor(pop_n, ordered = TRUE), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE), 
                part_n = paste(part, pop_n, sep = "_"),
                part_n = factor(part_n, levels = stringr::str_sort(unique(part_n), 
                                                                   numeric = TRUE)), 
                part_nutr = paste(part, nutrients_pool, sep = "_"),
                part_nutr = factor(part_nutr, levels = stringr::str_sort(unique(part_nutr)))) 


#### Table ####

complete_table_rel_dist <- purrr::map_dfr(model_runs, calc_rel_diff_dist, 
                                          breaks = c(0, 1, 2, 3, 4, 5, 27.5, 32.5), .id = "id") %>% 
  dplyr::filter(class_dist != "(5,27.5]") %>% 
  dplyr::mutate(class_dist = factor(class_dist, labels = c("1", "2", "3", "4", "5", "30"))) %>% 
  dplyr::left_join(experiment, by = "id") %>%
  dplyr::group_by(class_dist, name, nutrients_pool, pop_n) %>% 
  dplyr::summarise(value = mean(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = c(name, class_dist), values_from = value) %>% 
  dplyr::arrange(pop_n, nutrients_pool) # %>% 
  # dplyr::select(pop_n, nutrients_pool,
  #               ag_biomass_5, ag_biomass_30, 
  #               ag_production_5, ag_production_30, 
  #               bg_biomass_5, bg_biomass_30, 
  #               bg_production_5, bg_production_30) %>%
  # dplyr::mutate_at(dplyr::vars(dplyr::starts_with(c("ag_", "bg_"))),
  #                  formatC, digits = 2, format = "e")

readr::write_delim(complete_table_rel_dist, 
                   file = "02_Data/02_Modified/03_run_model/complete_table_rel_dist.csv",
                   delim = ";")

#### Setup ggplot ####

# function to create 5 ticks on y axis
breaks_fun <- function(x) seq(from = min(x), to = max(x), length.out = 5)

# font size
base_size <- 8

# margins
mar <- c(t = 0, r = 2, b = 0, l = 2)

# set position dodge
pd <- position_dodge(width = 0.25)

# point shape 
shape <- 20

# create x labels
labels_x <- seq(from = clss_width,
                to = length(unique(response_ratios$dist_clss)) * clss_width, 
                by = clss_width)

# # print each labels
# n <- 2
# 
# # replace every 2nd label
# labels_x[seq(n, length(labels_x), n)] <- ""

# set position dodge
pd <- position_dodge(width = 0.25)

# create labeller for panales
lab_part <- as_labeller(c("ag" = "Aboveground value", "bg" = "Belowground value", 
                          "ttl" = "Total value"))

lab_part_n <- as_labeller(c("ag_1" = "Aboveground value",
                            "ag_2" = "", "ag_4" = "",
                            "ag_8" = "", "ag_16" = "", "ag_32" = "",
                            "bg_1" = "Belowground value",
                            "bg_2" = "", "bg_4" = "",
                            "bg_8" = "", "bg_16" = "", "bg_32" = "",
                            "ttl_1" = "Total value",
                            "ttl_2" = "", "ttl_4" = "",
                            "ttl_8" = "", "ttl_16" = "", "ttl_32" = ""))

lab_pop_n <- as_labeller(c(`1` = "1 individuals", `2` = "2 individuals", 
                           `4` = "4 individuals", `8` = "8 individuals", 
                           `16` = "16 individuals", `32` = "32 individuals"))

lab_pop_n_emtpy <- as_labeller(c(`1` = "", `2` = "", 
                                 `4` = "", `8` = "", 
                                 `16` = "", `32` = ""))

# # get absolute largest values
# limits_full <- dplyr::group_by(response_ratios, part) %>% 
#   dplyr::summarise(l = max(abs(c(lo, hi))))

#### Full design production ####

# print 2 digits on y-axsis
scale_fun <- function(x) sprintf("%.2f", x)

gg_full_ag_a_prod <- ggplot(data = filter(response_ratios, part == "ag", 
                                          measure == "production", 
                                          pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.3, 0.8, length.out = 5), 
                     limits = c(-0.3, 0.8)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_ag_b_prod <- ggplot(data = filter(response_ratios, part == "ag", 
                                          measure == "production", 
                                          pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-1.2, 13, length.out = 5), 
                     limits = c(-1.2, 13)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_bg_a_prod <- ggplot(data = filter(response_ratios, part == "bg", 
                                          measure == "production", 
                                          pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.2, 1.1, length.out = 5), 
                     limits = c(-0.2, 1.1)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "Log response ratios") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_bg_b_prod <- ggplot(data = filter(response_ratios, part == "bg", 
                                          measure == "production", 
                                          pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-1.05, 2.5, length.out = 5), 
                     limits = c(-1.05, 2.5)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_ttl_a_prod <- ggplot(data = filter(response_ratios, part == "ttl", 
                                           measure == "production", 
                                           pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.2, 1.1, length.out = 5), 
                     limits = c(-0.2, 1.1)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

gg_full_ttl_b_prod <- ggplot(data = filter(response_ratios, part == "ttl", 
                                           measure == "production", 
                                           pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-1.05, 2.75, length.out = 5), 
                     limits = c(-1.05, 2.75)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

legend_prod <- get_legend(
  gg_full_ttl_b_prod
)

gg_full_design_prod <- cowplot::plot_grid(gg_full_ag_a_prod + theme(legend.position = "none"),
                                          gg_full_ag_b_prod + theme(legend.position = "none"),
                                          gg_full_bg_a_prod + theme(legend.position = "none"),
                                          gg_full_bg_b_prod + theme(legend.position = "none"), 
                                          gg_full_ttl_a_prod + theme(legend.position = "none"),  
                                          gg_full_ttl_b_prod + theme(legend.position = "none"), 
                                          ncol = 2, nrow = 3)

gg_full_design_prod <- plot_grid(gg_full_design_prod, legend_prod,
                                 ncol = 1, nrow = 2, rel_heights = c(1, 0.1)) + 
  draw_label(label = "Distance to reef [m]", y = 0.095, size = base_size)

#### Full design biomass ####

scale_fun <- function(x) sprintf("%.4f", x)

gg_full_ag_a_biom <- ggplot(data = filter(response_ratios, part == "ag", 
                                          measure == "biomass", 
                                          pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean,
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)),
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.0005, 0.0015, length.out = 5), 
                     limits = c(-0.0005, 0.0015)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_ag_b_biom <- ggplot(data = filter(response_ratios, part == "ag", 
                                          measure == "biomass", 
                                          pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.2, 3.25, length.out = 5),
                     limits = c(-0.2, 3.25)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_bg_a_biom <- ggplot(data = filter(response_ratios, part == "bg", 
                                          measure == "biomass", 
                                          pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.1, 0.35, length.out = 5),
                     limits = c(-0.1, 0.35)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "Log response ratios") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_bg_b_biom <- ggplot(data = filter(response_ratios, part == "bg", 
                                          measure == "biomass", 
                                          pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.3, 0.9, length.out = 5),
                     limits = c(-0.3, 0.9)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_ttl_a_biom <- ggplot(data = filter(response_ratios, part == "ttl", 
                                           measure == "biomass", 
                                           pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.05, 0.35, length.out = 5),
                     limits = c(-0.05, 0.35)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

gg_full_ttl_b_biom <- ggplot(data = filter(response_ratios, part == "ttl", 
                                           measure == "biomass", 
                                           pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = dist_clss, y = mean, 
                group = interaction(nutrients_pool, measure)),
            col = "lightgrey", position = pd) +
  geom_point(aes(x = dist_clss, y = mean, col = nutrients_pool), shape = shape, 
             position = pd) +
  geom_linerange(aes(x = dist_clss, ymin = lo, ymax = hi,
                     col = nutrients_pool, group = interaction(nutrients_pool, measure)), 
                 position = pd) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3,
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_emtpy))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.3, 1.1, length.out = 5),
                     limits = c(-0.3, 1.1)) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting nutrient pool [g/cell]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

legend_biom <- get_legend(
  gg_full_ttl_b_biom
)

gg_full_design_biom <- cowplot::plot_grid(gg_full_ag_a_biom + theme(legend.position = "none"),
                                          gg_full_ag_b_biom + theme(legend.position = "none"),
                                          gg_full_bg_a_biom + theme(legend.position = "none"),
                                          gg_full_bg_b_biom + theme(legend.position = "none"), 
                                          gg_full_ttl_a_biom + theme(legend.position = "none"),  
                                          gg_full_ttl_b_biom + theme(legend.position = "none"), 
                                          ncol = 2, nrow = 3)

gg_full_design_biom <- plot_grid(gg_full_design_biom, legend_biom,
                                 ncol = 1, nrow = 2, rel_heights = c(1, 0.1)) + 
  draw_label(label = "Distance to reef [m]", y = 0.095, size = base_size)

### Save ggplot ####

overwrite <- T

suppoRt::save_ggplot(plot = gg_full_design_prod, filename = "gg_full_design_dist_prod.pdf",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_full_design_biom, filename = "gg_full_design_dist_biom.pdf",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = overwrite)
