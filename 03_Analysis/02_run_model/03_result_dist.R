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

sim_experiment <- readr::read_rds("02_Data/02_Modified/02_run_model/sim_experiment.rds")

model_runs <- readr::read_rds("02_Data/02_Modified/02_run_model/model-runs_-25_2.rds")

#### Preprocess data #### 

# add row id to sim_experiment
sim_experiment <- dplyr::mutate(sim_experiment, 
                                id = 1:nrow(sim_experiment), 
                                .before = "starting_biomass",
                                starting_biomass = starting_biomass * 100)

norm <- FALSE

class_width <- 5

#### Calc total excretion

if (norm) {

  excretion_rand <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "rand") %>%
      magrittr::extract2("seafloor") %>% 
      calc_total_excretion()}, .id = "id") %>% 
    dplyr::mutate(id = as.numeric(id)) %>% 
    dplyr::left_join(sim_experiment, by = "id")
  
  excretion_attr <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "attr") %>%
      magrittr::extract2("seafloor") %>% 
      calc_total_excretion()}, .id = "id") %>% 
    dplyr::mutate(id = as.numeric(id)) %>% 
    dplyr::left_join(sim_experiment, by = "id")

}

#### Calculate total biomass ####

# calculate total biomass rand/attr
biomass_rand <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_biomass(class_width = class_width)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id)) %>% 
  dplyr::left_join(sim_experiment, by = "id")

biomass_attr <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_biomass(class_width = class_width)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id)) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# normalize by excretion
if (norm) {
  
  excretion_rand <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "rand") %>%
      magrittr::extract2("seafloor") %>% 
      calc_total_excretion()}, .id = "id") %>% 
    dplyr::mutate(id = as.numeric(id)) %>% 
    dplyr::left_join(sim_experiment, by = "id")
  
  biomass_rand <- dplyr::left_join(biomass_rand, excretion_rand, 
                                   by = c("id", "starting_biomass", "pop_n"),
                   suffix = c(".biom", ".excr")) %>% 
    dplyr::mutate(value = value.biom / value.excr) %>% 
    dplyr::select(-c(value.biom, value.excr))
  
  excretion_attr <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "attr") %>%
      magrittr::extract2("seafloor") %>% 
      calc_total_excretion()}, .id = "id") %>% 
    dplyr::mutate(id = as.numeric(id)) %>% 
    dplyr::left_join(sim_experiment, by = "id")
  
  biomass_attr <- dplyr::left_join(biomass_attr, excretion_attr, 
                                   by = c("id", "starting_biomass", "pop_n"),
                                   suffix = c(".biom", ".excr")) %>% 
    dplyr::mutate(value = value.biom / value.excr) %>% 
    dplyr::select(-c(value.biom, value.excr))
  
}

# combine data.frames
biomass_wide <- dplyr::left_join(x = biomass_rand, y = biomass_attr, 
                                 by = c("id", "dist_class", "part"), 
                                 suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# calculate total bg/ag biomass
biomass_wide_ttl <- dplyr::group_by(biomass_wide, 
                                    id, dist_class, starting_biomass, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_biomass")

#### Calculate total production ####

# calculate total production rand/attr
production_rand <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_production(class_width = class_width)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id)) %>% 
  dplyr::left_join(sim_experiment, by = "id")

production_attr <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_dist_production(class_width = class_width)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id)) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# normalize by excretion
if (norm) {
  
  production_rand <- dplyr::left_join(production_rand, excretion_rand, 
                                      by = c("id", "starting_biomass", "pop_n"),
                                      suffix = c(".prod", ".excr")) %>% 
    dplyr::mutate(value = value.prod / value.excr) %>% 
    dplyr::select(-c(value.prod, value.excr))
  
  production_attr <- dplyr::left_join(production_attr, excretion_attr, 
                                      by = c("id", "starting_biomass", "pop_n"),
                                      suffix = c(".prod", ".excr")) %>% 
    dplyr::mutate(value = value.prod / value.excr) %>% 
    dplyr::select(-c(value.prod, value.excr))
  
}

# combine data.frames
production_wide <- dplyr::left_join(x = production_rand, y = production_attr, 
                                    by = c("id", "dist_class", "part"), 
                                    suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# calculate total bg/ag production
production_wide_ttl <- dplyr::group_by(production_wide, 
                                       id, dist_class, starting_biomass, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_production")

#### Response ratios after max_i ####

ci_quietly <- purrr::quietly(boot::boot.ci)

pb <- progress::progress_bar$new(total = 2160, width = 60,
                                 format = " Progress [:bar] :percent Remaining: :eta")

repetitions <- 1000

response_ratios <- dplyr::bind_rows(biomass = biomass_wide, 
                                    biomass_ttl = biomass_wide_ttl,
                                    production = production_wide, 
                                    production_ttl = production_wide_ttl) %>% 
  # dplyr::filter(pop_n %in% c(2, 8, 32)) %>% 
  dplyr::group_by(dist_class, part, starting_biomass, pop_n) %>% 
  dplyr::group_split() %>%  # length()
  purrr::map_dfr(function(i) {
    
    pb$tick()
    
    bootstrap <- boot::boot(data = tibble::tibble(rand = i$value.rand, 
                                                  attr = i$value.attr), 
                            statistic = log_response, R = repetitions)
    
    bootstrap_ci <- ci_quietly(bootstrap, type = "norm", conf = 0.95)
      
    boot_mean <- mean(bootstrap$t[, 1])
    
    lo <- ifelse(test = is.null(bootstrap_ci$result), 
                 yes = boot_mean, no = bootstrap_ci$result$normal[2])
    
    hi <- ifelse(test = is.null(bootstrap_ci$result), 
                 yes = boot_mean, no = bootstrap_ci$result$normal[3])
    
    tibble(part = unique(i$part), dist_class = unique(i$dist_class),
           pop_n = unique(i$pop_n), starting_biomass = unique(i$starting_biomass),
           mean = boot_mean, lo = lo, hi = hi)}) %>% 
  tidyr::separate(col = part, into = c("part", "measure"), sep = "_") %>% 
  dplyr::mutate(part = factor(part, levels = c("ag", "bg", "ttl")), 
                measure = factor(measure, levels = c("biomass", "production")), 
                pop_n = factor(pop_n, ordered = TRUE), 
                starting_biomass = factor(starting_biomass, ordered = TRUE), 
                part_n = paste(part, pop_n, sep = "_"),
                part_n = factor(part_n, levels = stringr::str_sort(unique(part_n), 
                                                                   numeric = TRUE)), 
                part_nutr = paste(part, starting_biomass, sep = "_"),
                part_nutr = factor(part_nutr, levels = stringr::str_sort(unique(part_nutr)))) 

#### Table ####

rel_table_dist <- purrr::map_dfr(model_runs, calc_rel_diff_dist, 
                                 breaks = c(0, 3, 28.5, 31.5, 75), .id = "id") %>% 
  dplyr::filter(class_dist %in% c("(0,3]", "(28.5,31.5]")) %>% 
  dplyr::mutate(id = as.numeric(id), 
                class_dist = factor(class_dist, labels = c("3", "30"))) %>% 
  dplyr::left_join(sim_experiment, by = "id") %>%
  dplyr::group_by(class_dist, name, starting_biomass, pop_n) %>% 
  dplyr::summarise(value = mean(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = c(name, class_dist), values_from = value) %>% 
  dplyr::mutate_at(dplyr::vars(tidyr::starts_with(c("ag_", "bg_"))), round, digits = 0) %>%
  dplyr::arrange(pop_n, starting_biomass) %>% 
  dplyr::select(pop_n, starting_biomass, 
                ag_biomass_3, ag_biomass_30, ag_production_3, ag_production_30, 
                bg_biomass_3, bg_biomass_30, bg_production_3, bg_production_30)

filename <- (model_runs[[1]]$rand$parameters$seagrass_thres * 100) %>% 
  paste0("02_Data/02_Modified/02_run_model/rel-table-dist_", ., "_", 
         model_runs[[1]]$rand$parameters$seagrass_slope) %>% 
  stringr::str_replace(pattern = "\\.", replacement = "") %>% 
  paste0(".csv")
  
readr::write_delim(rel_table_dist, file = filename, delim = ";")

#### Setup ggplot ####

# print 2 digits on y-axsis
scale_fun <- function(x) sprintf("%.2f", x)

# margins
mar <- c(t = 0, r = 2, b = 0, l = 2)

# point shape 
shape <- 20

size_base <- 8

size_text <- 2

size_line <- 0.25

size_point <- 1

# set position dodge
pd <- position_dodge(width = 0.25)

# create x labels
labels_x <- seq(from = class_width,
                to = length(unique(response_ratios$dist_class)) * class_width, 
                by = class_width)

# create labeller for panales
lab_part <- as_labeller(c("ag" = "Aboveground value", "bg" = "Belowground value", 
                          "ttl" = "Total value"))

lab_part_n <- as_labeller(c("ag_1" = "Aboveground",
                            "ag_2" = "", "ag_4" = "",
                            "ag_8" = "", "ag_16" = "", "ag_32" = "",
                            "bg_1" = "Belowground",
                            "bg_2" = "", "bg_4" = "",
                            "bg_8" = "", "bg_16" = "", "bg_32" = "",
                            "ttl_1" = "Total",
                            "ttl_2" = "", "ttl_4" = "",
                            "ttl_8" = "", "ttl_16" = "", "ttl_32" = ""))

lab_pop_n <- as_labeller(c(`1` = "1 indiv.", `2` = "2 indiv.", 
                           `4` = "4 indiv.", `8` = "8 indiv.", 
                           `16` = "16 indiv.", `32` = "32 indiv."))

lab_pop_n_empty <- as_labeller(c(`1` = "", `2` = "", 
                                 `4` = "", `8` = "", 
                                 `16` = "", `32` = ""))



#### Full design production ####

# create figures

# round the next full digit
next_full <- 0.1

# get limits
limits <- dplyr::mutate(response_ratios, 
                        pop_n_class = dplyr::case_when(pop_n %in% c(1, 2, 4) ~ "low", 
                                                       pop_n %in% c(8, 16, 32) ~ "high"), 
                        pop_n_class = factor(pop_n_class, levels = c("low", "high"))) %>% 
  dplyr::filter(measure == "production") %>% 
  dplyr::group_by(part, pop_n_class) %>% 
  dplyr::summarise(min = min(lo), max = max(hi), .groups = "drop") %>% 
  dplyr::mutate(min = floor(min / next_full) * next_full, max = ceiling(max / next_full) * next_full) %>% 
  dplyr::mutate(min = dplyr::case_when(min == 0 ~ -next_full, 
                                       TRUE ~ min), 
                max = dplyr::case_when(max == 0 ~ next_full, 
                                       TRUE ~ max))

data_temp <- dplyr::filter(response_ratios, part == "ag", 
                           measure == "production", pop_n %in% c(1, 2, 4))

gg_full_ag_a_prod <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[1], limits$max[1], length.out = 5), 
                     limits = c(limits$min[1], limits$max[1])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "ag", 
                           measure == "production", pop_n %in% c(8, 16, 32))

gg_full_ag_b_prod <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[2], limits$max[2], length.out = 5), 
                     limits = c(limits$min[2], limits$max[2])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "bg", 
                           measure == "production", pop_n %in% c(1, 2, 4))

gg_full_bg_a_prod <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[3], limits$max[3], length.out = 5), 
                     limits = c(limits$min[3], limits$max[3])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "Log response ratios") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "bg", 
                           measure == "production", pop_n %in% c(8, 16, 32))

gg_full_bg_b_prod <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[4], limits$max[4], length.out = 5), 
                     limits = c(limits$min[4], limits$max[4])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "ttl", 
                           measure == "production", pop_n %in% c(1, 2, 4))

gg_full_ttl_a_prod <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[5], limits$max[5], length.out = 5), 
                     limits = c(limits$min[5], limits$max[5])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

data_temp <- dplyr::filter(response_ratios, part == "ttl", 
                           measure == "production", pop_n %in% c(8, 16, 32))

gg_full_ttl_b_prod <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[6], limits$max[6], length.out = 5), 
                     limits = c(limits$min[6], limits$max[6])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

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

# round the next full digit
next_full <- 0.1

# get limits
limits <- dplyr::mutate(response_ratios, 
                        pop_n_class = dplyr::case_when(pop_n %in% c(1, 2, 4) ~ "low", 
                                                       pop_n %in% c(8, 16, 32) ~ "high"), 
                        pop_n_class = factor(pop_n_class, levels = c("low", "high"))) %>% 
  dplyr::filter(measure == "biomass") %>% 
  dplyr::group_by(part, pop_n_class) %>% 
  dplyr::summarise(min = min(lo), max = max(hi), .groups = "drop") %>% 
  dplyr::mutate(min = floor(min / next_full) * next_full, max = ceiling(max / next_full) * next_full) %>% 
  dplyr::mutate(min = dplyr::case_when(min == 0 ~ -next_full, 
                                       TRUE ~ min), 
                max = dplyr::case_when(max == 0 ~ next_full, 
                                       TRUE ~ max))

data_temp <- dplyr::filter(response_ratios, part == "ag", 
                           measure == "biomass", pop_n %in% c(1, 2, 4))

gg_full_ag_a_biom <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[1], limits$max[1], length.out = 5), 
                     limits = c(limits$min[1], limits$max[1])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "ag", 
                           measure == "biomass", pop_n %in% c(8, 16, 32))

gg_full_ag_b_biom <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[2], limits$max[2], length.out = 5), 
                     limits = c(limits$min[2], limits$max[2])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "bg", 
                           measure == "biomass", pop_n %in% c(1, 2, 4))

gg_full_bg_a_biom <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[3], limits$max[3], length.out = 5), 
                     limits = c(limits$min[3], limits$max[3])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "Log response ratios") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "bg", 
                           measure == "biomass", pop_n %in% c(8, 16, 32))

gg_full_bg_b_biom <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[4], limits$max[4], length.out = 5), 
                     limits = c(limits$min[4], limits$max[4])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "ttl", 
                           measure == "biomass", pop_n %in% c(1, 2, 4))

gg_full_ttl_a_biom <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[5], limits$max[5], length.out = 5), 
                     limits = c(limits$min[5], limits$max[5])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

data_temp <- dplyr::filter(response_ratios, part == "ttl", 
                           measure == "biomass", pop_n %in% c(8, 16, 32))

gg_full_ttl_b_biom <- ggplot(data = data_temp) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = dist_class, y = mean, group = interaction(starting_biomass, measure)),
            col = "lightgrey", size = size_line) +
  geom_point(aes(x = dist_class, y = mean, col = starting_biomass), 
             shape = shape, size = size_point) +
  geom_linerange(aes(x = dist_class, ymin = lo, ymax = hi, col = starting_biomass,
                     group = interaction(starting_biomass, measure)), size = size_line) +
  facet_wrap(. ~ pop_n + part_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(limits$min[6], limits$max[6], length.out = 5), 
                     limits = c(limits$min[6], limits$max[6])) +
  scale_x_discrete(labels = labels_x) + 
  scale_color_viridis_d(name = "Starting capacity biomass [%]") +
  labs(x = "", y = "") + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

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

filename_prod <- (model_runs[[1]]$rand$parameters$seagrass_thres * 100) %>% 
  paste0("gg-full-design-dist-prod_", ., "_", 
         model_runs[[1]]$rand$parameters$seagrass_slope) %>% 
  stringr::str_replace(pattern = "\\.", replacement = "") %>% 
  paste0(".pdf")

filename_biom <- (model_runs[[1]]$rand$parameters$seagrass_thres * 100) %>% 
  paste0("gg-full-design-dist-biom_", ., "_", 
         model_runs[[1]]$rand$parameters$seagrass_slope) %>% 
  stringr::str_replace(pattern = "\\.", replacement = "") %>% 
  paste0(".pdf")

suppoRt::save_ggplot(plot = gg_full_design_prod, filename = filename_prod,
                     path = "04_Figures/02_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = FALSE)

suppoRt::save_ggplot(plot = gg_full_design_biom, filename = filename_biom,
                     path = "04_Figures/02_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = FALSE)
