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

sim_experiment <- readr::read_rds("02_Data/02_Modified/03_run_model/sim_experiment.rds")

model_runs <- readr::read_rds("02_Data/02_Modified/03_run_model/model_runs.rds")

#### Preprocess data #### 

# add row id to sim_experiment
sim_experiment <- dplyr::mutate(sim_experiment, 
                                id = as.character(1:nrow(sim_experiment)), 
                                .before = "nutrients_pool")

norm <- FALSE

#### Calculate relationship bg/ag ####

magnitude_diff <- purrr::map_dfr(model_runs, function(i) {
  
  dplyr::bind_rows(rand = i$rand$seafloor, attr = i$attr$seafloor, .id = "move") %>% 
    dplyr::filter(timestep == max(timestep)) %>% 
    dplyr::group_by(move) %>% 
    dplyr::summarise(bg_biomass = sum(bg_biomass, na.rm = TRUE), 
                     ag_biomass = sum(ag_biomass, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::mutate(diff_magnitude = (floor(log10(bg_biomass)) + 1) - 
                    (floor(log10(ag_biomass)) + 1))}, .id = "id") %>%
  dplyr::group_by(move) %>%
  dplyr::summarise(diff_mean = mean(diff_magnitude), 
                   diff_sd = sd(diff_magnitude))

#### Calculate total biomass ####

# calculate total biomass rand/attr
biomass_rand <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_biomass(norm = norm), .id = "id")

biomass_attr <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_biomass(norm = norm), .id = "id")

# combine data.frames
biomass_wide <- dplyr::left_join(x = biomass_rand, y = biomass_attr, 
                                 by = c("id", "part"), 
                                 suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# calculate total bg/ag biomass
biomass_wide_ttl <- dplyr::group_by(biomass_wide, 
                                    id, nutrients_pool, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_biomass")

#### Calculate total production ####

# calculate total production rand/attr
production_rand <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_production(norm = norm), .id = "id")

production_attr <- purrr::map_dfr(model_runs, function(i) 
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_production(norm = norm), .id = "id")

# combine data.frames
production_wide <- dplyr::left_join(x = production_rand, y = production_attr, 
                                 by = c("id", "part"), 
                                 suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# calculate total bg/ag production
production_wide_ttl <- dplyr::group_by(production_wide, 
                                       id, nutrients_pool, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_production")

#### Response ratios after max_i ####

pb <- progress::progress_bar$new(total = 180, width = 60,
                                 format = " Progress [:bar] :percent Remaining: :eta")

repetitions <- 1000

response_ratios <- dplyr::bind_rows(biomass = biomass_wide, 
                                    biomass_ttl = biomass_wide_ttl,
                                    production = production_wide, 
                                    production_ttl = production_wide_ttl) %>% 
  dplyr::group_by(part, nutrients_pool, pop_n) %>% 
  dplyr::group_split() %>% 
  purrr::map_dfr(function(i) {
    
    pb$tick()
    
    bootstrap <- boot::boot(data = tibble::tibble(rand = i$value.rand, 
                                                  attr = i$value.attr), 
                            statistic = log_response, R = repetitions)
    
    bootstrap_ci <- boot::boot.ci(bootstrap, type = "norm", conf = 0.95)
    
    tibble(part = unique(i$part),
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
  
#### Overview table ####

biomass_table <- dplyr::group_by(biomass_wide, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(value.rand = mean(value.rand), 
                   value.attr = mean(value.attr), .groups = "drop") %>% 
  tidyr::separate(col = part, into = c("part", "measure"), sep = "_") %>% 
  dplyr::mutate(pop_n = factor(pop_n, ordered = TRUE), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE)) %>% 
  dplyr::left_join(response_ratios, by = c("part", "measure", "pop_n", "nutrients_pool")) %>% 
  dplyr::select(pop_n, nutrients_pool, part, value.rand, value.attr, lo, mean, hi) %>% 
  tidyr::pivot_wider(names_from = part, values_from = c(value.rand, value.attr, lo, mean, hi))

production_table <- dplyr::group_by(production_wide, part, nutrients_pool, pop_n) %>% 
  dplyr::summarise(value.rand = mean(value.rand), 
                   value.attr = mean(value.attr), .groups = "drop") %>% 
  tidyr::separate(col = part, into = c("part", "measure"), sep = "_") %>% 
  dplyr::mutate(pop_n = factor(pop_n, ordered = TRUE), 
                nutrients_pool = factor(nutrients_pool, ordered = TRUE)) %>% 
  dplyr::left_join(response_ratios, by = c("part", "measure", "pop_n", "nutrients_pool")) %>% 
  dplyr::select(pop_n, nutrients_pool, part, value.rand, value.attr, lo, mean, hi) %>% 
  tidyr::pivot_wider(names_from = part, values_from = c(value.rand, value.attr, lo, mean, hi))

complete_table <- dplyr::left_join(x = biomass_table, y = production_table, 
                                   by = c("pop_n", "nutrients_pool"), 
                                   suffix = c(".biom", ".prod")) %>% 
  dplyr::mutate(rr_ag.biom = dplyr::case_when(lo_ag.biom < 0 & hi_ag.biom < 0 ~ "rand", 
                                              lo_ag.biom > 0 & hi_ag.biom > 0 ~ "attr",
                                              TRUE ~ "n.s."), 
                rr_ag.prod = dplyr::case_when(lo_ag.prod < 0 & hi_ag.prod < 0 ~ "rand", 
                                              lo_ag.prod > 0 & hi_ag.prod > 0 ~ "attr",
                                              TRUE ~ "n.s."), 
                rr_bg.biom = dplyr::case_when(lo_bg.biom < 0 & hi_bg.biom < 0 ~ "rand", 
                                              lo_bg.biom > 0 & hi_bg.biom > 0 ~ "attr",
                                              TRUE ~ "n.s."),
                rr_bg.prod = dplyr::case_when(lo_bg.prod < 0 & hi_bg.prod < 0 ~ "rand", 
                                              lo_bg.prod > 0 & hi_bg.prod > 0 ~ "attr",
                                              TRUE ~ "n.s.")) %>% 
  dplyr::select(pop_n, nutrients_pool, 
                value.rand_ag.biom, value.attr_ag.biom, rr_ag.biom,
                value.rand_ag.prod, value.attr_ag.prod, rr_ag.prod,
                value.rand_bg.biom, value.attr_bg.biom, rr_bg.biom, 
                value.rand_bg.prod, value.attr_bg.prod, rr_bg.prod) %>%
  dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value")), round, digits = 1) %>%
  dplyr::arrange(pop_n)

readr::write_delim(complete_table, file = "02_Data/02_Modified/03_run_model/complete_table.csv",
                   delim = ";")

complete_table_rr <- dplyr::left_join(x = biomass_table, y = production_table, 
                                      by = c("pop_n", "nutrients_pool"), 
                                      suffix = c(".biom", ".prod")) %>% 
  dplyr::select(pop_n, nutrients_pool, 
                lo_ag.biom, mean_ag.biom, hi_ag.biom,
                lo_ag.prod, mean_ag.prod, hi_ag.prod,
                lo_bg.biom, mean_bg.biom, hi_bg.biom,
                lo_bg.prod, mean_bg.prod, hi_bg.prod) %>% 
  dplyr::mutate_at(dplyr::vars(-dplyr::all_of(c("pop_n", "nutrients_pool"))), formatC, digits = 2, format = "e") %>%
  dplyr::arrange(pop_n)

readr::write_delim(complete_table_rr, file = "02_Data/02_Modified/03_run_model/complete_table_rr.csv",
                   delim = ";")

complete_table_rel <- dplyr::mutate(complete_table, 
                                    biom_ag_rel = (value.attr_ag.biom - value.rand_ag.biom) / value.rand_ag.biom * 100,
                                    biom_bg_rel = (value.attr_bg.biom - value.rand_bg.biom) / value.rand_bg.biom * 100, 
                                    prod_ag_rel = (value.attr_ag.prod - value.rand_ag.prod) / value.rand_ag.prod * 100,
                                    prod_bg_rel = (value.attr_bg.prod - value.rand_bg.prod) / value.rand_bg.prod * 100, 
                                    biom_ttl_rel = ((value.attr_ag.biom + value.attr_bg.biom) - 
                                                      (value.rand_ag.biom + value.rand_bg.biom)) / 
                                                      (value.rand_ag.biom + value.rand_bg.biom) * 100,
                                    prod_ttl_rel = ((value.attr_ag.prod + value.attr_bg.prod) - 
                                                      (value.rand_ag.prod + value.rand_bg.prod)) / 
                                      (value.rand_ag.prod + value.rand_bg.prod) * 100) %>% 
  dplyr::select(pop_n, nutrients_pool, biom_ag_rel, prod_ag_rel, biom_bg_rel, prod_bg_rel, biom_ttl_rel, prod_ttl_rel) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with(c("biom_", "prod_"))), formatC, digits = 2, format = "e")

readr::write_delim(complete_table_rel, file = "02_Data/02_Modified/03_run_model/complete_table_rel.csv",
                   delim = ";")

#### Setup ggplots ####

# print 2 digits on y-axsis
scale_fun <- function(x) sprintf("%.3f", x)

# # function to create 5 ticks on y axis
# breaks_fun <- function(x) seq(from = min(x), to = max(x), length.out = 5)

# font size
base_size <- 8

# margins
mar <- c(t = 0, r = 2, b = 0, l = 2)

# set position dodge
pd <- position_dodge(width = 0.25)

# point shape 
shape <- 20

#### Increasing fish population, stable nutrients ####

# # create labeller for panels
# lab_part <- as_labeller(c("ag" = "Aboveground value", "bg" = "Belowground value",
#                           "ttl" = "Total value"))
# 
# # # get absolute largest values
# # limits_fish <- dplyr::filter(response_ratios, nutrients_pool == 0.75) %>%
# #   dplyr::group_by(part) %>%
# #   dplyr::summarise(l = max(abs(c(lo, hi))))
# 
# gg_fish_ag <- dplyr::filter(response_ratios, nutrients_pool == 0.75, part == "ag") %>%
#   ggplot() +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
#   geom_line(aes(x = pop_n, y = mean, group = measure),
#             col = "lightgrey", position = pd) +
#   geom_point(aes(x = pop_n, y = mean, col = measure), shape = shape, position = pd) +
#   geom_linerange(aes(x = pop_n, ymin = lo, ymax = hi,
#                      col = measure, group = measure),
#                  position = pd) +
#   facet_wrap(. ~ part, scales = "fixed", nrow = 1, ncol = 1, 
#              labeller = lab_part)  + 
#   scale_y_continuous(labels = scale_fun, breaks = breaks_fun) +
#   scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"),
#                      labels = c("Biomass", "Production")) +
#   guides(col = FALSE) +
#   labs(x = "", y = "") +
#   theme_classic(base_size = base_size) +
#   theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
#         strip.background = element_blank(), plot.margin = margin(mar))
# 
# gg_fish_bg <- dplyr::filter(response_ratios, nutrients_pool == 0.75, part == "bg") %>%
#   ggplot() +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
#   geom_line(aes(x = pop_n, y = mean, group = measure),
#             col = "lightgrey", position = pd) +
#   geom_point(aes(x = pop_n, y = mean, col = measure), shape = shape, position = pd) +
#   geom_linerange(aes(x = pop_n, ymin = lo, ymax = hi,
#                      col = measure, group = measure),
#                  position = pd, size = 0.5) +
#   facet_wrap(. ~ part, scales = "fixed", nrow = 1, ncol = 1, 
#              labeller = lab_part)  + 
#   scale_y_continuous(labels = scale_fun, breaks = breaks_fun) +
#   scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"),
#                      labels = c("Biomass", "Production")) +
#   guides(col = FALSE) +
#   labs(x = "", y = "Log response ratios") +
#   theme_classic(base_size = base_size) +
#   theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
#         strip.background = element_blank(), plot.margin = margin(mar))
# 
# gg_fish_ttl <- dplyr::filter(response_ratios, nutrients_pool == 0.75, part == "ttl") %>%
#   ggplot() +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
#   geom_line(aes(x = pop_n, y = mean, group = measure),
#             col = "lightgrey", position = pd) +
#   geom_point(aes(x = pop_n, y = mean, col = measure), shape = shape, position = pd) +
#   geom_linerange(aes(x = pop_n, ymin = lo, ymax = hi,
#                      col = measure, group = measure),
#                  position = pd, size = 0.5) +
#   facet_wrap(. ~ part, scales = "fixed", nrow = 1, ncol = 1, 
#              labeller = lab_part)  + 
#   scale_y_continuous(labels = scale_fun, breaks = breaks_fun) +
#   scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"),
#                      labels = c("Biomass", "Production")) +
#   labs(x = "Fish population size", y = "") +
#   theme_classic(base_size = base_size) +
#   theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
#         strip.background = element_blank(), plot.margin = margin(mar))
# 
# gg_fish_design <- cowplot::plot_grid(gg_fish_ag, gg_fish_bg, gg_fish_ttl,
#                                      ncol = 1, nrow = 3)

##### Full design ####

complete_table_text <- dplyr::filter(complete_table_rel, nutrients_pool == 0.75) %>% 
  tidyr::pivot_longer(-c(pop_n, nutrients_pool)) %>% 
  tidyr::separate(name, sep = "_", into = c("measure", "part", "misc")) %>% 
  dplyr::mutate(measure = factor(dplyr::case_when(measure == "biom" ~ "biomass", 
                                           measure == "prod" ~ "production")),
                part = factor(part),
                part_n = paste(part, pop_n, sep = "_"),
                part_n = factor(part_n), 
                value = round(as.numeric(value), 3),
                misc = NULL)

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

lab_pop_n_empty <- as_labeller(c(`1` = "", `2` = "", 
                                 `4` = "", `8` = "", 
                                 `16` = "", `32` = ""))

# # get absolute largest values
# limits_full <- dplyr::group_by(response_ratios, part) %>% 
#   dplyr::summarise(lim = max(abs(c(lo, hi))))

gg_full_ag_a <- ggplot(data = filter(response_ratios, part == "ag", pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = nutrients_pool, y = mean, group = measure), col = "lightgrey",
            position = pd) +
  geom_point(aes(x = nutrients_pool, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = nutrients_pool, ymin = lo, ymax = hi,
                     col = measure, group = measure), position = pd) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ag", 
                                 pop_n %in% c(1, 2, 4), 
                                 measure == "biomass"), 
            aes(x = 1.75, y = 0.4, label = paste0(value, "%")), col = "#46ACC8", size = 2) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ag", 
                                 pop_n %in% c(1, 2, 4), 
                                 measure == "production"), 
            aes(x = 3.5, y = 0.4, label = paste0(value, "%")), col = "#B40F20", size = 2) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.1, 0.4, length.out = 5), 
                     limits = c(-0.1, 0.4)) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_ag_b <- ggplot(data = filter(response_ratios, part == "ag", pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = nutrients_pool, y = mean, group = measure), col = "lightgrey",
            position = pd) +
  geom_point(aes(x = nutrients_pool, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = nutrients_pool, ymin = lo, ymax = hi,
                     col = measure, group = measure), position = pd) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ag", 
                                 pop_n %in% c(8, 16, 32), 
                                 measure == "biomass"), 
            aes(x = 1.75, y = 9, label = paste0(value, "%")), col = "#46ACC8", size = 2) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ag", 
                                 pop_n %in% c(8, 16, 32), 
                                 measure == "production"), 
            aes(x = 3.5, y = 9, label = paste0(value, "%")), col = "#B40F20", size = 2) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.5, 9, length.out = 5), 
                     limits = c(-0.5, 9)) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_bg_a <- ggplot(data = filter(response_ratios, part == "bg", pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = nutrients_pool, y = mean, group = measure), col = "lightgrey",
            position = pd) +
  geom_point(aes(x = nutrients_pool, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = nutrients_pool, ymin = lo, ymax = hi,
                     col = measure, group = measure), position = pd) +
  geom_text(data = dplyr::filter(complete_table_text, part == "bg", 
                                 pop_n %in% c(1, 2, 4), 
                                 measure == "biomass"), 
            aes(x = 1.75, y = 0.01, label = paste0(value, "%")), col = "#46ACC8", size = 2) +
  geom_text(data = dplyr::filter(complete_table_text, part == "bg", 
                                 pop_n %in% c(1, 2, 4), 
                                 measure == "production"), 
            aes(x = 3.5, y = 0.01, label = paste0(value, "%")), col = "#B40F20", size = 2) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.005, 0.01, length.out = 5), 
                     limits = c(-0.005, 0.01)) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  labs(x = "", y = "Log response ratios") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_bg_b <- ggplot(data = filter(response_ratios, part == "bg", pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = nutrients_pool, y = mean, group = measure), col = "lightgrey",
            position = pd) +
  geom_point(aes(x = nutrients_pool, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = nutrients_pool, ymin = lo, ymax = hi,
                     col = measure, group = measure), position = pd) +
  geom_text(data = dplyr::filter(complete_table_text, part == "bg", 
                                 pop_n %in% c(8, 16, 32), 
                                 measure == "biomass"), 
            aes(x = 1.75, y = 0.1, label = paste0(value, "%")), col = "#46ACC8", size = 2) +
  geom_text(data = dplyr::filter(complete_table_text, part == "bg", 
                                 pop_n %in% c(8, 16, 32), 
                                 measure == "production"), 
            aes(x = 3.5, y = 0.1, label = paste0(value, "%")), col = "#B40F20", size = 2) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.1, 0.1, length.out = 5), 
                     limits = c(-0.1, 0.1)) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

gg_full_ttl_a <- ggplot(data = filter(response_ratios, part == "ttl", pop_n %in% c(1, 2, 4))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = nutrients_pool, y = mean, group = measure), col = "lightgrey",
            position = pd) +
  geom_point(aes(x = nutrients_pool, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = nutrients_pool, ymin = lo, ymax = hi, 
                     col = measure, group = measure), position = pd) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ttl", 
                                 pop_n %in% c(1, 2, 4), 
                                 measure == "biomass"), 
            aes(x = 1.75, y = 0.01, label = paste0(value, "%")), col = "#46ACC8", size = 2) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ttl", 
                                 pop_n %in% c(1, 2, 4), 
                                 measure == "production"), 
            aes(x = 3.5, y = 0.01, label = paste0(value, "%")), col = "#B40F20", size = 2) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.005, 0.01, length.out = 5), 
                     limits = c(-0.005, 0.01)) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

gg_full_ttl_b <- ggplot(data = filter(response_ratios, part == "ttl", pop_n %in% c(8, 16, 32))) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey") +
  geom_line(aes(x = nutrients_pool, y = mean, group = measure), col = "lightgrey",
            position = pd) +
  geom_point(aes(x = nutrients_pool, y = mean, col = measure), shape = shape, position = pd) +
  geom_linerange(aes(x = nutrients_pool, ymin = lo, ymax = hi, 
                     col = measure, group = measure), position = pd) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ttl", 
                                 pop_n %in% c(8, 16, 32), 
                                 measure == "biomass"), 
            aes(x = 1.75, y = 0.125, label = paste0(value, "%")), col = "#46ACC8", size = 2) +
  geom_text(data = dplyr::filter(complete_table_text, part == "ttl", 
                                 pop_n %in% c(8, 16, 32), 
                                 measure == "production"), 
            aes(x = 3.5, y = 0.125, label = paste0(value, "%")), col = "#B40F20", size = 2) +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-0.1, 0.125, length.out = 5),
                     limits = c(-0.1, 0.125)) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

legend <- get_legend(
  gg_full_ttl_b
)

gg_full_design <- cowplot::plot_grid(gg_full_ag_a + theme(legend.position = "none"),
                                     gg_full_ag_b + theme(legend.position = "none"),
                                     gg_full_bg_a + theme(legend.position = "none"),
                                     gg_full_bg_b + theme(legend.position = "none"), 
                                     gg_full_ttl_a + theme(legend.position = "none"),  
                                     gg_full_ttl_b + theme(legend.position = "none"), 
                                     ncol = 2, nrow = 3)

gg_full_design <- plot_grid(gg_full_design, legend,
                            ncol = 1, nrow = 2, rel_heights = c(1, 0.1)) + 
  draw_label(label = "Starting nutrient pool [g/cell]", y = 0.095, size = base_size)

### Save ggplot ####

# set defaults for plotting
overwrite <- FALSE

# suppoRt::save_ggplot(plot = gg_fish_design, filename = "gg_fish_design.pdf",
#                      path = "04_Figures/03_simulation_experiment/",
#                      width = width, height = height * 0.5, dpi = dpi, units = units,
#                      overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_full_design, filename = "gg_full_design.pdf",
                     path = "04_Figures/03_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = overwrite)
