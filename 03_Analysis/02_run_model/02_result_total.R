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

sim_experiment <- readr::read_rds("02_Data/02_Modified/02_run_model/sim_experiment.rds")

model_runs <- readr::read_rds("02_Data/02_Modified/02_run_model/model-runs_-75_2.rds")

#### Preprocess data #### 

# add row id to sim_experiment
sim_experiment <- dplyr::mutate(sim_experiment, 
                                id = 1:nrow(sim_experiment), 
                                .before = "starting_biomass",
                                starting_biomass = starting_biomass * 100)

# argument if result should be normalized by excretion
norm <- FALSE

#### Calc total excretion

timestep <- 219000 # 109500 219000

#### Calculate total biomass ####

# calculate total biomass rand/attr
biomass_rand <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_biomass(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

biomass_attr <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_biomass(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

# normalize by excretion
if (norm) {
  
  excretion_rand <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "rand") %>%
      magrittr::extract2("seafloor") %>% 
      calc_total_excretion(i = timestep)}, .id = "id") %>% 
    dplyr::mutate(id = as.numeric(id)) %>% 
    dplyr::left_join(sim_experiment, by = "id")
  
  # biomass has two rows for each id because of ag/bg
  biomass_rand$value <- biomass_rand$value / rep(excretion_rand$value, each = 2)
  
  excretion_attr <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "attr") %>%
      magrittr::extract2("seafloor") %>% 
      calc_total_excretion(i = timestep)}, .id = "id") %>% 
    dplyr::mutate(id = as.numeric(id)) %>% 
    dplyr::left_join(sim_experiment, by = "id")
  
  biomass_attr$value <- biomass_attr$value / rep(excretion_attr$value, each = 2)
  
}

# combine data.frames
biomass_wide <- dplyr::left_join(x = biomass_rand, y = biomass_attr, 
                                 by = c("id", "part"), 
                                 suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# calculate total bg/ag biomass
biomass_wide_ttl <- dplyr::group_by(biomass_wide, 
                                    id, starting_biomass, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_biomass")

#### Calculate total production ####

# calculate total production rand/attr
production_rand <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "rand") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_production(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

production_attr <- purrr::map_dfr(model_runs, function(i) {
    magrittr::extract2(i, "attr") %>%
    magrittr::extract2("seafloor") %>% 
    calc_total_production(i = timestep)}, .id = "id") %>% 
  dplyr::mutate(id = as.numeric(id))

# normalize by excretion
if (norm) {
  
  # biomass has two rows for each id because of ag/bg
  production_rand$value <- production_rand$value / rep(excretion_rand$value, each = 2)
  
  production_attr$value <- production_attr$value / rep(excretion_attr$value, each = 2)
  
}

# combine data.frames
production_wide <- dplyr::left_join(x = production_rand, y = production_attr, 
                                 by = c("id", "part"), 
                                 suffix = c(".rand", ".attr")) %>% 
  dplyr::left_join(sim_experiment, by = "id")

# calculate total bg/ag production
production_wide_ttl <- dplyr::group_by(production_wide, 
                                       id, starting_biomass, pop_n) %>% 
  dplyr::summarise(value.rand = sum(value.rand), value.attr = sum(value.attr), 
                   .groups = "drop") %>% 
  dplyr::mutate(part = "ttl_production")

#### Response ratios after i ####

pb <- progress::progress_bar$new(total = 108, width = 60,
                                 format = " Progress [:bar] :percent Remaining: :eta")

repetitions <- 1000

response_ratios <- dplyr::bind_rows(biomass = biomass_wide, 
                                    biomass_ttl = biomass_wide_ttl,
                                    production = production_wide, 
                                    production_ttl = production_wide_ttl) %>% 
  dplyr::group_by(part, starting_biomass, pop_n) %>% 
  dplyr::group_split() %>% # length
  purrr::map_dfr(function(i) {
    
    pb$tick()
    
    bootstrap <- boot::boot(data = tibble::tibble(rand = i$value.rand, 
                                                  attr = i$value.attr), 
                            statistic = log_response, R = repetitions)
    
    bootstrap_ci <- boot::boot.ci(bootstrap, type = "norm", conf = 0.95)
    
    tibble(part = unique(i$part),
           pop_n = unique(i$pop_n), starting_biomass = unique(i$starting_biomass),
           mean = mean(bootstrap$t[, 1]), 
           lo = bootstrap_ci$normal[2], 
           hi = bootstrap_ci$normal[3])}) %>% 
  tidyr::separate(col = part, into = c("part", "measure"), sep = "_") %>% 
  dplyr::mutate(part = factor(part, levels = c("ag", "bg", "ttl")), 
                measure = factor(measure, levels = c("biomass", "production")), 
                pop_n = factor(pop_n, ordered = TRUE), 
                starting_biomass = factor(starting_biomass, ordered = TRUE), 
                part_n = paste(part, pop_n, sep = "_"),
                part_n = factor(part_n, levels = stringr::str_sort(unique(part_n), 
                                                                   numeric = TRUE))) 
  
#### Overview table ####

biomass_table <- dplyr::bind_rows(biomass_wide, biomass_wide_ttl) %>% 
  dplyr::group_by(part, starting_biomass, pop_n) %>% 
  dplyr::summarise(value.rand = mean(value.rand), 
                   value.attr = mean(value.attr), .groups = "drop") %>% 
  tidyr::separate(col = part, into = c("part", "measure"), sep = "_") %>% 
  dplyr::mutate(pop_n = factor(pop_n, ordered = TRUE), 
                starting_biomass = factor(starting_biomass, ordered = TRUE)) %>% 
  dplyr::left_join(response_ratios, by = c("part", "measure", "pop_n", "starting_biomass")) %>% 
  dplyr::select(pop_n, starting_biomass, part, value.rand, value.attr, lo, mean, hi) %>%
  tidyr::pivot_wider(names_from = part, values_from = c(value.rand, value.attr, lo, mean, hi), 
                     names_sep = ".")

production_table <- dplyr::bind_rows(production_wide, production_wide_ttl) %>% 
  dplyr::group_by(part, starting_biomass, pop_n) %>% 
  dplyr::summarise(value.rand = mean(value.rand), 
                   value.attr = mean(value.attr), .groups = "drop") %>% 
  tidyr::separate(col = part, into = c("part", "measure"), sep = "_") %>% 
  dplyr::mutate(pop_n = factor(pop_n, ordered = TRUE), 
                starting_biomass = factor(starting_biomass, ordered = TRUE)) %>% 
  dplyr::left_join(response_ratios, by = c("part", "measure", "pop_n", "starting_biomass")) %>% 
  dplyr::select(pop_n, starting_biomass, part, value.rand, value.attr, lo, mean, hi) %>% 
  tidyr::pivot_wider(names_from = part, values_from = c(value.rand, value.attr, lo, mean, hi), 
                     names_sep = ".")

complete_table <- dplyr::left_join(x = biomass_table, y = production_table, 
                                   by = c("pop_n", "starting_biomass"), 
                                   suffix = c(".biom", ".prod")) %>% 
  dplyr::mutate(value.ag.biom.rel = (value.attr.ag.biom - value.rand.ag.biom) / value.rand.ag.biom * 100,
                value.bg.biom.rel = (value.attr.bg.biom - value.rand.bg.biom) / value.rand.bg.biom * 100,
                value.ttl.biom.rel = (value.attr.ttl.biom - value.rand.ttl.biom) / value.rand.ttl.biom * 100,
                value.ag.prod.rel = (value.attr.ag.prod - value.rand.ag.prod) / value.rand.ag.prod * 100,
                value.bg.prod.rel = (value.attr.bg.prod - value.rand.bg.prod) / value.rand.bg.prod * 100, 
                value.ttl.prod.rel = (value.attr.ttl.prod - value.rand.ttl.prod) / value.rand.ttl.prod * 100,
                rr.ag.biom = dplyr::case_when(hi.ag.biom < 0 ~ "rand", lo.ag.biom > 0 ~ "attr",
                                              TRUE ~ "n.s."),
                rr.ag.prod = dplyr::case_when(hi.ag.prod < 0 ~ "rand", lo.ag.prod > 0 ~ "attr",
                                              TRUE ~ "n.s."), 
                rr.bg.biom = dplyr::case_when(hi.bg.biom < 0 ~ "rand", lo.bg.biom > 0 ~ "attr",
                                              TRUE ~ "n.s."),
                rr.bg.prod = dplyr::case_when(hi.bg.prod < 0 ~ "rand", lo.bg.prod > 0 ~ "attr",
                                              TRUE ~ "n.s.") ,
                rr.ttl.biom = dplyr::case_when(hi.ttl.biom < 0 ~ "rand", lo.ttl.biom > 0 ~ "attr",
                                              TRUE ~ "n.s."),
                rr.ttl.prod = dplyr::case_when(hi.ttl.prod < 0 ~ "rand", lo.ttl.prod > 0 ~ "attr",
                                               TRUE ~ "n.s.")) %>% 
  dplyr::select(pop_n, starting_biomass, 
                value.rand.ag.biom, value.attr.ag.biom, value.ag.biom.rel, rr.ag.biom,
                value.rand.ag.prod, value.attr.ag.prod, value.ag.prod.rel, rr.ag.prod,
                value.rand.bg.biom, value.attr.bg.biom, value.bg.biom.rel, rr.bg.biom, 
                value.rand.bg.prod, value.attr.bg.prod, value.bg.prod.rel, rr.bg.prod, 
                value.rand.ttl.biom, value.attr.ttl.biom, value.ttl.biom.rel, rr.ttl.biom, 
                value.rand.ttl.prod, value.attr.ttl.prod, value.ttl.prod.rel, rr.ttl.prod) %>%
  dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), round, digits = 0) %>%
  dplyr::arrange(pop_n)

# remove ttl cols for csv table
complete_table_csv <- dplyr::select(complete_table, -dplyr::contains("ttl"))

if (norm) {
  
  # summarize excr by treatmants
  excretion_rand <- dplyr::group_by(excretion_rand, starting_biomass, pop_n) %>% 
    dplyr::summarise(value = mean(value), .groups = "drop") %>% 
    dplyr::arrange(pop_n)
  
  excretion_attr <- dplyr::group_by(excretion_attr, starting_biomass, pop_n) %>% 
    dplyr::summarise(value = mean(value), .groups = "drop") %>% 
    dplyr::arrange(pop_n)
  
  # calculate total values again
  complete_table_csv <- dplyr::mutate(complete_table_csv, 
                                      value.rand.ag.biom = value.rand.ag.biom * excretion_rand$value, 
                                      value.attr.ag.biom = value.attr.ag.biom * excretion_attr$value, 
                                      value.rand.bg.biom = value.rand.bg.biom * excretion_rand$value, 
                                      value.attr.bg.biom = value.attr.bg.biom * excretion_attr$value, 
                                      value.rand.ag.prod = value.rand.ag.prod * excretion_rand$value, 
                                      value.attr.ag.prod = value.attr.ag.prod * excretion_attr$value, 
                                      value.rand.bg.prod = value.rand.bg.prod * excretion_rand$value, 
                                      value.attr.bg.prod = value.attr.bg.prod * excretion_attr$value)
}

filename <- (model_runs[[1]]$rand$parameters$seagrass_thres * 100) %>%
  paste0("02_Data/02_Modified/02_run_model/complete-table_", ., "_",
         model_runs[[1]]$rand$parameters$seagrass_slope) %>%
  stringr::str_replace(pattern = "\\.", replacement = "") %>%
  paste0(".csv")

readr::write_delim(complete_table_csv, file = filename,
                   delim = ";")

#### Setup ggplots ####

# print 2 digits on y-axsis
scale_fun <- function(x) sprintf("%.2f", x)

# margins
mar <- c(t = 0, r = 2, b = 0, l = 2)

# set position dodge
pd <- position_dodge(width = 0.2)

# point shape 
shape <- 20

base_size <- 8

size_text <- 2

size_line <- 0.25

size_point <- 0.75

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

##### Full design ####

complete_table_text <- dplyr::mutate(complete_table, 
                                     value.ttl.biom.rel = ((value.attr.ag.biom + value.attr.bg.biom) - 
                                                             (value.rand.ag.biom + value.rand.bg.biom)) /
                                       (value.rand.ag.biom + value.rand.bg.biom) * 100, 
                                     value.ttl.prod.rel = ((value.attr.ag.prod + value.attr.bg.prod) - 
                                                             (value.rand.ag.prod + value.rand.bg.prod)) /
                                       (value.rand.ag.prod + value.rand.bg.prod) * 100) %>% 
  dplyr::mutate_at(dplyr::vars(tidyr::starts_with("value.")), round, digits = 0) %>%
  dplyr::select(pop_n, starting_biomass, 
                value.ag.biom.rel, value.ag.prod.rel,
                value.bg.biom.rel, value.bg.prod.rel,
                value.ttl.biom.rel, value.ttl.prod.rel) %>% 
  tidyr::pivot_longer(-c(pop_n, starting_biomass)) %>% 
  tidyr::separate(name, sep = "\\.", into = c("misc_a", "part", "measure", "misc_b")) %>% 
  dplyr::mutate(measure = factor(dplyr::case_when(measure == "biom" ~ "biomass", 
                                           measure == "prod" ~ "production")),
                part = factor(part),
                part_n = paste(part, pop_n, sep = "_"),
                part_n = factor(part_n), 
                misc_a = NULL, misc_b = NULL)

complete_table_text <- dplyr::select(complete_table, pop_n, starting_biomass, 
                                     rr.ag.biom, rr.ag.prod, rr.bg.biom, rr.bg.prod, 
                                     rr.ttl.biom, rr.ttl.prod) %>% 
  tidyr::pivot_longer(-c(pop_n, starting_biomass)) %>% 
  tidyr::separate(name, sep = "\\.", into = c("misc", "part", "measure")) %>% 
  dplyr::mutate(measure = factor(dplyr::case_when(measure == "biom" ~ "biomass", 
                                                  measure == "prod" ~ "production"))) %>% 
  dplyr::select(-misc) %>% 
  dplyr::left_join(complete_table_text, suffix = c(".sign", ".perc"),
                   by = c("pop_n", "starting_biomass", "part", "measure")) %>% 
  dplyr::mutate(color = dplyr::case_when(value.sign == "n.s." ~ "#9B9B9B",
                                         value.sign != "n.s." & measure == "biomass" ~ "#46ACC8", 
                                         value.sign != "n.s." & measure == "production" ~ "#B40F20"))

# create figures

limits <- dplyr::mutate(response_ratios, 
                        pop_n_class = dplyr::case_when(pop_n %in% c(1, 2, 4) ~ "low", 
                                                       pop_n %in% c(8, 16, 32) ~ "high"), 
                        pop_n_class = factor(pop_n_class, levels = c("low", "high")),
                        lo_abs = abs(lo), hi_abs = abs(hi)) %>% 
  dplyr::group_by(part, pop_n_class) %>% 
  dplyr::summarise(rr = max(c(lo_abs, hi_abs)), .groups = "drop") %>% 
  dplyr::mutate(rr = ceiling(rr / 0.25) * 0.25)

data_temp <- dplyr::filter(response_ratios, part == "ag", pop_n %in% c(1, 2, 4))
text_temp <- dplyr::filter(complete_table_text, part == "ag", pop_n %in% c(1, 2, 4))

gg_full_ag_a <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = starting_biomass, y = mean, group = measure), 
            col = "lightgrey", position = pd, size = size_line) +
  geom_point(aes(x = starting_biomass, y = mean, col = measure), 
             shape = shape, position = pd, size = size_point) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  geom_linerange(aes(x = starting_biomass, ymin = lo, ymax = hi, col = measure, group = measure), 
                 position = pd, size = size_line) +
  ggnewscale::new_scale_color() +
  geom_text(data = dplyr::filter(text_temp, measure == "biomass"), 
            aes(x = starting_biomass, y = limits$rr[1] * 1.2, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  geom_text(data = dplyr::filter(text_temp, measure == "production"), 
            aes(x = starting_biomass, y = limits$rr[1] * 1.05, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  scale_color_manual(name = "", values = c("#46ACC8" = "#46ACC8", 
                                           "#B40F20" = "#B40F20", 
                                           "#9B9B9B" = "#9B9B9B")) +
  guides(col = "none") +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-limits$rr[1], limits$rr[1], length.out = 5), 
                     limits = c(-limits$rr[1], limits$rr[1] * 1.2)) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "ag", pop_n %in% c(8, 16, 32))
text_temp <- dplyr::filter(complete_table_text, part == "ag", pop_n %in%  c(8, 16, 32))

gg_full_ag_b <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = starting_biomass, y = mean, group = measure), 
            col = "lightgrey", position = pd, size = size_line) +
  geom_point(aes(x = starting_biomass, y = mean, col = measure), 
             shape = shape, position = pd, size = size_point) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  geom_linerange(aes(x = starting_biomass, ymin = lo, ymax = hi, col = measure, group = measure), 
                 position = pd, size = size_line) +
  ggnewscale::new_scale_color() +
  geom_text(data = dplyr::filter(text_temp, measure == "biomass"), 
            aes(x = starting_biomass, y = limits$rr[2] * 1.2, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  geom_text(data = dplyr::filter(text_temp, measure == "production"), 
            aes(x = starting_biomass, y = limits$rr[2] * 1.05, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  scale_color_manual(name = "", values = c("#46ACC8" = "#46ACC8", 
                                           "#B40F20" = "#B40F20", 
                                           "#9B9B9B" = "#9B9B9B")) +
  guides(col = "none") +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-limits$rr[2], limits$rr[2], length.out = 5), 
                     limits = c(-limits$rr[2], limits$rr[2] * 1.2)) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "bg", pop_n %in% c(1, 2, 4))
text_temp <- dplyr::filter(complete_table_text, part == "bg", pop_n %in% c(1, 2, 4))

gg_full_bg_a <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = starting_biomass, y = mean, group = measure), 
            col = "lightgrey", position = pd, size = size_line) +
  geom_point(aes(x = starting_biomass, y = mean, col = measure), 
             shape = shape, position = pd, size = size_point) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  geom_linerange(aes(x = starting_biomass, ymin = lo, ymax = hi, col = measure, group = measure), 
                 position = pd, size = size_line) +
  ggnewscale::new_scale_color() +
  geom_text(data = dplyr::filter(text_temp, measure == "biomass"), 
            aes(x = starting_biomass, y = limits$rr[3] * 1.2, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  geom_text(data = dplyr::filter(text_temp, measure == "production"), 
            aes(x = starting_biomass, y = limits$rr[3] * 1.05, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  scale_color_manual(name = "", values = c("#46ACC8" = "#46ACC8", 
                                           "#B40F20" = "#B40F20", 
                                           "#9B9B9B" = "#9B9B9B")) +
  guides(col = "none") +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-limits$rr[3], limits$rr[3], length.out = 5), 
                     limits = c(-limits$rr[3], limits$rr[3] * 1.2)) +
  labs(x = "", y = "Log response ratios") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "bg", pop_n %in% c(8, 16, 32))
text_temp <- dplyr::filter(complete_table_text, part == "bg", pop_n %in%  c(8, 16, 32))

gg_full_bg_b <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = starting_biomass, y = mean, group = measure), 
            col = "lightgrey", position = pd, size = size_line) +
  geom_point(aes(x = starting_biomass, y = mean, col = measure), 
             shape = shape, position = pd, size = size_point) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  geom_linerange(aes(x = starting_biomass, ymin = lo, ymax = hi, col = measure, group = measure), 
                 position = pd, size = size_line) +
  ggnewscale::new_scale_color() +
  geom_text(data = dplyr::filter(text_temp, measure == "biomass"), 
            aes(x = starting_biomass, y = limits$rr[4] * 1.2, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  geom_text(data = dplyr::filter(text_temp, measure == "production"), 
            aes(x = starting_biomass, y = limits$rr[4] * 1.05, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  scale_color_manual(name = "", values = c("#46ACC8" = "#46ACC8", 
                                           "#B40F20" = "#B40F20", 
                                           "#9B9B9B" = "#9B9B9B")) +
  guides(col = "none") +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-limits$rr[4], limits$rr[4], length.out = 5), 
                     limits = c(-limits$rr[4], limits$rr[4] * 1.2)) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar), 
        axis.text.x = element_blank())

data_temp <- dplyr::filter(response_ratios, part == "ttl", pop_n %in% c(1, 2, 4))
text_temp <- dplyr::filter(complete_table_text, part == "ttl", pop_n %in%  c(1, 2, 4))

gg_full_ttl_a <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = starting_biomass, y = mean, group = measure), 
            col = "lightgrey", position = pd, size = size_line) +
  geom_point(aes(x = starting_biomass, y = mean, col = measure), 
             shape = shape, position = pd, size = size_point) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  geom_linerange(aes(x = starting_biomass, ymin = lo, ymax = hi, col = measure, group = measure), 
                 position = pd, size = size_line) +
  ggnewscale::new_scale_color() +
  geom_text(data = dplyr::filter(text_temp, measure == "biomass"), 
            aes(x = starting_biomass, y = limits$rr[5] * 1.2, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  geom_text(data = dplyr::filter(text_temp, measure == "production"), 
            aes(x = starting_biomass, y = limits$rr[5] * 1.05, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  scale_color_manual(name = "", values = c("#46ACC8" = "#46ACC8", 
                                           "#B40F20" = "#B40F20", 
                                           "#9B9B9B" = "#9B9B9B")) +
  guides(col = "none") +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-limits$rr[5], limits$rr[5], length.out = 5), 
                     limits = c(-limits$rr[5], limits$rr[5] * 1.2)) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

data_temp <- dplyr::filter(response_ratios, part == "ttl", pop_n %in% c(8, 16, 32))
text_temp <- dplyr::filter(complete_table_text, part == "ttl", pop_n %in%  c(8, 16, 32))

gg_full_ttl_b <- ggplot(data = data_temp) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgrey", size = size_line) +
  geom_line(aes(x = starting_biomass, y = mean, group = measure), 
            col = "lightgrey", position = pd, size = size_line) +
  geom_point(aes(x = starting_biomass, y = mean, col = measure), 
             shape = shape, position = pd, size = size_point) +
  scale_color_manual(name = "", values = c("#46ACC8", "#B40F20"), 
                     labels = c("Biomass", "Production")) +
  geom_linerange(aes(x = starting_biomass, ymin = lo, ymax = hi, col = measure, group = measure), 
                 position = pd, size = size_line) +
  ggnewscale::new_scale_color() +
  geom_text(data = dplyr::filter(text_temp, measure == "biomass"), 
            aes(x = starting_biomass, y = limits$rr[6] * 1.2, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  geom_text(data = dplyr::filter(text_temp, measure == "production"), 
            aes(x = starting_biomass, y = limits$rr[6] * 1.05, label = paste0(value.perc, "%"), col = color), 
            size = size_text) +
  scale_color_manual(name = "", values = c("#46ACC8" = "#46ACC8", 
                                           "#B40F20" = "#B40F20", 
                                           "#9B9B9B" = "#9B9B9B")) +
  guides(col = "none") +
  facet_wrap(. ~ part_n + pop_n, scales = "fixed", nrow = 1, ncol = 3, 
             labeller = labeller(part_n = lab_part_n, pop_n = lab_pop_n_empty))  + 
  scale_y_continuous(labels = scale_fun, breaks = seq(-limits$rr[6], limits$rr[6], length.out = 5), 
                     limits = c(-limits$rr[6], limits$rr[6] * 1.2)) +
  labs(x = "", y = "") + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0), 
        strip.background = element_blank(), plot.margin = margin(mar))

legend <- get_legend(gg_full_ttl_b)

gg_full_design <- cowplot::plot_grid(gg_full_ag_a + theme(legend.position = "none"),
                                     gg_full_ag_b + theme(legend.position = "none"),
                                     gg_full_bg_a + theme(legend.position = "none"),
                                     gg_full_bg_b + theme(legend.position = "none"), 
                                     gg_full_ttl_a + theme(legend.position = "none"),  
                                     gg_full_ttl_b + theme(legend.position = "none"), 
                                     ncol = 2, nrow = 3)

gg_full_design <- plot_grid(gg_full_design, legend,
                            ncol = 1, nrow = 2, rel_heights = c(1, 0.1)) + 
  draw_label(label = "Starting capacity biomass [%]", y = 0.095, size = base_size)

### Save ggplot ####

filename <- (model_runs[[1]]$rand$parameters$seagrass_thres * 100) %>%
  paste0("gg-full-design_", ., "_",
         model_runs[[1]]$rand$parameters$seagrass_slope) %>%
  stringr::str_replace(pattern = "\\.", replacement = "") %>%
  paste0(".pdf")

suppoRt::save_ggplot(plot = gg_full_design, filename = filename,
                     path = "04_Figures/02_simulation_experiment/",
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = FALSE)
