##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

#-------------------#
# Purpose of Script # Results of sobol analysis
#-------------------#

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/calc_biomass_sobol.R")

#### Load data ####

model_runs_sobol <- readr::read_rds(file = "02_Data/02_Modified/01_sensitivity/model_runs_sobol_100.rds")

model_sobol2007 <- readr::read_rds(file = "02_Data/02_Modified/01_sensitivity/model_sobol2007_100.rds")

#### Preprocess data ####

parameters <- c("ag_biomass_max", "ag_gamma", 
                "bg_biomass_max",
                "pop_b", "pop_linf", "pop_n_body",
                "resp_intercept", "resp_slope", "resp_temp_low", "resp_temp_optm",
                "seagrass_slough")

# get sum of biomass and production and center by mean
model_sobol_list <- purrr::map_dfr(model_runs_sobol, calc_biomass_sobol) %>% 
  dplyr::group_by(name) %>% 
  dplyr::mutate(value = value - mean(value)) %>% 
  dplyr::group_split() %>%
  purrr::set_names(c("ag_biomass", "ag_production", "bg_biomass", "bg_production")) %>% 
  purrr::map(function(i) {sensitivity::tell(model_sobol2007, i$value)})

#### Convert Sobol to df ####

model_sobol_df <- purrr::map_dfr(model_sobol_list, function(i) {
  
  main_effect <- tibble::as_tibble(i$S) %>% 
    purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
    dplyr::mutate(parameter = parameters, effect = "Main effect")
  
  total_effect <- tibble::as_tibble(i$T) %>% 
    purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
    dplyr::mutate(parameter = parameters, effect = "Total effect")
  
  dplyr::bind_rows(main_effect, total_effect)}, .id = "name") %>% 
  dplyr::mutate(value = dplyr::case_when(value < 0 ~ 0, 
                                         value > 1 ~ 1, 
                                         TRUE ~ value), 
                min_ci = dplyr::case_when(min_ci < 0 ~ 0, 
                                          TRUE ~ min_ci), 
                max_ci = dplyr::case_when(max_ci > 1 ~ 1, 
                                          TRUE ~ max_ci)) %>% 
  tidyr::separate(col = name, sep = "_", into = c("part", "measure"), 
                  remove = FALSE)

#### Create ggplot ####

# font size
base_size <- 8

# margins
mar <- c(t = 0, r = 2, b = 0, l = 2)

# create labeller for panels
lab_name <- as_labeller(c("ag_biomass" = "Aboveground value", 
                          "ag_production" = "",
                          "bg_biomass" = "Belowground value", 
                          "bg_production" = ""))

lab_measure <- as_labeller(c("biomass" = "Biomass", 
                             "production" = "Production"))

lab_measure_empty <- as_labeller(c("biomass" = "", 
                                   "production" = ""))

# create ggplot
ggplot_sobol_ag_biom <- ggplot(data = dplyr::filter(model_sobol_df, 
                                                    part == "ag", measure == "biomass")) +
  geom_hline(yintercept = 0, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 0.5, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 1, lty = 2, color = "lightgrey") + 
  geom_point(aes(x = parameter, y = value, col = effect),
             size = 1.5, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(x  = parameter, ymin = min_ci, ymax = max_ci, col = effect),
                 position = position_dodge(width = 0.5),size = 0.5) +
  facet_wrap(. ~ name + measure, scales = "free", ncol = 2, nrow = 2, 
             labeller = labeller(name = lab_name, measure = lab_measure)) +
  scale_color_manual(name = "", values = c("Main effect" = "#0D0887FF",
                                           "Total effect" = "#ED7953FF")) +
  scale_y_continuous(name = "Effect strength", limits = c(0, 1)) +
  scale_x_discrete(name = "") +
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar),
        axis.text.x = element_blank(), axis.title.x = element_blank())

ggplot_sobol_ag_prod <- ggplot(data = dplyr::filter(model_sobol_df, 
                                                    part == "ag", measure == "production")) +
  geom_hline(yintercept = 0, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 0.5, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 1, lty = 2, color = "lightgrey") + 
  geom_point(aes(x = parameter, y = value, col = effect),
             size = 1.5, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(x  = parameter, ymin = min_ci, ymax = max_ci, col = effect),
                 position = position_dodge(width = 0.5),size = 0.5) +
  facet_wrap(. ~ name + measure, scales = "free", ncol = 2, nrow = 2, 
             labeller = labeller(name = lab_name, measure = lab_measure)) +
  scale_color_manual(name = "", values = c("Main effect" = "#0D0887FF",
                                           "Total effect" = "#ED7953FF")) +
  scale_y_continuous(name = "", limits = c(0, 1)) +
  scale_x_discrete(name = "") +
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar),
        axis.text.x = element_blank(), axis.title.x = element_blank(), 
        axis.text.y = element_blank())

ggplot_sobol_bg_biom <- ggplot(data = dplyr::filter(model_sobol_df,
                                                    part == "bg", measure == "biomass")) +
  geom_hline(yintercept = 0, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 0.5, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 1, lty = 2, color = "lightgrey") + 
  geom_point(aes(x = parameter, y = value, col = effect),
             size = 1.5, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(x  = parameter, ymin = min_ci, ymax = max_ci, col = effect),
                 position = position_dodge(width = 0.5),size = 0.5) +
  facet_wrap(. ~ name + measure, scales = "free", ncol = 2, nrow = 2, 
             labeller = labeller(name = lab_name, measure = lab_measure_empty)) +
  scale_color_manual(name = "", values = c("Main effect" = "#0D0887FF",
                                           "Total effect" = "#ED7953FF")) +
  scale_y_continuous(name = "Effect strength", limits = c(0, 1)) +
  scale_x_discrete(name = "") +
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar),
        axis.text.x = element_text(angle = 65, hjust = 1))

ggplot_sobol_bg_prod <- ggplot(data = dplyr::filter(model_sobol_df, 
                                                    part == "bg", measure == "production")) +
  geom_hline(yintercept = 0, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 0.5, lty = 2, color = "lightgrey") + 
  geom_hline(yintercept = 1, lty = 2, color = "lightgrey") + 
  geom_point(aes(x = parameter, y = value, col = effect),
             size = 1.5, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(x  = parameter, ymin = min_ci, ymax = max_ci, col = effect),
                 position = position_dodge(width = 0.5),size = 0.5) +
  facet_wrap(. ~ name + measure, scales = "free", ncol = 2, nrow = 2, 
             labeller = labeller(name = lab_name, measure = lab_measure_empty)) +
  scale_color_manual(name = "", values = c("Main effect" = "#0D0887FF",
                                           "Total effect" = "#ED7953FF")) +
  scale_y_continuous(name = "", limits = c(0, 1)) +
  scale_x_discrete(name = "") +
  theme_classic(base_size = base_size) +
  theme(legend.position = "bottom", strip.text = element_text(hjust = 0),
        strip.background = element_blank(), plot.margin = margin(mar),
        axis.text.x = element_text(angle = 65, hjust = 1), 
        axis.text.y = element_blank())


legend <- get_legend(
  ggplot_sobol_ag_biom
)

ggplot_sobol <- cowplot::plot_grid(ggplot_sobol_ag_biom + theme(legend.position = "none"),
                                   ggplot_sobol_ag_prod + theme(legend.position = "none"),
                                   ggplot_sobol_bg_biom + theme(legend.position = "none"),
                                   ggplot_sobol_bg_prod + theme(legend.position = "none"),
                                   ncol = 2, nrow = 2, rel_heights = c(0.4, 0.6))

ggplot_sobol_lgd <- plot_grid(ggplot_sobol, legend,
                              ncol = 1, nrow = 2, rel_heights = c(1, 0.1)) + 
  draw_label(label = "", y = 0.095, size = base_size)

#### save result ####

suppoRt::save_ggplot(plot = ggplot_sobol_lgd, 
                     filename = "ggplot-sa-sobol_nobiom.pdf", 
                     path = "04_Figures/01_sensitivity_analysis/",     
                     width = width, height = height * 0.5, dpi = dpi, units = units,
                     overwrite = FALSE)
