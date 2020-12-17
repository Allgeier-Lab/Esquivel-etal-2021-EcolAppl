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
source("Helper_functions/setup.R")

#### Load data ####

model_runs_sobol <- readr::read_rds(file = "Data/Modified/02_sensitivity/model_runs_sobol.rds")

model_sobol2007 <- readr::read_rds(file = "Data/Modified/02_sensitivity/model_sobol2007.rds")

# # import data for random movement of last timestep
# model_runs_sobol <- list.files(path = "Data/Modified/02_sensitivity/", 
#                                pattern = "^sobol_", full.names = TRUE) %>% 
#   stringr::str_sort(numeric = TRUE) %>% 
#   purrr::map(readr::read_rds)

#### Preprocess data ####

# get mean of biomass
bg_biomass <- purrr::map_dbl(model_runs_sobol, function(i) mean(i$seafloor$bg_biomass, na.rm = TRUE))

ag_biomass <- purrr::map_dbl(model_runs_sobol, function(i) mean(i$seafloor$ag_biomass, na.rm = TRUE))

# center results by mean
bg_biomass <- bg_biomass - mean(bg_biomass)

ag_biomass <- ag_biomass - mean(ag_biomass)

# get mean fish_length
fish_length <- purrr::map_dbl(model_runs_sobol, function(i) mean(i$fishpop$length))

fish_length <- fish_length - mean(fish_length)

# get mean fish_weight
fish_weight <- purrr::map_dbl(model_runs_sobol, function(i) mean(i$fishpop$weight))

fish_weight <- fish_weight - mean(fish_weight)

#### Add data to Sobol objects ####

# add the simulation results to the sobol instance #
model_sobol_bg <- sensitivity::tell(model_sobol2007, bg_biomass)

model_sobol_ag <- sensitivity::tell(model_sobol2007, ag_biomass)

model_sobol_length <- sensitivity::tell(model_sobol2007, fish_length)

model_sobol_weight <- sensitivity::tell(model_sobol2007, fish_weight)

# convert bg biomass to df (main effect)
model_sobol_bg_df <- tibble::as_tibble(model_sobol_bg$S) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Main effect", 
                output = "Belowground biomass")

# convert bg biomass to df (total effect)
model_sobol_bg_df <- tibble::as_tibble(model_sobol_bg$T) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Total effect", 
                output = "Belowground biomass") %>% 
  dplyr::bind_rows(model_sobol_bg_df, .) %>% 
  dplyr::mutate(value = dplyr::case_when(value < 0 ~ 0, 
                                         value > 1 ~ 1, 
                                         value > 0 & value < 1 ~ value), 
                min_ci = dplyr::case_when(min_ci < 0 ~ 0, 
                                          min_ci > 0 ~ min_ci), 
                max_ci = dplyr::case_when(max_ci > 1 ~ 1, 
                                          max_ci < 1 ~ max_ci))

# convert ag biomass to df (main effect)
model_sobol_ag_df <- tibble::as_tibble(model_sobol_ag$S) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Main effect", 
                output = "Aboveground biomass")

# convert ag biomass to df (total effect)
model_sobol_ag_df <- tibble::as_tibble(model_sobol_ag$T) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Total effect", 
                output = "Aboveground biomass") %>% 
  dplyr::bind_rows(model_sobol_ag_df, .) %>% 
  dplyr::mutate(value = dplyr::case_when(value < 0 ~ 0, 
                                         value > 1 ~ 1, 
                                         value > 0 & value < 1 ~ value), 
                min_ci = dplyr::case_when(min_ci < 0 ~ 0, 
                                          min_ci > 0 ~ min_ci), 
                max_ci = dplyr::case_when(max_ci > 1 ~ 1, 
                                          max_ci < 1 ~ max_ci))

# convert length to df (main effect)
model_sobol_length_df <- tibble::as_tibble(model_sobol_length$S) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Main effect", 
                output = "Length")

# convert length to df (total effect)
model_sobol_length_df <- tibble::as_tibble(model_sobol_length$T) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Total effect", 
                output = "Length") %>% 
  dplyr::bind_rows(model_sobol_length_df, .) %>% 
  dplyr::mutate(value = dplyr::case_when(value < 0 ~ 0, 
                                         value > 1 ~ 1, 
                                         value > 0 & value < 1 ~ value), 
                min_ci = dplyr::case_when(min_ci < 0 ~ 0, 
                                          min_ci > 0 ~ min_ci), 
                max_ci = dplyr::case_when(max_ci > 1 ~ 1, 
                                          max_ci < 1 ~ max_ci))

# convert weight to df (main effect)
model_sobol_weight_df <- tibble::as_tibble(model_sobol_weight$S) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Main effect", 
                output = "Weight")

# convert weight to df (total effect)
model_sobol_weight_df <- tibble::as_tibble(model_sobol_weight$T) %>% 
  purrr::set_names(c("value", "bias", "std_error", "min_ci", "max_ci")) %>% 
  dplyr::mutate(parameter = c("ag_biomass_max", "bg_biomass_max", "bg_k_m", 
                              "detritus_ratio", "pop_a_grunt", "pop_b_grunt", 
                              "pop_k_grunt", "pop_linf_grunt", "pop_n_body", 
                              "resp_slope", "resp_intercept", "resp_temp_low", 
                              "resp_temp_optm"), 
                effect = "Total effect", 
                output = "Weight") %>% 
  dplyr::bind_rows(model_sobol_weight_df, .) %>% 
  dplyr::mutate(value = dplyr::case_when(value < 0 ~ 0, 
                                         value > 1 ~ 1, 
                                         value > 0 & value < 1 ~ value), 
                min_ci = dplyr::case_when(min_ci < 0 ~ 0, 
                                          min_ci > 0 ~ min_ci), 
                max_ci = dplyr::case_when(max_ci > 1 ~ 1, 
                                          max_ci < 1 ~ max_ci))

#### Combine to total data.frame ####

model_sobol_overall_df <- dplyr::bind_rows(model_sobol_ag_df, 
                                           model_sobol_bg_df,
                                           model_sobol_length_df, 
                                           model_sobol_weight_df) %>% 
  dplyr::mutate(effect = factor(effect, levels = c("Main effect", 
                                                   "Total effect")), 
                output = factor(output, levels = c("Belowground biomass",
                                                   "Aboveground biomass", 
                                                   "Length", "Weight")))

# create ggplot
ggplot_sobol <- ggplot(data = model_sobol_overall_df) + 
  geom_point(aes(x = parameter, y = value, col = effect),
             size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x  = parameter, ymin = min_ci, ymax = max_ci,col = effect),
                width = 0.1, position = position_dodge(width = 0.5),
                size = 0.25) +
  facet_wrap(~ output, scales = "free_x", ncol = 2, nrow = 2) +
  scale_color_manual(name = "", values = c("Main effect" = "#0D0887FF",
                                           "Total effect" = "#ED7953FF")) +
  scale_y_continuous(name = "Effect strength", limits = c(0, 1)) +
  scale_x_discrete(name = "Parameter") +
  theme_classic(base_size = 12.5) + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1))

#### save result ####
suppoRt::save_ggplot(plot = ggplot_sobol, 
                     filename = "ggplot_sa_sobol.png", 
                     path = "Figures/",     
                     dpi = dpi,
                     width = height_full, height = width_full, units = units, 
                     overwrite = FALSE)
