##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

source("Helper_functions/setup.R")
source("Helper_functions/compare_models.R")

#### load data and parameters ####

# load default starting values and parameters
parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

# get tibble with experiment design
full_design <- readr::read_rds(file = "Data/Modified/results_full_design/full_design.rds")

# get names result of model runs (too large to load whole object)
model_runs <- list.files(path = "Data/Modified/results_full_design", full.names = TRUE) %>% 
  stringr::str_subset(pattern = "future_*") %>% 
  stringr::str_sort(numeric = TRUE)

# get default baseline model
results_def <- readr::read_rds(file = "Data/Modified/results_full_design/result_def.rds")

#### Preprocess data #### 
full_design <- dplyr::mutate(full_design, 
                             describ = paste(pop_n, nutrients_pool,
                                             round(full_design$bg_biomass_max,0), 
                                             detritus_ratio, detritus_mineralization, 
                                             sep = "_"),
                             id = 1:nrow(full_design))

# get default parameter values
pop_n_range <- sort(unique(full_design$pop_n))

nutrients_pool_range <- sort(unique(full_design$nutrients_pool))

bg_biomass_max_range <- sort(unique(full_design$bg_biomass_max))

detritus_ratio_range <- sort(unique(full_design$detritus_ratio))

detritus_mineralization_range <- sort(unique(full_design$detritus_mineralization))

#### Pop n ####
id_temp <- dplyr::filter(full_design,
                         nutrients_pool == nutrients_pool_range[[5]], 
                         bg_biomass_max == bg_biomass_max_range[[4]], 
                         detritus_ratio == detritus_ratio_range[[2]], 
                         detritus_mineralization == detritus_mineralization_range[[1]])

result_temp <- compare_models(x = model_runs, id = id_temp, col = "pop_n") %>% 
  dplyr::filter(summary == "mean")

gg_pop_n <- ggplot(data = result_temp) + 
  geom_line(aes(x = timestep, y = value, col = design_id)) + 
  facet_wrap(~name, scales = "free_y") + 
  scale_color_viridis_d(name = "# Fish population", option = "C") + 
  theme_classic()

#### Nutrients_pool ####
id_temp <- dplyr::filter(full_design, 
                         pop_n == pop_n_def, 
                         bg_biomass_max == bg_biomass_max_def, 
                         detritus_ratio == detritus_ratio_def, 
                         detritus_mineralization == detritus_mineralization_def)

result_temp <- compare_models(x = model_runs, id = id_temp, col = "nutrients_pool") %>% 
  dplyr::filter(summary == "mean")

gg_nutrients <- ggplot(data = result_temp) + 
  geom_line(aes(x = timestep, y = value, col = design_id)) + 
  facet_wrap(~name, scales = "free_y") + 
  scale_color_viridis_d(name = "Nutrients pool", option = "C") + 
  theme_classic()

#### Bg biomass max ####
id_temp <- dplyr::filter(full_design, 
                         pop_n == pop_n_def, 
                         nutrients_pool == nutrients_pool_def,
                         detritus_ratio == detritus_ratio_def, 
                         detritus_mineralization == detritus_mineralization_def)

result_temp <- compare_models(x = model_runs, id = id_temp, col = "bg_biomass_max") %>% 
  dplyr::filter(summary == "mean")

gg_biomass <- ggplot(data = result_temp) + 
  geom_line(aes(x = timestep, y = value, col = design_id)) + 
  facet_wrap(~name, scales = "free_y") + 
  scale_color_viridis_d(name = "Bg biomass max", option = "C") + 
  theme_classic()

#### Detritus ratio ####
id_temp <- dplyr::filter(full_design, 
                         pop_n == pop_n_def, 
                         nutrients_pool == nutrients_pool_def,
                         bg_biomass_max == bg_biomass_max_def, 
                         detritus_mineralization == detritus_mineralization_def)

result_temp <- compare_models(x = model_runs, id = id_temp, col = "detritus_ratio") %>% 
  dplyr::filter(summary == "mean")

gg_detritus <- ggplot(data = result_temp) + 
  geom_line(aes(x = timestep, y = value, col = design_id)) + 
  facet_wrap(~name, scales = "free_y") + 
  scale_color_viridis_d(name = "Detritus ratio", option = "C") + 
  theme_classic()

#### Detritus mineralization ####
id_temp <- dplyr::filter(full_design, 
                         pop_n == pop_n_def, 
                         nutrients_pool == nutrients_pool_def,
                         bg_biomass_max == bg_biomass_max_def, 
                         detritus_ratio == detritus_ratio_def)

result_temp <- compare_models(x = model_runs, id = id_temp, col = "detritus_mineralization") %>% 
  dplyr::filter(summary == "mean")

gg_mineral <- ggplot(data = result_temp) + 
  geom_line(aes(x = timestep, y = value, col = design_id)) + 
  facet_wrap(~name, scales = "free_y") + 
  scale_color_viridis_d(name = "Mineralization", option = "C") + 
  theme_classic()

#### Save results ####
