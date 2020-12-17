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
# Plot biomass over distance from reef for all parameter combinations that 
# did not decrease bg biomass and filter results with non-realistic patterns.

source("Helper_functions/setup.R")

source("Helper_functions/calc_biomass_dist.R")

source("Helper_functions/calc_biomass_range.R")

source("Helper_functions/calc_biomass_change.R")

#### load data and parameters ####

# # load default starting values and parameters
# parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")
# 
# starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

# get tibble with experiment design
full_design <- readr::read_rds(file = "Data/Modified/01_explore_para_range/explore_design.rds")

model_runs_rand <- readr::read_rds(file = "Data/Modified/01_explore_para_range/model_runs_rand.rds")

model_runs_attr <- readr::read_rds(file = "Data/Modified/01_explore_para_range/model_runs_attr.rds")

# # get names result of model runs
# model_runs_rand <- list.files(path = "Data/Modified/01_explore_para_range", 
#                               pattern = "rand\\.rds$", full.names = TRUE) %>% 
#   stringr::str_sort(numeric = TRUE) %>% 
#   purrr::map(function(i) arrR::filter_mdlrn(readr::read_rds(i)))
# 
# model_runs_attr <- list.files(path = "Data/Modified/01_explore_para_range", 
#                               pattern = "attr\\.rds$", full.names = TRUE) %>% 
#   stringr::str_sort(numeric = TRUE) %>% 
#   purrr::map(function(i) arrR::filter_mdlrn(readr::read_rds(i)))

# get tibble with parameter combination and output
result_rand <- readr::read_rds(file = "Data/Modified/01_explore_para_range/explore_rand_tbl.rds")

result_attr <- readr::read_rds(file = "Data/Modified/01_explore_para_range/explore_attr_tbl.rds")

#### Preprocess data #### 

# add id to design for later join
full_design <- tibble::add_column(full_design, id = 1:nrow(full_design), .before = TRUE)

# get id of all random models that did not decrease bg
id_inc_rand <- dplyr::filter(result_rand, !result %in% c("bg-//ag-", "bg-//ag+")) %>% 
  dplyr::pull(id)

# get id of all attracted models that did not decrease bg
id_inc_attr <- dplyr::filter(result_attr, !result %in% c("bg-//ag-", "bg-//ag+")) %>% 
  dplyr::pull(id)

# get all ids that are present for both movement patterns
id_intersect <- intersect(id_inc_rand, id_inc_attr)

### Calculate biomass at distance #### 

# calculate biomass at distance r for random movement
biomass_dist_rand <- purrr::map_dfr(id_intersect, 
                                    function(i) calc_biomass_dist(model_runs_rand[[i]])) %>% 
  dplyr::mutate(id = rep(id_intersect, each = nrow(.) / length(id_intersect)), 
                type = "rand")

# calculate biomass at distance r for attracted movement
biomass_dist_attr <- purrr::map_dfr(id_intersect, 
                                    function(i) calc_biomass_dist(model_runs_attr[[i]])) %>% 
  dplyr::mutate(id = rep(id_intersect, each = nrow(.) / length(id_intersect)), 
                type = "attr")

# combine to one data frame and reshape long
biomass_dist_full <- dplyr::bind_rows(biomass_dist_rand, biomass_dist_attr) %>% 
  dplyr::mutate(id = factor(id), type = factor(type, levels = c("rand", "attr"),
                                               labels = c("Random movement", 
                                                          "Attracted movement"))) %>% 
  tidyr::pivot_longer(-c(id, reef_dist_clss, type), names_to = "biomass")

#### Filter parameter combinations ####

# percentage between minimum and maximum biomass value (standardized by max biomass)
range_thres <- 25

# calculate relative range of all attracted movement models
biomass_range_attr <- purrr::map_dfr(id_intersect, 
                                     function(i) calc_biomass_range(model_runs_attr[[i]])) %>% 
  dplyr::mutate(id = id_intersect)

# filter only parameter combinations above threshold
biomass_range_attr_fltrd <- dplyr::filter(biomass_range_attr, rel_range >= range_thres)

ggplot(data = dplyr::filter(biomass_dist_full, id %in% biomass_range_attr_fltrd$id)) +
  geom_line(aes(x = reef_dist_clss, y = value, col = id, group = id)) +
  facet_wrap(biomass ~ type, scales = "free_y") +
  guides(col = guide_legend(nrow = 2)) +
  scale_color_viridis_d(option = "D") +
  scale_x_continuous(breaks = seq(from = 1, to = 35, by = 3)) +
  labs(x = "Classified distance to reef [m] ", y = "Biomass dry [g]") +
  theme_classic() +
  theme(legend.position = "bottom")

# calculate relative biomass change at distance r in relation to biomass at reef
biomass_change_attr <- purrr::map_dfr(id_intersect, 
                                      function(i) calc_biomass_change(model_runs_attr[[i]])) %>% 
  dplyr::mutate(id = rep(id_intersect, each = nrow(.) / length(id_intersect)))

# filter all combinations that have a higher change already at short distance
biomass_change_attr_fltrd <- group_by(biomass_change_attr, id) %>%
  mutate(bg_change_sum = cumsum(bg_change), 
         ag_change_sum = cumsum(ag_change)) %>% 
  dplyr::filter(reef_dist_clss == 5, bg_change_sum > -0.5)

# get all id that are included in both filters
id_range_change <- intersect(biomass_range_attr_fltrd$id, biomass_change_attr_fltrd$id)

# plot all realistic parameter combinations
gg_biomass_dist_sub <- ggplot(data = dplyr::filter(biomass_dist_full, 
                                                   id %in% id_range_change)) + 
  geom_line(aes(x = reef_dist_clss, y = value, col = id, group = id)) + 
  facet_wrap(biomass ~ type, scales = "free_y") +  
  guides(col = guide_legend(nrow = 2)) +  
  scale_color_viridis_d(option = "D") +
  scale_x_continuous(breaks = seq(from = 1, to = 35, by = 3)) +
  labs(x = "Classified distance to reef [m] ", y = "Biomass dry [g]") +
  theme_classic() + 
  theme(legend.position = "bottom")

# combine parameter and results in tibble
result_dist_change_fltrd <- dplyr::filter(full_design, id %in% id_range_change) %>% 
  dplyr::left_join(dplyr::select(result_rand, id, result), by = "id") %>% 
  dplyr::left_join(dplyr::select(result_attr, id, result), by = "id", suffix = c(".rand", ".attr"))

#### Save results ####

overwrite <- FALSE

suppoRt::save_rds(object = result_dist_change_fltrd, filename = "explore_dist_fltrd.rds", 
                  path = "Data/Modified/01_explore_para_range/", overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_biomass_dist, 
                     filename = "Figures/gg_explore_biomass_dist.png", 
                     height = width_full, width = height_full, units = units, 
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_biomass_dist_sub, 
                     filename = "Figures/gg_explore_biomass_dist_fltrd.png", 
                     height = width_full, width = height_full, units = units, 
                     dpi = dpi, overwrite = overwrite)
