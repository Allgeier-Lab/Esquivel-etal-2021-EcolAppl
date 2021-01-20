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

source("01_Helper_functions/setup.R")

source("01_Helper_functions/calc_biomass_dist.R")

source("01_Helper_functions/calc_biomass_dist_range.R")

source("01_Helper_functions/calc_biomass_dist_change.R")

#### load data and parameters ####

# # import data model runs
# model_runs <- list.files(path = "~/Downloads/results/",
#                          pattern = "^explore_run_", full.names = TRUE) %>%
#   stringr::str_sort(numeric = TRUE) %>%
#   purrr::map(function(i) readr::read_rds(i))
# 
# # save into one object
# suppoRt::save_rds(object = model_runs, filename = "model_runs.rds", 
#                   path = "Data/Modified/01_explore_para_range/", 
#                   overwrite = FALSE)

# import data model runs
model_runs <- readr::read_rds(file = "02_Data/02_Modified/01_explore_para_range/model_runs.rds")

# get tibble with experiment design
full_design <- readr::read_rds(file = "02_Data/02_Modified/01_explore_para_range/explore_design.rds")

# get classified levels
biomass_change <- readr::read_rds("02_Data/02_Modified/01_explore_para_range/biomass_change_tbl.rds")

#### Preprocess data #### 

# add id to design for later join
full_design <- tibble::add_column(full_design, id = 1:nrow(full_design), .before = TRUE)

# get id of all models that did not decrease bg
id_increase <- dplyr::filter(biomass_change, !result %in% c("bg-//ag-", "bg-//ag+")) %>% 
  dplyr::pull(id)

### Calculate biomass at distance #### 

# calculate biomass at distance r 
biomass_dist <- purrr::map_dfr(id_increase, 
                               function(i) calc_biomass_dist(model_runs[[i]])) %>% 
  dplyr::mutate(id = factor(rep(id_increase, each = nrow(.) / length(id_increase)))) %>% 
  tidyr::pivot_longer(-c(id, reef_dist_clss), names_to = "biomass")

#### Filter parameter combinations ####

# percentage between minimum and maximum biomass value (standardized by max biomass)
range_thres <- 0.5

# calculate relative range of all attracted movement models
biomass_range <- purrr::map_dfr(id_increase, 
                                function(i) calc_biomass_dist_range(model_runs[[i]])) %>% 
  dplyr::mutate(id = factor(id_increase))

# filter only parameter combinations above threshold
biomass_range_fltrd <- dplyr::filter(biomass_range, rel_range >= range_thres)

ggplot(data = dplyr::filter(biomass_dist, id %in% biomass_range_fltrd$id)) +
  geom_line(aes(x = reef_dist_clss, y = value, col = id, group = id)) +
  facet_wrap(~biomass, scales = "free_y") +
  guides(col = guide_legend(nrow = 2)) +
  scale_color_viridis_d(option = "D") +
  scale_x_continuous(breaks = seq(from = 1, to = 35, by = 3)) +
  labs(x = "Classified distance to reef [m] ", y = "Biomass dry [g]") +
  theme_classic() +
  theme(legend.position = "bottom")

# calculate relative biomass change at distance r in relation to biomass at reef
biomass_change_dist <- purrr::map_dfr(id_increase, 
                                 function(i) calc_biomass_dist_change(model_runs[[i]])) %>% 
  dplyr::mutate(id = factor(rep(id_increase, each = nrow(.) / length(id_increase))))

# filter all combinations that have a higher change already at short distance
biomass_change_dist_fltrd <- group_by(biomass_change_dist, id) %>%
  mutate(bg_change_sum = cumsum(bg_change), 
         ag_change_sum = cumsum(ag_change)) %>% 
  dplyr::filter(reef_dist_clss == 5, bg_change_sum > -0.75)

# get all id that are included in both filters
id_range_change <- as.numeric(intersect(biomass_range_fltrd$id, biomass_change_dist_fltrd$id))

# plot all realistic parameter combinations
gg_biomass_dist_sub <- ggplot(data = dplyr::filter(biomass_dist, id %in% id_range_change)) + 
  geom_line(aes(x = reef_dist_clss, y = value, col = id, group = id)) + 
  facet_wrap(~biomass, scales = "free_y") +  
  scale_color_viridis_d(option = "D") +
  scale_x_continuous(breaks = seq(from = 1, to = 35, by = 3)) +
  labs(x = "Classified distance to reef [m] ", y = "Biomass dry [g]") +
  theme_classic() + 
  theme(legend.position = "bottom")

# combine parameter and results in tibble
result_dist_change_fltrd <- dplyr::filter(full_design, id %in% id_range_change) %>% 
  dplyr::left_join(biomass_change, by = "id", suffix = c(".value", ".level"))

#### Save results ####

overwrite <- FALSE

suppoRt::save_rds(object = result_dist_change_fltrd, filename = "explore_dist_fltrd.rds", 
                  path = "02_Data/02_Modified/01_explore_para_range/", overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_biomass_dist_sub, 
                     filename = "04_Figures/01_explore_para_range/gg_explore_biomass_dist_fltrd.png", 
                     height = width_full, width = height_full, units = units, 
                     dpi = dpi, overwrite = overwrite)
