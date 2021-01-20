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
# Plot possible outcome of bg_thres, detritus_ratio and  detritus_mineralization 
# parameter values.

source("01_Helper_functions/setup.R")

source("01_Helper_functions/classify_range.R")

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

#### Preprocess data ####

# tibble with values of levels
labels_design <- tibble::tibble(levels = c(1, 2, 3, 4, 5), 
                                bg_thres = unique(full_design$bg_thres), 
                                detritus_ratio = unique(full_design$detritus_ratio))

# add id col to full_design tibble and convert as factors
full_design_lvl <- dplyr::mutate(full_design, 
                                 bg_thres = factor(x = bg_thres, 
                                                   levels = labels_design$bg_thres, 
                                                   labels = labels_design$levels),
                                 detritus_ratio = factor(x = detritus_ratio, 
                                                         levels = labels_design$detritus_ratio, 
                                                         labels = labels_design$levels)) %>% 
  tibble::add_column(id = 1:nrow(.), .before = TRUE)

#### Check if maximum biomass is larger than mean ####

# random movement pattern
biomass_change <- purrr::map_chr(model_runs, classify_range, threshold = 0.25) %>% 
  dplyr::bind_cols(full_design_lvl, result = .) %>%
  dplyr::mutate(result = factor(result, levels = c("bg-//ag-", "bg-//ag+",
                                                   "bg+//ag-", "bg+//ag+")))

#### Total outcome possibilities #### 

# total count of result levels
biomass_change_sum <- dplyr::group_by(biomass_change, result, .drop = FALSE) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(n_rel = (n / sum(n)) * 100)

# create ggplot
gg_change_sum <- ggplot(data = biomass_change_sum) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 50, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 100, linetype = 2, col = "lightgray") +
  geom_linerange(aes(x = result, ymin = 0, ymax = n_rel),
                 col = "lightgrey", size = 1.5) +
  geom_point(aes(x = result, y = n_rel, col = result),
             size = 5, position = position_dodge(0.5)) + 
  scale_color_viridis_d(option = "D") +
  guides(col = FALSE) +
  labs(x = "Biomass change", y = "Relative count [%]") +
  theme_classic(base_size = base_size)

#### Parameter share outcomes #### 

# reshape to long format
biomass_change_lng <- tidyr::pivot_longer(biomass_change, -c(id, result),
                                          values_to = "level") %>%
  dplyr::group_by(result, name, level) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100) %>%
  dplyr::ungroup() %>%
  tidyr::complete(result, name, level, fill = list(n = 0, n_rel = 0)) %>% 
  dplyr::filter(result != "bg-//ag+")

# broken down for parameters
gg_para <- ggplot(data = biomass_change_lng) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 50, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 100, linetype = 2, col = "lightgray") +
  geom_linerange(aes(x = level, ymin = 0, ymax = n_rel),
                 col = "lightgrey", size = 1.5) +
  geom_point(aes(x = level, y = n_rel, col = level),
             size = 3.5,) +
  facet_grid(name ~ result) + 
  scale_color_viridis_d(option = "C") +
  guides(col = FALSE) +
  labs(x = "Factor level", y = "Relative count [%]") +
  theme_classic(base_size = base_size)

#### Save results ####

overwrite <- FALSE

suppoRt::save_rds(object = biomass_change, filename = "biomass_change_tbl.rds", 
                  path = "02_Data/02_Modified/01_explore_para_range",
                  overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_change_sum, 
                     filename = "04_Figures/01_explore_para_range/gg_explore_overview.png", 
                     height = width_full, width = height_full, units = units, 
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_para, 
                     filename = "04_Figures/01_explore_para_range/gg_explore_levels.png", 
                     height = width_full, width = height_full, units = units, 
                     dpi = dpi, overwrite = overwrite)
