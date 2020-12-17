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

source("Helper_functions/setup.R")

source("Helper_functions/classify_result.R")

#### load data and parameters ####

model_runs_rand <- readr::read_rds(file = "Data/Modified/01_explore_para_range/model_runs_rand.rds")

model_runs_attr <- readr::read_rds(file = "Data/Modified/01_explore_para_range/model_runs_attr.rds")

# # load default starting values and parameters
# parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")
# 
# starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")
# 
# # import data for random movement of last timestep
# model_runs_rand <- list.files(path = "Data/Modified/01_explore_para_range/", 
#                               pattern = "rand\\.rds$", full.names = TRUE) %>% 
#   stringr::str_sort(numeric = TRUE) %>% 
#   purrr::map(function(i) arrR::filter_mdlrn(readr::read_rds(i)))
# 
# model_runs_attr <- list.files(path = "Data/Modified/01_explore_para_range/", 
#                               pattern = "attr\\.rds$", full.names = TRUE) %>% 
#   stringr::str_sort(numeric = TRUE) %>% 
#   purrr::map(function(i) arrR::filter_mdlrn(readr::read_rds(i)))

# get tibble with experiment design
full_design <- readr::read_rds(file = "Data/Modified/01_explore_para_range/explore_design.rds")

#### Preprocess data ####

# tibble with values of levels
labels_design <- tibble::tibble(levels = c(1, 2, 3, 4, 5), 
                                bg_thres = unique(full_design$bg_thres), 
                                detritus_ratio = unique(full_design$detritus_ratio), 
                                detritus_mineralization = unique(full_design$detritus_mineralization))

# add id col to full_design tibble and convert as factors
full_design_lvl <- dplyr::mutate(full_design, 
                                 bg_thres = factor(x = bg_thres, 
                                                   levels = labels_design$bg_thres, 
                                                   labels = labels_design$levels),
                                 detritus_ratio = factor(x = detritus_ratio, 
                                                         levels = labels_design$detritus_ratio, 
                                                         labels = labels_design$levels),
                                 detritus_mineralization = factor(x = detritus_mineralization, 
                                                                  levels = labels_design$detritus_mineralization, 
                                                                  labels = labels_design$levels)) %>% 
  tibble::add_column(id = 1:nrow(.), .before = TRUE)

#### Check if resulting biomass is < or > starting values ####

# random movement pattern
biomass_change_rand <- purrr::map_chr(model_runs_rand, classify_result)

# reef attraction movement pattern
biomass_change_attr <- purrr::map_chr(model_runs_attr, classify_result)

# create result tibbles
result_rand <- dplyr::bind_cols(full_design_lvl, result = biomass_change_rand) %>% 
  dplyr::mutate(result = factor(result, levels = c("bg-//ag-", "bg-//ag+", 
                                                   "bg+//ag-", "bg+//ag+", "bg=//ag=")))

result_attr <- dplyr::bind_cols(full_design_lvl, result = biomass_change_attr) %>% 
  dplyr::mutate(result = factor(result, levels = c("bg-//ag-", "bg-//ag+", 
                                                   "bg+//ag-", "bg+//ag+", "bg=//ag=")))

#### Total outcome possibilities #### 

# total count of result levels
result_rand_sum <- dplyr::group_by(result_rand, result, .drop = FALSE) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(n_rel = (n / sum(n)) * 100,
                type = "random")

result_attr_sum <- dplyr::group_by(result_attr, result, .drop = FALSE) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(n_rel = (n / sum(n)) * 100,
                type = "attract")

# combine to one data.frame
result_total_sum <- dplyr::bind_rows(result_rand_sum, result_attr_sum) %>% 
  dplyr::mutate(type = factor(type, levels = c("random", "attract"),
                              labels = c("Random movement", "Attracted movement")))

# create ggplot
gg_total_result <- ggplot(data = result_total_sum) + 
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 50, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 100, linetype = 2, col = "lightgray") +
  geom_linerange(aes(x = result, ymin = 0, ymax = n_rel, group = type),
                 col = "lightgrey", size = 1.5, position = position_dodge(0.5)) +
  geom_point(aes(x = result, y = n_rel, col = type, shape = type),
             size = 5, position = position_dodge(0.5)) + 
  scale_color_viridis_d(option = "D") +
  scale_shape_manual(name = "Movement type", values = c(15, 17)) +
  guides(col = FALSE) +
  labs(x = "Biomass change", y = "Relative count [%]") +
  theme_classic(base_size = base_size)

#### Parameter share outcomes #### 

# reshape to long format
result_rand_lng <- tidyr::pivot_longer(result_rand, -c(id, result),
                                       values_to = "level") %>%
  dplyr::group_by(result, name, level) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100,
                type = "random") %>%
  dplyr::ungroup() %>%
  tidyr::complete(result, name, level, fill = list(n = 0, n_rel = 0, type = "random"))

result_attr_lng <- tidyr::pivot_longer(result_attr, -c(id, result),
                                       values_to = "level") %>%
  dplyr::group_by(result, name, level) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100,
                type = "attract") %>%
  dplyr::ungroup() %>%
  tidyr::complete(result, name, level, fill = list(n = 0, n_rel = 0, type = "attract"))

# combine to one data.frame; remove results that are not present
result_para_lng <- dplyr::bind_rows(result_rand_lng, result_attr_lng) %>%
  dplyr::filter(!result %in% c("bg-//ag+", "bg=//ag=")) %>% 
  dplyr::mutate(type = factor(type, levels = c("random", "attract"),
                              labels = c("Random movement", "Attracted movement")), 
                name = factor(name, levels = c("bg_thres", "detritus_ratio", 
                                               "detritus_mineralization")))

# broken down for parameters
gg_para <- ggplot(data = result_para_lng) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 50, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 100, linetype = 2, col = "lightgray") +
  geom_linerange(aes(x = level, ymin = 0, ymax = n_rel, group = type),
                 col = "lightgrey", size = 1.5, position = position_dodge(0.5)) +
  geom_point(aes(x = level, y = n_rel, col = level, shape = type),
             size = 3.5, position = position_dodge(0.5)) +
  facet_grid(name ~ result) + 
  scale_color_viridis_d(option = "C") +
  scale_shape_manual(name = "Movement type", values = c(15, 17)) +
  guides(col = FALSE) +
  labs(x = "Factor level", y = "Relative count [%]") +
  theme_classic(base_size = base_size)

# broken down for parameters; ag+/bg+ only
# gg_para_inc <- ggplot(data = dplyr::filter(result_para_lng, result == "bg+//ag+")) +
#   geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
#   geom_hline(yintercept = 50, linetype = 2, col = "lightgray") +
#   geom_hline(yintercept = 100, linetype = 2, col = "lightgray") +
#   geom_linerange(aes(x = level, ymin = 0, ymax = n_rel, group = type),
#                  col = "lightgrey", size = 1.5, position = position_dodge(0.5)) +
#   geom_point(aes(x = level, y = n_rel, col = level, shape = type),
#              size = 3.5, position = position_dodge(0.5)) +
#   facet_grid(name ~ result) +
#   scale_color_viridis_d(option = "C") +
#   scale_shape_manual(name = "Movement type", values = c(15, 17)) +
#   guides(col = FALSE) +
#   labs(x = "Factor level", y = "Relative count [%]") +
#   theme_classic()


#### Possible combinations of possibility 4 ####

# # filter data only including possibility 4); count possibilities
# result_attr_inc <- dplyr::filter(result_attr, result == "bg+//ag+") %>% 
#   dplyr::group_by(detritus_ratio, detritus_mineralization, bg_thres, .drop = FALSE) %>% 
#   dplyr::summarise(freq = dplyr::n())
# 
# gg_alluvial_inc <- ggplot(data = result_attr_inc, 
#                           aes(y = freq, axis1 = bg_thres, axis2 = detritus_ratio, 
#                               axis3 = detritus_mineralization)) + 
#   geom_alluvium(aes(fill = bg_thres)) + 
#   geom_stratum(width = 1/8) + 
#   geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
#   scale_fill_viridis_d(option = "B") + 
#   scale_x_continuous(breaks = 1:3, labels = c("bg_thres", "detritus_ratio", "detritus_mineralization")) + 
#   theme_classic(base_size = base_size)

#### Save results ####

overwrite <- FALSE

suppoRt::save_rds(object = result_rand, filename = "explore_rand_tbl.rds", 
                  path = "Data/Modified/01_explore_para_range",
                  overwrite = overwrite)

suppoRt::save_rds(object = result_attr, filename = "explore_attr_tbl.rds", 
                  path = "Data/Modified/01_explore_para_range", 
                  overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_total_result, 
                     filename = "Figures/gg_explore_overview.png", 
                     height = width_full, width = height_full, units = units, 
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_para, 
                     filename = "Figures/gg_explore_separately.png", 
                     height = width_full, width = height_full, units = units, 
                     dpi = dpi, overwrite = overwrite)

# suppoRt::save_ggplot(plot = gg_alluvial_inc, 
#                      filename = "Figures/gg_explore_para_alluvial.png", 
#                      height = width_full, width = height_full, units = units, 
#                      dpi = dpi, overwrite = overwrite)
