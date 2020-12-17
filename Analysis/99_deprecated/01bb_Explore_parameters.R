##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

source("Helper_functions/setup.R")

#### load data and parameters ####

# load default starting values and parameters
parameters <- arrR::read_parameters("Data/Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("Data/Raw/starting_values.csv", sep = ";")

# # get names result of model runs (too large to load whole object)
# model_runs <- list.files(path = "Data/Modified/results_full_design", full.names = TRUE) %>% 
#   stringr::str_subset(pattern = "future_*") %>% 
#   stringr::str_sort(numeric = TRUE)

# get names result of model runs
model_runs_rand <- list.files(path = "Data/Modified/results_full_design", 
                         pattern = "rand\\.rds$", full.names = TRUE) %>% 
  stringr::str_sort(numeric = TRUE) %>% 
  purrr::map(readr::read_rds)

model_runs_attr <- list.files(path = "Data/Modified/results_full_design", 
                              pattern = "attr\\.rds$", full.names = TRUE) %>% 
  stringr::str_sort(numeric = TRUE) %>% 
  purrr::map(readr::read_rds)

# get tibble with experiment design
full_design <- readr::read_rds(file = "Data/Modified/results_full_design/full_design.rds")

# overwrite argument for saving
overwrite <- FALSE

#### Preprocess data ####

# tibble with values of levels
(labels_new <- tibble(levels = c(1, 2, 3, 4, 5), 
                      pop_n = unique(full_design$pop_n), 
                      nutrients_pool = unique(full_design$nutrients_pool), 
                      bg_biomass_max = rev(unique(full_design$bg_biomass_max)), # ! REMOVE REVERSE ! # 
                      detritus_ratio = unique(full_design$detritus_ratio), 
                      detritus_mineralization = unique(full_design$detritus_mineralization)))

# add id col to full_design tibble and convert as factors
full_design <- dplyr::mutate(full_design, 
                             id = 1:nrow(full_design), 
                             pop_n = factor(pop_n, labels = labels_new$levels),
                             nutrients_pool = factor(nutrients_pool, labels = labels_new$levels),
                             bg_biomass_max = factor(bg_biomass_max, labels = labels_new$levels),
                             detritus_ratio = factor(detritus_ratio, labels = labels_new$levels),
                             detritus_mineralization = factor(detritus_mineralization, labels = labels_new$levels))

#### Parameter combinations with change in ag biomass ####

ag_change_rand <- purrr::map_lgl(model_runs_rand, function(model_temp) {
  
  # # read current model
  # model_temp <- readr::read_rds(model_runs_rand[[i]])
  
  # get seafloor results
  seafloor_temp <- dplyr::filter(model_temp$seafloor, timestep == max(timestep))
  
  # calculate mean ag biomass of all cells
  ag_temp <- mean(seafloor_temp$ag_biomass, na.rm = TRUE)
  
  ifelse(test = ag_temp > starting_values$ag_biomass, 
         yes = TRUE, no = FALSE)
})

ag_change_attr <- purrr::map_lgl(model_runs_attr, function(model_temp) {
  
  # # read current model
  # model_temp <- readr::read_rds(model_runs_attr[[i]])
  
  # get seafloor results
  seafloor_temp <- dplyr::filter(model_temp$seafloor, timestep == max(timestep))
  
  # calculate mean ag biomass of all cells
  ag_temp <- mean(seafloor_temp$ag_biomass, na.rm = TRUE)
  
  ifelse(test = ag_temp > starting_values$ag_biomass, 
         yes = TRUE, no = FALSE)
})

#### Parameter combinations with mortality above threshold  ####

mortality_rand <- purrr::map_lgl(model_runs_rand, function(model_temp) {

    # # read current model
    # model_temp <- readr::read_rds(model_runs_rand[[i]])

    # get seafloor results
    fish_temp <- dplyr::filter(model_temp$fish_population, timestep == max(timestep))

    # calculate mean ag biomass of all cells
    mortality <- mean(fish_temp$died_consumption, na.rm = TRUE) / max(fish_temp$timestep)

    ifelse(test = mortality > 1, # 1/3
           yes = TRUE, no = FALSE)
})

mortality_attr <- purrr::map_lgl(model_runs_attr, function(model_temp) {
  
  # # read current model
  # model_temp <- readr::read_rds(model_runs_attr[[i]])
  
  # get seafloor results
  fish_temp <- dplyr::filter(model_temp$fish_population, timestep == max(timestep))
  
  # calculate mean ag biomass of all cells
  mortality <- mean(fish_temp$died_consumption, na.rm = TRUE) / max(fish_temp$timestep)
  
  ifelse(test = mortality > 1, # 1/3
         yes = TRUE, no = FALSE)
})

#### Parameter combinations with ag change and low mortality ####

# which ids are in ag_change but not high mortality
id_combined_rand <- setdiff(which(ag_change_rand), which(mortality_rand))

# calculate percentage of all parameter combinations
(perc_combined_rand <- (length(id_combined_rand) / length(model_runs_rand)) * 100)

# which ids are in ag_change but not high mortality
id_combined_attr <- setdiff(which(ag_change_attr), which(mortality_attr))

# calculate percentage of all parameter combinations
(perc_combined_attr <- (length(id_combined_attr) / length(model_runs_attr)) * 100)

# check which ids are only present in rand or attr
if (sum(ag_change_rand) > sum(ag_change_rand)) {
  
  id_combined_only <- setdiff(which(ag_change_rand), which(ag_change_attr))
  
  id_only_lab <- "random"
  
} else {
  
  id_combined_only <- setdiff(which(ag_change_attr), which(ag_change_rand))
  
  id_only_lab <- "attracted"
  
}

#### Count number of levels #### 

# reshape to long format
combined_rand_df <- dplyr::slice(full_design, id_combined_rand) %>%
  tidyr::pivot_longer(-id, values_to = "levels") %>%
  dplyr::group_by(name, levels) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100,
                levels = factor(levels),
                name = factor(name, levels = c("pop_n", "nutrients_pool", "bg_biomass_max",
                                               "detritus_ratio", "detritus_mineralization")), 
                type = "random") %>%
  dplyr::ungroup() %>% 
  tidyr::complete(name, levels, fill = list(n = 0, n_rel = 0, type = "random"))

# reshape to long format
combined_attr_df <- dplyr::slice(full_design, id_combined_attr) %>%
  tidyr::pivot_longer(-id, values_to = "levels") %>%
  dplyr::group_by(name, levels) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100,
                levels = factor(levels),
                name = factor(name, levels = c("pop_n", "nutrients_pool", "bg_biomass_max",
                                               "detritus_ratio", "detritus_mineralization")), 
                type = "attract") %>%
  dplyr::ungroup() %>% 
  tidyr::complete(name, levels, fill = list(n = 0, n_rel = 0, type = "attract"))

# reshape to long format
combined_only_df <- dplyr::slice(full_design, id_combined_only) %>%
  tidyr::pivot_longer(-id, values_to = "levels") %>%
  dplyr::group_by(name, levels) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100,
                levels = factor(levels),
                name = factor(name, levels = c("pop_n", "nutrients_pool", "bg_biomass_max",
                                               "detritus_ratio", "detritus_mineralization")), 
                type = "only") %>%
  dplyr::ungroup() %>% 
  tidyr::complete(name, levels, fill = list(n = 0, n_rel = 0, type = "only"))

# ag_change_df <- dplyr::bind_rows(combined_rand_df, combined_attr_df, combined_only_df) %>%
#   dplyr::mutate(type = factor(type, levels = c("random", "attract", "only"),
#                               labels = c("Random movement", "Attracted movement", 
#                                          paste0("Only ", id_only_lab, " movement"))))

ag_change_df <- dplyr::bind_rows(combined_rand_df, combined_attr_df) %>%
  dplyr::mutate(type = factor(type, levels = c("random", "attract"),
                              labels = c("Random movement", "Attracted movement")))

#### Create resulting ggplot ####

# create ggplot
gg_explore_para_ag <- ggplot(data = ag_change_df) +
  geom_hline(yintercept = 0, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 50, linetype = 2, col = "lightgray") +
  geom_hline(yintercept = 100, linetype = 2, col = "lightgray") +
  geom_linerange(aes(x = levels, ymin = 0, ymax = n_rel, group = type),
                 col = "lightgrey", size = 1.5, position = position_dodge(0.5)) +
  geom_point(aes(x = levels, y = n_rel, col = levels, shape = type),
             size = 3.5, position = position_dodge(0.5)) +
  facet_wrap(~ name, nrow = 5) + 
  scale_color_viridis_d(option = "C") +
  scale_shape_manual(name = "Movement type", values = c(15, 17)) +
  # scale_shape_manual(name = "Movement type", values = c(15, 16, 17)) +
  guides(col = FALSE) +
  labs(x = "Factor level", y = "Relative count [%]", 
       title = paste0("ag biomass changed: Random=", round(perc_combined_rand, digits = 1), 
                      "%; Attracted=", round(perc_combined_attr, digits = 1), "%"), 
       subtitle = paste0("Total parameter combinations: ", length(model_runs_rand))) +
  theme_classic() +
  theme(title = element_text(size = 10)) 

#### Save results ####

suppoRt::save_rds(full_design_subset, 
                  filename = "Data/Modified/results_full_design/full_design_subset.rds", 
                  overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_explore_para_ag, 
                     filename = "Figures/gg_explore_para_ag.png", 
                     height = height_full, width = width_full, units = units, 
                     dpi = dpi, overwrite = overwrite)
