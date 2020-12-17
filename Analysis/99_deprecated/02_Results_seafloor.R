##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

source("Helper_functions/setup.R")

#### Load data and parameters ####

result_nofish <- readr::read_rds("Data/Modified/result_nofish.rds")

result_randmove <- readr::read_rds("Data/Modified/result_randmove.rds")

result_attrmove <- readr::read_rds("Data/Modified/result_attrmove.rds")

#### Preprocess data ####

# get limits for better plotting
limits_list <- get_limits(list(result_nofish, result_randmove, result_attrmove))

# get coordinates of research area
poly <- data.frame(x = c(result_nofish$extent[1], result_nofish$extent[2],
                         result_nofish$extent[2], result_nofish$extent[1]),
                   y = c(result_nofish$extent[3], result_nofish$extent[3],
                         result_nofish$extent[4], result_nofish$extent[4]))

#### Seafloor maps ####

# map of seafloor values
gg_nofish_sflr <- plot(result_nofish, what = "seafloor", limits = limits_list)

gg_randmove_sflr <- plot(result_randmove, what = "seafloor", limits = limits_list)

gg_attrmove_sflr <- plot(result_attrmove, what = "seafloor", limits = limits_list)

#### Seafloor value over time #### 

gg_nofish_sflr_sum <- plot(result_nofish, summarize = TRUE, what = "seafloor")

gg_randmove_sflr_sum <- plot(result_randmove, summarize = TRUE, what = "seafloor")

gg_attrmove_sflr_sum <- plot(result_attrmove, summarize = TRUE, what = "seafloor")

#### Compare no fish vs random movement #### 

# get results of last time step
base_df <- dplyr::filter(result_nofish$seafloor,timestep == max(timestep))

compare_df <- dplyr::filter(result_randmove$seafloor, timestep == max(timestep))

# get densities
density_df <- get_density(result = result_nofish) %>%
  dplyr::full_join(get_density(result = result_randmove),
                   by = c("x", "y"), suffix = c(".nofish", ".random_move")) %>%
  dplyr::mutate(density_delta = density.nofish - density.random_move)

# get limits for plotting
density_limits <- dplyr::pull(density_df, density_delta) %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

# create ggplots
gg_dens_nofish_rand <- ggplot(data = density_df) +
  geom_raster(aes(x = x, y = y, fill = density_delta)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-density_limits, density_limits),
                       name = "Difference density\n[# / timesteps]") +
  coord_equal() +
  labs(title = "No fish pop vs. random movement") +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(12.5, "mm"))

# difference of biomass
biomass_df <- dplyr::full_join(x = base_df, y = compare_df,
                               by = c("x", "y", "timestep", "reef", "reef_dist"),
                               suffix = c(".base", ".compare")) %>%
  dplyr::mutate(ag_delta = ag_biomass.base - ag_biomass.compare,
                bg_delta = bg_biomass.base - bg_biomass.compare) %>%
  dplyr::select(x, y,
                ag_delta, bg_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("ag_delta", "bg_delta")))

# difference of nutrients
nutrients_df <- dplyr::full_join(x = base_df, y = compare_df,
                                 by = c("x", "y", "timestep", "reef", "reef_dist"),
                                 suffix = c(".base", ".compare")) %>%
  dplyr::mutate(nutrients_delta = nutrients_pool.base - nutrients_pool.compare,
                detritus_delta = detritus_pool.base - detritus_pool.compare) %>%
  dplyr::select(x, y, nutrients_delta, detritus_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("nutrients_delta", "detritus_delta")))

# difference of slough, consumption, excretion
dynamics_df <- dplyr::full_join(x = base_df, y = compare_df,
                                by = c("x", "y", "timestep", "reef", "reef_dist"),
                                suffix = c(".base", ".compare")) %>%
  dplyr::mutate(slough_delta = slough.base - slough.compare,
                consumption_delta = consumption.base - consumption.compare,
                excretion_delta = excretion.base - excretion.compare) %>%
  dplyr::select(x, y, slough_delta, consumption_delta, excretion_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("slough_delta",
                                               "consumption_delta", "excretion_delta")))

# get limits for plotting
biomass_limits <- biomass_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

nutrients_limits <- nutrients_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

dynamics_limits <- dynamics_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

# create ggplots
biomass_gg <- ggplot(data = biomass_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 2) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-biomass_limits, biomass_limits),
                       name = "Difference\nbiomass [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

# create ggplots
nutrients_gg <- ggplot(data = nutrients_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 2) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-nutrients_limits, nutrients_limits),
                       name = "Difference\nnutrients [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

dynamics_gg <- ggplot(data = dynamics_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 3) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-dynamics_limits, dynamics_limits),
                       name = "Difference\nnutrients [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

# create total plot
gg_overall <- cowplot::plot_grid(biomass_gg, 
                                 nutrients_gg, 
                                 dynamics_gg, 
                                 nrow = 1, ncol = 3)

# now add the title
title <- cowplot::ggdraw() + 
  cowplot::draw_label(label = "No fish pop vs. random movement",
                      x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

gg_nofish_random <- cowplot::plot_grid(title, gg_overall,
                                       ncol = 1, rel_heights = c(0.1, 1))

#### Compare random movement vs attraction #### 

# get results of last time step
base_df <- dplyr::filter(result_randmove$seafloor, timestep == max(timestep))

compare_df <- dplyr::filter(result_attrmove$seafloor, timestep == max(timestep))

# get densities
density_df <- get_density(result = result_randmove) %>%
  dplyr::full_join(get_density(result = result_attrmove),
                   by = c("x", "y"), suffix = c(".random_move", ".attrmove")) %>%
  dplyr::mutate(density_delta = density.random_move - density.attrmove)

# get limits for plotting
density_limits <- dplyr::pull(density_df, density_delta) %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

# create ggplots
gg_dens_rand_attr <- ggplot(data = density_df) +
  geom_raster(aes(x = x, y = y, fill = density_delta)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-density_limits, density_limits),
                       name = "Difference density\n[# / timesteps]") +
  coord_equal() +
  labs(title = "Random movement vs. attraction movement") +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(12.5, "mm"))

# difference of biomass
biomass_df <- dplyr::full_join(x = base_df, y = compare_df,
                               by = c("x", "y", "timestep", "reef", "reef_dist"),
                               suffix = c(".base", ".compare")) %>%
  dplyr::mutate(ag_delta = ag_biomass.base - ag_biomass.compare,
                bg_delta = bg_biomass.base - bg_biomass.compare) %>%
  dplyr::select(x, y,
                ag_delta, bg_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("ag_delta", "bg_delta")))

# difference of nutrients
nutrients_df <- dplyr::full_join(x = base_df, y = compare_df,
                                 by = c("x", "y", "timestep", "reef", "reef_dist"),
                                 suffix = c(".base", ".compare")) %>%
  dplyr::mutate(nutrients_delta = nutrients_pool.base - nutrients_pool.compare,
                detritus_delta = detritus_pool.base - detritus_pool.compare) %>%
  dplyr::select(x, y, nutrients_delta, detritus_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("nutrients_delta", "detritus_delta")))

# difference of slough, consumption, excretion
dynamics_df <- dplyr::full_join(x = base_df, y = compare_df,
                                by = c("x", "y", "timestep", "reef", "reef_dist"),
                                suffix = c(".base", ".compare")) %>%
  dplyr::mutate(slough_delta = slough.base - slough.compare,
                consumption_delta = consumption.base - consumption.compare,
                excretion_delta = excretion.base - excretion.compare) %>%
  dplyr::select(x, y, slough_delta, consumption_delta, excretion_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("slough_delta",
                                               "consumption_delta", "excretion_delta")))

# get limits for plotting
biomass_limits <- biomass_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

nutrients_limits <- nutrients_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

dynamics_limits <- dynamics_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

# create ggplots
biomass_gg <- ggplot(data = biomass_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 2) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-biomass_limits, biomass_limits),
                       name = "Difference\nbiomass [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

# create ggplots
nutrients_gg <- ggplot(data = nutrients_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 2) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-nutrients_limits, nutrients_limits),
                       name = "Difference\nnutrients [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

dynamics_gg <- ggplot(data = dynamics_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 3) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-dynamics_limits, dynamics_limits),
                       name = "Difference\nnutrients [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

# create total plot
gg_overall <- cowplot::plot_grid(biomass_gg, 
                                 nutrients_gg, 
                                 dynamics_gg, 
                                 nrow = 1, ncol = 3)

# now add the title
title <- cowplot::ggdraw() + 
  cowplot::draw_label(label = "Random movement vs. attraction movement",
                      x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

gg_random_attr <- cowplot::plot_grid(title, gg_overall, 
                                     ncol = 1, rel_heights = c(0.1, 1))


#### Compare no fish vs attraction #### 

# get results of last time step
base_df <- dplyr::filter(result_nofish$seafloor, timestep == max(timestep))

compare_df <- dplyr::filter(result_attrmove$seafloor, timestep == max(timestep))

# get densities
density_df <- get_density(result = result_nofish) %>%
  dplyr::full_join(get_density(result = result_attrmove),
                   by = c("x", "y"), suffix = c(".nofish", ".attrmove")) %>%
  dplyr::mutate(density_delta = density.nofish - density.attrmove)

# get limits for plotting
density_limits <- dplyr::pull(density_df, density_delta) %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

# create ggplots
gg_dens_nofish_attr <- ggplot(data = density_df) +
  geom_raster(aes(x = x, y = y, fill = density_delta)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-density_limits, density_limits),
                       name = "Difference density\n[# / timesteps]") +
  coord_equal() +
  labs(title = "No fish pop vs. attraction movement") +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(12.5, "mm"))

# difference of biomass
biomass_df <- dplyr::full_join(x = base_df, y = compare_df,
                               by = c("x", "y", "timestep", "reef", "reef_dist"),
                               suffix = c(".base", ".compare")) %>%
  dplyr::mutate(ag_delta = ag_biomass.base - ag_biomass.compare,
                bg_delta = bg_biomass.base - bg_biomass.compare) %>%
  dplyr::select(x, y,
                ag_delta, bg_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("ag_delta", "bg_delta")))

# difference of nutrients
nutrients_df <- dplyr::full_join(x = base_df, y = compare_df,
                                 by = c("x", "y", "timestep", "reef", "reef_dist"),
                                 suffix = c(".base", ".compare")) %>%
  dplyr::mutate(nutrients_delta = nutrients_pool.base - nutrients_pool.compare,
                detritus_delta = detritus_pool.base - detritus_pool.compare) %>%
  dplyr::select(x, y, nutrients_delta, detritus_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("nutrients_delta", "detritus_delta")))

# difference of slough, consumption, excretion
dynamics_df <- dplyr::full_join(x = base_df, y = compare_df,
                                by = c("x", "y", "timestep", "reef", "reef_dist"),
                                suffix = c(".base", ".compare")) %>%
  dplyr::mutate(slough_delta = slough.base - slough.compare,
                consumption_delta = consumption.base - consumption.compare,
                excretion_delta = excretion.base - excretion.compare) %>%
  dplyr::select(x, y, slough_delta, consumption_delta, excretion_delta) %>%
  tidyr::pivot_longer(!c(x, y)) %>%
  dplyr::mutate(name = factor(name, levels = c("slough_delta",
                                               "consumption_delta", "excretion_delta")))

# get limits for plotting
biomass_limits <- biomass_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

nutrients_limits <- nutrients_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

dynamics_limits <- dynamics_df$value %>%
  range(na.rm = TRUE) %>%
  abs() %>%
  max()

# create ggplots
biomass_gg <- ggplot(data = biomass_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 2) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-biomass_limits, biomass_limits),
                       name = "Difference\nbiomass [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

# create ggplots
nutrients_gg <- ggplot(data = nutrients_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 2) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-nutrients_limits, nutrients_limits),
                       name = "Difference\nnutrients [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

dynamics_gg <- ggplot(data = dynamics_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = poly, aes(x = x, y = y), col = "black", fill = NA, size = 0.25) +
  facet_wrap(~ name, ncol = 1, nrow = 3) +
  scale_fill_gradientn(colours = c("#368AC0", "#FFFFFF", "#EC747F"),
                       na.value = "#9B964A", limits = c(-dynamics_limits, dynamics_limits),
                       name = "Difference\nnutrients [g]") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

# create total plot
gg_overall <- cowplot::plot_grid(biomass_gg, 
                                 nutrients_gg, 
                                 dynamics_gg, 
                                 nrow = 1, ncol = 3)

# now add the title
title <- cowplot::ggdraw() + 
  cowplot::draw_label(label = "No fish pop vs. attraction movement",
                      x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

gg_nofish_attr <- cowplot::plot_grid(title, gg_overall, 
                                     ncol = 1, rel_heights = c(0.1, 1))


#### Save ggplots ####

overwrite = FALSE

# save seafloor plots
suppoRt::save_ggplot(plot = gg_nofish_sflr, 
                     filename = "Figures/gg_nofish_sflr.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_randmove_sflr, 
                     filename = "Figures/gg_randmove_sflr.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_attrmove_sflr, 
                     filename = "Figures/gg_attrmove_sflr.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

# save seafloor over time
suppoRt::save_ggplot(plot = gg_nofish_sflr_sum, 
                     filename = "Figures/gg_nofish_sflr_sum.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_randmove_sflr_sum, 
                     filename = "Figures/gg_randmove_sflr_sum.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_attrmove_sflr_sum, 
                     filename = "Figures/gg_attrmove_sflr_sum.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

# save comparison plots
suppoRt::save_ggplot(plot = gg_nofish_random, 
                     filename = "Figures/gg_nofish_random.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_random_attr, 
                     filename = "Figures/gg_random_attr.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_nofish_attr, 
                     filename = "Figures/gg_nofish_attr.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)


