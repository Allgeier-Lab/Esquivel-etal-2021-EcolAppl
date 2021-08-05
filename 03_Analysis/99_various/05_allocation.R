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
# Create figure with allocation ratio

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting-values.csv", sep = ";")

#### Create data ####

# create vector with biomass values
bg_biomass <- seq(from = parameters$bg_biomass_min,
                  to = parameters$bg_biomass_max, by = 0.1)

# create vector with threshold values
threshold <- c(-1/4, -1/2, -3/4)

# get ratios for each threshold and biomass (min - max)
ratio <- purrr::map_dfr(threshold, function(i) {
  
  ratio_temp <- purrr::map_dfr(bg_biomass, function(j) {
    
    value_temp <- rcpp_allocation_ratio(biomass = j, biomass_min = parameters$bg_biomass_min,
                                        biomass_max = parameters$bg_biomass_max,
                                        threshold = i, slope = parameters$seagrass_slope)
    
    data.frame(biomass = j, bg = value_temp)
    
  })
  
  cbind(threshold = i, ratio_temp)
  
})

# convert threshold to postive values (easier to read) and convert to long
ratio <- dplyr::mutate(ratio, threshold = factor(threshold * -1, ordered = TRUE), 
                       ag = 1 - bg) %>% 
  tidyr::pivot_longer(-c(threshold, biomass), names_to = "part") %>% 
  dplyr::mutate(part = factor(part, levels = c("bg", "ag"), 
                              labels = c("Belowground", "Aboveground")))

#### Create ggplot ####

ggplot_ratio <- ggplot(data = ratio) + 
  geom_line(aes(x = biomass, y = value, linetype = part, col = threshold)) + 
  scale_x_continuous(limits = c(parameters$bg_biomass_min, parameters$bg_biomass_max), 
                     breaks = seq(from = parameters$bg_biomass_min, to = parameters$bg_biomass_max, 
                                  length.out = 5)) +
  scale_color_manual(name = expression(paste("Midpoint ", tau)), 
                     values = c("#FF2500", "#5ABCD6", "#000000")) +
  scale_linetype_manual(name = "Biomass", values = c(1, 2)) +
  labs(x = "Belowground biomass", y = "Ratio of total nutrient uptake") + 
  theme_classic() + 
  theme(legend.position = "bottom")

#### Save plot ####

suppoRt::save_ggplot(plot = ggplot_ratio, filename = "gg_ratio.pdf", 
                     path = "04_Figures/03_various/", overwrite = FALSE, 
                     width = width, height = height / 2, dpi = dpi, units = "mm")

