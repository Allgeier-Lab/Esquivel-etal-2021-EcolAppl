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
# Create data for biomass starting values and parameters

# Treatment #
# A = no fertilizer, less fish
# B = no fertilizer, more fish

# C = fertilizer, less fish
# D = fertilizer, more fish

# Classification #
# ag = Blades + Sheath
# bg = Rhizomes + Roots

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

# load data layman
layman_2016 <- readxl::read_xlsx("02_Data/01_Raw/Layman_2016_Blade_data.xlsx")

yo_mama <- readxl::read_xlsx("02_Data/01_Raw/YoMama_Blade_data.xlsx")

# read data Bridget
bridget_2021 <- readxl::read_xlsx("02_Data/01_Raw/Data_Bridget_040121.xlsx", 
                                  sheet = "Biomass")

# # load Bahamas data
# bahamas_2018 <- readxl::read_xlsx("02_Data/01_Raw/Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx",
#                                   sheet = "Bahamas Blade Level Data")

surface_biomass_lm <- readr::read_rds("02_Data/02_Modified/04_various/surface_biomass_lm.rds")

#### Clean data ####

# rename cols without empty space
names(bridget_2021) <- stringr::str_replace_all(string = names(bridget_2021),
                                                pattern = " ", replace = "_") %>%
  stringr::str_to_lower()

# # rename cols without empty space
# names(bahamas_2018) <- stringr::str_replace_all(string = names(bahamas_2018), 
#                                                 pattern = " ", replace = "_") %>% 
#   stringr::str_to_lower()

#### Pre-process data ####

# filter only treatment A/B; scale to sqm
# "[...] Cores were taken with a 12.7cm diameter pvc pipe [...]"
layman_2016_cln <- dplyr::filter(layman_2016, treat %in% c("A", "B")) %>% 
  dplyr::select(dist, b.bio, below.bio) %>% 
  tidyr::pivot_longer(-dist) %>% 
  dplyr::mutate(value = value * pi * (12.7 / 2) ^ 2,
                name = dplyr::case_when(name == "b.bio" ~ "ag", 
                                        name == "below.bio" ~ "bg"))

# filter treatment a/b; convert area to biomass
yo_mama_cln <- dplyr::filter(yo_mama, treat %in% c("a", "b")) %>% 
  dplyr::mutate(value = exp(surface_biomass_lm$coefficients[[1]] + 
                                surface_biomass_lm$coefficients[[2]] * log(tot.b.area)) * 100) %>% 
  dplyr::select(tdist, value) %>% 
  dplyr::mutate(name = "ag") %>% 
  purrr::set_names(c("dist", "value", "name"))

# filter treatment A/B; only complete cases because NA just not measured yet; classify parts as ag/bg;
# get sum per core 
bridget_2021_cln <- dplyr::filter(bridget_2021, treatment %in% c("A", "B")) %>%
  dplyr::select(core_id, distance, shoots, sheaths, rhizomes, roots) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  tidyr::pivot_longer(-c(core_id, distance), names_to = "class", values_to = "value") %>% 
  dplyr::mutate(class = dplyr::case_when(class %in% c("shoots", "sheaths") ~ "ag",
                                         class %in% c("rhizomes", "roots") ~ "bg")) %>% 
  dplyr::group_by(core_id, distance, class) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop") %>% 
  dplyr::select(-core_id) %>% 
  purrr::set_names(c("dist", "name", "value"))

# remove last rows with NA; to get sum per core unique values because weight on shoot level; scale to sqm
bahamas_2018_cln <- dplyr::select(bahamas_2018, reef, dist, transect, blade_dry_weight) %>% 
  dplyr::filter(!is.na(blade_dry_weight)) %>% 
  dplyr::group_by(reef, dist, transect) %>% 
  dplyr::summarise(value = sum(unique(blade_dry_weight)) * 100, .groups = "drop") %>%
  select(-c(reef, transect)) %>% 
  dplyr::mutate(name = "ag")

# create one data.frame; filter distance between 5 - 100 m
biomass_pooled <- dplyr::bind_rows(layman = layman_2016_cln, 
                                   yo_mama = yo_mama_cln,  
                                   bridget = bridget_2021_cln, 
                                   # bahamas = bahamas_2018_cln, 
                                   .id = "source")

# get count of present distance classes
table(biomass_pooled$dist)

# create density plots
gg_biomass_range <- ggplot(data = biomass_pooled) + 
  geom_density(aes(value, col = source, fill = source), alpha = 0.1) + 
  facet_wrap(~name, scales = "free") + 
  scale_color_viridis_d(name = "Data source") +
  scale_fill_viridis_d(name = "Data source") +
  labs(x = "Biomass [g/sqm]", y = "Smoothed density") +
  theme_classic()

# summarize results
(biomass_values <- dplyr::group_by(biomass_pooled, name) %>% 
  dplyr::summarise(min = min(value), low = quantile(value, probs = 0.25),
                   mean = mean(value), median = median(value),
                   hi = quantile(value, probs = 0.75), max = max(value), 
                   sd = sd(value), n = dplyr::n()) %>% 
  dplyr::mutate(starting = min + ((max - min) * 0.01)))

#### Calculate uptake parameters ####
ag_v_max <- mean(c(8.5, 8.3, 12.8, 16.4, 11.7, 14.0, 16.5, 14.4))

bg_v_max <- mean(c(14.0, 10.9, 25.6, 73.3, 7.9, 52.6, 27.8, 18.4))


ag_k_m <- mean(c(15.0, 11.7, 7.7, 7.6, 15.2, 14.7, 19.4, 5.1))

bg_k_m <- mean(c(89.2, 172.1, 757.1, 765.5, 34.4, 649.5, 399.5, 60.8))

