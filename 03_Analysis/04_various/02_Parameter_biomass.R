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

layman_2016 <- readxl::read_xlsx("02_Data/01_Raw/Layman_2016_Blade_data.xlsx")

yo_mama <- readxl::read_xlsx("02_Data/01_Raw/YoMama_Blade_data.xlsx")

# read data Bridget
bridget_2021 <- readxl::read_xlsx("02_Data/01_Raw/Data_Bridget_040121.xlsx", 
                                  sheet = "Biomass")

# load Bahamas data
bahamas_2018 <- readxl::read_xlsx("02_Data/01_Raw/Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx",
                                  sheet = "Bahamas Blade Level Data")

surface_biomass_lm <- readr::read_rds("02_Data/02_Modified/04_various/surface_biomass_lm.rds")

#### Clean data ####

# rename cols without empty space
names(bridget_2021) <- stringr::str_replace_all(string = names(bridget_2021),
                                                pattern = " ", replace = "_") %>%
  stringr::str_to_lower()

# rename cols without empty space
names(bahamas_2018) <- stringr::str_replace_all(string = names(bahamas_2018), 
                                                pattern = " ", replace = "_") %>% 
  stringr::str_to_lower()

#### Pre-process data ####

layman_2016_cln <- dplyr::filter(layman_2016, treat %in% c("A", "B")) %>% 
  dplyr::select(dist, b.bio, below.bio) %>% 
  tidyr::pivot_longer(-dist) %>% 
  dplyr::mutate(value = value * 100,
                name = dplyr::case_when(name == "b.bio" ~ "ag", 
                                        name == "below.bio" ~ "bg"))

yo_mama_cln <- dplyr::filter(yo_mama, treat %in% c("a", "b")) %>% 
  dplyr::mutate(value = exp(surface_biomass_lm$coefficients[[1]] + 
                                surface_biomass_lm$coefficients[[2]] * log(tot.b.area)) * 100) %>% 
  dplyr::select(tdist, value) %>% 
  dplyr::mutate(name = "ag") %>% 
  purrr::set_names(c("dist", "value", "name"))

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

bahamas_2018_cln <- dplyr::select(bahamas_2018, reef, dist, transect, blade_dry_weight) %>% 
  dplyr::filter(!is.na(blade_dry_weight)) %>% 
  dplyr::group_by(reef, dist, transect) %>% 
  dplyr::summarise(value = sum(unique(blade_dry_weight)) * 100, .groups = "drop") %>%
  select(-c(reef, transect)) %>% 
  dplyr::mutate(name = "ag")

biomass_pooled <- dplyr::bind_rows(layman = layman_2016_cln, yo_mama = yo_mama_cln,  
                                   bridget = bridget_2021_cln, bahamas = bahamas_2018_cln, 
                                   .id = "source") %>% 
  dplyr::filter(dist < 100, dist > 5)

ggplot(data = biomass_pooled) + 
  geom_density(aes(value, col = source, fill = source), alpha = 0.1) + 
  facet_wrap(~name, scales = "free") + 
  scale_color_viridis_d(name = "Data source") +
  scale_fill_viridis_d(name = "Data source") +
  labs(x = "Biomass [g/sqm]", y = "Smoothed density") +
  theme_classic()

dplyr::group_by(biomass_pooled, name) %>% 
  dplyr::summarise(min = min(value), low = quantile(value, probs = 0.25),
                   mean = mean(value), median = median(value),
                   hi = quantile(value, probs = 0.75), max = max(value), 
                   sd = sd(value), n = dplyr::n())
