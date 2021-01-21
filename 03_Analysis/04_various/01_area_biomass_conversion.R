##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# Treatment #
# A = no fertilizer, less fish
# B = no fertilizer, more fish
# C = fertilizer, less fish
# D = fertilizer, more fish

#-------------------#
# Purpose of Script #
#-------------------#
# Find conversion constant to converte from surface area to biomass

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

# read layman data 
layman_2016 <- readxl::read_xlsx("02_Data/01_Raw/Layman_2016_Blade_data.xlsx")

# read data Bridget
bridget_2021 <- readxl::read_xlsx("02_Data/01_Raw/Data_Bridget_040121.xlsx", 
                                  sheet = "Blade_data")

# load Bahamas data
bahamas_2018_full <- readxl::read_xlsx("02_Data/01_Raw/Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx", 
                                       sheet = "Bahamas Blade Level Data")

# load Bahamas data (sub sheet)
bahamas_2018_sub <- readxl::read_xlsx("02_Data/01_Raw/Haiti_Bahamas_Morph_Growth_Herbivory_2018.xlsx", 
                                      sheet = "Only new growth calc")

#### Data cleaning and pre-processing ####

# rename cols without empty space
names(bridget_2021) <- stringr::str_replace_all(string = names(bridget_2021),
                                                pattern = " ", replace = "_") %>%
  stringr::str_to_lower()

# remove empty line
# bridget_2021 <- dplyr::slice(bridget_2021, -2168)
bridget_2021 <- dplyr::filter_all(bridget_2021, dplyr::any_vars(!is.na(.)))

# split and filter treatments and calculate surface and weight per shoot
bridget_2021_sum <- tidyr::separate(data = bridget_2021, col = core_id, sep = "_",
                                    into = c("treat", NA, NA, NA),
                                    remove = FALSE) %>%
  dplyr::mutate(treat = stringr::str_sub(treat, start = -1),
                surface = width * length) %>%
  dplyr::filter(treat %in% c("A", "B")) %>%
  dplyr::group_by(shoot_id) %>%
  dplyr::summarise(surface = sum(surface), shoot_dry_weight = mean(shoot_dry_weight))

# rename cols without empty space
names(bahamas_2018_full) <- stringr::str_replace_all(string = names(bahamas_2018_full), 
                                                     pattern = " ", replace = "_") %>% 
  stringr::str_to_lower()

bahamas_2018_full_sum <- dplyr::filter(bahamas_2018_full, !is.na(reef)) %>% 
  dplyr::mutate(surface = width * length) %>% 
  dplyr::group_by(shoot_id) %>% 
  dplyr::summarise(surface = sum(surface), blade_dry_weight = mean(blade_dry_weight)) %>% 
  dplyr::filter(complete.cases(.))

# rename cols without empty space
names(bahamas_2018_sub) <- stringr::str_replace_all(string = names(bahamas_2018_sub), 
                                                    pattern = " ", replace = "_") %>% 
  stringr::str_to_lower()

#### Create full data set #### 

# combine to one dataset
data_full <- dplyr::bind_rows(
  dplyr::bind_cols(surface = layman_2016$b.area,
                   biomass = layman_2016$b.bio, data = "layman"),
  dplyr::bind_cols(surface = bridget_2021_sum$surface,
                   biomass = bridget_2021_sum$shoot_dry_weight, data = "bridget"),
  dplyr::bind_cols(surface = bahamas_2018_sub$surface_area_only_new_growth,
                   biomass = bahamas_2018_sub$weight_only_new_growth, data = "bahamas_sub"),
  dplyr::bind_cols(surface = bahamas_2018_full_sum$surface, 
                   biomass = bahamas_2018_full_sum$blade_dry_weight, data = "bahamas_full"))

# run regression 
surface_biomass_lm <- stats::lm(log(biomass) ~ log(surface), 
                                data = data_full)

summary(data_full_lm)

# create new data tp predict
new_data <- seq(from = min(data_full$surface), 
                to = max(data_full$surface), 
                by = 1)

predict_data <- predict(object = surface_biomass_lm, 
                        newdata = data.frame(surface = new_data))

# create label for plot
label <- paste0("log(y) = ", round(coefficients(surface_biomass_lm)[[1]], 2), " + ", 
                round(coefficients(surface_biomass_lm)[[2]], 2), " * log(x); R2=", 
                round(summary(surface_biomass_lm)$adj.r.squared, 2))

gg_surface_biomass <- ggplot() + 
  geom_point(data = data_full, aes(x = surface, y = biomass, col = data), 
             shape = 1) + 
  geom_line(aes(x = new_data, y = exp(predict_data))) +
  geom_text(aes(x = 2250, y = 1.2, label = label)) +
  scale_color_viridis_d(name = "Data source") +
  labs(x = "Blade area [mm]", y = "Dry blade biomass [g]", title = "Combined dataset") +
  theme_classic()

#### Save data ####

overwrite <- FALSE

suppoRt::save_rds(object = surface_biomass_lm, filename = "surface_biomass_lm.rds", 
                  path = "02_Data/02_Modified/04_various/", overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_surface_biomass, filename = "gg_surface_biomass.png", 
                     path = "04_Figures/04_various/")

