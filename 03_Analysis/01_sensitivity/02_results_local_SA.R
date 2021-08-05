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
# Results of local, one-at-a-time sensitivity analysis of all parameters and 
# biomass and fishpop dimensions as output

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

source("01_Helper_functions/calc_result_sa.R")

# load parameter list
parameters_def <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting-values.csv", sep = ";")

# create vector with names
repetitions <- 25 # make sure identical to 01_run_local_SA.R

parameters_names <- paste(rep(x = names(parameters_def), each = repetitions),
                          1:repetitions, sep = "_")

# reverse parameter levels because of coord_flip()
parameter_levels <- rev(names(parameters_def))

# load default run
model_runs_def <- readr::read_rds(file = "02_Data/02_Modified/01_sensitivity/model_runs_def.rds")

# load 5% change
model_runs_inc_5 <- readr::read_rds(file = "02_Data/02_Modified/01_sensitivity/model_runs_inc_5.rds")

model_runs_dec_5 <- readr::read_rds(file = "02_Data/02_Modified/01_sensitivity/model_runs_dec_5.rds")

# load 10% change
model_runs_inc_10 <- readr::read_rds(file = "02_Data/02_Modified/01_sensitivity/model_runs_inc_10.rds")

model_runs_dec_10 <- readr::read_rds(file = "02_Data/02_Modified/01_sensitivity/model_runs_dec_10.rds")

#### Biomass ####

# Calculate mean difference of biomass

# parameters changed by 5%
biomass_inc_5 <- calc_biomass_sa(default = model_runs_def, changed = model_runs_inc_5) %>% 
  dplyr::mutate(direction = "Increased +5%")

biomass_dec_5 <- calc_biomass_sa(default = model_runs_def, changed = model_runs_dec_5) %>% 
  dplyr::mutate(direction = "Decreased -5%")

# parameters changed by 10%
biomass_inc_10 <- calc_biomass_sa(default = model_runs_def, changed = model_runs_inc_10) %>% 
  dplyr::mutate(direction = "Increased +10%")

biomass_dec_10 <- calc_biomass_sa(default = model_runs_def, changed = model_runs_dec_10) %>% 
  dplyr::mutate(direction = "Decreased -10%")

# combine to one data.frame 
sa_biomass <- dplyr::bind_rows(biomass_inc_5,
                               biomass_dec_5,
                               biomass_inc_10,
                               biomass_dec_10) %>% 
  dplyr::mutate(name = factor(name, levels = c("ag_biomass", "ag_production", 
                                               "bg_biomass", "bg_production")), 
                parameter = factor(parameter, levels = parameter_levels),
                direction = factor(direction, levels = c("Decreased -10%", "Decreased -5%",
                                                         "Increased +5%", "Increased +10%")))
# all parameters that changed more than relative change
parameter_changed <- dplyr::filter(sa_biomass, diff_mean > 5.0 | diff_mean < -5.0) %>% 
  dplyr::select(name, parameter, direction, diff_mean) %>% 
  dplyr::mutate(name = as.character(name), 
                parameter = as.character(parameter),
                diff_mean = round(diff_mean, digits = 2)) %>% 
  tidyr::pivot_wider(names_from = direction, values_from = diff_mean) %>% 
  dplyr::arrange(name, parameter) %>% 
  dplyr::select(name, parameter, `Increased +5%`, `Increased +10%`,
                `Decreased -5%`, `Decreased -10%`)

# print only parameter names
unique(parameter_changed$parameter) %>% 
  as.character() %>% 
  stringr::str_sort()

#### Create and save ggplot ####

ggplot_sa_local <- ggplot(data = sa_biomass) + 
  geom_bar(aes(x = parameter, y = diff_mean, fill = direction, group = direction), 
           stat = "identity", position = "dodge") + 
  geom_hline(yintercept = -10, linetype = 2, col = "#B51820") +
  geom_hline(yintercept = -5, linetype = 2, col = "#E58601") +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = 5, linetype = 2, col = "#85D4E3") +
  geom_hline(yintercept = 10, linetype = 2, col = "#296C9A") +
  coord_flip() +
  facet_wrap(. ~ name, scales = "free_x") +
  scale_x_discrete(name = "Parameter") +
  scale_y_continuous(name = "Difference model output [%]") +
  scale_fill_manual(name = "Parameter change",
                    values = c("#B51820", "#E58601" ,
                               "#85D4E3", "#296C9A")) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  theme_classic(base_size = base_size) + 
  theme(legend.position = "bottom")

# save plot
suppoRt::save_ggplot(plot = ggplot_sa_local, 
                     filename = "ggplot-sa-local.pdf", 
                     path = "04_Figures/01_sensitivity_analysis/",     
                     dpi = dpi, width = width, height = height, units = units, 
                     overwrite = FALSE)
