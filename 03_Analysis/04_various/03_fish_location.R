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
# Create figure with location of indiividual for random and attracted movement

# load packages #
source("01_Helper_functions/setup.R")

parameters <- arrR::read_parameters(file = "02_Data/01_Raw/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters(file = "02_Data/01_Raw/starting_values.csv", sep = ";")

starting_values$pop_n <- 5

#### Set arguments to run model ####

# set minutes per iteration
min_per_i <- 120

# run model for n years
years <- 1

max_i <- (60 * 24 * 365 * years) / min_per_i

# save each m days
days <- 5

save_each <- (24 / (min_per_i / 60)) * days

# check if combination of max_i and save_each are possible
max_i %% save_each

# extent and grain of seafloor
extent <- c(50, 50)

grain <- c(1, 1)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# use starting log distribution of size
use_log <- TRUE

# print progress
verbose <- TRUE

#### Setup ####

# create seafloor
input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                       reefs = reef_matrix,
                                       starting_values = starting_values,
                                       verbose = verbose)

# create fish individual
input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor, 
                                     starting_values = starting_values, 
                                     parameters = parameters, use_log = use_log, 
                                     verbose = verbose)

# random movement
result_rand <- arrR::run_simulation(seafloor = input_seafloor,
                                    fishpop = input_fishpop,
                                    parameters = parameters,
                                    reef_attraction = FALSE,
                                    max_i = max_i, min_per_i = min_per_i,
                                    save_each = save_each,
                                    verbose = verbose) 

# attracted movement
result_attr <- arrR::run_simulation(seafloor = input_seafloor,
                                    fishpop = input_fishpop,
                                    parameters = parameters,
                                    reef_attraction = TRUE,
                                    max_i = max_i, min_per_i = min_per_i,
                                    save_each = save_each,
                                    verbose = verbose) 

#### Create dataframe with locations #### 

seafloor_df <- raster::as.data.frame(input_seafloor$reef, xy = TRUE) %>% 
  dplyr::mutate(reef = factor(reef))

location_combined <- dplyr::bind_rows(rand = result_rand$fishpop, attr = result_attr$fishpop, 
                                      .id = "id_move") %>% 
  dplyr::mutate(id_move = factor(id_move, levels = c("rand", "attr"), 
                                 labels = c("Random movement", "Attracted movement")))

#### Create plot #### 

# create plot
gg_fish_location <- ggplot(data = location_combined) + 
  geom_raster(data = seafloor_df, aes(x = x, y = y, fill = reef)) + 
  geom_point(aes(x = x, y = y), alpha = 0.5) + 
  geom_polygon(data = data.frame(x = c(-25, 25, 25, -25), y = c(-25, -25, 25, 25)), 
               aes(x = x, y = y), col = "black", fill = NA) +
  scale_fill_manual(values = c("#CCFBFF", "#9B964A")) +
  facet_wrap(~id_move) +
  guides(fill = FALSE) +
  coord_equal() + 
  theme_void()

# save ggplot
suppoRt::save_ggplot(plot = gg_fish_location, filename = "gg_fish_location.png", 
                     path = "04_Figures/04_various/", overwrite = FALSE)
