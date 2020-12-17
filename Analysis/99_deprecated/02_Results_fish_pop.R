##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

source("Helper_functions/setup.R")

#### load data and parameters ####

result_randmove <- readr::read_rds("Data/Modified/result_randmove.rds")

result_attrmove <- readr::read_rds("Data/Modified/result_attrmove.rds")

#### Fish population over time #### 

gg_rand_move_fsh <- plot(result_randmove, summarize = TRUE, what = "fish_population")

gg_attr_move_fsh <- plot(result_attrmove, summarize = TRUE, what = "fish_population")

#### Save ggplots ####

overwrite = TRUE

suppoRt::save_ggplot(plot = gg_rand_move_fsh, 
                     filename = "Figures/gg_rand_move_fsh.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)

suppoRt::save_ggplot(plot = gg_attr_move_fsh, 
                     filename = "Figures/gg_attr_move_fsh.png", 
                     height = width_full, width = height_full, units = units,
                     dpi = dpi, overwrite = overwrite)
