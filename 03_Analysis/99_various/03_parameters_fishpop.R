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
# Mean and SD of Fishbase parameters

#### Import libraries and data ####

# load packages #
source("01_Helper_functions/setup.R")

haemulon_plumierii <- readr::read_csv(file = "02_Data/01_Raw/LW_Haemulon-plumierii.csv")


#### Calculate values ####

pop_para <- dplyr::summarise(haemulon_plumierii, 
                             pop_a = mean(a), pop_a_sd = sd(a), 
                             pop_b = mean(b), pop_b_sd = sd(b))
