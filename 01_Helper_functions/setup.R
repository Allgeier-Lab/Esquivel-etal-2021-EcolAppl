##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# load packages #
library(arrR) # devtools::install_github("Allgeier-Lab/arrR", ref = "development")
library(boot)
library(cowplot)
library(magrittr)
library(raster)
library(sensitivity)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt")
library(stringr)
library(tgp)
library(tidyverse)

library(future)
library(future.batchtools)
library(future.apply)
