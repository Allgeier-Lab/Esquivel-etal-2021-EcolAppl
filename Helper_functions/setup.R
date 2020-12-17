##--------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth     ##
##    Coastal Ecology and Conservation Lab    ##
##    University of Michigan                  ##
##    mhessel@umich.edu                       ##
##    www.github.com/mhesselbarth             ##
##--------------------------------------------##

# load packages #
library(arrR) # devtools::install_github("Allgeier-Lab/arrR")
library(cowplot)
library(ggalluvial)
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

# set parameters plotting #
base_size <- 12.5

width_full <- 210
width_small <- 175

height_full <- 297
height_small <- 125

units <- "mm"

dpi <- 300
