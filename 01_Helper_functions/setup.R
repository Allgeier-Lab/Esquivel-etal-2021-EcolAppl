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
library(progress)
library(raster)
library(sensitivity)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt")
library(stringr)
library(tgp)
library(tidyverse)

library(future)
library(future.batchtools)
library(future.apply)

# Set some plotting defaults

# set base_size
base_size <- 10

# DINA4
units <- "mm"

width <- 210

height <- 297

# set pixels
dpi <- 900
