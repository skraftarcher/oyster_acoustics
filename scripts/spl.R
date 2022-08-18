# This script starts data processing for the study looking at 
# acoustic signals of biodiversity on oyster reefs 
# @knitr loadpackages
# First load functions and packages ----
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")


source('PAM-R/PAMGuide-edited.R')
source('PAM-R/Meta-edited.R')
source('PAM-R/format_dates.R')
source('PAM-R/Viewer.R')	

# note: numeric dates are base on origin = "1970-01-01"
# get the calibration number from the ocean instruments site
# http://oceaninstruments.azurewebsites.net/App/#/%23
# for high gain setting

# potential calibration values----
# remove # from in front of the one that you are working with
# the calib value goes with the file_prefix value
# immediately preceeding it 
# file_prefix <- "5678"
# calib_value <- -176.5
# 
 # file_prefix <- "5680"
 # calib_value <- -176.4
# 
# file_prefix <- "5674"
# calib_value <- -176.6
# 
file_prefix <- "5679"
calib_value <- -176.8


## averaging across time produces more reliable measures of random signals such as sound levels in a habitat
# set_welch <- 40 # 20 sec time resolution
set_welch <- 120 # 1 min time resolution

# FOR BROADBAND
# set_lcut <- 20 # seems to be the extent of low band in data
# set_hcut <- "none"

## low band
# set_lcut <- 20 # seems to be the extent of low band in data
#set_hcut <- 2000 # high frequency cut off on Hz

## high band
set_lcut <- 1000 # seems to be the extent of low band in data
set_hcut <- 10000 # high frequency cut off on Hz

## boat band
# set_lcut <- 200 # seems to be the extent of low band in data
# set_hcut <- 1000 # high frequency cut off on Hz

# calling PAMMeta to analyze every file in a folder
# failed for folder name with - "wav-denman-2020"; works with just "denman" 
PAMMeta(
  atype = "Broadband",# this option is for doing SPL
  outwrite = 1,# this tells it you want to outwrite the analysis file
  calib = 1, # this tells it you want to use calibration information
  envi = "Wat",# this tells it you recorded in water
  ctype = "EE",# type of calibration, for soundtraps it is end to end
  Si = calib_value,# calibration value from the ocean instruments site
  lcut = set_lcut,# low frequency cut off on Hz
  hcut = set_hcut,# high frequency cut off on Hz
  welch = set_welch,# assuming default of 50% overlap, this is the # seconds x2
  plottype = "none",# tells it whether or not to plot the output
  timestring = paste0(file_prefix, ".%y%m%d%H%M%S.wav")
)


