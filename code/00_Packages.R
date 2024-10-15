if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(colorspace, cowplot, cowplot, exactextractr, furrr,
               fuzzyjoin, geomtextpath, ggdensity, ggdist, ggExtra, ggnewscale,
               ggpattern, ggpointdensity, ggtext, ggthemes, here, metR, parallel, 
               patchwork, pbapply, png, raster, readxl, rnaturalearth, rphylopic,
               scales, sf, stringr, tidyverse, viridis)



library(sf)
library(parallel)
library(tidyverse)
library(furrr)
library(here)
library(terra)
library(exactextractr)
library(zoo)
