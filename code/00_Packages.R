if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(colorspace, exactextractr, furrr, ggdist, 
               ggthemes, here, parallel, patchwork, pbapply, 
               phyloregion, phytools, readxl, rlang, rnaturalearth, rphylopic, 
               sf, terra, tidyverse, zoo)

