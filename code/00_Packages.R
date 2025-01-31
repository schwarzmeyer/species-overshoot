if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(colorspace, cowplot, exactextractr, furrr, ggdist, ggExtra,
               ggpointdensity, ggpubr, ggthemes, glue, here, parallel, patchwork, pbapply, 
               phyloregion, phytools, readxl, rlang, rnaturalearth, rphylopic, 
               scales, sf, terra, tidyverse, zoo)

