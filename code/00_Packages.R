if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# pacman::p_load(colorspace, cowplot, cowplot, exactextractr, furrr,
#                fuzzyjoin, geomtextpath, ggdensity, ggdist, ggExtra, ggnewscale,
#                ggpattern, ggpointdensity, ggtext, ggthemes, here, metR, parallel, 
#                patchwork, pbapply, png, raster, readxl, rnaturalearth, rphylopic,
#                scales, sf, stringr, tidyverse, viridis)

pacman::p_load(colorspace, exactextractr, furrr, ggdist, 
               ggthemes, here, parallel, patchwork, pbapply, 
               phyloregion, phytools, readxl, rlang, rnaturalearth, rphylopic, 
               sf, terra, tidyverse, zoo)

