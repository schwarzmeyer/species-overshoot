if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
if(!requireNamespace("rnaturalearthhires", quietly = TRUE)) install.packages("rnaturalearthhires")

library(pacman)

p_load(arrow, tictoc, colorspace, cowplot, exactextractr, furrr, ggdist, ggExtra,
       ggpointdensity, ggpubr, ggthemes, glue, here, pbmcapply, parallel, patchwork, 
       pbapply, phyloregion, phytools, readxl, rlang, rnaturalearth, 
       rphylopic, scales, scico, sf, terra, tidyverse, zoo)

gc()