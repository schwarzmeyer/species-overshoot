if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(here, scales, tictoc, scico, glue, zoo, rnaturalearth, exactextractr, pbmcapply, rphylopic, readxl,
       colorspace, ggdist, cowplot, patchwork, tidyterra, terra, sf, tidyverse, rstatix, furrr, ggtext)

# p_load(arrow, tictoc, colorspace, cowplot, exactextractr, furrr, ggdist, ggExtra,
#        ggpointdensity, ggpubr, ggthemes, glue, here, pbmcapply, parallel, patchwork, 
#        pbapply, phyloregion, phytools, readxl, rlang, rnaturalearth, 
#        rphylopic, scales, scico, sf, terra, tidyverse, zoo)

gc()

