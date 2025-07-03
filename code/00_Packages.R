if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {remotes::install_github("ropensci/rnaturalearthhires")

install.packages(
  "rnaturalearthhires",
  repos = "https://ropensci.r-universe.dev",
  type = "source"
  )
}

library(pacman)

p_load(here, tidyverse, terra, sf, tictoc, glue, zoo, rnaturalearth, exactextractr, pbmcapply, rphylopic, readxl,
       colorspace, ggdist, patchwork)

# p_load(arrow, tictoc, colorspace, cowplot, exactextractr, furrr, ggdist, ggExtra,
#        ggpointdensity, ggpubr, ggthemes, glue, here, pbmcapply, parallel, patchwork, 
#        pbapply, phyloregion, phytools, readxl, rlang, rnaturalearth, 
#        rphylopic, scales, scico, sf, terra, tidyverse, zoo)

gc()
