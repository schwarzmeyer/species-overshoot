if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(here, scales, tictoc, scico, glue, zoo, rnaturalearth, exactextractr, pbmcapply, rphylopic, readxl,
       colorspace, ggdist, cowplot, patchwork, tidyterra, terra, sf, tidyverse, rstatix, furrr, ggtext)

gc()

