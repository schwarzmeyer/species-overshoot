packages <- c("colorspace",
              "cowplot",
              "exactextractr",
              "extrafont",
              "furrr",
              "fuzzyjoin",
              "geomtextpath",
              "ggdensity",
              "ggdist",
              "ggExtra",
              "ggnewscale",
              "ggpattern",
              "ggpointdensity",
              "ggtext",
              "ggthemes",
              "metR",
              "parallel",
              "patchwork",
              "pbapply",
              "png",
              "raster",
              "readxl",
              "rnaturalearth",
              "rphylopic",
              "scales",
              "sf",
              "stringr",
              "tidyverse",
              "viridis")


install.packages(setdiff(packages, rownames(installed.packages())))

lapply(packages, library, character.only = TRUE)

rm(list = ls())

path <- "/Users/andreas/Library/CloudStorage/Dropbox/Projects/Overshoot2/"

gc()
