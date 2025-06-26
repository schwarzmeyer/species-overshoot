source("code/00_Packages.R")

print("Running: 03_Get_Niche_Limits.R")

# script to compute upper and lowet thermal niche limits for each species

#############################################################

# load files
spp_files <- list.files(here("processed_data/species_data/range_maps_grid_cells"), full.names = T)
temp_matrices <- list.files(here("processed_data/climate_data/temperature_matrices"), full.names = T)
models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
groups <- c("Amphibians", "Birds", "Mammals", "Reptiles", "Fishes")
n_out <- 3
n_cores <- 7

# function to compute the limits
get_niche_limits <- function(species_ranges, temperature_matrix, n_out) {
  t_matrix <- temperature_matrix[temperature_matrix$world_id %in% species_ranges, ]

  if (nrow(t_matrix) == 0) return(tibble(niche_max = NA))
  

  if (nrow(t_matrix) == 1) {
    result <- t_matrix |>
      rename(niche_max = max_temp) |>
      select(niche_max)
  } else {
    result <- t_matrix |>
      mutate(
        mean_val_max = mean(max_temp, na.rm = TRUE),
        sd_val_max = sd(max_temp, na.rm = TRUE),
        is_outlier = max_temp > (mean_val_max + n_out * sd_val_max)
      ) |>
      filter(!is_outlier) |>
      summarise(
        niche_max = max(max_temp, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(result)
}




result_list <- list()

for (.model in models) {
  for (.group in groups) {
    tic(glue("-- Running time: {.model} {.group}"))

    if (.group != "Fishes") {
      temp_files <- grepv(pattern = "tas", temp_matrices)
    } else {
      temp_files <-grepv(pattern = "tos", temp_matrices)
    }

    temp_tbl <- temp_files |>
      grepv(pattern = .model) |>
      readRDS()

    temp_tbl_filtered <- temp_tbl |>
      filter(year <= 2014) |>
      group_by(world_id) |>
      mutate(
        mean_val = mean(temp, na.rm = TRUE),
        sd_val = sd(temp, na.rm = TRUE),
        is_outlier = temp > (mean_val + n_out * sd_val) | temp < (mean_val - n_out * sd_val)
      ) |>
      filter(!is_outlier) |>
      summarise(
        max_temp = max(temp, na.rm = TRUE),
        .groups = "drop"
      )

    spp_data <- spp_files |>
      grepv(pattern = .group) |>
      readRDS()

    species_list <- spp_data |>
      group_by(species) |>
      summarise(world_ids = list(world_id), .groups = "drop") |>
      deframe()

    niche_limits <- pbmclapply(species_list,
      function(.x) get_niche_limits(.x, temp_tbl_filtered, n_out = n_out),
      mc.cores = n_cores
    ) |>
      bind_rows(.id = "species") |>
      mutate(
        model = .model,
        group = .group
      )

    result_list <- append(result_list, list(niche_limits))
    toc()
  }
}

niche_limits <- bind_rows(result_list)

saveRDS(niche_limits, here("processed_data/species_data/niche_limits/niche_limits.rds"))

print("DONE: 03_Get_Niche_Limits.R")
