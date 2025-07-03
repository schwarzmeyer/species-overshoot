source("code/00_Packages.R")

print("Running: 04_Raw_Results.R")

# script to generate the raw results used to construct the horizons profiles

############################################################

# the script generate matrices in which rows represent the species occurring in an assemblage (i.e. grid cell)
# and columns are the years in the time series. the cell is assigned with 1 if the climate is suitable
# for the species in a given year. the cell is assigned with 0 if the climate is unsuitable.
 
spp_ranges <- list.files(here("processed_data/species_data/range_maps_grid_cells"), full.names = T)
temp_matrices <- list.files(here("processed_data/climate_data/temperature_matrices"), full.names = T)
niche_limits <- readRDS(here("processed_data/species_data/niche_limits/niche_limits.rds"))
models <- c("ACCESS-ESM1-5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")
groups <- c("Amphibians","Birds","Fishes","Mammals","Reptiles")

n_cores <- 7


# .species <- species[7147]
# climate_data <- temp_tbl
# niche_data <- niche

get_raw_results <- function(.species, climate_data, niche_data){
  
  spp_data <- species_range[[.species]]
  
  spp_matrix <- climate_data |> 
    filter(world_id %in% spp_data) |> 
    na.omit()
  
  spp_niche <- niche_data$niche_max[niche_data$species == .species]
  
  result <- spp_matrix |> 
    mutate(across(-world_id, ~ ifelse(. > spp_niche, 1L, 0L))) |> 
    mutate(species = factor(.species)) |>
    relocate(species) 

  
  return(result)
  
}


for(.model in models){
  for(.group in groups){
    
    tic(glue("-- Running time: {.model} {.group}"))
    
    .var <- if(.group != "Fishes") "tas" else "tos"
    
    temp_tbl <- temp_matrices |> 
      grepv(pattern = .var) |> 
      grepv(pattern = .model) |> 
      readRDS()
  
    
    species_range <- spp_ranges |> 
      grepv(pattern = .group) |>
      readRDS()
    
    species <- unique(species_range$species) |> 
      as.character()
    
    species_world_ids <- unique(species_range$world_id)
    
    species_range <- species_range |>
      group_by(species) |>
      summarise(world_ids = list(world_id), .groups = 'drop') |>
      deframe()
    
    temp_tbl <- temp_tbl |> 
      filter(year > 2014,
             year < 2221,
             world_id %in% species_world_ids) |> 
      pivot_wider(names_from = year,
                  values_from = temp) 
    
    niche <- niche_limits |> 
      filter(group == .group,
             model == .model) 
    
    result <- mclapply(species,
                       FUN = get_raw_results,
                       climate_data = temp_tbl,
                       niche_data = niche,
                       mc.cores = n_cores)
    
    # result <- result |>
    #   bind_rows() |>
    #   mutate(model = factor(.model),
    #          group = factor(.group))

    # write_parquet(result, here(glue("results/raw_results/{.model}_{.group}.parquet")))
    
    saveRDS(result,
            here(glue("results/raw_results/{.model}_{.group}.rds")))
    
    rm(result)
    
    gc()
    
    toc()
    
    }
  }
  
print("DONE: 04_Raw_Results.R")
