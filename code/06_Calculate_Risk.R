source("code/00_packages.R")

# this script calculates the proportion of range exposed and the duration of exposure
# the script is also divided in two parts:
# one calculate risk using the whole data time series.
# the other spliting the data at different global warming levels.



# load data
files <- list.files(here("results/species_exposure_times"), full.names = TRUE, rec = FALSE, pattern = ".rds")
ranges <- list.files(here("processed_data/species_data/range_maps_grid_cells/"), full.names = TRUE, rec = FALSE, pattern = ".rds")

models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
groups <- c("Amphibians", "Birds", "Mammals", "Reptiles", "Fishes")

os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds")) 
global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))

# function to compute risk metrics: proportion of range exposed and duration of exposure
get_risk_metrics <- function(x, range.sizes, add.threshold = FALSE){
  
  if(add.threshold) {
    
    df <- x |> 
      filter(deexposure >= threshold_year) |> # Filter only the ongoing exposure events
      group_by(group, species, world_id) |> 
      summarise(duration_threshold = sum(duration_threshold),
                .groups = "drop_last") |>
      left_join(range.sizes, by = c("species", "world_id"), relationship = "many-to-many") |> 
      summarise(range_exposed = sum(range_proportion),
                weighted_duration = weighted.mean(duration_threshold, range_proportion),
                .groups = "drop") |> 
      filter(weighted_duration > 0) # Filter out grid cells that haven't been exposed yet
    
    # Add non-exposed species
    non_exposed_species <- setdiff(unique(range.sizes$species), df$species)
    non_exposed_species <- tibble(group = unique(x$group),
                                  species = non_exposed_species,
                                  range_exposed = 0,
                                  weighted_duration = 0)
    
    df <- bind_rows(df, non_exposed_species)
    
    df$threshold <- factor(x$threshold[1])
    
    return(df)
    
  }
  
  df <- x |> 
    group_by(group, species, world_id) |> 
    summarise(duration = sum(duration),
              .groups = "drop_last") |> 
    left_join(range.sizes, by = c("species", "world_id"), relationship = "many-to-many") |> 
    summarise(range_exposed = sum(range_proportion),
              weighted_duration = weighted.mean(duration, range_proportion),
              .groups = "drop")
  
  # Add non-exposed species
  non_exposed_species <- setdiff(unique(range.sizes$species), df$species)
  non_exposed_species <- tibble(group = unique(df$group),
                                species = non_exposed_species,
                                range_exposed = 0,
                                weighted_duration = 0)
  
  df <- bind_rows(df, non_exposed_species)
  
  return(df)
}

############################################################

# part 1 ----

res_list <- list()

for(.group in groups){
  for(.model in models){
    
    begin_os <- os |> 
      filter(model == .model) |> 
      pull(begin_os)
    
    end_os <- os |> 
      filter(model == .model) |> 
      pull(end_os)
    
    df <- files |> 
      grepv(pattern = .group) |>
      grepv(pattern = .model) |> 
      readRDS() |> 
      filter(exposure < end_os)
    
    species_range <- ranges |> 
      grepv(pattern = .group) |>
      readRDS() |> 
      group_by(species, world_id) |> 
      summarise(range_proportion = sum(range_proportion),
                .groups = "drop") # This step is to merge cells that are crossed by the polygon in more than one region
    
    res <- get_risk_metrics(df, species_range) |> 
      mutate(model = factor(.model),
             group = factor(.group)) |> 
      relocate(group)
    
    res_list <- append(res_list, list(res))
    
    print(glue("{.model} {.group}"))
    
  }
}

res_final <- bind_rows(res_list)

saveRDS(res_final, here("results/risk/risk_raw_models.rds"))

#####################################################################

# calculate risk at different global warming level 
# part 2 ----


gwl_df <- global_temp_avg |> 
  group_by(model) |>
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) |> 
  mutate(gwl = temperature_rolling - pre_industrial_avg,
         gwl = round(gwl, 2)) |> 
  select(model, year, gwl) |> 
  filter(year >= 2015,
         year <= 2220) |> 
  ungroup() |> 
  left_join(os |> 
              select(model, year_peak), by = "model")

gwl_thresholds <- gwl_df |> 
  mutate(phase = case_when(year < year_peak ~ "warming",
                           year > year_peak ~ "cooling",
                           year == year_peak ~ "peak")) |>
  mutate(gwl_step = ifelse(phase == "warming", floor(gwl*10)/10, ceiling(gwl*10)/10)) |> 
  mutate(threshold = case_when(
    phase == "warming" ~ paste0(gwl_step, "w"),
    phase == "cooling" ~ paste0(gwl_step, "c"),
    phase == "peak" ~ "peak",
    TRUE ~ NA_character_
  )) |>
  group_by(model, threshold) |>
  slice_min(year) |>
  mutate(threshold = factor(threshold, levels = c(paste0(seq(1.5, 3, 0.1), "w"), 
                                                  "peak", 
                                                  paste0(seq(3, 1.5, -0.1), "c"))),
         model = factor(model)) |> 
  na.omit()

saveRDS(gwl_thresholds, here("processed_data/climate_data/global_averages/gwl_thresholds.rds"))

res_list <- list()

for(.group in groups){
  for(.model in models){
    
    begin_os <- os |> 
      filter(model == .model) |> 
      pull(begin_os)
    
    end_os <- os |> 
      filter(model == .model) |> 
      pull(end_os)
    
    df <- files |> 
      grepv(pattern = .group) |>
      grepv(pattern = .model) |> 
      readRDS() |> 
      filter(exposure < end_os)
    
    species_range <- ranges |> 
      grepv(pattern = .group) |>
      readRDS() |> 
      group_by(species, world_id) |> 
      summarise(range_proportion = sum(range_proportion),
                .groups = "drop") # This step is to merge cells that are crossed by the polygon in more than one region
    
    
    thresholds <- gwl_thresholds |> 
      filter(model == .model) |> 
      pull(threshold) |> 
      unique()
    
    result_thresholds <- map(thresholds, function(.threshold){
      
      gwl_df <- gwl_thresholds |> 
        filter(model == .model,
               threshold == .threshold)
      
      max_year <- gwl_df$year
      
      if(length(max_year) == 0) return(NULL)
      
      result <- df |> 
        filter(exposure <= max_year) |> 
        mutate(deexposure_threshold = ifelse(deexposure <= max_year, deexposure, max_year),
               .before = 5) |> 
        mutate(duration_threshold = deexposure_threshold - exposure,
               .before = 6) |> 
        mutate(threshold = .threshold,
               threshold_year = max_year) 
      
      return(result)
    }) |> 
      discard(is.null) 
    
    plan("multisession", workers = 6)
    res <- future_map(result_thresholds, ~ get_risk_metrics(.x, species_range, add.threshold = TRUE)) |>
      bind_rows() |>
      mutate(model = factor(.model),
             group = factor(.group)) |>
      relocate(group)

    
    res_list <- append(res_list, list(res))
    
    print(glue("{.model} {.group}"))
    
  }
}

res_final <- bind_rows(res_list) |>
  mutate(threshold = factor(threshold, levels = c(paste0(seq(1.5, 3, 0.1), "w"), 
                                                  "peak", 
                                                  paste0(seq(3, 1.5, -0.1), "c"))),
         model = factor(model))

saveRDS(res_final, here("results/risk/risk_thresholds_models.rds"))

