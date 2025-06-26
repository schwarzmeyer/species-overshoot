source("code/00_packages.R")

# this script calculates the proportion of range exposed and the duration of exposure

############################################################

# the script is also divided in two parts.
# one calculate risk using the whole data, other using the truncate estimates from script 05.

# part 1 ----


# load data
files <- list.files(here("results/species_exposure_times"), full.names = TRUE, rec = FALSE, pattern = ".rds")
ranges <- list.files(here("processed_data/species_data/range_maps_grid_cells/"), full.names = TRUE, rec = FALSE, pattern = ".rds")

models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
groups <- c("Amphibians", "Birds", "Mammals", "Reptiles", "Fishes")

os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds")) 

# function to calculate risk metrics: proportion of range exposed and duration of exposure

# x <- df
# range.sizes <- species_range

get_risk_metrics <- function(x, range.sizes, add.threshold = FALSE){
  
  if(add.threshold) {
    
    df <- x |> 
      filter(deexposure >= threshold_year) |>
      group_by(group, species, world_id) |> 
      summarise(duration_threshold = sum(duration_threshold)) |> 
      left_join(range.sizes, by = c("species", "world_id"), relationship = "many-to-many") |> 
      summarise(range_exposed = sum(range_proportion),
                weighted_duration = weighted.mean(duration_threshold, range_proportion),
                .groups = "drop") |> 
      filter(weighted_duration > 0)
    
    df$threshold <- factor(x$threshold[1])
    
    return(df)
    
  }
  
  df <- x |> 
    group_by(group, species, world_id) |> 
    summarise(duration = sum(duration)) |> 
    left_join(range.sizes, by = c("species", "world_id"), relationship = "many-to-many") |> 
    summarise(range_exposed = sum(range_proportion),
              weighted_duration = weighted.mean(duration, range_proportion),
              .groups = "drop")
  
  
  return(df)
}


# function to caluclate the years each global warming level is exceeded

get_thresholds <- function(x, thresholds = seq(1.5, 2, 0.1)){
  
  warming <- map_dfr(thresholds, ~{
    x |>
      group_by(model) |>
      filter(year < year_peak) |> 
      filter(gwl > .x) |> 
      slice_min(year) |> 
      mutate(threshold = paste0(.x, "w")) |> 
      select(-year_peak) |> 
      ungroup()
    
  }) |> 
    arrange(model) 
  
  cooling <- map_dfr(rev(thresholds), ~{
    x |>
      group_by(model) |>
      filter(year > year_peak) |> 
      filter(gwl <= .x) |> 
      slice_min(year) |> 
      mutate(threshold = paste0(.x, "c"))  |> 
      select(-year_peak) |> 
      ungroup()
    
  }) |> 
    arrange(model) 
  
  peak <- x |> 
    filter(year == year_peak) |> 
    select(-year_peak) |> 
    mutate(threshold = "peak")
  
  gwl_thresholds <- bind_rows(warming, cooling, peak) |> 
    mutate(threshold = factor(threshold, levels = c("1.5w", "1.6w", "1.7w", "1.8w", "1.9w", "2w", "peak",
                                                    "2c", "1.9c", "1.8c", "1.7c", "1.6c", "1.5c")),
           model = factor(model)) |> 
    ungroup()
  
  return(gwl_thresholds)
  
  
}

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


# res_median <- res_final |> 
#   expand(model, species) |> 
#   left_join(res_final, by = c("model","species")) |> 
#   mutate(across(-c(model, species, group, range_size), ~replace(., is.na(.), 0))) |>  
#   group_by(species) |>
#   fill(range_size, .direction = "downup") |>
#   summarise(n_cells_exposed = median(n_cells_exposed),
#             total_duration = median(total_duration),
#             range_size = median(range_size),
#             range_exposed = median(range_exposed),
#             mean_local_duration = median(mean_local_duration)) |>
#   ungroup() |> 
#   left_join(res_final |> 
#               select(group, species), by = "species", multiple = "first") |> 
#   relocate(group) |> 
#   arrange(group, species) |> 
#   filter(range_size != 0)
# 
# saveRDS(res_median,  here("results/risk/risk_raw_median.rds"))


#####################################################################

# calculate risk at different global warming level 
# part 2 ----

global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))

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

gwl_thresholds <- get_thresholds(gwl_df)

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
    
    
    thresholds <- unique(gwl_thresholds$threshold)
    
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
    
    # result_thresholds |>
    #   bind_rows() |>
    #   filter(species == "Acris crepitans",
    #          model == "ACCESS-ESM1-5",
    #          world_id == 21689) |>
    #   view()
    #   
    
      # result_thresholds[[9]] |> 
      #   filter(species == "Abavorana luctuosa") |> 
      #   filter(deexposure >= threshold_year) |>
      #   group_by(species) |> 
      #   summarise(n_cells_exposed = length(unique(world_id)),
      #             total_duration = sum(duration_threshold, na.rm = T))
      
    res <- map(result_thresholds, ~ get_risk_metrics(.x, species_range, add.threshold = TRUE)) |> 
      bind_rows() |> 
      mutate(model = factor(.model),
             group = factor(.group)) |> 
      relocate(group)

      res_list <- append(res_list, list(res))
      
      print(glue("{.model} {.group}"))
      
  }
}

res_final <- bind_rows(res_list) |> 
  mutate(threshold = factor(threshold, levels = c("1.5w", "1.6w", "1.7w", "1.8w", "1.9w", "2w", "peak",
                                                  "2c", "1.9c", "1.8c", "1.7c", "1.6c", "1.5c")))

saveRDS(res_final, here("results/risk/risk_thresholds_models.rds"))

# res_expand <- res_final |> 
#   expand(model, species, threshold) |> 
#   left_join(gwl_thresholds, by = c("model","threshold")) |> 
#   drop_na(year) |> 
#   left_join(res_final, by = c("model","species", "threshold")) |> 
#   group_by(species) |> 
#   fill(group, range_size, .direction = "downup") |> 
#   ungroup() |> 
#   mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))
# 
# res_median <- res_expand |> 
#   group_by(species, threshold) |> 
#   summarise(n_cells_exposed = median(n_cells_exposed),
#             total_duration = median(total_duration),
#             range_size = median(range_size),
#             range_exposed = median(range_exposed),
#             mean_local_duration = median(mean_local_duration),
#             .groups = "drop") 
# 
# # add groups back
# res_median <- res_median |> 
#   left_join(res_final |> 
#               select(group, species) |>
#               distinct(), by = "species") 
# 
# saveRDS(res_median,  here("results/risk/risk_thresholds_median.rds"))

