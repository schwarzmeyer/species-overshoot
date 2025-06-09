source("code/00_packages.R")

# this script calculates the proportion of range exposed and the duration of exposure

############################################################

# the script is also divided in two parts.
# one calculate risk using the whole data, other using the truncate estimates from script 05.

# part 1 ----


# load data
files <- list.files(here("results/species_exposure_times"), full.names = T, rec = F, pattern = ".rds")
ranges_raw <- list.files(here("results/raw_results"), full.names = T, rec = F, pattern = ".rds")
models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
groups <- c("Amphibians", "Birds", "Mammals", "Reptiles", "Fishes")

os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds")) 

# function to calculate risk metrics: proportion of range exposed and duration of exposure

get_risk_metrics <- function(x, range.sizes, add.threshold = F){
  
  if(add.threshold) {
    
    df <- x %>% 
      group_by(species) %>% 
      filter(deexposure >= threshold_year) %>%
      summarise(n_cells_exposed = length(unique(world_id)),
                total_duration = sum(duration_threshold, na.rm = T)) %>% 
      ungroup() %>% 
      left_join(range.sizes, by = "species") %>% 
      mutate(range_exposed = n_cells_exposed / range_size,
             mean_local_duration = total_duration / n_cells_exposed)
    
    df$threshold <- factor(x$threshold[1])
    
    return(df)
    
  }
  
  df <- x %>% 
    group_by(species) %>% 
    summarise(n_cells_exposed = length(unique(world_id)),
              total_duration = sum(duration)) %>% 
    ungroup() %>% 
    left_join(range.sizes, by = "species") %>% 
    mutate(range_exposed = n_cells_exposed / range_size,
           mean_local_duration = total_duration / n_cells_exposed)
  
  
  
  return(df)
}


# function to caluclate the years each global warming level is exceeded

get_thresholds <- function(x, thresholds = seq(1.5, 2, 0.1)){
  
  warming <- map_dfr(thresholds, ~{
    x %>%
      group_by(model) %>%
      mutate(peak_year = year[which.max(gwl)]) %>%
      filter(year < peak_year) %>% 
      filter(gwl <= .x) %>% 
      slice_max(year) %>% 
      mutate(threshold = paste0(.x, "w")) %>% 
      select(-peak_year)
  }) %>% 
    arrange(model) 
  
  cooling <- map_dfr(rev(thresholds), ~{
    x %>%
      group_by(model) %>%
      mutate(peak_year = year[which.max(gwl)]) %>%
      filter(year > peak_year) %>% 
      filter(gwl <= .x) %>% 
      slice_min(year) %>% 
      mutate(threshold = paste0(.x, "c"))  %>% 
      select(-peak_year)
  }) %>% 
    arrange(model) 
  
  peak <- x %>% 
    group_by(model) %>% 
    slice_max(gwl) %>% 
    mutate(threshold = "peak")
  
  gwl_thresholds <- bind_rows(warming, cooling, peak) %>% 
    mutate(threshold = factor(threshold, levels = c("1.5w", "1.6w", "1.7w", "1.8w", "1.9w", "2w", "peak",
                                                    "2c", "1.9c", "1.8c", "1.7c", "1.6c", "1.5c")),
           model = factor(model)) %>% 
    ungroup()
  
  return(gwl_thresholds)
  
  
}

res_list <- list()

for(.group in groups){
  for(.model in models){
    
    begin_os <- os %>% 
      filter(model == .model) %>% 
      pull(begin_os)
    
    end_os <- os %>% 
      filter(model == .model) %>% 
      pull(end_os)
    
    df <- files %>% 
      grep(.group, ., value = T) %>%
      grep(.model, ., value = T) %>% 
      readRDS() %>% 
      filter(exposure < end_os,
             deexposure >= begin_os)

    species_range <- ranges_raw %>% 
      grep(.group, ., value = T) %>%
      grep(.model, ., value = T) %>% 
      readRDS()
    
    range_sizes <- map_dfr(species_range, ~ tibble(species = .x$species[1], range_size = nrow(.x)))
    
    res <- get_risk_metrics(df, range_sizes) %>% 
      mutate(model = factor(.model),
             group = factor(.group)) %>% 
      relocate(group)
    
    res_list <- append(res_list, list(res))
    
    print(glue("{.model} {.group}"))
    
  }
}

res_final <- bind_rows(res_list)

saveRDS(res_final, here("results/risk/risk_raw_models.rds"))


res_median <- res_final %>% 
  expand(model, species) %>% 
  left_join(res_final, by = c("model","species")) %>% 
  mutate(across(-c(model, species, group, range_size), ~replace(., is.na(.), 0))) %>%  
  group_by(species) %>%
  fill(range_size, .direction = "downup") %>%
  summarise(n_cells_exposed = median(n_cells_exposed),
            total_duration = median(total_duration),
            range_size = median(range_size),
            range_exposed = median(range_exposed),
            mean_local_duration = median(mean_local_duration)) %>%
  ungroup() %>% 
  left_join(res_final %>% 
              select(group, species), by = "species", multiple = "first") %>% 
  relocate(group) %>% 
  arrange(group, species) %>% 
  filter(range_size != 0)

saveRDS(res_median,  here("results/risk/risk_raw_median.rds"))


#####################################################################

# calculate risk at different global warming level 
# part 2 ----

global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))

gwl_df <- global_temp_avg %>% 
  group_by(model) %>%
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) %>% 
  mutate(gwl = temperature_rolling - pre_industrial_avg) %>% 
  select(model, year, gwl) %>% 
  filter(year >= 2015,
         year <= 2220) %>% 
  ungroup()

gwl_thresholds <- get_thresholds(gwl_df)

res_list <- list()

for(.group in groups){
  for(.model in models){
    
    begin_os <- os %>% 
      filter(model == .model) %>% 
      pull(begin_os)
    
    end_os <- os %>% 
      filter(model == .model) %>% 
      pull(end_os)
    
    df <- files %>% 
      grep(.group, ., value = T) %>%
      grep(.model, ., value = T) %>% 
      readRDS() %>% 
      filter(exposure < end_os,
             deexposure >= begin_os)
    
    species_range <- ranges_raw %>% 
      grep(.group, ., value = T) %>%
      grep(.model, ., value = T) %>% 
      readRDS()
    
    range_sizes <- map_dfr(species_range, ~ tibble(species = .x$species[1], range_size = nrow(.x)))
    
    thresholds <- unique(gwl_thresholds$threshold)
    
    result_thresholds <- map(thresholds, function(.threshold){
      
      gwl_df <- gwl_thresholds %>% 
        filter(model == .model,
               threshold == .threshold)
      
      max_year <- gwl_df$year
      
      if(length(max_year) == 0) return(NULL)
      
      result <- df %>% 
        filter(exposure <= max_year) %>% 
        mutate(deexposure_threshold = ifelse(deexposure <= max_year, deexposure, max_year),
               .before = 5) %>% 
        mutate(duration_threshold = deexposure_threshold - exposure,
               .before = 6) %>% 
        mutate(threshold = .threshold,
               threshold_year = max_year) 
      
      return(result)
    }) %>% 
      discard(is.null) 
    
    # result_thresholds %>%
    #   bind_rows() %>%
    #   filter(species == "Acris crepitans",
    #          model == "ACCESS-ESM1-5",
    #          world_id == 21689) %>%
    #   view()
    #   
    
      # result_thresholds[[9]] %>% 
      #   filter(species == "Abavorana luctuosa") %>% 
      #   filter(deexposure >= threshold_year) %>%
      #   group_by(species) %>% 
      #   summarise(n_cells_exposed = length(unique(world_id)),
      #             total_duration = sum(duration_threshold, na.rm = T))
      
    res <- map(result_thresholds, ~ get_risk_metrics(.x, range_sizes, add.threshold = T)) %>% 
      bind_rows() %>% 
      mutate(model = factor(.model),
             group = factor(.group)) %>% 
      relocate(group)

      res_list <- append(res_list, list(res))
      
      print(glue("{.model} {.group}"))
      
  }
}

res_final <- bind_rows(res_list) %>% 
  mutate(threshold = factor(threshold, levels = c("1.5w", "1.6w", "1.7w", "1.8w", "1.9w", "2w", "peak",
                                                  "2c", "1.9c", "1.8c", "1.7c", "1.6c", "1.5c")))

saveRDS(res_final, here("results/risk/risk_thresholds_models.rds"))

res_expand <- res_final %>% 
  expand(model, species, threshold) %>% 
  left_join(gwl_thresholds, by = c("model","threshold")) %>% 
  drop_na(year) %>% 
  left_join(res_final, by = c("model","species", "threshold")) %>% 
  group_by(species) %>% 
  fill(group, range_size, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

res_median <- res_expand %>% 
  group_by(species, threshold) %>% 
  summarise(n_cells_exposed = median(n_cells_exposed),
            total_duration = median(total_duration),
            range_size = median(range_size),
            range_exposed = median(range_exposed),
            mean_local_duration = median(mean_local_duration),
            .groups = "drop") 

# add groups back
res_median <- res_median %>% 
  left_join(res_final %>% 
              select(group, species) %>%
              distinct(), by = "species") 

saveRDS(res_median,  here("results/risk/risk_thresholds_median.rds"))

