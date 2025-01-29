# this script calculates the proportion of range exposed and the duration of exposure

############################################################

# the script is also divided in two parts.
# one calculate risk using the whole data, other using the truncate estimates from script 05.

# part 1 ----


# load data
files <- list.files(here("results/species_exposure_times"), full.names = T)
ranges_raw <- list.files(here("results/raw_results"), full.names = T, pattern = "raw", rec = F)
models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
groups <- c("Amphibians", "Birds", "Mammals", "Reptiles", "Fishes")

os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds"))

get_risk_metrics <- function(x, range.sizes){
  
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


res_list <- list()

for(.group in groups){
  for(.model in models){
    
    substring <- c(.group, .model)
    readme <- files[sapply(files, function(x) all(str_detect(x, substring)))]
    
    df <- readRDS(readme)
    
    begin_os <- os %>% 
      filter(model == .model) %>% 
      pull(begin_os)
    
    end_os <- os %>% 
      filter(model == .model) %>% 
      pull(end_os)
    
    df <- df %>% 
      filter(exposure < end_os,
             deexposure >= begin_os)
    

    readme <- ranges_raw[sapply(ranges_raw, function(x) all(str_detect(x, substring)))]
    species_range <- readRDS(readme)
    
    range_sizes <- map_dfr(species_range, ~ tibble(range_size = nrow(.x)), .id = "species")
    
    res <- get_risk_metrics(df, range_sizes)
    
    res$model <- .model
    res$group <- .group
    
    res <- res %>% 
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
  mutate_all( ~replace(., is.na(.), 0)) %>%  
  group_by(species) %>%
  summarise(n_cells_exposed = median(n_cells_exposed),
            total_duration = median(total_duration),
            range_size = median(range_size),
            range_exposed = median(range_exposed),
            mean_local_duration = median(mean_local_duration)) %>% 
  ungroup() %>% 
  left_join(res_final[,1:2], by = "species", multiple = "first") %>% 
  relocate(group) %>% 
  arrange(group, species) %>% 
  filter(range_size != 0)

saveRDS(res_median,  here("results/risk/risk_raw_median.rds"))


#####################################################################

# part 2 ----

# calculate risk at different global warming level 

files <- list.files(here("results/species_exposure_times_per_gwl"), full.names = T)
ranges_raw <- list.files(here("results/raw_results"), full.names = T, pattern = "raw", rec = F)

models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")

groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")
phases <- c("warming_phase", "cooling_phase")
gwl_levels <- seq(1.5, 2, by = 0.1)
thresholds <- c(gwl_levels, "peak")

global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))


# Compute thresholds for each model
gwl <- global_temp_avg %>% 
  group_by(model) %>%
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) %>% 
  mutate(gwl = temperature_rolling - pre_industrial_avg) %>% 
  select(model, year, gwl) %>% 
  filter(year >= 2015,
         year <= 2220)

gwl_times <- gwl %>% 
  group_by(model) %>%
  reframe(
    # find the peak year for each model
    peak_year = year[which.max(gwl)],
    # split data into warming and cooling phases
    map_dfr(gwl_levels, ~ {
      warming_phase <- year[gwl >= .x & year <= peak_year][1]
      cooling_phase <- year[gwl <= .x & year > peak_year][1]
      tibble(threshold = factor(.x), warming_phase = warming_phase, cooling_phase = cooling_phase)
    })) %>% 
  select(-peak_year) %>%
  pivot_longer(cols = c(warming_phase, cooling_phase),
               names_to = "phase",
               values_to = "year") %>% 
  bind_rows(gwl %>% 
              slice_max(gwl) %>% 
              mutate(threshold = "peak",
                     phase = "warming_phase") %>% 
              select(model, threshold, phase, year)
  ) %>% 
  drop_na()



get_risk_metrics <- function(x, range.sizes){
  
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


res_list <- list()


for(.group in groups){
  for(.model in models){
    
    readme <- ranges_raw[sapply(ranges_raw, function(x) all(str_detect(x, c(.group, .model))))]
    species_range <- readRDS(readme)
    range_sizes <- map_dfr(species_range, ~ tibble(range_size = nrow(.x)), .id = "species")
    
    for(.threshold in thresholds){
      for(.phase in phases){
        
        substring <- c(.group, .model, glue("{.phase}_{.threshold}"))
        readme <- files[sapply(files, function(x) all(str_detect(x, substring)))]
        
        if(length(readme) == 0) next
        
        df <- readRDS(readme)
        
        any(names(df) %in% "deexposure")        
        
        gwl_df <- gwl_times %>% 
          filter(model == .model,
                 threshold == .threshold,
                 phase == .phase) 
        
        max_year <- gwl_df$year
        
        df <- df %>% 
          filter(deexposure > max_year)
        
        print(substring)
        print(nrow(df))
        
        res <- get_risk_metrics(df, range_sizes)
        
        res$model <- .model
        res$group <- .group
        res$phase <- .phase
        res$threshold <- .threshold
        
        res <- res %>% 
          relocate(group)
        
        res_list <- append(res_list, list(res))
      }
    }
  }
}


res_final <- bind_rows(res_list)

saveRDS(res_final, here("results/risk/risk_per_gwl_models.rds"))


# calculate median

res_expand <- res_final %>% 
  expand(model, species, threshold, phase) %>% 
  left_join(gwl_times, by = c("model","threshold", "phase")) %>% 
  drop_na(year) %>% 
  # filter(species == "Abavorana luctuosa") %>%
  # slice(1:3000) %>% 
  left_join(res_final, by = c("model","species", "threshold", "phase")) %>% 
  group_by(species) %>% 
  fill(group, range_size, .direction = "downup") %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


res_median <- res_expand %>% 
  group_by(species, threshold, phase) %>% 
  summarise(n_cells_exposed = median(n_cells_exposed),
            total_duration = median(total_duration),
            range_size = median(range_size),
            range_exposed = median(range_exposed),
            mean_local_duration = median(mean_local_duration),
            .groups = "drop") 


saveRDS(res_median,  here("results/risk/risk_per_gwl_median.rds"))

