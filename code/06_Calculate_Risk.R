# Risk is calculated as the proportion of geographic range exposed 
# and duration of exposure

# prepare data ----
# raw climate ----
files <- list.files(here("results/species_exposure_dates"), full.names = T)
ranges_raw <- list.files(here("results/raw_results"), full.names = T, pattern = "raw", rec = F)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")
groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")

get_risk_metrics <- function(x, range.sizes = range_sizes){
  
  df <- x %>% 
    group_by(species) %>% 
    summarise(n_cells_exposed = length(unique(world_id)),
              total_duration = sum(duration)) %>% 
    ungroup() %>% 
    left_join(range_sizes, by = "species") %>% 
    mutate(range_exposed = n_cells_exposed / range_size,
           mean_local_duration = total_duration / n_cells_exposed)
  
  
  return(df)
}


res_final_list <- list()

for(i in seq_along(groups)){
  
  files_tmp <- grep(groups[i], files, value = T)
  res_list <- list()
  
  for(j in seq_along(models)) {
    
    df <- readRDS(grep(pattern = models[j], files_tmp, value = T))
    
    ranges_tmp <- grep(groups[i], ranges_raw, value = T)
    ranges_tmp <- grep(models[j], ranges_tmp, value = T)
    
    species_range <- readRDS(ranges_tmp)
    
    range_sizes <- map_dfr(species_range, ~ tibble(range_size = nrow(.x)), .id = "species")
    
    res <- get_risk_metrics(df, range_sizes)
    
    res$model <- models[j]
    res$group <- groups[i]
    
    res <- res %>% 
      relocate(group)
    
    res_list[[j]] <- res
    
  }
  
  res_final_list[[i]] <- bind_rows(res_list)
  
  
}

res_final <- bind_rows(res_final_list)

saveRDS(res_final, here("results/risk/risk_raw_models.rds"))


res_median <- res_final %>% 
  expand(model, species) %>% 
  left_join(res_final, by = c("model","species")) %>% 
  mutate_all( ~replace(., is.na(.), 0)) %>%  group_by(model, group, species) %>% 
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
