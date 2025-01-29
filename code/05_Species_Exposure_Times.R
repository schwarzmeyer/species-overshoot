# script to calculate dates of exposure and de-exposure 

############################################################

# the script is divided in two parts.
# the first part calculates exposure and de-exposure times using the whole data,
# the second part calculates the same thing, but for each global warming threshold (1.5°C to 2°C, peak, than 2°C to 1.5°C) 
# the script truncates the data at the year global warming levels reach reach the threshold.
# hence, is possible to calculate our metrics at each threshold.

# part 1 ----

files <- list.files(here("results/raw_results"), rec = T, full.names = T)
models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")


get_exposure_times <- function(x, original.state, consecutive.elements){
  
  species <- x[1]
  world_id <- x[2]

  n <- as.numeric(x[-c(1,2)])

  # Calculate shift sequences
  rle_x <- data.frame(unclass(rle(n)))
  
  # Add year
  rle_x$year <- 2015 + cumsum(rle_x$lengths) - rle_x$lengths
  
  # Select only shifts with n or more consecuitve elements 
  rle_x <- rle_x[rle_x$lengths >= consecutive.elements,]
  
  # Add line with original state
  rle_x <- rbind(c(1, original.state, 2000), rle_x)
  
  # Remove lines with shifts that are not separeted for n consecutive elements
  rle_x <- rle_x[c(TRUE, diff(rle_x$values) != 0),]
  
  # Remove first line because the first line is either the original state 
  # or the same value as the original state
  rle_x <- rle_x[-1,]
  
  # if there are no rows in rle_x, it means no exposure
  if(nrow(rle_x) == 0) {
    
    exposure <- NA
    deexposure <- NA
    duration <- NA
    
    return(tibble(species, world_id, exposure, deexposure, duration))   
    
  }

    
    # if the only value in x$values is 0, it means that there was a single exposure event
    # with no de-exposure
    
  if(length(unique(rle_x$values)) == 1){
    if(unique(rle_x$values) == 0){
      
      exposure <- rle_x$year[1]
      deexposure <- 2221
      duration <- deexposure - exposure
      return(tibble(species, world_id, exposure, deexposure, duration))          
    }
  }
  
  # the remaining data will always have 0 and 1 on rle_x$values
  if(length(unique(rle_x$values)) == 2){
    
    exposure <- rle_x %>%
      filter(values == 0) %>%
      pull(year)
    
    deexposure <- rle_x %>%
      filter(values == 1) %>%
      pull(year)
    
    if(length(exposure) > length(deexposure))  deexposure[length(exposure)] <- 2221
    
    duration <- deexposure - exposure
    
    return(tibble(species, world_id, exposure, deexposure, duration))

  }
}

cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, "get_exposure_times")


for(i in seq_along(models)){
  
  my_files <- grep(models[i], files, value=T)
    
    for(j in seq_along(groups)){
      
      raw_results <- readRDS(grep(groups[j], my_files, value = T))
      
      raw_results <- raw_results %>% 
        bind_rows() %>% 
        # select(-as.character(2201:2219)) %>% 
        mutate(sum = rowSums(select(., starts_with("2")))) %>% 
        filter(sum < 202) %>%  # select only cells with < 202 suitable years (>= 202 years means no exposure)
        select(-sum)
      
      res_final <- pbapply(X = raw_results, 
                          MARGIN = 1, 
                          FUN = function(x) get_exposure_times(x = x, original.state = 1, consecutive.elements = 5),
                          cl = cl)
    
      res_final <- res_final %>% 
        bind_rows() %>% 
        na.omit() %>% 
        mutate(group = groups[j],
               model = models[i]) %>% 
        relocate(group) 
        
      saveRDS(res_final,
              file = here(glue("results/species_exposure_times/raw_{groups[j]}_{models[i]}.rds")))
      
  }
}

stopCluster(cl) 


#####################################################################

# part 2 ----

# get exposure times at different global warming level 

files <- list.files(here("results/raw_results"), rec = T, full.names = T)
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



get_exposure_times <- function(x, original.state, consecutive.elements, no.deexposure.year){
  
  species <- x[1]
  world_id <- x[2]
  
  n <- as.numeric(x[-c(1,2)])
  
  # Calculate shift sequences
  rle_x <- data.frame(unclass(rle(n)))
  
  # Add year
  rle_x$year <- 2015 + cumsum(rle_x$lengths) - rle_x$lengths
  
  # Select only shifts with n or more consecuitve elements 
  rle_x <- rle_x[rle_x$lengths >= consecutive.elements,]
  
  # Add line with original state
  rle_x <- rbind(c(1, original.state, 2000), rle_x)
  
  # Remove lines with shifts that are not separeted for n consecutive elements
  rle_x <- rle_x[c(TRUE, diff(rle_x$values) != 0),]
  
  # Remove first line because the first line is either the original state 
  # or the same value as the original state
  rle_x <- rle_x[-1,]
  
  # if there are no rows in rle_x, it means no exposure
  if(nrow(rle_x) == 0) {
    
    exposure <- NA
    deexposure <- NA
    duration <- NA
    
    return(tibble(species, world_id, exposure, deexposure, duration))   
    
  }
  
  
  # if the only value in x$values is 0, it means that there was a single exposure event
  # with no de-exposure
  
  if(length(unique(rle_x$values)) == 1){
    if(unique(rle_x$values) == 0){
      
      exposure <- rle_x$year[1]
      deexposure <- no.deexposure.year
      duration <- deexposure - exposure
      return(tibble(species, world_id, exposure, deexposure, duration))          
    }
  }
  
  # the remaining data will always have 0 and 1 on rle_x$values
  if(length(unique(rle_x$values)) == 2){
    
    exposure <- rle_x %>%
      filter(values == 0) %>%
      pull(year)
    
    deexposure <- rle_x %>%
      filter(values == 1) %>%
      pull(year)
    
    if(length(exposure) > length(deexposure))  deexposure[length(exposure)] <- no.deexposure.year
    
    duration <- deexposure - exposure
    
    return(tibble(species, world_id, exposure, deexposure, duration))
    
  }
}

cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, "get_exposure_times")

thresholds <- "peak"

for(.group in groups){
  for(.model in models){
    
    substring <- c(.group, .model)
    readme <- files[sapply(files, function(x) all(str_detect(x, substring)))]
    
    raw_results <- readRDS(readme)
    raw_results <- bind_rows(raw_results)
    
    for(.phase in phases){
      for(.threshold in thresholds){
        
        gwl_df <- gwl_times %>% 
          filter(model == .model,
                 threshold == .threshold,
                 phase == .phase) 
        
        max_year <- gwl_df$year
        
        if(length(max_year) == 0) next
        if(max_year <= 2018) next # exposure needs 5 consecutive years. so it cannot happen before 2018.
        
        no_exposure_threshold <-  length(2015:max_year) - 4
        
        raw_results_tmp <- raw_results %>% 
          select(c(species, WorldID, as.character(2015:max_year))) %>%
          mutate(sum = rowSums(select(., starts_with("2")))) %>% 
          filter(sum < no_exposure_threshold) %>%  # select only cells with < 202 suitable years (>= 202 years means no exposure)
          select(-sum)
        
        
        clusterExport(cl, varlist = "max_year") 
        
        res_final <- pbapply(X = raw_results_tmp, 
                             MARGIN = 1, 
                             FUN = function(x) get_exposure_times(x = x, original.state = 1, consecutive.elements = 5, no.deexposure.year = max_year + 1),
                             cl = cl)
        
        
        res_final <- res_final %>% 
          bind_rows() %>% 
          drop_na() %>% 
          mutate(group = .group,
                 model = .model) %>% 
          relocate(group) 
        
        saveRDS(res_final,
                file = here(glue("results/species_exposure_times_per_gwl/threshold_{.group}_{.model}_{.phase}_{.threshold}.rds")))
        
        print(glue("{.group}_{.model}_{.phase}_{.threshold}"))
        
      }
    }
  }
}


stopCluster(cl) 