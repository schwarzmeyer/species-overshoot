# code to compute upper and lowet thermal niche limits for each species

# load files
spp_files <- list.files(here("processed_data/species_data/range_maps_grid_cells"), full.names = T)
spp_names <- list.files(here("processed_data/species_data/range_maps_grid_cells"), full.names = F)
temp_matrices <- list.files(here("processed_data/climate_data/temperature_matrices/"), full.names = T)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")


# function to compute the limits
get_niche_limits <- function(species_ranges, temperature_matrix, temperature_data_type){
  
  if(temperature_data_type == "raw") first_year <- 1850
  if(temperature_data_type == "rolling_30yr") first_year <- 1879

  data <- temperature_matrix %>% 
    select(WorldID, as.character(first_year:2014)) %>% 
    filter(WorldID %in% species_ranges) %>% 
    select(-WorldID) %>% 
    na.omit()
  
  # when the range don't overlap with the climate data, return NA
  if(nrow(data) == 0) return(tibble(niche_max = NA, niche_min = NA))
  
  means <- apply(data, 1, mean)
  sds <- apply(data, 1, sd) * 3
  
  upper_limit <- means + sds
  lower_limit <- means - sds
  
  upper_outliers <- sweep(data ,1, upper_limit)
  lower_outliers <- sweep(data ,1, lower_limit)
  
  data[upper_outliers > 0] <- NA
  data[lower_outliers < 0] <- NA
  
  row_max <- apply(data, 1, max, na.rm = T)
  row_min <- apply(data, 1, min, na.rm = T)
  
  row_max_mean <- mean(row_max)
  row_max_sd <- sd(row_max) * 3
  
  row_min_mean <- mean(row_min)
  row_min_sd <- sd(row_min) * 3
  
  if(!is.na(row_max_sd)){
    
    row_max_upper <- row_max_mean + row_max_sd
    row_max_lower <- row_max_mean - row_max_sd
    
    row_min_upper <- row_min_mean + row_min_sd
    row_min_lower <- row_min_mean - row_min_sd    
    
    pre_max <- row_max[which(row_max <= row_max_upper & row_max >= row_max_lower)]
    pre_min <- row_min[which(row_min <= row_min_upper & row_min >= row_min_lower)]
    
    niche_max <- max(pre_max)
    niche_min <- min(pre_min)
    
    res <- data.frame(niche_max,niche_min)

  } else {
    
    niche_max <- apply(data, 1, max, na.rm = T)
    niche_min <- apply(data, 1, min, na.rm = T)
    
    res <- data.frame(niche_max,niche_min)
    
  }
  
  # res2 <- do.call(rbind, res)
  # res2 <- rownames_to_column(res2, "species")
  # res2 <- as_tibble(res2)
  
  return(as_tibble(res))
  
}

# raw temperature ----
temperature_data_type <- "raw"

for(i in seq_along(spp_files)){
    
  spp_data <- readRDS(spp_files[i])
  
  for(my_model in models){
    
    if(any(str_detect(spp_files[i], c("Amphibians","Birds","Mammals","Reptiles")))) {
      
      temp_matrices_filtered <- grep("land", temp_matrices, value = T)
      
    } else {
      
      temp_matrices_filtered <- grep("ocean", temp_matrices, value = T)
      
    } 
    
    # load temperature matrix
    temp_matrix <- readRDS(grep(my_model, temp_matrices_filtered, value = T))
    
    
    # run
    plan("multisession", workers = availableCores())
    
    res <- future_map_dfr(spp_data, ~ get_niche_limits(.x, temp_matrix, temperature_data_type), .id = "species", .progress = T)

    res <- na.omit(res)
    
    res# save
    saveRDS(res, 
            file = here("processed_data/species_data/niche_limits", paste0(temperature_data_type,"/niche_",my_model,"_",spp_names[i])))
    
    gc()
  }
  
} 

