files <- list.files(paste0(path, "Results/Raw Results"), rec=T, full.names=T)
files_raw <- grep("raw", files, value = T)
files_rolling_20yr <- grep("rolling_20yr", files, value = T)
files_rolling_30yr <- grep("rolling_30yr", files, value = T)

models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")




# groups <- c("Abalones","Amphibians","Birds","Conesnails","Fishes","Lobsters",
#             "Mammals","Mangroves","Reefs","Reptiles","Seagrasses")

get_exposure_times <- function(x, original.state, consecutive.elements){
  
  species <- x[1]
  world_id <- x[2]

  n <- as.numeric(x[-c(1,2)])

  # x <- c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)
  
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
      deexposure <- 2201
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
    
    # if(length(deexposure) == 0) deexposure <- 2201
    if(length(exposure) > length(deexposure))  deexposure[length(exposure)] <- 2201
    
    duration <- deexposure - exposure
    
    return(tibble(species, world_id, exposure, deexposure, duration))

  }
}

cl <- makeCluster(7)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, "get_exposure_times")


for(i in seq_along(models)){
  
  my_files <- grep(models[i], files_raw, value=T)
    
    for(j in seq_along(groups)){
      
      raw_results <- readRDS(grep(groups[j], my_files, value = T))
      
      raw_results <- raw_results %>% 
        bind_rows() %>% 
        select(-as.character(2201:2219)) %>% 
        mutate(sum = rowSums(select(., starts_with("2")))) %>% 
        filter(sum < 182) %>%  # select only cells with < 182 suitable years (>= 182 years means no exposure)
        select(-sum)
      
      res_final <- pbapply(X = raw_results, 
                          MARGIN = 1, 
                          FUN = function(x) get_exposure_times(x = x, original.state = 1, consecutive.elements = 5),
                          cl = cl)
    
      res_final <- res_final %>% 
        bind_rows() %>% 
        na.omit() %>% 
        mutate(group = groups[j]) %>% 
        relocate(group)
        
      saveRDS(res_final,
              file = paste0(path, "Results/Species Exposure Dates/raw_",groups[j],"_",models[i],".rds"))
      
  }
}

stopCluster(cl)

# rolling 20yr  -----


cl <- makeCluster(7)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, "get_exposure_times")

for(i in seq_along(models)){
  
  my_files <- grep(models[i], files_rolling_20yr, value=T)
  
  for(j in seq_along(groups)){
    
    raw_results <- readRDS(grep(groups[j], my_files, value = T))
    
    raw_results <- raw_results %>% 
      bind_rows() %>% 
      select(-as.character(2201:2219)) %>% 
      mutate(sum = rowSums(select(., starts_with("2")))) %>% 
      filter(sum < 182) %>%  # select only cells with < 182 suitable years (>= 182 years means no exposure)
      select(-sum)
    
    res_final <- pbapply(X = raw_results, 
                         MARGIN = 1, 
                         FUN = function(x) get_exposure_times(x = x, original.state = 1, consecutive.elements = 5),
                         cl = cl)
    
    res_final <- res_final %>% 
      bind_rows() %>% 
      na.omit() %>% 
      mutate(group = groups[j]) %>% 
      relocate(group)
    
    saveRDS(res_final,
            file = paste0(path, "Results/Species Exposure Dates/rolling_20yr_",groups[j],"_",models[i],".rds"))
    
  }
}

stopCluster(cl)




# rolling 30yr  -----


cl <- makeCluster(7)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, "get_exposure_times")

for(i in seq_along(models)){
  
  my_files <- grep(models[i], files_rolling_30yr, value=T)
  
  for(j in seq_along(groups)){
     
    raw_results <- readRDS(grep(groups[j], my_files, value = T))
    
    raw_results <- raw_results %>% 
      bind_rows() %>% 
      select(-as.character(2201:2219)) %>% 
      mutate(sum = rowSums(select(., starts_with("2")))) %>% 
      filter(sum < 182) %>%  # select only cells with < 182 suitable years (>= 182 years means no exposure)
      select(-sum)
    
    res_final <- pbapply(X = raw_results, 
                         MARGIN = 1, 
                         FUN = function(x) get_exposure_times(x = x, original.state = 1, consecutive.elements = 5),
                         cl = cl)
    
    res_final <- res_final %>% 
      bind_rows() %>% 
      na.omit() %>% 
      mutate(group = groups[j]) %>% 
      relocate(group)
    
    saveRDS(res_final,
            file = paste0(path, "Results/Species Exposure Dates/rolling_30yr_",groups[j],"_",models[i],".rds"))
    
  }
}

stopCluster(cl)