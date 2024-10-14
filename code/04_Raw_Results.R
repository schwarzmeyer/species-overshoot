# Code to generate the raw results used to construct the horizons profiles
# Raw results are matrices in which rows represent the species occurring in an assemblage (i.e. grid cell)
# and columns are the years in the time series. The cell is assigned with 1 if the climate is suitable
# for the species in a given year. The cell is assigned with 0 if the climate is unsuitable.

spp_ranges <- list.files(paste0(path, "Data/Species Data/Range Maps Grid Cells"), full.names = T)
spp_names <- list.files(paste0(path,"Data/Species Data/Range Maps Grid Cells"), full.names = F)

temp_matrices <- list.files(paste0(path, "Data/Climate Data/Temperature Matrices"), full.names = T)
temp_matrices_ocean <- grep("ocean", temp_matrices, value = T)
temp_matrices_rolling_20yr <- list.files(paste0(path, "Data/Climate Data/Temperature Matrices Rolling Avg"), pattern = "20yr", full.names = T)
temp_matrices_rolling_30yr <- list.files(paste0(path, "Data/Climate Data/Temperature Matrices Rolling Avg"), pattern = "30yr", full.names = T)

# Not all climate models overlap with our ocean grid.
# Some grid cells are missing in some models.
# We only used grid cells that contain climate data for all models.
# mod1 <- readRDS(temp_matrices_ocean[1])$WorldID
# mod2 <- readRDS(temp_matrices_ocean[2])$WorldID
# mod3 <- readRDS(temp_matrices_ocean[3])$WorldID
# mod4 <- readRDS(temp_matrices_ocean[4])$WorldID
# mod5 <- readRDS(temp_matrices_ocean[5])$WorldID
# 
# ocean_ids <- Reduce(intersect, list(mod1,mod2,mod3,mod4,mod5))


niche_data_raw <- list.files(paste0(path, "Data/Species Data/Niche Limits/raw"), full.names = T)
niche_data_rolling_20yr <- list.files(paste0(path, "Data/Species Data/Niche Limits/rolling_20yr"), full.names = T)
niche_data_rolling_30yr <- list.files(paste0(path, "Data/Species Data/Niche Limits/rolling_30yr"), full.names = T)

models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

groups <- c("Amphibians","Birds","Fishes","Mammals","Reptiles")


get_raw_results <- function(x){
  
  spp_data <- species_range[[x]]
  spp_name <- names(species_range)[[x]]
  
  spp_matrix <- temp_matrix %>% 
    filter(WorldID %in% spp_data) %>% 
    na.omit()
  
  spp_niche <- niche %>%
    filter(species %in% spp_name)
  
  
  spp_matrix <- spp_matrix %>% 
    mutate(across(2:ncol(spp_matrix), ~ case_when(. <= spp_niche$niche_max ~ 1,
                                                  . > spp_niche$niche_max ~ 0)))
  
  spp_matrix$species <- spp_name
  spp_matrix <- spp_matrix %>% 
    relocate(species)
  
  return(spp_matrix)
  
}

# running raw climate data 

niche_data <- niche_data_raw

for(j in seq_along(models)){
  
  temp_matrix_tmp <- grep(models[j], temp_matrices, value = T)
  temp_matrix_land <- readRDS(grep("land", temp_matrix_tmp, value = T))
  temp_matrix_ocean <- readRDS(grep("ocean", temp_matrix_tmp, value = T))
  
  temp_matrix_land <- temp_matrix_land %>% 
    select(WorldID, as.character(2015:2219))
  
  temp_matrix_ocean <- temp_matrix_ocean %>% 
    select(WorldID, as.character(2015:2219))
  
  niche_data_tmp <- grep(models[j], niche_data, value = T)
  
  
  for(i in seq_along(groups)){
    
    species_range <- readRDS(grep(groups[i], spp_ranges, value = T))
    niche <- readRDS(grep(groups[i], niche_data_tmp, value = T))
    
    cl <- makeCluster(7)
    clusterEvalQ(cl, library(dplyr))
    
    if(any(str_detect(groups[i], c("Amphibians","Birds","Mammals","Reptiles")))) {
      
      temp_matrix <- temp_matrix_land
      clusterExport(cl, c("species_range","temp_matrix","niche"))
      
      res <- pblapply(1:length(species_range), get_raw_results, cl = cl)
      
    } else {
      
      temp_matrix <- temp_matrix_ocean
      
      clusterExport(cl, c("species_range","temp_matrix","niche"))
      
      res <- pblapply(1:length(species_range), get_raw_results, cl = cl)
      
    } 
  
    stopCluster(cl)
    
    names(res) <- names(species_range)
  
  
    saveRDS(res, file = paste0(path,"Results/Raw Results/raw_",models[j],"_",groups[i],".rds"))
  
  }
}


# 20yr rolling average ----

niche_data <- niche_data_rolling_20yr


for(j in seq_along(models)){
  
  temp_matrix_tmp <- grep(models[j], temp_matrices_rolling_20yr, value = T)
  temp_matrix_land <- readRDS(grep("land", temp_matrix_tmp, value = T))
  temp_matrix_ocean <- readRDS(grep("ocean", temp_matrix_tmp, value = T))
  
  temp_matrix_land <- temp_matrix_land %>% 
    select(WorldID, as.character(2015:2219))
  
  temp_matrix_ocean <- temp_matrix_ocean %>% 
    select(WorldID, as.character(2015:2219))
  
  niche_data_tmp <- grep(models[j], niche_data, value = T)
  
  
  for(i in seq_along(groups)){
    
    species_range <- readRDS(grep(groups[i], spp_ranges, value = T))
    niche <- readRDS(grep(groups[i], niche_data_tmp, value = T))
    
    cl <- makeCluster(7)
    clusterEvalQ(cl, library(dplyr))
    
    if(any(str_detect(groups[i], c("Amphibians","Birds","Mammals","Reptiles")))) {
      
      temp_matrix <- temp_matrix_land
      clusterExport(cl, c("species_range","temp_matrix","niche"))
      
      res <- pblapply(1:length(species_range), get_raw_results, cl = cl)
      
    } else {
      
      temp_matrix <- temp_matrix_ocean
      
      clusterExport(cl, c("species_range","temp_matrix","niche"))
      
      res <- pblapply(1:length(species_range), get_raw_results, cl = cl)
      
    } 
    
    stopCluster(cl)
    
    names(res) <- names(species_range)
    
    
    saveRDS(res, file = paste0(path,"Results/Raw Results/rolling_20yr_",models[j],"_",groups[i],".rds"))
    
  }
}

# 20yr rolling average ----

niche_data <- niche_data_rolling_30yr


for(j in seq_along(models)){
  
  temp_matrix_tmp <- grep(models[j], temp_matrices_rolling_30yr, value = T)
  temp_matrix_land <- readRDS(grep("land", temp_matrix_tmp, value = T))
  temp_matrix_ocean <- readRDS(grep("ocean", temp_matrix_tmp, value = T))
  
  temp_matrix_land <- temp_matrix_land %>% 
    select(WorldID, as.character(2015:2219))
  
  temp_matrix_ocean <- temp_matrix_ocean %>% 
    select(WorldID, as.character(2015:2219))
  
  niche_data_tmp <- grep(models[j], niche_data, value = T)
  
  
  for(i in seq_along(groups)){
    
    species_range <- readRDS(grep(groups[i], spp_ranges, value = T))
    niche <- readRDS(grep(groups[i], niche_data_tmp, value = T))
    
    cl <- makeCluster(7)
    clusterEvalQ(cl, library(dplyr))
    
    if(any(str_detect(groups[i], c("Amphibians","Birds","Mammals","Reptiles")))) {
      
      temp_matrix <- temp_matrix_land
      clusterExport(cl, c("species_range","temp_matrix","niche"))
      
      res <- pblapply(1:length(species_range), get_raw_results, cl = cl)
      
    } else {
      
      temp_matrix <- temp_matrix_ocean
      
      clusterExport(cl, c("species_range","temp_matrix","niche"))
      
      res <- pblapply(1:length(species_range), get_raw_results, cl = cl)
      
    } 
    
    stopCluster(cl)
    
    names(res) <- names(species_range)
    
    
    saveRDS(res, file = paste0(path,"Results/Raw Results/rolling_30yr_",models[j],"_",groups[i],".rds"))
    
  }
}
