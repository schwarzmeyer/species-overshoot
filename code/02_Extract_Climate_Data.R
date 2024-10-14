# code to extract climate data to the Ocean and Land grids

# load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files(paste0(path, "Data/Climate Data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)
netcdfs_land <- grep("tas", netcdfs, value = T)
netcdfs_ocean <- grep("tos", netcdfs, value = T)

models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

# grid
terrestrial_grid <- readRDS(paste0(path, "Data/Spatial Data/terrestrial_grid_equal_area.rds"))
ocean_grid <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_equal_area.rds"))

# Time span of the datasets

# CanESM5 overshoot: 2040-2300
# CNRM-ESM2-1 overshoot: 2015-2300
# GISS-E2-1-G overshoot: 2040-2300
# IPSL-CM6A-LR overshoot: 2040-300
# MRI-ESM2-0: 2040-2300


beginCluster(8)

# surface temperature on land
for(my_model in models){
  
  models_tmp <- grep(my_model, netcdfs_land, value = T)
  
  models_historical <- grep("historical", models_tmp, value = T)
  models_ssp585     <- grep("ssp585", models_tmp, value = T)
  models_ssp534     <- grep("ssp534", models_tmp, value = T)
  
  r_historical <- raster::stack(models_historical)
  r_ssp585     <- raster::stack(models_ssp585)
  r_ssp534     <- raster::stack(models_ssp534)
    
  
  # calculate the yearly averages 
  # first, getting the indices for each stack
  # historical: 1850-2014 = 165 years
  index_historical <- rep(1:165, each = 12)
  
  # ssp585: 2015-2100 = 86 years
  if(my_model == "CanESM5" || my_model == "MRI-ESM2-0" || my_model == "GISS-E2-1-G"){
    index_ssp585 <- rep(1:286, each = 12)
  } else {
  index_ssp585 <- rep(1:86, each = 12)
  }
  # ssp534: 2040-2299 = 260 years
  if(my_model != "CNRM-ESM2-1"){
    index_ssp534 <- rep(1:260, each = 12)
  } else {
    index_ssp534 <- rep(1:285, each = 12)
  }
  
  r_historical_year <- raster::clusterR(r_historical, stackApply, args=list(indices = index_historical, fun = mean, na.rm = TRUE))
  if(my_model != "CNRM-ESM2-1") r_ssp585_year <- raster::clusterR(r_ssp585, stackApply, args=list(indices = index_ssp585, fun = mean, na.rm = TRUE))
  r_ssp534_year <- raster::clusterR(r_ssp534, stackApply, args=list(indices = index_ssp534, fun = mean, na.rm = TRUE))

  # rotate raster
  r_historical_rotate <- raster::rotate(r_historical_year)
  if(my_model != "CNRM-ESM2-1") r_ssp585_rotate <- raster::rotate(r_ssp585_year)
  r_ssp534_rotate <- raster::rotate(r_ssp534_year)
  
  # extract the average climate value per cell
  result_historical <- exact_extract(r_historical_rotate, terrestrial_grid, fun = "mean")
  if(my_model != "CNRM-ESM2-1") result_ssp585 <- exact_extract(r_ssp585_rotate, terrestrial_grid, fun = "mean")
  result_ssp534 <- exact_extract(r_ssp534_rotate, terrestrial_grid, fun = "mean")
  
  # cbind the results
  if(my_model != "CNRM-ESM2-1"){
    df <- cbind(terrestrial_grid$WorldID, result_historical, result_ssp585[,1:length(2015:2039)], result_ssp534)
  } else {
    
    df <- cbind(terrestrial_grid$WorldID, result_historical, result_ssp534)
    
  }
  colnames(df) <- c("WorldID", 1850:2299)
  df <- df %>% 
    as_tibble() %>% 
    mutate_at(vars(as.character(1850:2299)), function(x) {x - 273.15})
  
  saveRDS(df, paste0(path, "Data/Climate Data/Temperature Matrices/land_temp_mat_", my_model, ".rds"))
  
  
}


# sea surface temperature
for(my_model in models){
  
  models_tmp <- grep(my_model, netcdfs_ocean, value = T)
  
  models_historical <- grep("historical", models_tmp, value = T)
  models_ssp585     <- grep("ssp585", models_tmp, value = T)
  models_ssp534     <- grep("ssp534", models_tmp, value = T)
  
  r_historical <- raster::stack(models_historical)
  r_ssp585     <- raster::stack(models_ssp585)
  r_ssp534     <- raster::stack(models_ssp534)
  
  
  # calculate the yearly averages 
  # first, getting the indices for each stack
  # historical: 1850-2014 = 165 years
  index_historical <- rep(1:165, each = 12)
  
  # ssp585: 2015-2100 = 86 years
  if(my_model == "CanESM5" || my_model == "MRI-ESM2-0" || my_model == "GISS-E2-1-G"){
    index_ssp585 <- rep(1:286, each = 12)
  } else {
    index_ssp585 <- rep(1:86, each = 12)
  }
  # ssp534: 2040-2299 = 260 years
  if(my_model != "CNRM-ESM2-1"){
    index_ssp534 <- rep(1:260, each = 12)
  } else {
    index_ssp534 <- rep(1:285, each = 12)
  }
  
  r_historical_year <- raster::clusterR(r_historical, stackApply, args=list(indices = index_historical, fun = mean, na.rm = TRUE))
  if(my_model != "CNRM-ESM2-1") r_ssp585_year <- raster::clusterR(r_ssp585, stackApply, args=list(indices = index_ssp585, fun = mean, na.rm = TRUE))
  r_ssp534_year <- raster::clusterR(r_ssp534, stackApply, args=list(indices = index_ssp534, fun = mean, na.rm = TRUE))
  
  # rotate raster
  r_historical_rotate <- raster::rotate(r_historical_year)
  if(my_model != "CNRM-ESM2-1") r_ssp585_rotate <- raster::rotate(r_ssp585_year)
  r_ssp534_rotate <- raster::rotate(r_ssp534_year)
  
  # extract the average climate value per cell
  result_historical <- exact_extract(r_historical_rotate, ocean_grid, fun = "mean")
  if(my_model != "CNRM-ESM2-1") result_ssp585 <- exact_extract(r_ssp585_rotate, ocean_grid, fun = "mean")
  result_ssp534 <- exact_extract(r_ssp534_rotate, ocean_grid, fun = "mean")
  
  # cbind the results
  if(my_model != "CNRM-ESM2-1"){
    df <- cbind(ocean_grid$WorldID, result_historical, result_ssp585[,1:length(2015:2039)], result_ssp534)
  } else {
    
    df <- cbind(ocean_grid$WorldID, result_historical, result_ssp534)
    
  }
  colnames(df) <- c("WorldID", 1850:2299)
  df <- df %>% 
    as_tibble() %>% 
    na.omit() 
  
  saveRDS(df, paste0(path, "Data/Climate Data/Temperature Matrices/ocean_temp_mat_", my_model, ".rds"))
  
  
}


endCluster()


# global warming levels ----

# Load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files(paste0(path, "Data/Climate Data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)
netcdfs <- grep("tas", netcdfs, value = T)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")



res_list_rolling_20yr <- res_list_rolling_30yr <- res_list <- list()

beginCluster(8)


for(i in seq_along(models)){
  
  models_tmp <- grep(models[i], netcdfs, value = T)
  
  models_historical <- grep("historical", models_tmp, value = T)
  models_ssp585     <- grep("ssp585", models_tmp, value = T)
  models_ssp534     <- grep("ssp534", models_tmp, value = T)
  
  r_historical <- raster::stack(models_historical)
  r_ssp585     <- raster::stack(models_ssp585)
  r_ssp534     <- raster::stack(models_ssp534)
    
  
  # calculate the yearly averages 
  # first, getting the indices for each stack
  # historical: 1850-2014 = 165 years
  index_historical <- rep(1:165, each = 12)
  
  # ssp585: 2015-2100 = 86 years
  if(models[i] == "CanESM5" || models[i] == "MRI-ESM2-0" || models[i] == "GISS-E2-1-G"){
    index_ssp585 <- rep(1:286, each = 12)
  } else {
    index_ssp585 <- rep(1:86, each = 12)
  }
  # ssp534: 2040-2299 = 260 years
  if(models[i] != "CNRM-ESM2-1"){
    index_ssp534 <- rep(1:260, each = 12)
  } else {
    index_ssp534 <- rep(1:285, each = 12)
  }
  
  r_historical_year <- raster::clusterR(r_historical, stackApply, args=list(indices = index_historical, fun = mean, na.rm = TRUE))
  if(models[i] != "CNRM-ESM2-1") r_ssp585_year <- raster::clusterR(r_ssp585, stackApply, args=list(indices = index_ssp585, fun = mean, na.rm = TRUE))
  r_ssp534_year <- raster::clusterR(r_ssp534, stackApply, args=list(indices = index_ssp534, fun = mean, na.rm = TRUE))
  
  a <- raster::area(r_historical_year) / 10000
  y <- r_historical_year * a
  temp <- cellStats(y, sum) / cellStats(a, sum)
  temp_hist <- temp - 273.15
  
  a <- raster::area(r_ssp585_year) / 10000
  y <- r_ssp585_year * a
  temp <- cellStats(y, sum) / cellStats(a, sum)
  temp_ssp585 <- temp - 273.15
  
  a <- raster::area(r_ssp534_year) / 10000
  y <- r_ssp534_year * a
  temp <- cellStats(y, sum) / cellStats(a, sum)
  temp_ssp534 <- temp - 273.15
  
  
  # cbind the results
  if(models[i] != "CNRM-ESM2-1"){
    time_series <- c(temp_hist, temp_ssp585[1:length(2015:2039)], temp_ssp534)
  } else {
    time_series <- c(temp_hist, temp_ssp534)
  }
  
  res <- tibble(model = models[i], 
                year = 1850:2299, 
                temperature = time_series)
  

  res_rolling_right_20yr <- tibble(model = models[i],
                                   year = 1869:2299,
                                   temperature = zoo::rollapply(time_series, width = 20, FUN = mean, align = "right"))
  
  
  res_rolling_right_30yr <- tibble(model = models[i],
                                   year = 1879:2299,
                                   temperature = zoo::rollapply(time_series, width = 30, FUN = mean, align = "right"))
  
  
  res_list[[i]] <- res
  res_list_rolling_20yr[[i]] <- res_rolling_right_20yr
  res_list_rolling_30yr[[i]] <- res_rolling_right_30yr
  
  print(i)
  
}

endCluster()

res_final <- bind_rows(res_list)
res_final_rolling_20yr <- bind_rows(res_list_rolling_20yr)
res_final_rolling_30yr <- bind_rows(res_list_rolling_30yr)



saveRDS(res_final, paste0(path, "Data/Climate Data/Global Averages/global_averages.rds"))
saveRDS(res_final_rolling_30yr, paste0(path, "Data/Climate Data/Global Averages/global_averages_rolling_30yr.rds"))
saveRDS(res_final_rolling_20yr, paste0(path, "Data/Climate Data/Global Averages/global_averages_rolling_20yr.rds"))



# rolling average ---------

temp_mats <- list.files(paste0(path, "Data/Climate Data/Temperature Matrices"), full.names = T)
temp_mats_names <- list.files(paste0(path, "Data/Climate Data/Temperature Matrices"), full.names = F)




for(i in seq_along(temp_mats)){
  
  temp_mat <- readRDS(temp_mats[i])
  temp_mat_roll <- apply(temp_mat[,-1],1, function(x) zoo::rollapply(x, width = 20, FUN = mean, align = "right"))
  temp_mat_roll <- as_tibble(t(temp_mat_roll))
  temp_mat_roll <- tibble(WorldID = temp_mat$WorldID, temp_mat_roll )
  
  saveRDS(temp_mat_roll, paste0(path, "Data/Climate Data/Temperature Matrices Rolling Avg/rolling_20yr_", temp_mats_names[i]))
  
}

for(i in seq_along(temp_mats)){
  
  temp_mat <- readRDS(temp_mats[i])
  temp_mat_roll <- apply(temp_mat[,-1],1, function(x) zoo::rollapply(x, width = 30, FUN = mean, align = "right"))
  temp_mat_roll <- as_tibble(t(temp_mat_roll))
  temp_mat_roll <- tibble(WorldID = temp_mat$WorldID, temp_mat_roll )
  
  saveRDS(temp_mat_roll, paste0(path, "Data/Climate Data/Temperature Matrices Rolling Avg/rolling_30yr_", temp_mats_names[i]))
  
}
