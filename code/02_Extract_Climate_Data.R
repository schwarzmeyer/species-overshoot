# code to extract climate data to the Ocean and Land grids
 
# load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files(here("raw_data/climate_data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)
netcdfs_land <- grep("tas", netcdfs, value = T)
netcdfs_ocean <- grep("tos", netcdfs, value = T)

models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

# grid
terrestrial_grid <- readRDS(here("raw_data/spatial_data/terrestrial_grid_equal_area.rds"))
ocean_grid <- readRDS(here("raw_data/spatial_data/ocean_grid_equal_area.rds"))

# Time span of the datasets

# CanESM5 overshoot: 2040-2300
# CNRM-ESM2-1 overshoot: 2015-2300
# GISS-E2-1-G overshoot: 2040-2300
# IPSL-CM6A-LR overshoot: 2040-300
# MRI-ESM2-0: 2040-2300

# surface temperature on land
for(my_model in models){
  
  models_tmp <- grep(my_model, netcdfs_land, value = T)
  
  models_historical <- grep("historical", models_tmp, value = T)
  models_ssp585     <- grep("ssp585", models_tmp, value = T)
  models_ssp534     <- grep("ssp534", models_tmp, value = T)
  
  r_historical <- rast(models_historical)
  r_ssp585     <- rast(models_ssp585)
  r_ssp534     <- rast(models_ssp534)
    
  
  # calculate yearly averages
  r_historical_year <- tapp(r_historical, index = "years", fun = mean, na.rm = TRUE)
  r_ssp585_year <- tapp(r_ssp585, index = "years", fun = mean, na.rm = TRUE)
  r_ssp534_year <- tapp(r_ssp534, index = "years", fun = mean, na.rm = TRUE)
  
  # rotate
  r_historical_rotate <- rotate(r_historical_year)
  r_ssp585_rotate <- rotate(r_ssp585_year)
  r_ssp534_rotate <- rotate(r_ssp534_year)
  
  # extract the average climate value per cell
  result_historical <- exact_extract(r_historical_rotate, terrestrial_grid, fun = "mean")
  result_ssp585 <- exact_extract(r_ssp585_rotate, terrestrial_grid, fun = "mean")
  result_ssp534 <- exact_extract(r_ssp534_rotate, terrestrial_grid, fun = "mean")
  

  # cbind the results
  if(my_model != "CNRM-ESM2-1"){
    
    df <- cbind(terrestrial_grid$WorldID, result_historical, result_ssp585[,1:length(2015:2039)], result_ssp534)
    
  } else {
    
    df <- cbind(terrestrial_grid$WorldID, result_historical, result_ssp534)
    
  }
  
  colnames(df) <- c("WorldID", 1850:2299)
  
  df <- df %>% 
    select(1:length(1850:2299)) %>% 
    as_tibble() %>% 
    mutate_at(vars(starts_with(c("1", "2"))), function(x) {x - 273.15})
  
  saveRDS(df, here("processed_data/climate_data/temperature_matrices", 
                   paste0("land_temp_mat_", my_model, ".rds")))
  
  
}



# surface temperature on ocean
for(my_model in models){
  
  models_tmp <- grep(my_model, netcdfs_ocean, value = T)
  
  models_historical <- grep("historical", models_tmp, value = T)
  models_ssp585     <- grep("ssp585", models_tmp, value = T)
  models_ssp534     <- grep("ssp534", models_tmp, value = T)
  
  r_historical <- rast(models_historical)
  
  # the models IPSL has no time associated
  # adding the time here
  if(my_model == "IPSL-CM6A-LR"){
    
    r_historical <- subset(r_historical, "area", negate=TRUE)

    time_values <- seq.Date(from = as.Date("1850-01-01"), 
                            to = as.Date("2014-12-01"), 
                            by = "month")
    
    terra::time(r_historical) <- as.Date(time_values)

  }
  
  r_ssp585     <- rast(models_ssp585)
  r_ssp534     <- rast(models_ssp534)
  
  
  # calculate yearly averages
  r_historical_year <- tapp(r_historical, index = "years", fun = mean, na.rm = TRUE)
  r_ssp585_year <- tapp(r_ssp585, index = "years", fun = mean, na.rm = TRUE)
  r_ssp534_year <- tapp(r_ssp534, index = "years", fun = mean, na.rm = TRUE)
  
  # rotate
  r_historical_rotate <- rotate(r_historical_year)
  r_ssp585_rotate <- rotate(r_ssp585_year)
  r_ssp534_rotate <- rotate(r_ssp534_year)
  
  # extract the average climate value per cell
  result_historical <- exact_extract(r_historical_rotate, ocean_grid, fun = "mean")
  result_ssp585 <- exact_extract(r_ssp585_rotate, ocean_grid, fun = "mean")
  result_ssp534 <- exact_extract(r_ssp534_rotate, ocean_grid, fun = "mean")
  
  
  # cbind the results
  if(my_model != "CNRM-ESM2-1"){
    
    df <- cbind(ocean_grid$WorldID, result_historical, result_ssp585[,1:length(2015:2039)], result_ssp534)
    
  } else {
    
    df <- cbind(ocean_grid$WorldID, result_historical, result_ssp534)
    
  }
  
  colnames(df) <- c("WorldID", 1850:2299)
  
  df <- df %>% 
    select(1:length(1850:2299)) %>% 
    as_tibble() %>% 
    mutate_at(vars(starts_with(c("1", "2"))), function(x) {x - 273.15})
  
  saveRDS(df, here("processed_data/climate_data/temperature_matrices", 
                   paste0("ocean_temp_mat_", my_model, ".rds")))
  
  
}


# global warming levels ----

# Load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files(here("raw_data/climate_data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)
netcdfs <- grep("tas", netcdfs, value = T)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

res_list_rolling_30yr <- res_list <- list()

for(i in seq_along(models)){
  
  models_tmp <- grep(models[i], netcdfs_land, value = T)
  
  models_historical <- grep("historical", models_tmp, value = T)
  models_ssp585     <- grep("ssp585", models_tmp, value = T)
  models_ssp534     <- grep("ssp534", models_tmp, value = T)
  
  r_historical <- rast(models_historical)
  r_ssp585     <- rast(models_ssp585)
  r_ssp534     <- rast(models_ssp534)
    
  # calculate yearly averages
  r_historical_year <- tapp(r_historical, index = "years", fun = mean, na.rm = TRUE)
  r_ssp585_year <- tapp(r_ssp585, index = "years", fun = mean, na.rm = TRUE)
  r_ssp534_year <- tapp(r_ssp534, index = "years", fun = mean, na.rm = TRUE)
  
  global_averages <- function(r){
    
    expanse(r, unit = "km", byValue = T) %>%
      as_tibble() %>% 
      mutate(area = area/10000) %>% 
      mutate(w.value = value * area) %>% 
      group_by(layer) %>% 
      summarise(sum.w.value = sum(w.value),
                sum.area = sum(area)) %>% 
      mutate(global.temp = (sum.w.value/sum.area) - 273.15) %>% 
      pull(global.temp)
    
  }
  # calculate area weighted global averages
  temp_hist <- global_averages(r_historical_year) 
  temp_ssp585 <- global_averages(r_ssp585_year) 
  temp_ssp534 <- global_averages(r_ssp534_year) 
  
  
  
  # cbind the results
  if(models[i] != "CNRM-ESM2-1"){
    time_series <- c(temp_hist, temp_ssp585[1:length(2015:2039)], temp_ssp534)
  } else {
    time_series <- c(temp_hist, temp_ssp534)
  }
  
  # remove year 2300, not present in all models
  time_series <-time_series[1:length(1850:2299)]
  
  res <- tibble(model = models[i], 
                year = 1850:2299, 
                temperature = time_series)

  
  res_rolling_right_30yr <- tibble(model = models[i],
                                   year = 1879:2299,
                                   temperature = zoo::rollapply(time_series, width = 30, FUN = mean, align = "right"))
  
  
  res_list[[i]] <- res
  res_list_rolling_30yr[[i]] <- res_rolling_right_30yr
  

}

res_final <- bind_rows(res_list)
res_final_rolling_30yr <- bind_rows(res_list_rolling_30yr)



saveRDS(res_final, here("processed_data/climate_data/global_averages/global_averages.rds"))
saveRDS(res_final_rolling_30yr, here("processed_data/climate_data/global_averages/global_averages_rolling_30yr.rds"))


