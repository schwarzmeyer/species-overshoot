# script to extract climate data to the ocean and land grids

#############################################################

# load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files(here("raw_data/climate_data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)
netcdfs_land <- grep("tas", netcdfs, value = T)
netcdfs_ocean <- grep("tos", netcdfs, value = T)

models <- c("ACCESS-ESM1-5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

# grid
terrestrial_grid <- readRDS(here("raw_data/spatial_data/terrestrial_grid_equal_area.rds"))
ocean_grid <- readRDS(here("raw_data/spatial_data/ocean_grid_equal_area.rds"))


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
  result_historical <- exact_extract(r_historical_rotate, terrestrial_grid, fun = "mean") - 273.15
  result_ssp585 <- exact_extract(r_ssp585_rotate, terrestrial_grid, fun = "mean") - 273.15
  result_ssp534 <- exact_extract(r_ssp534_rotate, terrestrial_grid, fun = "mean") - 273.15
  

  # cbind the results
  if(my_model != "CNRM-ESM2-1"){
    
    df <- cbind(terrestrial_grid$WorldID, result_historical, result_ssp585[,1:length(2015:2039)], result_ssp534)
    
  } else {
    
    df <- cbind(terrestrial_grid$WorldID, result_historical, result_ssp534)
    
  }
  
  colnames(df) <- c("WorldID", 1850:2299)
  
  df <- df %>% 
    select(1:length(1850:2299)) %>% 
    as_tibble() 
  
  saveRDS(df, here(glue("processed_data/climate_data/temperature_matrices/land_temp_mat_{my_model}.rds")))
  
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
    as_tibble() 
  
  saveRDS(df, here(glue("processed_data/climate_data/temperature_matrices/ocean_temp_mat_{my_model}.rds")))
  
  
  
}


# global warming levels ----

# Load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files(here("raw_data/climate_data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)
netcdfs <- grep("tas", netcdfs, value = T)
models <- c("ACCESS-ESM1-5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

res_list <- list()

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
  r_ssp585_year     <- tapp(r_ssp585, index = "years", fun = mean, na.rm = TRUE)
  r_ssp534_year     <- tapp(r_ssp534, index = "years", fun = mean, na.rm = TRUE)     
  
  global_averages <- function(r){
    
    expanse(r, unit = "km", byValue = T) %>%
      as_tibble() %>% 
      na.omit() %>% 
      mutate(area = area/10000) %>% 
      mutate(w.value = value * area) %>% 
      group_by(layer) %>% 
      summarise(sum.w.value = sum(w.value),
                sum.area = sum(area)) %>% 
      mutate(global.temp = (sum.w.value/sum.area) - 273.15) %>% 
      pull(global.temp)
    
  }
  
  # calculate area weighted global averages
  temp_hist   <- global_averages(r_historical_year) 
  temp_ssp585 <- global_averages(r_ssp585_year) 
  temp_ssp534 <- global_averages(r_ssp534_year) 
  
  
  
  # cbind the results
  if(models[i] != "CNRM-ESM2-1"){
    time_series <- c(temp_hist, temp_ssp585[1:length(2015:2039)], temp_ssp534)
  } else {
    time_series <- c(temp_hist, temp_ssp534)
  }
  
  # remove year 2300, not present in some models
  time_series <- time_series[1:length(1850:2299)]
  
  res <- tibble(model = models[i], 
                year = 1850:2299, 
                temperature = time_series,
                temperature_rolling = rollapply(time_series, width = 20, FUN = mean, align = "center", fill = NA))
  
  
  res_list[[i]] <- res


}

res_final <- bind_rows(res_list)

saveRDS(res_final, here("processed_data/climate_data/global_averages/global_averages.rds"))


# calculate overshoot times
global_averages <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))


gwl <- global_averages %>% 
  group_by(model) %>%
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) %>% 
  mutate(gwl = temperature - pre_industrial_avg,
         gwl_rolling = temperature_rolling - pre_industrial_avg) %>% 
  mutate(os = ifelse(gwl_rolling > 2, gwl_rolling, NA)) %>% 
  mutate(os = ifelse(model == "CNRM-ESM2-1" & year > 2179, NA, os)) %>% 
  filter(year >= 2014,
         year <= 2220) 

begin_os <- gwl %>% 
  group_by(model) %>% 
  filter(gwl_rolling > 2) %>% 
  slice_min(year) %>% 
  select(model, year) %>% 
  rename("begin_os" = year)

end_os <- gwl %>% 
  filter(year > 2050) %>% 
  group_by(model) %>% 
  filter(gwl_rolling <= 2) %>% 
  slice(1) %>% 
  select(model, year) %>% 
  rename("end_os" = year)

peak_gwl <- gwl %>% 
  group_by(model) %>% 
  slice_max(gwl_rolling) %>% 
  select(model, gwl_rolling) %>%
  rename("magnitude" = gwl_rolling) %>% 
  mutate(magnitude = magnitude - 2)

os <- begin_os %>% 
  left_join(end_os, by = "model") %>% 
  left_join(peak_gwl, by = "model") %>% 
  mutate(duration = end_os - begin_os)

global_averages <- saveRDS(os, here("processed_data/climate_data/overshoot_times/overshoot_times.rds"))

