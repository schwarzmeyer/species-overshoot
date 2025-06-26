source("code/00_Packages.R")
# script to extract climate data to the ocean and land grids

print("Running: 01_Prepare_Climate_Data.R")

#############################################################

# list files
netcdfs <- list.files(here("raw_data/climate_data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)

# pars
variables <- c("tas","tos")
models <- c("ACCESS-ESM1-5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")
scenarios <- c("historical","ssp534","ssp585")


prepare_climate_data <- function(data, r_template, realm = "land"){
  
  r <- rast(data)
  
  # correct historical models with no time associated
  if(any(is.na(terra::time(r))) & nlyr(r) == 1981) {
    
    r <- subset(r, "area", negate=TRUE)
    time_values <- seq.Date(from = as.Date("1850-01-01"), 
                            to = as.Date("2014-12-01"), 
                            by = "month")
    
    terra::time(r) <- as.Date(time_values)
    
  }
  
  r <- tapp(r, index = "years", fun = mean, na.rm = TRUE, cores = 7)
  r <- rotate(r) 
  
  
  # convert K to C if needed
  if(max(values(r), na.rm=T) > 100) r <- r - 273.15
  
  # r_template_reproj <- project(r_template, r)
  
  if(res(r)[1] != 1) r <- resample(r, r_template, method = "bilinear")
  
  if(realm == "land") {
    
    world <- ne_countries(scale = "large", returnclass = "sf") |>  
      st_transform(crs = crs(r)) 
    
    r <- r |> 
      mask(world, touches = TRUE) 
  }
  
  id_tbl <- r_template |> 
    as.data.frame(xy = T) |> 
    as_tibble() |> 
    rename(lon = x, lat = y) 
  
  r <- r |> 
    as.data.frame(xy = T) |> 
    as_tibble() |> 
    rename(lon = x, lat = y) |> 
    left_join(id_tbl, by = c("lon", "lat")) |> 
    relocate(world_id) |> 
    select(-c(lon, lat)) |> 
    drop_na(world_id)
  
  column_names <- str_replace(colnames(r), "y_", "")
  colnames(r) <- column_names
  
  return(r)
}


for(.model in models){
  for(.variable in variables){
    
    tic(glue("-- Running time: {.model} {.variable}"))
    
    netcdfs_tmp <- grepv(.variable, netcdfs)
    models_tmp  <- grepv(.model, netcdfs_tmp)
    
    models_historical <- grepv("historical", models_tmp)
    models_ssp585     <- grepv("ssp585", models_tmp)
    models_ssp534     <- grepv("ssp534", models_tmp)
    
    realm <- ifelse(.variable == "tas", "land", "ocean")
    
    r_template <- rast()
    values(r_template) <- 1:ncell(r_template)
    names(r_template) <- "world_id"
    
    r_historical <- prepare_climate_data(models_historical, r_template, realm)
    r_ssp585     <- prepare_climate_data(models_ssp585, r_template, realm)
    r_ssp534     <- prepare_climate_data(models_ssp534, r_template, realm)
    
    if(.model != "CNRM-ESM2-1"){
      
      result <- left_join(r_historical, 
                         r_ssp585 |> select(world_id, as.character(2015:2040)), 
                         by = "world_id") |> 
               left_join(r_ssp534 |> select(world_id, as.character(2041:2299)), 
                         by = "world_id")
        
      } else {
        
      result <- left_join(r_historical, r_ssp534, by = "world_id")
  
      }
    
    result <- result |> 
      pivot_longer(-1, names_to = "year", values_to = "temp") |> 
      mutate(world_id = as.integer(world_id),
             year = as.integer(year),
             temp = as.integer(round(temp, 3) * 1000))
    
    toc()
    
    saveRDS(result, here(glue("processed_data/climate_data/temperature_matrices/{.variable}_temp_mat_{.model}.rds")))
    
  }  
}


# global warming levels ----

# Load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files(here("raw_data/climate_data/CMIP6/Overshoot"), ".nc", recursive = T, full.names = T)
netcdfs <- grepv("tas", netcdfs)
models <- c("ACCESS-ESM1-5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

global_averages <- function(data){
  
  r <- rast(data)
  r <- tapp(r, index = "years", fun = mean, na.rm = TRUE, cores = 5)

  avgs <- expanse(r, unit = "km", byValue = T) |>
    as_tibble() |>
    na.omit() |>
    mutate(area = area/10000) |>
    mutate(w.value = value * area) |>
    group_by(layer) |>
    summarise(sum.w.value = sum(w.value),
              sum.area = sum(area)) |>
    mutate(global.temp = (sum.w.value/sum.area) - 273.15) |>
    pull(global.temp)
    
  return(avgs)
}

res_list <- list()

for(.model in models){

  tic(glue("-- Running time: {.model}"))
  
  models_tmp <- grepv(.model, netcdfs)

  models_historical <- grepv("historical", models_tmp)
  models_ssp585     <- grepv("ssp585", models_tmp)
  models_ssp534     <- grepv("ssp534", models_tmp)

  temp_hist   <- global_averages(models_historical)
  temp_ssp585 <- global_averages(models_ssp585)
  temp_ssp534 <- global_averages(models_ssp534)
  


  # cbind the results
  if(.model != "CNRM-ESM2-1"){
    time_series <- c(temp_hist, temp_ssp585[1:length(2015:2040)], temp_ssp534)
  } else {
    time_series <- c(temp_hist, temp_ssp534)
  }

  
  # remove year 2300, not present in some models
  time_series <- time_series[1:length(1850:2299)]

  result <- tibble(model = .model,
                  year = 1850:2299,
                  temperature = time_series,
                    temperature_rolling = rollapply(time_series, width = 20, FUN = mean, align = "center", fill = NA))

  toc()
  res_list <- append(res_list, list(result))
}

res_final <- bind_rows(res_list)

res_final <- res_final |> 
  mutate(temperature_rolling = round(temperature_rolling, 2))

saveRDS(res_final, here("processed_data/climate_data/global_averages/global_averages.rds"))


# calculate overshoot times
global_averages <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))

gwl <- global_averages |>
  group_by(model) |>
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) |>
  mutate(gwl = temperature - pre_industrial_avg,
         gwl_rolling = temperature_rolling - pre_industrial_avg) |>
  mutate(os = ifelse(gwl_rolling > 2, gwl_rolling, NA)) |>
  mutate(os = ifelse(model == "CNRM-ESM2-1" & year > 2179, NA, os)) |>
  filter(year >= 2015,
         year <= 2220) |> 
  mutate(gwl_rolling = round(gwl_rolling, 2))

begin_os <- gwl |>
  group_by(model) |>
  filter(gwl_rolling > 2) |>
  slice_min(year) |>
  select(model, year) |>
  rename("begin_os" = year)

end_os <- gwl |>
  filter(year > 2050) |>
  group_by(model) |>
  filter(gwl_rolling <= 2) |>
  slice(1) |>
  select(model, year) |>
  rename("end_os" = year)

peak_gwl <- gwl |>
  group_by(model) |>
  slice_max(gwl_rolling) |>
  select(model, gwl_rolling) |>
  rename("magnitude" = gwl_rolling) |>
  mutate(magnitude = magnitude - 2) |> 
  distinct()

peak_year <- gwl |>
  group_by(model) |>
  slice_max(gwl_rolling) |> 
  select(model, year) |>
  rename(year_peak = year) |> 
  summarise(year_peak = round(median(year_peak), 0))

os <- begin_os |>
  left_join(end_os, by = "model") |>
  left_join(peak_gwl, by = "model") |>
  left_join(peak_year, by = "model") |>
  mutate(duration = end_os - begin_os)

saveRDS(os, here("processed_data/climate_data/overshoot_times/overshoot_times.rds"))

print("DONE: 01_Prepare_Climate_Data.R")

