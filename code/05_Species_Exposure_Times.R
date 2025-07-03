source("code/00_Packages.R")

print("Running: 05_Species_Exposure_Times.R")

# script to calculate dates of exposure and de-exposure 

############################################################

# the script is divided in two parts.
# the first part calculates exposure and de-exposure times using the whole data,
# the second part calculates the same thing, but for each global warming threshold (1.5°C to 2°C, peak, than 2°C to 1.5°C) 
# the script truncates the data at the year global warming levels reach reach the threshold.
# hence, is possible to calculate our metrics at each threshold.

files <- list.files(here("results/raw_results"), rec = FALSE, full.names = TRUE, pattern = ".rds")
models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")
n_cores <- 10

# get temperature data and calculate global warming levels at each year
global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))

gwl_df <- global_temp_avg |> 
  group_by(model) |>
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) |> 
  mutate(gwl = temperature_rolling - pre_industrial_avg) |> 
  select(model, year, gwl) |> 
  filter(year >= 2015,
         year <= 2220) |> 
  ungroup()

# function to calculate exposure and de-exposure times
get_exposure_times<- function(x, consecutive_elements = 5){
  
  species <- x[1]
  world_id <- x[2]
  n <- as.numeric(x[-c(1,2)])
  
  # # test code
  # species <- x[1,1]
  # world_id <- x[1,2]
  # n <- as.numeric(x[1,-c(1,2)])
  # consecutive_elements <- 5
  # n[43:49] <- 0
  # n[200:206] <- 0
  # n
  
  # compute rle of the time series
  rle_result <- rle(n)
  
  # calculate start positions and years for each run
  start_positions <- cumsum(c(1L, rle_result$lengths))[seq_along(rle_result$lengths)]
  start_years <- 2015L + start_positions - 1L
  
  # filter runs with length >= consecutive_elements
  keep <- rle_result$lengths >= consecutive_elements
  filtered_values <- rle_result$values[keep]
  filtered_starts <- start_years[keep]
  
  # check if any runs remain after filtering
  if(length(filtered_values) == 0){
    return(tibble(species = species, world_id = world_id, exposure = NA_real_, deexposure = NA_real_, duration = NA_real_))
  }
  
  # merge consecutive same runs in filtered_values
  merged_rle <- rle(filtered_values)
  group <- rep(seq_along(merged_rle$lengths), merged_rle$lengths)
  merged_starts <- sapply(split(filtered_starts, group), min)
  merged_values <- merged_rle$values
  
  # find indices of 1s and 0s in merged_values
  ones_idx <- which(merged_values == 1)
  zeros_idx <- which(merged_values == 0)
  
  if(length(ones_idx) == 0){
    return(tibble(species = species, world_id = world_id, exposure = NA_real_, deexposure = NA_real_, duration = NA_real_))
  }
  
  # for each 1, find the next 0 after it
  next_zero <- sapply(ones_idx, function(i) {
    candidates <- zeros_idx[zeros_idx > i]
    if(length(candidates) == 0) NA_integer_ else candidates[1]
  })
  
  exposure <- merged_starts[ones_idx]
  deexposure <- ifelse(is.na(next_zero), 2221L, merged_starts[next_zero])
  
  # create the result tibble
  result <- tibble(
    species = species,
    world_id = world_id,
    exposure = exposure,
    deexposure = deexposure,
    duration = deexposure - exposure
  )
  
  ##########################################
  
  exposures <- result$exposure
  
  deexposure_null <- sapply(exposures, function(x){
    
    n_null <- n[1:length(2015:x)]
    n_null <- c(rep(0, 5), n_null)
    names(n_null) <- 2010 + seq_along(n_null) - 1L
    
    rle_null <- rle(n_null)
    
    # Find runs of zeros with length >= 5
    zeros_idx <- which(rle_null$values == 0 & rle_null$lengths >= 5)
    
    # If no qualifying runs found, return NA
    if (length(zeros_idx) == 0) {
      return(NA)
    }
    
    # Get the last qualifying run
    last_zero <- zeros_idx[length(zeros_idx)]
    
    # Calculate end position of the last qualifying run
    last_zero_position <- sum(rle_null$lengths[1:last_zero])
    
    # Calculate the corresponding year
    result <- 2010L + last_zero_position - 1L
    
    return(result)
  })
    
  
  result$deexposure_null <- deexposure_null
  return(result)
  
}

# test
# .model <- models[1]
# .group <- groups[1]

for(.model in models){
    for(.group in groups){
      
      gwl_subset <- gwl_df |> 
        filter(model == .model) |> 
        select(-model) 
      
      peak_info <- gwl_subset |>
        filter(gwl == max(gwl)) |>
        slice(1) 
      
      gwl_subset <- gwl_subset |> 
        mutate(phase = case_when(year < peak_info$year ~ "warming",
                                 year == peak_info$year ~ "peak",
                                 year > peak_info$year ~ "cooling"),
               phase = factor(phase))
      
      raw_results <- files |> 
        grepv(pattern = .model) |> 
        grepv(pattern = .group) |> 
        readRDS()
      
      tic(glue("-- Running time: {.group} and {.model}"))
      
      # first remove rows with less than 5 consecutive years of exposure
      # this significantly reduces the number of rows to process
      
      raw_results <- mclapply(X = raw_results, 
                              FUN = function(x){
                                x |> 
                                  # mutate(sum = rowSums(select(., starts_with("2")))) |>
                                  mutate(sum = rowSums(across(starts_with("2")))) |>
                                  filter(sum >= 5) |>  # select only cells with >=5 exposure years 
                                  select(-sum)
                              },
                              mc.cores = n_cores)
      
      result <- mclapply(X = raw_results, 
                         FUN = function(x){
                           apply(x, 1, get_exposure_times)},
                         mc.cores = n_cores) |> 
        bind_rows() |> 
        na.omit() |> 
        mutate(group = factor(.group),
               species = factor(species),
               model = factor(.model),
               world_id = as.integer(world_id),
               exposure = as.integer(exposure),
               deexposure = as.integer(deexposure),
               duration = as.integer(duration)) |> 
        relocate(group) 
      
      toc()
      
      # Join exposure results with GWL data
      result_with_gwl <- result |>
        left_join(gwl_subset, by = c("exposure" = "year")) |>
        rename(exposure_gwl = gwl,
               exposure_phase = phase) |>
        left_join(gwl_subset, by = c("deexposure" = "year")) |>
        rename(deexposure_gwl = gwl,
               deexposure_phase = phase) |> 
        left_join(gwl_subset, by = c("deexposure_null" = "year")) |>
        rename(deexposure_null_gwl = gwl,
               deexposure_null_phase = phase) |>
        mutate(peak_year = peak_info$year,
               peak_gwl = peak_info$gwl)
      
      saveRDS(result_with_gwl,
              file = here(glue("results/species_exposure_times/{.group}_{.model}.rds")))
      
  }
}

print("DONE: 05_Species_Exposure_Times.R")

# 
# #####################################################################
# 
# # part 2 ----
# 
# # get exposure times at different global warming level
# 
# files <- list.files(here("results/raw_results"), rec = F, full.names = T, pattern = ".rds")
# models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
# 
# groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")
# phases <- c("warming_phase", "cooling_phase")
# gwl_levels <- seq(1.5, 2, by = 0.1)
# thresholds <- c(gwl_levels, "peak")
# 
# 
# 
# 
# 
# # First get the global peak warming level and year
# get_peak_info <- function(gwl_df) {
#   gwl_df |>
#     filter(gwl == max(gwl)) |>
#     slice(1) |>  # in case of multiple peak years
#     select(peak_year = year, peak_gwl = gwl)
# }
# 
# # Main analysis function
# exposure_results <- res_final |> 
# filter(model == .model)
# 
# 
# for(.model in models){
#   for(.group in groups){
#     
#     raw_results <- files |> 
#       grep(.model, ., value = T) |> 
#       grep(.group, ., value = T) |> 
#       readRDS()
#     
#    
#       
#         
#         }
#     
#     
#     
#     
# 
#     
#     # Get the years of exposure and deexposure
#     exposure_results <- raw_results |>
#       select(species, world_id, exposure, deexposure) |>
#       filter(!is.na(exposure), !is.na(deexposure)) |>
#       mutate(across(c(exposure, deexposure), as.integer))
#     
#     # Save the exposure results
#     saveRDS(exposure_results,
#             file = here(glue("results/species_exposure_times_per_gwl/{.group}_{.model}.rds")))
#     
#   }
# }
# 
# exposure_results <- raw_results
# gwl_df <- global_temp_avg |> 
#   filter()
# 
# # First add warming level info to exposure results
# add_gwl_context <- function(exposure_results, gwl_subset) {
#   gwl_prepped <- gwl_subset |>
#     reframe(peak = year[which.max(glw)]) 
#     
#   mutate(phase = case_when(gwl > lag(gwl) ~ "ascending",
#                              gwl < lag(gwl) ~ "descending",
#                              TRUE ~ "stable"
#       ),
#       peak_gwl = cummax(gwl)
#     ) |> view
#   
#   exposure_results |>
#     left_join(gwl_prepped |> select(year, exposure_gwl = gwl, exposure_phase = phase), 
#               by = c("exposure" = "year")) |>
#     left_join(gwl_prepped |> select(year, deexposure_gwl = gwl, deexposure_phase = phase), 
#               by = c("deexposure" = "year")) |>
#     mutate(
#       peak_gwl_during_exposure = map2_dbl(exposure, ifelse(deexposure == 2221, 2220, deexposure), ~ {
#         gwl_prepped |>
#           filter(year >= .x, year <= .y) |>
#           pull(gwl) |>
#           max(na.rm = TRUE)
#       })
#     )
# }
# 
# # Main analysis function
# analyze_warming_levels <- function(exposure_results, gwl_df) {
#   targets <- c(seq(1.5, 2.0, 0.1), "peak", rev(seq(1.5, 2.0, 0.1)))
#   
#   exposure_with_gwl <- add_gwl_context(exposure_results, gwl_df)
#   
#   map_dfr(targets, ~ {
#     if(.x == "peak") {
#       exposure_with_gwl |>
#         filter(peak_gwl_during_exposure == max(peak_gwl_during_exposure)) |>
#         mutate(target_gwl = "peak")
#     } else {
#       target <- as.numeric(.x)
#       exposure_with_gwl |>
#         filter(
#           (exposure_phase == "ascending" & 
#              between(exposure_gwl, target - 0.05, target + 0.05)) |
#             (deexposure_phase == "descending" & 
#                between(deexposure_gwl, target - 0.05, target + 0.05))
#         ) |>
#         mutate(target_gwl = as.character(.x))
#     }
#   }) |>
#     select(species, world_id, target_gwl, exposure, deexposure,
#            exposure_gwl, deexposure_gwl, peak_gwl_during_exposure)
# }
# 
# # Usage example:
# # 1. First get your exposure_results from previous analysis
# # 2. Load your gwl_df with columns (year, gwl)
# # 3. Run:
# # gwl_analysis <- analyze_warming_levels(exposure_results, gwl_df)
# 
# 
# 
# # Compute thresholds for each model
# gwl <- global_temp_avg |>
#   group_by(model) |>
#   mutate(pre_industrial_avg = mean(temperature[year <= 1900])) |>
#   mutate(gwl = temperature_rolling - pre_industrial_avg) |>
#   select(model, year, gwl) |>
#   filter(year >= 2015,
#          year <= 2220)
# 
# gwl_times <- gwl |>
#   group_by(model) |>
#   reframe(
#     # find the peak year for each model
#     peak_year = year[which.max(gwl)],
#     # split data into warming and cooling phases
#     map_dfr(gwl_levels, ~ {
#       warming_phase <- year[gwl >= .x & year <= peak_year][1]
#       cooling_phase <- year[gwl <= .x & year > peak_year][1]
#       tibble(threshold = factor(.x), warming_phase = warming_phase, cooling_phase = cooling_phase)
#     })) |>
#   select(-peak_year) |>
#   pivot_longer(cols = c(warming_phase, cooling_phase),
#                names_to = "phase",
#                values_to = "year") |>
#   bind_rows(gwl |>
#               slice_max(gwl) |>
#               mutate(threshold = "peak",
#                      phase = "warming_phase") |>
#               select(model, threshold, phase, year)
#   ) |>
#   drop_na()
# 
# 
# 
# get_exposure_times <- function(x, original.state, consecutive.elements, no.deexposure.year){
# 
#   species <- x[1]
#   world_id <- x[2]
# 
#   n <- as.numeric(x[-c(1,2)])
# 
#   # Calculate shift sequences
#   rle_x <- data.frame(unclass(rle(n)))
# 
#   # Add year
#   rle_x$year <- 2015 + cumsum(rle_x$lengths) - rle_x$lengths
# 
#   # Select only shifts with n or more consecuitve elements
#   rle_x <- rle_x[rle_x$lengths >= consecutive.elements,]
# 
#   # Add line with original state
#   rle_x <- rbind(c(1, original.state, 2000), rle_x)
# 
#   # Remove lines with shifts that are not separeted for n consecutive elements
#   rle_x <- rle_x[c(TRUE, diff(rle_x$values) != 0),]
# 
#   # Remove first line because the first line is either the original state
#   # or the same value as the original state
#   rle_x <- rle_x[-1,]
# 
#   # if there are no rows in rle_x, it means no exposure
#   if(nrow(rle_x) == 0) {
# 
#     exposure <- NA
#     deexposure <- NA
#     duration <- NA
# 
#     return(tibble(species, world_id, exposure, deexposure, duration))
# 
#   }
# 
# 
#   # if the only value in x$values is 0, it means that there was a single exposure event
#   # with no de-exposure
# 
#   if(length(unique(rle_x$values)) == 1){
#     if(unique(rle_x$values) == 0){
# 
#       exposure <- rle_x$year[1]
#       deexposure <- no.deexposure.year
#       duration <- deexposure - exposure
#       return(tibble(species, world_id, exposure, deexposure, duration))
#     }
#   }
# 
#   # the remaining data will always have 0 and 1 on rle_x$values
#   if(length(unique(rle_x$values)) == 2){
# 
#     exposure <- rle_x |>
#       filter(values == 0) |>
#       pull(year)
# 
#     deexposure <- rle_x |>
#       filter(values == 1) |>
#       pull(year)
# 
#     if(length(exposure) > length(deexposure))  deexposure[length(exposure)] <- no.deexposure.year
# 
#     duration <- deexposure - exposure
# 
#     return(tibble(species, world_id, exposure, deexposure, duration))
# 
#   }
# }
# 
# cl <- makeCluster(detectCores() - 1)
# clusterEvalQ(cl, library(dplyr))
# clusterExport(cl, "get_exposure_times")
# 
# thresholds <- "peak"
# 
# for(.group in groups){
#   for(.model in models){
# 
#     substring <- c(.group, .model)
#     readme <- files[sapply(files, function(x) all(str_detect(x, substring)))]
# 
#     raw_results <- readRDS(readme)
#     raw_results <- bind_rows(raw_results)
# 
#     for(.phase in phases){
#       for(.threshold in thresholds){
# 
#         gwl_df <- gwl_times |>
#           filter(model == .model,
#                  threshold == .threshold,
#                  phase == .phase)
# 
#         max_year <- gwl_df$year
# 
#         if(length(max_year) == 0) next
#         if(max_year <= 2018) next # exposure needs 5 consecutive years. so it cannot happen before 2018.
# 
#         no_exposure_threshold <-  length(2015:max_year) - 4
# 
#         raw_results_tmp <- raw_results |>
#           select(c(species, WorldID, as.character(2015:max_year))) |>
#           mutate(sum = rowSums(select(., starts_with("2")))) |>
#           filter(sum < no_exposure_threshold) |>  # select only cells with < 202 suitable years (>= 202 years means no exposure)
#           select(-sum)
# 
# 
#         clusterExport(cl, varlist = "max_year")
# 
#         res_final <- pbapply(X = raw_results_tmp,
#                              MARGIN = 1,
#                              FUN = function(x) get_exposure_times(x = x, original.state = 1, consecutive.elements = 5, no.deexposure.year = max_year + 1),
#                              cl = cl)
# 
# 
#         res_final <- res_final |>
#           bind_rows() |>
#           drop_na() |>
#           mutate(group = .group,
#                  model = .model) |>
#           relocate(group)
# 
#         saveRDS(res_final,
#                 file = here(glue("results/species_exposure_times_per_gwl/threshold_{.group}_{.model}_{.phase}_{.threshold}.rds")))
# 
#         print(glue("{.group}_{.model}_{.phase}_{.threshold}"))
# 
#       }
#     }
#   }
# }
# 
# 
# stopCluster(cl)
