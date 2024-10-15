# load grid
terrestrial_grid <- readRDS(here("raw_data/spatial_data/terrestrial_grid_equal_area.rds"))
ocean_grid <- readRDS(here("raw_data/spatial_data/ocean_grid_equal_area.rds"))

# this function intersects IUCN shapefiles with our 100 km grid,
# returning a list where each element is a species, and the values indicate 
# the grid cells that intersected with the grid.
prepare_range <- function(range_data, grid, realm){
  
  
  # filter presence (extant), origin (native and reintroduced), and seasonal (resident and breeding)
  range_filtered <- range_data %>%
    dplyr::filter(presence == 1,
                  origin %in% c(1,2),
                  seasonal %in% c(1,2))
  
  if(realm == "terrestrial"){

    range_filtered <- range_filtered %>% 
      dplyr::filter(terrestial == "true")
    
  }
  
  if(realm == "marine"){
    
    range_filtered <- range_filtered %>% 
      dplyr::filter(marine == "true")
    
  }
  
  
  plan("multisession", workers = availableCores() - 1)
  
  res <- future_map(st_geometry(range_filtered), possibly(function(x){
    y <- st_intersects(x, grid)
    y <- unlist(y)
    y <- grid %>% 
      slice(y) %>% 
      pull(WorldID)
    y
    
  }, quiet = T), .progress = TRUE)
  
  
  names(res) <- range_filtered$sci_name
  
  res <- discard(res, is.null)
  
  # combining elements with same name
  res_final <- tapply(unlist(res, use.names = FALSE), rep(names(res), lengths(res)), FUN = c)
  
  return(res_final)
  
}


# increase max memory
options(future.globals.maxSize = 9 * 1024^3)


# amphibians ----
ranges <- read_sf(here("raw_data/species_data/range_maps_iucn/AMPHIBIANS/AMPHIBIANS.shp"))

# run
res <- prepare_range(ranges, terrestrial_grid, realm = "terrestrial")


# write result
saveRDS(res, here("processed_data/species_data/range_maps_grid_cells/Amphibians.rds"))
 
# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Amphibians.csv"), row.names = F)



# mammals ----
ranges <- read_sf(here("raw_data/species_data/range_maps_iucn/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp"))

# run
res <- prepare_range(ranges, terrestrial_grid, realm = "terrestrial")

# write result
saveRDS(res, here("processed_data/species_data/range_maps_grid_cells/Mammals.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Mammals.csv"), row.names = F)





# reptiles ----
ranges1 <- read_sf(here("raw_data/species_data/range_maps_iucn/REPTILES/REPTILES_PART1.shp"))
ranges2 <- read_sf(here("raw_data/species_data/range_maps_iucn/REPTILES/REPTILES_PART2.shp"))
ranges1 <- dplyr::select(ranges1, -OBJECTID)
ranges <- rbind(ranges1, ranges2)

# run
res <- prepare_range(ranges, terrestrial_grid, realm = "terrestrial")

# write result
saveRDS(res, here("processed_data/species_data/range_maps_grid_cells/Reptiles.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Reptiles.csv"), row.names = F)





# birds ----
ranges <- st_read(here("raw_data/species_data/range_maps_iucn/BIRDS/BOTW.gdb"), layer = "All_Species")

# run (spliting the data into four parts to avoid filling the RAM)
birds <- unique(ranges$sci_name)
res1 <- prepare_range(ranges[ranges$sci_name %in% birds[1:2000],], terrestrial_grid, realm = "")
res2 <- prepare_range(ranges[ranges$sci_name %in% birds[2001:5000],], terrestrial_grid, realm = "")
res3 <- prepare_range(ranges[ranges$sci_name %in% birds[5001:8000],], terrestrial_grid, realm = "")
res4 <- prepare_range(ranges[ranges$sci_name %in% birds[8001:length(birds)],], terrestrial_grid, realm = "")

res <- c(res1, res2, res3, res4)

# write result
saveRDS(res, here("processed_data/species_data/range_maps_grid_cells/Birds.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Birds.csv"), row.names = F)



# fishes ---- 
ranges1 <- read_sf(here("raw_data/species_data/range_maps_iucn/MARINEFISH/MARINEFISH_PART1.shp"))
ranges2 <- read_sf(here("raw_data/species_data/range_maps_iucn/MARINEFISH/MARINEFISH_PART2.shp"))
ranges3 <- read_sf(here("raw_data/species_data/range_maps_iucn/MARINEFISH/MARINEFISH_PART3.shp"))
ranges <- rbind(ranges1, ranges2, ranges3)

# run (spliting the data into three parts to avoid filling the RAM)
res1 <- prepare_range(ranges1, ocean_grid, realm = "marine")
res2 <- prepare_range(ranges2, ocean_grid, realm = "marine")
res3 <- prepare_range(ranges3, ocean_grid, realm = "marine")

res <- c(res1, res2, res3)

# write result
saveRDS(res, here("processed_data/species_data/range_maps_grid_cells/Fishes.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Fishes.csv"), row.names = F)


