# load grid
terrestrial_grid <- readRDS(paste0(path, "Data/Spatial Data/terrestrial_grid_equal_area.rds"))
ocean_grid <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_equal_area.rds"))

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




# amphibians ----
ranges <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/AMPHIBIANS/AMPHIBIANS.shp"))

# run
res <- prepare_range(ranges, terrestrial_grid, realm = "terrestrial")


# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Amphibians.rds"))
 
# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Amphibians.csv"), row.names = F)





# mammals ----
ranges <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp"))

# run
res <- prepare_range(ranges, terrestrial_grid, realm = "terrestrial")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Mammals.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Mammals.csv"), row.names = F)





# reptiles ----
ranges1 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/REPTILES/REPTILES_PART1.shp"))
ranges2 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/REPTILES/REPTILES_PART2.shp"))
ranges1 <- dplyr::select(ranges1, -OBJECTID)
ranges <- rbind(ranges1, ranges2)

# run
res <- prepare_range(ranges, terrestrial_grid, realm = "terrestrial")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Reptiles.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Reptiles.csv"), row.names = F)





# birds ----
ranges <- st_read(paste0(path, "Data/Species Data/Range Maps IUCN/BIRDS/BOTW.gdb"), layer = "All_Species")

# run (spliting the data into four parts to avoid filling the RAM)
birds <- unique(ranges$sci_name)
res1 <- prepare_range(ranges[ranges$sci_name %in% birds[1:2000],], terrestrial_grid, realm = "")
res2 <- prepare_range(ranges[ranges$sci_name %in% birds[2001:5000],], terrestrial_grid, realm = "")
res3 <- prepare_range(ranges[ranges$sci_name %in% birds[5001:8000],], terrestrial_grid, realm = "")
res4 <- prepare_range(ranges[ranges$sci_name %in% birds[8001:length(birds)],], terrestrial_grid, realm = "")

res <- c(res1, res2, res3, res4)

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Birds.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Birds.csv"), row.names = F)




# abalones ----
ranges <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/ABALONES/ABALONES.shp"))

# run
res <- prepare_range(ranges, ocean_grid, realm = "marine")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Abalones.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Abalones.csv"), row.names = F)





# conesnails ----
ranges <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/CONESNAILS/CONESNAILS.shp"))

# run
res <- prepare_range(ranges, ocean_grid, realm = "marine")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Conesnails.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Conesnails.csv"), row.names = F)




# lobsters ----
ranges <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/LOBSTERS/LOBSTERS.shp"))

# run
res <- prepare_range(ranges, ocean_grid, realm = "marine")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Lobsters.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Lobsters.csv"), row.names = F)





# mangrooves ----
ranges <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/MANGROVES/MANGROVES.shp"))

# run
res <- prepare_range(ranges, ocean_grid, realm = "marine")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Mangroves.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Mangroves.csv"), row.names = F)





# fishes ---- 
ranges1 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/MARINEFISH/MARINEFISH_PART1.shp"))
ranges2 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/MARINEFISH/MARINEFISH_PART2.shp"))
ranges3 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/MARINEFISH/MARINEFISH_PART3.shp"))
ranges <- rbind(ranges1, ranges2, ranges3)

# run (spliting the data into three parts to avoid filling the RAM)
res1 <- prepare_range(ranges1, ocean_grid, realm = "marine")
res2 <- prepare_range(ranges2, ocean_grid, realm = "marine")
res3 <- prepare_range(ranges3, ocean_grid, realm = "marine")

res <- c(res1, res2, res3)

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Fishes.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Fishes.csv"), row.names = F)





# reefs ----
ranges1 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/REEF_FORMING_CORALS/REEF_FORMING_CORALS_PART1.shp"))
ranges2 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/REEF_FORMING_CORALS/REEF_FORMING_CORALS_PART2.shp"))
ranges3 <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/REEF_FORMING_CORALS/REEF_FORMING_CORALS_PART3.shp"))
ranges <- rbind(ranges1, ranges2, ranges3)

# run
res <- prepare_range(ranges, ocean_grid, realm = "marine")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Reefs.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Reefs.csv"), row.names = F)





# seagrass ----
ranges <- read_sf(paste0(path, "Data/Species Data/Range Maps IUCN/SEAGRASSES/SEAGRASSES.shp"))

# run
res <- prepare_range(ranges, ocean_grid, realm = "marine")

# write result
saveRDS(res, paste0(path, "Data/Species Data/Range Maps Grid Cells/Seagrasses.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, paste0(path, "Data/Species Data/Attribute Tables/Seagrasses.csv"), row.names = F)


