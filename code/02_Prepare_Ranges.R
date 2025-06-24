source("code/00_packages.R")

print("Running: 02_Prepare_Ranges.R")

# script to transform IUCN ranges to the gridded format.

#################################################################


# this script transforms IUCN shapefiles into gridded format, 
# where each species is represented by a list of grid cells that intersect with the species range.

# function to filter ranges
prepare_range <- function(range_data, realm = "terrestrial"){
  
  
  # filter presence (extant), origin (native and reintroduced), and seasonal (resident and breeding)
  range_filtered <- range_data %>%
    dplyr::filter(presence == 1,
                  origin %in% c(1,2),
                  seasonal %in% c(1,2))
  
  if(realm == "terrestrial" & "terrestial" %in% names(range_data)){
    
    range_filtered <- range_filtered %>% 
      dplyr::filter(terrestial == "true")
    
  }
  
  if(realm == "marine"){
    
    range_filtered <- range_filtered %>% 
      dplyr::filter(marine == "true")
    
  }
  
  return(range_filtered)
}

# function to transform shp into gridded format

shp_to_grid <- function(raster_template, shapefiles, r){
  
  result <- exact_extract(raster_template, shapefiles, include_cols = "sci_name")
  
  result %>% 
    bind_rows() %>% 
    as_tibble() %>% 
    rename(world_id = value,
           species = sci_name) %>% 
    mutate(world_id = as.integer(world_id)) %>% 
    group_by(species) %>% 
    arrange(species) %>% 
    mutate(weigth = sum(coverage_fraction)) %>% 
    mutate(range_proportion = coverage_fraction / weigth) %>% 
    ungroup() %>% 
    select(species, world_id, coverage_fraction, range_proportion) %>% 
    mutate(species = factor(species))
  
}



# create raster template
r_template <- rast()
values(r_template) <- 1:ncell(r_template)

sf_use_s2(FALSE)

world <- ne_countries(scale = "large", returnclass = "sf") %>% 
  st_transform(crs = crs(r_template)) %>% 
  vect()

r_template_terrestrial <- r_template %>% 
  crop(world, touches = TRUE, mask = TRUE, snap = "out")


#####################################################################
############################## RUN ##################################
#####################################################################

# amphibians ----
tic("-- Running time: Amphibians")
ranges <- read_sf(here("raw_data/species_data/range_maps_iucn/AMPHIBIANS/AMPHIBIANS.shp"))
ranges <- prepare_range(ranges)
species <- sort(unique(ranges$sci_name))

spp_data <- shp_to_grid(r_template_terrestrial, ranges)

# write result
saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Amphibians.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Amphibians.csv"), row.names = FALSE)
toc()

# mammals ----
tic("-- Running time: Mammals")
ranges <- read_sf(here("raw_data/species_data/range_maps_iucn/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp"))
ranges <- prepare_range(ranges)
species <- sort(unique(ranges$sci_name))

spp_data <- shp_to_grid(r_template_terrestrial, ranges)

# write result
saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Mammals.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Mammals.csv"), row.names = FALSE)
toc()

# reptiles ----
tic("-- Running time: Reptiles")
ranges_1 <- read_sf(here("raw_data/species_data/range_maps_iucn/REPTILES/REPTILES_PART1.shp"))
ranges_2 <- read_sf(here("raw_data/species_data/range_maps_iucn/REPTILES/REPTILES_PART2.shp"))
ranges_1 <- dplyr::select(ranges_1, -OBJECTID)
ranges <- rbind(ranges_1, ranges_2)

ranges <- prepare_range(ranges)
species <- sort(unique(ranges$sci_name))

spp_data <- shp_to_grid(r_template_terrestrial, ranges)

# write result
saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Reptiles.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Reptiles.csv"), row.names = FALSE)
toc()

# birds ----
tic("-- Running time: Birds")
# ranges <- st_read(here("raw_data/species_data/range_maps_iucn/BIRDS/BOTW.gdb"), layer = "All_Species")
ranges <- st_read(here("raw_data/species_data/range_maps_iucn/BIRDS/BOTW.gdb"), 
                  query = "SELECT * 
                        FROM \"All_Species\" 
                        WHERE 
                            \"seasonal\" IN (1, 2) AND 
                            \"presence\" = 1 AND 
                            \"origin\" IN (1, 2)")


ranges <- st_cast(ranges, "MULTIPOLYGON")
ranges <- prepare_range(ranges)

spp_data <- shp_to_grid(r_template_terrestrial, ranges)

saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Birds.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Birds.csv"), row.names = FALSE)
print("Birds done")
toc()

# fishes ---- 
tic("-- Running time: Fishes")
ranges1 <- read_sf(here("raw_data/species_data/range_maps_iucn/MARINEFISH/MARINEFISH_PART1.shp"))
ranges2 <- read_sf(here("raw_data/species_data/range_maps_iucn/MARINEFISH/MARINEFISH_PART2.shp"))
ranges3 <- read_sf(here("raw_data/species_data/range_maps_iucn/MARINEFISH/MARINEFISH_PART3.shp"))
ranges <- rbind(ranges1, ranges2, ranges3)

ranges <- prepare_range(ranges, realm = "marine")
species <- sort(unique(ranges$sci_name))

spp_data <- shp_to_grid(r_template, ranges)

# write result
saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Fishes.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Fishes.csv"), row.names = FALSE)
toc()

print("DONE: 02_Prepare_Ranges.R")
