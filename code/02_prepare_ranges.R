source("code/00_packages.R")

print("Running: 02_prepare_ranges.R")

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

shp_to_grid <- function(species_name, shapefiles, r){
  
  spp_range <- shapefiles[which(shapefiles$sci_name == species_name),]
  spp_range <- st_transform(spp_range, crs = crs(r))
  spp_range <- vect(spp_range)
  output <- terra::extract(r, spp_range, touches = TRUE, ID = FALSE, weights = TRUE, exact = TRUE) %>% 
    as_tibble() %>% 
    mutate(range_proportion = weight / sum(weight)) %>% 
    na.omit()
  
  return(output)

}



# create raster template
r_template <- rast()
values(r_template) <- 1:ncell(r_template)
names(r_template) <- "world_id"

sf_use_s2(FALSE)

world <- ne_countries(scale = "large", returnclass = "sf") %>% 
  st_transform(crs = crs(r_template)) %>% 
  vect()

r_template_terrestrial <- r_template %>% 
  crop(world, touches = TRUE, mask = TRUE, snap = "out")

terrestrial_ids <- values(r_template_terrestrial) %>% 
  na.omit() %>% 
  as.integer() %>% 
  unique()


# set number of cores
n_cores <- 7



#####################################################################
############################## RUN ##################################
#####################################################################

# amphibians ----
tic("-- Running time: Amphibians")
ranges <- read_sf(here("raw_data/species_data/range_maps_iucn/AMPHIBIANS/AMPHIBIANS.shp"))
ranges <- prepare_range(ranges)
species <- sort(unique(ranges$sci_name))

spp_data <- pbmclapply(species, shp_to_grid, shapefiles = ranges, r = r_template_terrestrial, mc.cores = n_cores) %>% 
  set_names(species)


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

spp_data <- pbmclapply(species, shp_to_grid, shapefiles = ranges, r = r_template_terrestrial, mc.cores = n_cores) %>% 
  set_names(species)

# write result
saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Mammals.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Mammals.csv"), row.names = FALSE)
toc()

# reptiles ----
tic("-- Running time: Reptiles")
ranges1 <- read_sf(here("raw_data/species_data/range_maps_iucn/REPTILES/REPTILES_PART1.shp"))
ranges2 <- read_sf(here("raw_data/species_data/range_maps_iucn/REPTILES/REPTILES_PART2.shp"))
ranges1 <- dplyr::select(ranges1, -OBJECTID)
ranges <- rbind(ranges1, ranges2)

ranges <- prepare_range(ranges)
species <- sort(unique(ranges$sci_name))

spp_data <- pbmclapply(species, shp_to_grid, shapefiles = ranges, r = r_template_terrestrial, mc.cores = n_cores) %>% 
  set_names(species)

# write result
saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Reptiles.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Reptiles.csv"), row.names = FALSE)
toc()

# birds ----
tic("-- Running time: Birds")
ranges <- st_read(here("raw_data/species_data/range_maps_iucn/BIRDS/BOTW.gdb"), layer = "All_Species")
ranges <- st_cast(ranges, "MULTIPOLYGON")
ranges <- prepare_range(ranges)
species <- sort(unique(ranges$sci_name))

spp_data <- pbmclapply(species, shp_to_grid, shapefiles = ranges, r = r_template_terrestrial, mc.cores = n_cores) %>% 
  set_names(species)

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

spp_data <- pbmclapply(species, shp_to_grid, shapefiles = ranges, r = r_template, mc.cores = n_cores) 
spp_data <- spp_data %>% set_names(species)

# write result
saveRDS(spp_data, here("processed_data/species_data/range_maps_grid_cells/Fishes.rds"))

# write species attribute table
ranges_df <- st_drop_geometry(ranges)
write.csv2(ranges_df, here("processed_data/species_data/attribute_tables/Fishes.csv"), row.names = FALSE)
toc()

print("DONE: 02_prepare_ranges.R")
