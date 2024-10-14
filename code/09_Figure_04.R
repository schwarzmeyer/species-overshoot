files_raw <- list.files(paste0(path, "/Results/Species Exposure Dates"), pattern = "raw", full.names = T)
files_30yr <- list.files(paste0(path, "/Results/Species Exposure Dates"), pattern = "30yr", full.names = T)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")
niche <- list.files(paste0(path, "Data/Species Data/Niche Limits/raw"), full.names = T)


amph <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Amphibians.rds"))
bird <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Birds.rds"))
mamm <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Mammals.rds"))
rept <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Reptiles.rds"))
fish <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Fishes.rds"))

countries <- ne_countries(returnclass = "sf")
countries <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")



# prepare data frames for raw and 30yr results
res_final <- list()
for(i in seq_along(models)){
  
  res_list <- list()
  files_tmp <- grep(models[i], files_raw, value = T)
  
  for(j in seq_along(files_tmp)){
    
    tmp <- readRDS(files_tmp[j])
    tmp$model <- models[i]
    res_list[[j]] <- tmp 
  }
  
  res_final[[i]] <- bind_rows(res_list)
  
}

data_raw <- bind_rows(res_final) 


# join global warming levels


df_gwl <- readRDS(paste0(path, "Data/Climate Data/Global Averages/global_averages_rolling_30yr.rds"))


# get gwl levels for each year
gwl <- df_gwl %>%
  group_by(model) %>%
  mutate(historic_temp = mean(temperature[year <= 1900])) %>% 
  mutate(gwl = temperature - historic_temp) %>% 
  select(-historic_temp, -temperature)

# select the year of peak warming
year_peak <- gwl %>% 
  group_by(model) %>% 
  slice(which.max(gwl)) %>%
  select(-gwl) %>% 
  rename("year_peak" = year)

# select the year of post-OS lowest temp
year_low <- gwl %>% 
  filter(year >= 2100,
         year <= 2200) %>% 
  group_by(model) %>% 
  slice(which.min(gwl)) %>%
  rename("year_low" = year,
         "gwl_low" = gwl)

# select the year OS starts 
year_os <- gwl %>% 
  filter(year < 2100) %>% 
  left_join(year_low, by = "model") %>% 
  filter(gwl <= gwl_low) %>% 
  group_by(model) %>% 
  slice(which.max(gwl)) %>% 
  rename("year_os_starts" = year,
         "gwl_os" = gwl)

# this df will be used to classify each phase of the OS
code_df <- left_join(year_os, year_peak, by = "model") %>% 
  select(-c(gwl_os, gwl_low))


data_raw <- data_raw %>% 
  left_join(code_df, by ="model") %>% 
  mutate(overshoot_phase = case_when(exposure <= year_os_starts ~ "pre_os",
                                     exposure > year_os_starts & exposure <= year_peak ~ "os_warming",
                                     exposure > year_peak & exposure <= year_low ~ "os_cooling",
                                     exposure > year_low ~ "post_os"))

# binding the lowest temp data to the main data
# gwl_diff the difference in temperature at exposure and deexposure
# if the value is negative, deexposure occurred at a lower gwl
data_raw <- data_raw %>% 
  left_join(gwl, by = c("model", "exposure" = "year")) %>% 
  rename("gwl_exposure" = gwl) %>% 
  left_join(gwl, by = c("model", "deexposure" = "year")) %>% 
  rename("gwl_deexposure" = gwl) %>% 
  mutate(gwl_diff = gwl_deexposure - gwl_exposure) 


# now, I classify the exposure events according to the gwl


data_raw <- data_raw %>% 
  mutate(code = case_when(gwl_diff > 0.04 ~ "De-exposure needs less cooling",
                          gwl_diff <= 0.04 & gwl_diff >= -0.04 ~ "De-exposure at the same GWL",
                          gwl_diff < -0.04 ~ "De-exposure needs more cooling")) %>% 
  mutate(code = ifelse(deexposure == 2201, "De-exposure never happened", code))






range_data_land <- c(amph, bird, mamm, rept) 
range_data_ocean <- fish 



length_land <- sapply(range_data_land, function(x) length(x))
length_ocean <- sapply(range_data_ocean, function(x) length(x))

range_data_land <- tibble(species = rep(names(length_land), times = length_land),
                          WorldID = unlist(range_data_land))

range_data_ocean <- tibble(species = rep(names(length_ocean), times = length_ocean),
                           WorldID = unlist(range_data_ocean))

richness_land <- range_data_land %>% 
  count(WorldID)

richness_ocean <- range_data_ocean %>% 
  count(WorldID)



more_cool_land <- data_raw %>% 
  filter(group != "Fishes") %>% 
  filter(code == "De-exposure needs more cooling") %>% 
  group_by(model, world_id) %>% 
  summarise(n = n(),
            tim_exposure = median(exposure),
            tim_deexposure = median(deexposure)) %>% 
  mutate(world_id = as.numeric(world_id)) %>% 
  ungroup() %>% 
  group_by(world_id) %>% 
  summarise(more = median(n),
            tim_exp_more = median(tim_exposure),
            tim_deexp_more = median(tim_deexposure)) %>% 
  left_join(richness_land, by = c("world_id" = "WorldID")) %>% 
  mutate(more_perc = more/n*100)

less_cool_land <- data_raw %>% 
  filter(group != "Fishes") %>% 
  filter(code == "De-exposure needs less cooling") %>% 
  group_by(model, world_id) %>% 
  summarise(n = n(),
            tim_exposure = median(exposure),
            tim_deexposure = median(deexposure)) %>% 
  mutate(world_id = as.numeric(world_id)) %>% 
  ungroup() %>% 
  group_by(world_id) %>% 
  summarise(less = median(n),
            tim_exp_less = median(tim_exposure),
            tim_deexp_less = median(tim_deexposure)) %>% 
  left_join(richness_land, by = c("world_id" = "WorldID")) %>% 
  mutate(less_perc = less/n*100)

more_cool_ocean <- data_raw %>% 
  filter(group == "Fishes") %>% 
  filter(code == "De-exposure needs more cooling") %>% 
  group_by(model, world_id) %>% 
  summarise(n = n(),
            tim_exposure = median(exposure),
            tim_deexposure = median(deexposure)) %>% 
  mutate(world_id = as.numeric(world_id)) %>% 
  ungroup() %>% 
  group_by(world_id) %>% 
  summarise(more = median(n),
            tim_exp_more = median(tim_exposure),
            tim_deexp_more = median(tim_deexposure)) %>% 
  left_join(richness_ocean, by = c("world_id" = "WorldID")) %>% 
  mutate(more_perc = more/n*100)

less_cool_ocean <- data_raw %>% 
  filter(group == "Fishes") %>% 
  filter(code == "De-exposure needs less cooling") %>% 
  group_by(model, world_id) %>% 
  summarise(n = n(),
            tim_exposure = median(exposure),
            tim_deexposure = median(deexposure)) %>% 
  mutate(world_id = as.numeric(world_id)) %>% 
  ungroup() %>% 
  group_by(world_id) %>% 
  summarise(less = median(n),
            tim_exp_less = median(tim_exposure),
            tim_deexp_less = median(tim_deexposure)) %>% 
  left_join(richness_ocean, by = c("world_id" = "WorldID")) %>% 
  mutate(less_perc = less/n*100)


grid_land <- readRDS(paste0(path, "Data/Spatial Data/terrestrial_grid_robin.rds"))
grid_land <- left_join(grid_land, more_cool_land, by = c("WorldID" = "world_id"))
grid_land <- left_join(grid_land, less_cool_land, by = c("WorldID" = "world_id"))

grid_ocean <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_robin.rds"))
grid_ocean <- left_join(grid_ocean, more_cool_ocean, by = c("WorldID" = "world_id"))
grid_ocean <- left_join(grid_ocean, less_cool_ocean, by = c("WorldID" = "world_id"))

bks <- c(0,1,5,10,20,30,40,50,75,100)

more <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey80",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = more_perc, colour = more_perc), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = more_perc, colour = more_perc), show.legend = T) +
  geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.2) +
  scale_fill_viridis_b(option = "A", direction = 1, breaks = bks, name = "De-exposure needs more cooling (% of populations)",
                       end = 0.9, begin = 0.18, na.value = NA, limits = c(0,100)) +
  scale_colour_viridis_b(option = "A", direction = 1, breaks = bks, name = "De-exposure needs more cooling (% of populations)",
                         end = 0.9, begin = 0.18, na.value = NA, limits = c(0,100)) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.6, 'lines')))

less <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey80",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = less_perc, colour = less_perc), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = less_perc, colour = less_perc), show.legend = T) +
  geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.2) +
  scale_fill_viridis_b(option = "A", direction = 1, breaks = bks, name = "De-exposure needs less cooling (% of populations)",
                       end = 0.9, begin = 0.18, na.value = NA, limits = c(0,100)) +
  scale_colour_viridis_b(option = "A", direction = 1, breaks = bks, name = "De-exposure needs less cooling (% of populations)",
                         end = 0.9, begin = 0.18, na.value = NA, limits = c(0,100)) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.6, 'lines')))

bks_tim <- seq(2015,2200,15)

tim_exp_more <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey80",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = tim_exp_more, colour = tim_exp_more), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = tim_exp_more, colour = tim_exp_more), show.legend = T) +
  geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.2) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of Exposure | De-exposure needs more cooling",
                       end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of Exposure | De-exposure needs more cooling",
                         end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.6, 'lines'))); tim_exp_more

tim_exp_less <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey80",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = tim_exp_less, colour = tim_exp_less), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = tim_exp_less, colour = tim_exp_less), show.legend = T) +
  geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.2) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of Exposure | De-exposure needs less cooling",
                       end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of Exposure | De-exposure needs less cooling",
                         end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.6, 'lines')));


tim_deexp_more <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey80",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = tim_deexp_more, colour = tim_deexp_more), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = tim_deexp_more, colour = tim_deexp_more), show.legend = T) +
  geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.2) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of De-exposure | De-exposure needs more cooling",
                       end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of De-exposure | De-exposure needs more cooling",
                         end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.6, 'lines')));

tim_deexp_less <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey80",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = tim_deexp_less, colour = tim_deexp_less), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = tim_deexp_less, colour = tim_deexp_less), show.legend = T) +
  geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.2) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of De-exposure | De-exposure needs less cooling",
                       end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks_tim, name = "Timing of De-exposure | De-exposure needs less cooling",
                         end = 0.9, begin = 0.1, na.value = NA, limits = c(min(bks_tim), max(bks_tim))) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.6, 'lines')));


p <- more + less + 
  tim_exp_more + tim_exp_less +  
  tim_deexp_more + tim_deexp_less +
  plot_layout(ncol = 2, byrow = T)



ggsave(paste0(path, "Figures/_Fig_04.jpg"),
       p,
       width = 24, height = 25, units = "cm", dpi = 700)


grid_ocean <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_robin.rds"))


grid_ocean <- left_join(grid_ocean, data_ocean, by = "WorldID")


library(biscale)

dim <- 3

bks <- c(0,5,15,100)
grid_land_bi <- grid_land %>% 
  mutate(more_perc = ifelse(is.na(more_perc), 0, more_perc),
         less_perc = ifelse(is.na(less_perc), 0, less_perc)) %>% 
  filter(more_perc != 0 & less_perc != 0) %>% 
  mutate(more_perc = cut(more_perc, breaks = bks),
         less_perc = cut(less_perc, breaks = bks)) %>% 
  bi_class(x = more_perc, y = less_perc, dim = dim)

grid_ocean_bi <- grid_ocean %>% 
  mutate(more_perc = ifelse(is.na(more_perc), 0, more_perc),
         less_perc = ifelse(is.na(less_perc), 0, less_perc)) %>% 
  filter(more_perc != 0 & less_perc != 0) %>% 
  mutate(more_perc = cut(more_perc, breaks = bks),
         less_perc = cut(less_perc, breaks = bks)) %>% 
  bi_class(x = more_perc, y = less_perc, dim = dim)

pal <- "DkBlue"


biplot <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey90",  linewidth = 0.2) +
  geom_sf(data = grid_ocean_bi, aes(fill = bi_class, colour = bi_class)) +
  geom_sf(data = grid_land_bi, aes(fill = bi_class, colour = bi_class)) +
  geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.2) +
  bi_scale_fill(pal = pal, dim = dim) +
  bi_scale_color(pal = pal, dim = dim) +
  bi_theme() +
  theme(legend.position = "none")

legend <- bi_legend(pal = pal,
                    dim = dim,
                    x = "More cooling",
                    y = "Less cooling")

ggdraw() +
  draw_plot(biplot) +
  draw_plot(legend, scale = 0.3, x = -0.4, y = -.2)

