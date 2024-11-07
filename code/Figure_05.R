files <- list.files(here("results/species_exposure_times"), full.names = T)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

data_raw <- bind_rows(map(files, readRDS))

# calculate pre-industrial averages for each model
global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))

pre_industrial_avg <- global_temp_avg %>% 
  group_by(model) %>%
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) %>% 
  filter(year == 1850) %>% 
  select(-year, -temperature)

# calculate global warming averages from the 30-yr rolling average data
global_temp_roll <- readRDS(here("processed_data/climate_data/global_averages/global_averages_rolling_30yr.rds"))

gwl <- global_temp_roll %>%
  left_join(pre_industrial_avg, by = "model") %>% 
  mutate(gwl = temperature - pre_industrial_avg) %>% 
  select(-pre_industrial_avg, -temperature)


# binding the lowest temp data to the main data
# gwl_diff the difference in temperature at exposure and deexposure
# if the value is negative, deexposure required further cooling

data <- data_raw %>% 
  left_join(gwl, by = c("model", "exposure" = "year")) %>% 
  rename("gwl_exposure" = gwl) %>% 
  left_join(gwl, by = c("model", "deexposure" = "year")) %>% 
  rename("gwl_deexposure" = gwl) %>% 
  mutate(gwl_diff = gwl_deexposure - gwl_exposure) 


# select the year of peak warming
year_peak_warming <- gwl %>% 
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
year_OS_starts <- gwl %>% 
  filter(year < 2100) %>% 
  left_join(year_low, by = "model") %>% 
  filter(gwl <= gwl_low) %>% 
  group_by(model) %>% 
  slice(which.max(gwl)) %>% 
  rename("year_os_starts" = year,
         "gwl_os" = gwl)



# this df will be used to classify each phase of the OS
code_df <- left_join(year_OS_starts, year_peak_warming, by = "model") %>% 
  select(-c(gwl_os, gwl_low))

# classify exposure events according to the gwl 
data <- data %>% 
  left_join(code_df, by ="model") %>% 
  mutate(overshoot_phase = case_when(exposure < year_os_starts ~ "pre_os",
                                     exposure >= year_os_starts & exposure <= year_peak ~ "warming_phase",
                                     exposure > year_peak & exposure <= year_low ~ "cooling_phase",
                                     exposure > year_low ~ "post_os")) %>% 
  mutate(code = case_when(gwl_diff >= 0.04 ~ "less_cooling",
                          gwl_diff < 0.04 & gwl_diff >= -0.04 ~ "same_gwl",
                          gwl_diff < -0.04 ~ "more_cooling")) %>% 
  mutate(code = ifelse(deexposure == 2201, "never", code))




data_land <- data %>% 
  filter(group != "Fishes") %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  count(model, world_id, code) %>% 
  group_by(world_id, code) %>% 
  summarise(median_n = median(n, na.rm = T), .groups = "drop") %>% 
  filter(median_n >= 5) %>% 
  pivot_wider(names_from = code, values_from = median_n) %>% 
  mutate(world_id = as.numeric(world_id))

data_ocean <- data %>% 
  filter(group == "Fishes") %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  count(model, world_id, code) %>% 
  group_by(world_id, code) %>% 
  summarise(median_n = median(n, na.rm = T), .groups = "drop") %>% 
  filter(median_n >= 5) %>% 
  pivot_wider(names_from = code, values_from = median_n) %>% 
  mutate(world_id = as.numeric(world_id))



# load countries polygon
countries <- ne_countries(returnclass = "sf")
countries <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")


# create bounding box to plot around the maps
bound <- readRDS(here("raw_data/random/bbox.rds"))

grid_land <- readRDS(here("raw_data/spatial_data/terrestrial_grid_robin.rds"))
grid_ocean <- readRDS(here("raw_data/spatial_data/ocean_grid_robin.rds"))

grid_land_join <- left_join(grid_land, data_land, by = c("WorldID" = "world_id"))
grid_ocean_join <- left_join(grid_ocean, data_ocean, by = c("WorldID" = "world_id"))



bks <- c(1,5,10,20,30,50,100,1000)

fill_columns <- c("never", "more_cooling")

p_map <- map(fill_columns, function(x){
  
  bound_colour <- ifelse(x == "never", "#ED2E4C","#366297")
  
  plot_title <- ifelse(x == "never", "No de-exposure","De-exposure needed additional cooling")
  
  ggplot() +
    geom_sf(data = bound, colour = NA, fill = lighten("#221577", 0.45)) +
    geom_sf(data = countries, colour = NA, fill = "#221577",  linewidth = 0.15) +
    geom_sf(data = grid_ocean_join, aes(fill = !!sym(x)), colour = NA, show.legend = T) +
    geom_sf(data = grid_land_join, aes(fill = !!sym(x)), colour = NA, show.legend = T) +
    geom_sf(data = countries, colour = "grey85", fill = NA,  linewidth = 0.15) +
    # geom_sf(data = bound, colour = bound_colour, fill = NA, linewidth = 0.3) +
    # coord_sf(ylim = c(-5500000,8000000)) +
    scale_fill_viridis_b(option = "C", direction = 1, breaks = bks, name = "N. of populations",
                         end = 1, begin = 0.25, na.value = NA, limits = range(bks),
                         labels = c("", bks[2:(length(bks)-1)], "")) +
    labs(subtitle = plot_title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = "center", 
          plot.margin = margin(0,0,0,0),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          plot.subtitle = element_text(size = 8, hjust = 0.5, vjust = 1)) +
    guides(fill = guide_colorsteps(title.position = "top",
                                   title.hjust = .5, 
                                   barwidth = unit(11, "lines"), 
                                   barheight = unit(.3, "lines")))
  
  
  
})



p <- wrap_plots(p_map) + 
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = 2, size = 9))

ggsave(here("figures/Fig_05.jpg"),
       p,
       width = 18, height = 7, units = "cm", dpi = 700)



