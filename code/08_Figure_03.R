files <- list.files(paste0(path, "/Results/Species Exposure Dates"), pattern = "raw", full.names = T)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

# prepare data frames 
res_final <- list()
for(i in seq_along(models)){
  
  res_list <- list()
  files_tmp <- grep(models[i], files, value = T)
  
  for(j in seq_along(files_tmp)){
    
    tmp <- readRDS(files_tmp[j])
    tmp$model <- models[i]
    res_list[[j]] <- tmp 
  }
  
  res_final[[i]] <- bind_rows(res_list)
  
}

data_raw <- bind_rows(res_final) 

# calculate pre-industrial averages for each model
global_temp_avg <- readRDS(paste0(path, "Data/Climate Data/Global Averages/global_averages.rds"))

pre_industrial_avg <- global_temp_avg %>% 
  group_by(model) %>%
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) %>% 
  filter(year == 1850) %>% 
  select(-year, -temperature)
  
# calculate global warming averages from the 30-yr rolling average data
global_temp_roll <- readRDS(paste0(path, "Data/Climate Data/Global Averages/global_averages_rolling_30yr.rds"))

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

data %>% 
  pivot_longer(cols = c(gwl_exposure, gwl_deexposure)) %>% 
  ggplot() +
  geom_boxplot(aes(x = name, y = value), coef = 5) +
  facet_wrap(~ model)

data %>% 
  group_by(model) %>% 
  summarise(median_exp = median(gwl_exposure),
            median_dexp = median(gwl_deexposure),
            median_diff = median(gwl_diff)) 



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

###########################################
# Figure S1

gwl_s1 <- gwl %>% 
  filter(year >= 2015,
         year <= 2200) %>% 
  rename("Global Warming Level (°C)" = gwl,
         "Year" = year)
  
s1_cnrm <- gwl_s1 %>% 
  filter(model == "CNRM-ESM2-1") %>% 
  ggplot(aes(x = Year, y = `Global Warming Level (°C)`)) +
  annotate("rect", 
           xmin = code_df$year_os_starts[1],
           xmax = code_df$year_low[1],
           ymin = -Inf, ymax = Inf,
           fill = "firebrick1", alpha = 0.4
           ) +
  geom_line() +
  labs(subtitle = "CNRM-ESM2-1", y = "") +
  theme_bw(); s1_cnrm

s1_can <- gwl_s1 %>% 
  filter(model == "CanESM5") %>% 
  ggplot(aes(x = Year, y = `Global Warming Level (°C)`)) +
  annotate("rect", 
           xmin = code_df$year_os_starts[2],
           xmax = code_df$year_low[2],
           ymin = -Inf, ymax = Inf,
           fill = "firebrick1", alpha = 0.4
  ) +
  geom_line() +
  labs(subtitle = "CanESM5") +
  theme_bw(); s1_can

s1_giss <- gwl_s1 %>% 
  filter(model == "GISS-E2-1-G") %>% 
  ggplot(aes(x = Year, y = `Global Warming Level (°C)`)) +
  annotate("rect", 
           xmin = code_df$year_os_starts[3],
           xmax = code_df$year_low[3],
           ymin = -Inf, ymax = Inf,
           fill = "firebrick1", alpha = 0.4
  ) +
  geom_line() +
  labs(subtitle = "GISS-E2-1-G", y = "") +
  theme_bw(); s1_giss

s1_ipsl <- gwl_s1 %>% 
  filter(model == "IPSL-CM6A-LR") %>% 
  ggplot(aes(x = Year, y = `Global Warming Level (°C)`)) +
  annotate("rect", 
           xmin = code_df$year_os_starts[4],
           xmax = code_df$year_low[4],
           ymin = -Inf, ymax = Inf,
           fill = "firebrick1", alpha = 0.4
  ) +
  geom_line() +
  labs(subtitle = "IPSL-CM6A-LR") +
  theme_bw(); s1_ipsl

s1_mri <- gwl_s1 %>% 
  filter(model == "MRI-ESM2-0") %>% 
  ggplot(aes(x = Year, y = `Global Warming Level (°C)`)) +
  annotate("rect", 
           xmin = code_df$year_os_starts[5],
           xmax = code_df$year_low[5],
           ymin = -Inf, ymax = Inf,
           fill = "firebrick1", alpha = 0.4
  ) +
  geom_line() +
  labs(subtitle = "MRI-ESM2-0", y = "") +
  theme_bw(); s1_mri


p_s1 <- (s1_can | s1_cnrm | s1_giss) /
  ((plot_spacer() + s1_ipsl + s1_mri + plot_spacer()) +
  plot_layout(widths = c(1,2,2,1))); p_s1

ggsave(paste0(path, "Figures/_Fig_S1.jpg"),
       p_s1,
       width = 21, height = 12, units = "cm", dpi = 700)



data <- data %>% 
  left_join(code_df, by ="model") %>% 
  mutate(overshoot_phase = case_when(exposure < year_os_starts ~ "pre_os",
                                     exposure >= year_os_starts & exposure <= year_peak ~ "warming_phase",
                                     exposure > year_peak & exposure <= year_low ~ "cooling_phase",
                                     exposure > year_low ~ "post_os"))




# classifying exposure events according to the gwl at de-exposure
data <- data %>% 
  mutate(code = case_when(gwl_diff >= 0.04 ~ "less_cooling",
                          gwl_diff < 0.04 & gwl_diff >= -0.04 ~ "same_gwl",
                          gwl_diff < -0.04 ~ "more_cooling")) %>% 
  mutate(code = ifelse(deexposure == 2201, "never", code))

  
#
p1 <- data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  group_by(model) %>% 
  mutate(code2 = case_when(code == "less_cooling" ~ "Additional cooling is not needed",
                           code == "same_gwl" ~ "Additional cooling is not needed",
                           code == "more_cooling" ~ "Additional cooling is needed",
                           code == "never" ~ "De-exposure never happened")) %>% 
  count(code2) %>% 
  mutate(perc = round(n/sum(n)*100, 0)) %>% 
  mutate(code2 = fct_reorder(code2, perc)) %>%
  ggplot(aes(x = perc, y = model, fill = (code2))) +
  geom_col() +
  labs(x = "Percentage (%)", y = "") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_viridis_d(option = "G", begin = 0.45, end = 0.9, name = "") +
  geom_text(aes(label = perc),
            position = position_stack(vjust = 0.5), colour = "black", fontface = 2, size = 3, family = "Tahoma") +
  theme_tidybayes() +
  theme(legend.position = "top",
        legend.direction = "vertical",
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9)); p1



p2 <- data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase"),
         code == "more_cooling") %>% 
  ggplot(aes(y = model, x = gwl_diff, fill = "", colour = "")) +
  ggdist::stat_halfeye(
    adjust = 1, 
    width = .6, 
    .width = 0, 
    justification = -.3,
    point_colour = NA,
    show.legend = F) + 
  geom_boxplot(
    width = .3, 
    outlier.shape = NA,
    show.legend = F,
    alpha = 0.4,
    linewidth = 0.5
  ) +
  labs(x = "Additional cooling needed (°C)", y = "") +
  scale_fill_manual(values = "#3aabac") +
  scale_colour_manual(values = "#3aabac") +
  scale_x_reverse(limits = c(0, -0.8), expand = c(0,0)) +
  theme_tidybayes() +
  theme(text = element_text(family = "Tahoma"),
        axis.text = element_text(size = 9, family = "Tahoma"),
        panel.grid.major = element_line(linewidth = 0.3),
        panel.grid.minor = element_line(linewidth = 0.3),
        axis.line.y = element_blank()); p2




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
bound <- st_sf(geometry = st_sfc(
  st_polygon(x = list(cbind(c(-180, rep(180, 100), rep(-180, 100)),
                            c(-90, seq(-90, 90, length = 100), 
                              seq(90, -90, length = 100))))), crs = 'WGS84')) %>% 
  st_transform("+proj=robin")

grid_land <- readRDS(paste0(path, "Data/Spatial Data/terrestrial_grid_robin.rds"))
grid_ocean <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_robin.rds"))


grid_land_join <- left_join(grid_land, data_land, by = c("WorldID" = "world_id"))
grid_ocean_join <- left_join(grid_ocean, data_ocean, by = c("WorldID" = "world_id"))

bks <- c(5,10,20,40,60,80,100,125,150,1500)

my_theme <-     theme(legend.position = "bottom",
                      legend.direction = "horizontal",
                      plot.margin = margin(t=0,0,0,0),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 8),
                      plot.title = element_text(size = 9, hjust = 0.5),
                      plot.subtitle = element_text(size = 7)) 

my_guide <-     guides(fill = guide_colorsteps(title.position = 'top',
                                               title.hjust = .5, 
                                               barwidth = unit(9, 'lines'), barheight = unit(.4, 'lines')))

viridis_begin <- 0.1

map_less <- ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey91") +
  geom_sf(data = countries, colour = NA, fill = "grey78",  linewidth = 0.15) +
  geom_sf(data = grid_ocean_join, aes(fill = less_cooling, colour = less_cooling), show.legend = T) +
  geom_sf(data = grid_land_join, aes(fill = less_cooling, colour = less_cooling), show.legend = T) +
  geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.15) +
  geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                       end = 0.98, begin = viridis_begin, na.value = NA) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = viridis_begin, na.value = NA) +
  labs(title = "Less") +
  theme_map() +
  my_theme +
  my_guide; 


map_more <- ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey91") +
  geom_sf(data = countries, colour = NA, fill = "grey78",  linewidth = 0.15) +
  geom_sf(data = grid_ocean_join, aes(fill = more_cooling, colour = more_cooling), show.legend = T) +
  geom_sf(data = grid_land_join, aes(fill = more_cooling, colour = more_cooling), show.legend = T) +
  geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.15) +
  geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                       end = 0.98, begin = viridis_begin, na.value = NA) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = viridis_begin, na.value = NA) +
  labs(title = "more") +
  theme_map() +
  my_theme +
  my_guide


map_never <- ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey91") +
  geom_sf(data = countries, colour = NA, fill = "grey78",  linewidth = 0.15) +
  geom_sf(data = grid_ocean_join, aes(fill = never, colour = never), show.legend = T) +
  geom_sf(data = grid_land_join, aes(fill = never, colour = never), show.legend = T) +
  geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.15) +
  geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                       end = 0.98, begin = viridis_begin, na.value = NA) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = viridis_begin, na.value = NA) +
  # coord_sf(ylim = c(-5500000,8000000)) +
  labs(title = "no") +
  theme_map() +
  my_theme +
  my_guide


px <- (p1 / p2) | (map_less / map_more / map_never) +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom") &
  guides(fill = guide_colorsteps(title.position = "top",
                                 title.hjust = 0.5,
                                 barwidth = unit(10, 'lines'), barheight = unit(.4, 'lines')))
pp <- px + plot_layout(widths = c(1,1.5))

ggsave(paste0(path, "Figures/_Fig_03.jpg"),
       pp,
       width = 21, height = 23, units = "cm", dpi = 500)


######### 
# map individual models


# load countries polygon
countries <- ne_countries(returnclass = "sf")
countries <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")


# create bounding box to plot around the maps
bound <- st_sf(geometry = st_sfc(
  st_polygon(x = list(cbind(c(-180, rep(180, 100), rep(-180, 100)),
                            c(-90, seq(-90, 90, length = 100), 
                              seq(90, -90, length = 100))))), crs = 'WGS84')) %>% 
  st_transform("+proj=robin")

grid_land <- readRDS(paste0(path, "Data/Spatial Data/terrestrial_grid_robin.rds"))
grid_ocean <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_robin.rds"))

plots <- lapply(models, function(x){
  
  data_land <- data %>% 
    filter(group != "Fishes") %>% 
    filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
    count(model, world_id, code) %>% 
    filter(model == x) %>% 
    group_by(world_id, code) %>% 
    summarise(median_n = median(n), .groups = "drop") %>% 
    filter(median_n >= 5) %>% 
    pivot_wider(names_from = code, values_from = median_n) %>% 
    mutate(world_id = as.numeric(world_id))
  
  data_ocean <- data %>% 
    filter(group == "Fishes") %>% 
    filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
    count(model, world_id, code) %>% 
    filter(model == x) %>% 
    group_by(world_id, code) %>% 
    summarise(median_n = median(n), .groups = "drop") %>% 
    filter(median_n >= 5) %>% 
    pivot_wider(names_from = code, values_from = median_n) %>% 
    mutate(world_id = as.numeric(world_id))
  
  
  grid_land_join <- left_join(grid_land, data_land, by = c("WorldID" = "world_id"))
  grid_ocean_join <- left_join(grid_ocean, data_ocean, by = c("WorldID" = "world_id"))
  
  bks <- c(5,10,20,40,60,80,100,125,150,1500)
  
  my_theme <-     theme(legend.position = "top",
                        legend.direction = "horizontal",
                        plot.margin = margin(t=0,0,0,0),
                        legend.title = element_text(size = 10),
                        legend.text = element_text(size = 8),
                        plot.title = element_text(size = 9, hjust = 0.5),
                        plot.subtitle = element_text(size = 7)) 
  
  my_guide <-     guides(fill = guide_colorsteps(title.position = 'top',
                                                 title.hjust = .5, 
                                                 barwidth = unit(9, 'lines'), barheight = unit(.4, 'lines')))
  
  viridis_begin <- 0.1
  if(x == models[1]) my_title <- c("De-exposure needs LESS cooling", "De-exposure needs MORE cooling", "No de-exposure") else my_title <- c("","","")
  
  map_less <- ggplot() +
    geom_sf(data = bound, colour = NA, fill = "grey91") +
    geom_sf(data = countries, colour = NA, fill = "grey78",  linewidth = 0.15) +
    geom_sf(data = grid_ocean_join, aes(fill = less_cooling, colour = less_cooling), show.legend = T) +
    geom_sf(data = grid_land_join, aes(fill = less_cooling, colour = less_cooling), show.legend = T) +
    geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.15) +
    geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
    scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = viridis_begin, na.value = NA) +
    scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                           end = 0.98, begin = viridis_begin, na.value = NA) +
    # coord_sf(ylim = c(-5500000,8000000)) +
    labs(title = my_title[1]) +
    theme_map() +
    annotate("text", label = x, x = -2*(10^7), y = 0, angle = 90, size = 3.2) +
    my_theme +
    theme(plot.margin = margin(l = 20)) +
    my_guide; 

  
  map_more <- ggplot() +
    geom_sf(data = bound, colour = NA, fill = "grey91") +
    geom_sf(data = countries, colour = NA, fill = "grey78",  linewidth = 0.15) +
    geom_sf(data = grid_ocean_join, aes(fill = more_cooling, colour = more_cooling), show.legend = T) +
    geom_sf(data = grid_land_join, aes(fill = more_cooling, colour = more_cooling), show.legend = T) +
    geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.15) +
    geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
    scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = viridis_begin, na.value = NA) +
    scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                           end = 0.98, begin = viridis_begin, na.value = NA) +
    # coord_sf(ylim = c(-5500000,8000000)) +
    labs(title = my_title[2]) +
    theme_map() +
    my_theme +
    my_guide
  
  
  map_never <- ggplot() +
    geom_sf(data = bound, colour = NA, fill = "grey91") +
    geom_sf(data = countries, colour = NA, fill = "grey78",  linewidth = 0.15) +
    geom_sf(data = grid_ocean_join, aes(fill = never, colour = never), show.legend = T) +
    geom_sf(data = grid_land_join, aes(fill = never, colour = never), show.legend = T) +
    geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.15) +
    geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
    scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = viridis_begin, na.value = NA) +
    scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                           end = 0.98, begin = viridis_begin, na.value = NA) +
    # coord_sf(ylim = c(-5500000,8000000)) +
    labs(title = my_title[3]) +
    theme_map() +
    my_theme +
    my_guide
  
  return(list(map_less, map_more, map_never))
  
})


test <- unlist(plots, recursive = F)

p <- wrap_plots(test) + plot_layout(ncol = 3, nrow = 5, byrow = T, guides = "collect") &
  theme(legend.position = "bottom") &
  guides(fill = guide_colorsteps(title.position = "top",
                                 title.hjust = 0.5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.4, 'lines')))

ggsave(paste0(path, "Figures/_test.jpg"),
       p,
       width = 21, height = 23, units = "cm", dpi = 500)



data_land <- data %>% 
  filter(group != "Fishes") %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  count(model, world_id, code) %>% 
  filter(model == "GISS-E2-1-G") %>% 
  group_by(world_id, code) %>% 
  summarise(median_n = median(n), .groups = "drop") %>% 
  filter(median_n >= 5) %>% 
  pivot_wider(names_from = code, values_from = median_n) %>% 
  mutate(world_id = as.numeric(world_id))

data_ocean <- data %>% 
  filter(group == "Fishes") %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  count(model, world_id, code) %>% 
  filter(model == "GISS-E2-1-G") %>% 
  group_by(world_id, code) %>% 
  summarise(median_n = median(n), .groups = "drop") %>% 
  filter(median_n >= 5) %>% 
  pivot_wider(names_from = code, values_from = median_n) %>% 
  mutate(world_id = as.numeric(world_id))


grid_land_join <- left_join(grid_land, data_land, by = c("WorldID" = "world_id"))
grid_ocean_join <- left_join(grid_ocean, data_ocean, by = c("WorldID" = "world_id"))



bks <- c(5,10,20,50,75,100,150,200,1500)

ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey91") +
  geom_sf(data = countries, colour = NA, fill = "grey73",  linewidth = 0.2) +
  geom_sf(data = grid_ocean_join, aes(fill = less_cooling, colour = less_cooling), show.legend = T) +
  geom_sf(data = grid_land_join, aes(fill = less_cooling, colour = less_cooling), show.legend = T) +
  geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.3) +
  geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                       end = 0.98, begin = 0.27, na.value = NA) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = 0.27, na.value = NA) +
  # coord_sf(ylim = c(-5500000,8000000)) +
  labs(title = "De-exposure needs less cooling") +
  theme_map() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        plot.margin = margin(t=20,0,0,0),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5, 
                                 barwidth = unit(18, 'lines'), barheight = unit(.65, 'lines')))

ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey91") +
  geom_sf(data = countries_join, colour = NA, fill = "grey73",  linewidth = 0.2) +
  geom_sf(data = grid_ocean_join, aes(fill = more_cooling, colour = more_cooling), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = more_cooling, colour = more_cooling), show.legend = T) +
  geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.3) +
  geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                       end = 0.98, begin = 0.27, na.value = NA) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = 0.27, na.value = NA) +
  # coord_sf(ylim = c(-5500000,8000000)) +
  labs(title = "De-exposure needs more cooling") +
  theme_map() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        plot.margin = margin(t=20,0,0,0),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5, 
                                 barwidth = unit(18, 'lines'), barheight = unit(.65, 'lines')))


ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey91") +
  geom_sf(data = countries, colour = NA, fill = "grey73",  linewidth = 0.2) +
  geom_sf(data = grid_ocean_join, aes(fill = never, colour = never), show.legend = T) +
  geom_sf(data = grid_land_join, aes(fill = never, colour = never), show.legend = T) +
  geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.3) +
  geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                       end = 0.98, begin = 0.27, na.value = NA) +
  scale_colour_viridis_b(option = "H", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.98, begin = 0.27, na.value = NA) +
  # coord_sf(ylim = c(-5500000,8000000)) +
  labs(title = "No de-expossure") +
  theme_map() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        plot.margin = margin(t=20,0,0,0),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5, 
                                 barwidth = unit(18, 'lines'), barheight = unit(.65, 'lines')))





p1 + p2 + plot_layout(ncol = 2)

ggsave(paste0(path, "Figures/_Fig_04_rescue.jpg"),
       width = 20, height = 8, units = "cm", dpi = 700)




count_exposure <- data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>%
  count(model, exposure, name = "n_exp") 

count_deexposure <- data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>%
  count(model, deexposure, name = "n_deexp") 


count_exposure %>% 
  left_join(count_deexposure, by = c("model", "exposure" = "deexposure")) %>% 
  mutate(n_deexp = replace_na(n_deexp, 0)) %>% 
  arrange(model, exposure) %>% 
  mutate(n_pop_cumsum = cumsum(n_exp - n_deexp)) %>% 
  left_join(gwl, by = c("model", "exposure" = "year")) %>% 
  ggplot(aes(x = gwl, y = n_pop_cumsum, colour = exposure)) +
  geom_point(size = 5) +
  scale_colour_viridis(option = "H") +
  facet_wrap(~model, scales = "free")

data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  filter(code == "never") %>% 
  view()





files_raw <- list.files(paste0(path, "/Results/Species Exposure Dates"), pattern = "30yr", full.names = T)
models <- c("CanESM5","CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

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

data_30yr <- bind_rows(res_final) 



# join global warming levels


df_gwl <- readRDS(paste0(path, "Data/Climate Data/Global Averages/global_averages_rolling_30yr.rds"))


# get gwl levels for each year
gwl <- df_gwl %>%
  group_by(model) %>%
  mutate(historic_temp = mean(temperature[year <= 1900])) %>% 
  mutate(gwl = temperature - historic_temp) %>% 
  select(-historic_temp, -temperature)

# binding the lowest temp data to the main data
# gwl_diff the difference in temperature at exposure and deexposure
# if the value is negative, deexposure occurred at a lower gwl
data_30yr <- data_30yr %>% 
  left_join(gwl, by = c("model", "exposure" = "year")) %>% 
  rename("gwl_exposure" = gwl) %>% 
  left_join(gwl, by = c("model", "deexposure" = "year")) %>% 
  rename("gwl_deexposure" = gwl) %>% 
  mutate(gwl_diff = gwl_deexposure - gwl_exposure) 

data_30yr %>% 
  group_by(model) %>% 
  summarise(median_exp = median(gwl_exposure),
            median_dexp = median(gwl_deexposure)) %>% 
  pivot_longer(cols = c(median_exp, median_dexp)) %>% 
  ggplot() +
  geom_boxplot(aes(x = name, y = value), coef = 5)

data_30yr %>% 
  group_by(model) %>% 
  summarise(median_exp = median(gwl_exposure),
            median_dexp = median(gwl_deexposure),
            median_diff = median(gwl_diff)) 



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


data_30yr <- data_30yr %>% 
  left_join(code_df, by ="model") %>% 
  mutate(overshoot_phase = case_when(exposure <= year_os_starts ~ "pre_os",
                                     exposure > year_os_starts & exposure <= year_peak ~ "os_warming",
                                     exposure > year_peak & exposure <= year_low ~ "os_cooling",
                                     exposure > year_low ~ "post_os"))




# now, I classify the exposure events according to the gwl

data_30yr <- data_30yr %>% 
  mutate(code = case_when(gwl_diff >= 0.04 ~ "De-exposure needs less cooling",
                          gwl_diff < 0.04 & gwl_diff >= -0.04 ~ "De-exposure at the same GWL",
                          gwl_diff < -0.04 ~ "De-exposure needs more cooling")) %>% 
  mutate(code = ifelse(deexposure == 2201, "De-exposure never happened", code))


data_30yr %>% 
  group_by(model) %>% 
  count(code) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  arrange(code, perc)

data_30yr %>% 
  group_by(model) %>% 
  count(code) %>% 
  mutate(perc = round(n/sum(n)*100, 0)) %>% 
  filter(code == "De-exposure needs more cooling") %>% 
  arrange(perc)

p1 <- data_30yr %>% 
  group_by(model) %>% 
  count(code) %>% 
  mutate(perc = round(n/sum(n)*100, 0)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  summarise(median = median(perc),
            min = min(perc),
            max = max(perc)) %>% 
  ggplot(aes(y = code, x = median, fill = code)) +
  geom_col() +
  geom_errorbar(aes(xmin = min, xmax = max), width = 0.3,
                colour = darken(rev(c("#d62828","#fcbf49","#3e92cc","#003049")), 0.35)) +
  scale_fill_manual(values = rev(c("#d62828","#fcbf49","#3e92cc","#003049"))) +
  labs(x = "Proportion %", y = "", title = "30yr rolling average") +
  theme_tidybayes() +
  theme(legend.position = "none",
        axis.text = element_text(size = 11, family = "Tahoma"),
        axis.text.y = element_blank()) +
  xlim(c(0,100)) 




p2 <- data_raw %>% 
  group_by(model) %>% 
  count(code) %>% 
  mutate(perc = round(n/sum(n)*100, 0)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  summarise(median = median(perc),
            min = min(perc),
            max = max(perc)) %>% 
  ggplot(aes(y = code, x = median, fill = code)) +
  geom_col() +
  geom_errorbar(aes(xmin = min, xmax = max), width = 0.3,
                colour = darken(rev(c("#d62828","#fcbf49","#3e92cc","#003049")), 0.35)) +
  scale_fill_manual(values = rev(c("#d62828","#fcbf49","#3e92cc","#003049"))) +
  labs(x = "Proportion %", y = "", title = "Raw climate data") +
  theme_tidybayes() +
  theme(legend.position = "none",
        axis.text = element_text(size = 11, family = "Tahoma")) +  
  xlim(c(0,100)) 


p2 + p1 + plot_layout(ncol = 2)


ggsave(paste0(path, "Figures/_Fig_03.jpg"),
       width = 20, height = 6, units = "cm", dpi = 700)




