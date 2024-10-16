files <- list.files(here("results/species_exposure_dates"), pattern = "raw", full.names = T)
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

ggsave(here("figures/Fig_S1.jpg"),
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




data_proportions <- data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  group_by(model) %>% 
  mutate(code2 = case_when(code == "less_cooling" ~ "Additional cooling was not needed",
                           code == "same_gwl" ~ "Additional cooling was not needed",
                           code == "more_cooling" ~ "De-exposure needed additional cooling",
                           code == "never" ~ "No de-exposure")) %>% 
  count(code2) %>% 
  mutate(perc = n/sum(n)) %>% 
  mutate(code2 = fct_reorder(code2, perc))

data_pi <- data_proportions %>% 
  group_by(model) %>% 
  mutate(start = lag(perc, default = 0) * 2 * pi,
         end = cumsum(perc) * 2 * pi,
         midpoint = cumsum(perc) - perc / 2)


p1 <- map(models, function(x){
  
  y_title <- ifelse(x == "CanESM5", "% of populations", "")
    
  my_plot <- data_pi %>% 
    filter(model == x) %>% 
    ggplot() +
    ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, 
                              r0 = 0.72, r = 1.08,
                              start = start,
                              end = end,
                              fill = code2), 
                          colour = NA, show.legend = T) +
    coord_equal(expand = F, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), clip = "off") + 
    scale_fill_manual(values = c("#DC143C","#FF8C00","#77a9d0")) +
    annotate("text", x = 0, y = 0, label = x, size = 2.6) +
    geom_text(aes(
      x = 1.35 * sin(2 * pi * midpoint),
      y = 1.35 * cos(2 * pi * midpoint),
      label = paste0(round(perc*100, 0), "%")
    ), size = 3.1, fontface = "bold",
    colour = darken(c("#77a9d0", "#FF8C00", "#DC143C"), amount = 0.15)) +
    theme_minimal() +
    labs(y = y_title, x = "") +
    theme(plot.margin = margin(t = 0, b = -0.1, l = 0 , r = 0, unit = "cm"),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title.y = element_text(size = 9, vjust = 3))
  
  return(my_plot)
  
  
})


p2 <- map(models, function(x){
  
  x_title <- ifelse(x == "GISS-E2-1-G", "Additional cooling needed for de-exposure (°C)", "")
  
  data %>% 
    filter(overshoot_phase %in% c("warming_phase", "cooling_phase"),
           code == "more_cooling",
           model == x) %>% 
    ggplot(aes(y = 1, x = gwl_diff, fill = "", colour = "")) +
    ggdist::stat_halfeye(
      adjust = 1, 
      width = 1, 
      .width = 0, 
      justification = -.4,
      point_colour = NA,
      show.legend = F) + 
    geom_boxplot(
      width = .4, 
      outlier.shape = NA,
      show.legend = F,
      alpha = 0.4,
      linewidth = 0.5
    ) +
    labs(x = x_title, y = "") +
    scale_fill_manual(values = "#FF8C00") +
    scale_colour_manual(values = "#FF8C00") +
    scale_x_reverse(limits = c(0, -0.8), expand = c(0,0)) +
    ylim(c(0.6 ,3.2)) +
    theme_tidybayes() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = 9.5, vjust = -3),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(t = 0.3, b = 0.1, r = 0, l= 0,  unit = "cm"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank())
  
})


# ggsave(here("figures/test.jpg"),
#        p,
#        width = 21, height = 7.5, units = "cm", dpi = 500)




# maps

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

bks <- c(5,10,20,40,60,80,100,125,150,1500)

bks <- c(5,10,20,30,40,60,80,100,1500)


viridis_begin <- 0.2

fill_columns <- c("less_cooling", "never", "more_cooling")

p_map <- map(fill_columns, function(x){
  
  title_colour <- darken(ifelse(x == "less_cooling", "#77a9d0",
                         ifelse(x == "never", "#DC143C","#FF8C00")), amount = 0.18)
  
  plot_title <- ifelse(x == "less_cooling", "Additional cooling was not needed",
                       ifelse(x == "never", "No de-exposure","De-exposure needed additional cooling"))
  
  
  
  ggplot() +
    geom_sf(data = bound, colour = NA, fill = "grey92") +
    geom_sf(data = countries, colour = NA, fill = "grey78",  linewidth = 0.15) +
    geom_sf(data = grid_ocean_join, aes(fill = !!sym(x)), colour = NA, show.legend = T) +
    geom_sf(data = grid_land_join, aes(fill = !!sym(x)), colour = NA, show.legend = T) +
    geom_sf(data = countries, colour = "grey11", fill = NA,  linewidth = 0.15) +
    geom_sf(data = bound, colour = title_colour, fill = NA, linewidth = 0.3) +
    scale_fill_viridis_b(option = "B", direction = 1, breaks = bks, name = "N. of species",
                         end = 1, begin = viridis_begin, na.value = NA) +
    # scale_colour_viridis_b(option = "B", direction = 1, breaks = bks, name = "N. of species",
    #                        end = 1, begin = viridis_begin, na.value = NA) +
    labs(subtitle = plot_title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          plot.margin = margin(t = 0.6, unit = "cm"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          plot.subtitle = element_text(size = 10.5, hjust = 0.5, vjust = -1, colour = title_colour)) +
    guides(fill = guide_colorsteps(title.position = 'top',
                                   title.hjust = .5, 
                                   barwidth = unit(9, 'lines'), 
                                   barheight = unit(.4, 'lines')))
  
  
  
})





p <- wrap_elements(
  p1[[1]] + plot_spacer() + p2[[1]] + 
    p1[[2]] + plot_spacer() + p2[[2]] +
    p1[[3]] + plot_spacer() + p2[[3]] + 
    p1[[4]] + plot_spacer() + p2[[4]] + 
    p1[[5]] + plot_spacer() + p2[[5]] +
    plot_layout(ncol = 5, byrow = F, heights = c(1.4,-0.54, 0.65), 
                guides = "collect") +
    plot_annotation(title = "a") &
    theme(legend.position = "top",
          legend.key.size = unit(0.4, "cm"),
          legend.text = element_text(size = 9),
          legend.margin = margin(b = 0.5, t = -0.5, unit = "cm"),
          plot.title = element_text(size = 13, face = "bold", vjust = 1))) 



p_maps <- wrap_elements(
  (p_map[[1]] + p_map[[2]]) / plot_spacer() / p_map[[3]] +
    plot_layout(guides = "collect", heights = c(1.05, -0.17, 1.2)) +
    plot_annotation(title = "b") &
    theme(legend.position = "bottom",
          plot.title = element_text(size = 13, face = "bold", vjust = -11)) &
    guides(fill = guide_colorsteps(title.position = "top",
                                   title.hjust = 0.5,
                                   barwidth = unit(15, 'lines'), 
                                   barheight = unit(.5, 'lines'))))
  

  
p_final <- p / p_maps + 
  plot_layout(heights = c(0.5, 1), guides = "collect")

ggsave(here("figures/Fig_04.jpg"),
       p_final,
       width = 21, height = 23, units = "cm", dpi = 400)


