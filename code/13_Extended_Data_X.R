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
  # mutate(realm = ifelse(group == "Fishes", "marine", "terrestrial")) %>% 
  group_by(model, group) %>% 
  mutate(code2 = case_when(code == "less_cooling" ~ "Additional cooling \nwas not needed",
                           code == "same_gwl" ~ "Additional cooling \nwas not needed",
                           code == "more_cooling" ~ "De-exposure needed \nadditional cooling",
                           code == "never" ~ "No de-exposure")) %>% 
  count(code2) %>% 
  mutate(perc = n/sum(n)) %>% 
  mutate(code2 = fct_reorder(code2, perc))

group <- c("Amphibians", "Birds", "Mammals", "Reptiles", "Fishes")




p1 <- map(models, function(x){
  
  # if(.x == "Amphibians") {phylopic <- "bd80bc51-460c-4dd9-8341-e5b460372efb"; h <- 0.7}
  # if(.x == "Birds") {phylopic <- "157d3109-7124-413c-8362-3abcc6889a3f"; h <- 0.8}
  # if(.x == "Mammals") {phylopic <- "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc"; h <- 0.9}
  # if(.x == "Reptiles") {phylopic <- "264fa655-afd7-451c-8f27-e0a9557376e6"; h <- 0.9}
  # if(.x == "Fishes") {phylopic <- "c90aa49b-d9c5-44a4-a709-4f8d9a33b559"; h <- 0.5}
  
  phylo_color <- "grey5"
  phylo_x <- 103
  my_plot <- data_proportions %>% 
    mutate(group = factor(group, levels = rev(c("Amphibians", "Birds", "Mammals", "Reptiles", "Fishes"))),
           perc = perc * 100) %>% 
    filter(model == x) %>% 
    ggplot(aes(x = perc, y = group, fill = code2, label = round(perc, 0))) +
    geom_col(position = "stack") +
    coord_cartesian(expand = F) +
    scale_fill_manual(values = c("#DC143C","#FF8C00","#77a9d0")) +
    add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color,
                 x = phylo_x, y = 5, height = 0.6, alpha = 1) +    
    add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color,
                 x = phylo_x, y = 4, height = 0.75, alpha = 1) +   
    add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color,
                 x = phylo_x, y = 3, height = 0.75, alpha = 1) +   
    add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color,
                 x = phylo_x, y = 2, height = 0.78, alpha = 1) +   
    add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color,
                 x = phylo_x, y = 1, height = 0.42, alpha = 1) +   
    geom_text(size = 2.5, position = position_stack(vjust = 0.5), colour = rep(c("black", "black", "white"), 5)) +
    scale_x_continuous(labels = paste0(seq(0,100,25), "%")) +
    theme_tidybayes() +
    labs(y = x, x = "") +
    theme(plot.margin = margin(t = 0.1, b = 0, l = 0.5 , r = 1, unit = "cm"),
          plot.title = element_text(size = 8, hjust = 0.5),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.title.y = element_text(size = 10, vjust = 3),
          axis.text = element_text(size = 9))
  
  return(my_plot)
  
})

p <- wrap_plots(
  p1, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9.5))


ggsave(here("figures/Extended_data_X.jpg"),
       p,
       width = 17, height = 22, units = "cm", dpi = 700)




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
          axis.title.x = element_text(size = 11, vjust = -3),
          axis.text.x = element_text(size = 9.5),
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
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 8),
          plot.subtitle = element_text(size = 11, hjust = 0.5, vjust = -1, colour = title_colour)) +
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
          legend.text = element_text(size = 10),
          legend.margin = margin(b = 0.8, t = -0.8, unit = "cm"),
          plot.title = element_text(size = 13, face = "bold", vjust = 5))) 


p
p_maps <- wrap_elements(
  (p_map[[1]] + p_map[[2]]) / plot_spacer() / p_map[[3]] +
    plot_layout(guides = "collect", heights = c(1.05, -0.17, 1.2)) +
    plot_annotation(title = "b") &
    theme(legend.position = "bottom",
          plot.title = element_text(size = 13, face = "bold", vjust = -5)) &
    guides(fill = guide_colorsteps(title.position = "top",
                                   title.hjust = 0.5,
                                   barwidth = unit(15, 'lines'), 
                                   barheight = unit(.5, 'lines'))))
  

  
p_final <- p / p_maps + 
  plot_layout(heights = c(0.5, 1), guides = "collect")

ggsave(here("figures/Fig_04.jpg"),
       p_final,
       width = 21, height = 24, units = "cm", dpi = 700)


