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



# plot

phylo_x_p1 <- 55
phylo_x_p2 <- -0.85
phylo_color <- "#4970A5" 
  
p1 <- data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase")) %>% 
  group_by(model, group) %>% 
  count(code) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  group_by(group, code) %>% 
  summarise(mid = median(perc),
            lower = min(perc),
            upper = max(perc), 
            .groups = "drop") %>% 
  mutate(group = fct_reorder(group, mid)) %>% 
  filter(code %in% c("never", "more_cooling")) %>% 
  ggplot(aes(x = mid, y = group, fill = code)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.8) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, position = position_dodge(width = 0.8),
                colour = rep(darken(c("#88C0D0",  "#DC143C"), 0.2), 5)) +
  scale_fill_manual(values = c("#4970A5", "#FF5D6F"), name = "") +
  coord_cartesian(clip = "off") +
  annotate("text", x = 0.1, y = 5.6, label = "No de-exposure", hjust = 0, colour = "#FF5D6F",
           size = 2.7, fontface = 3) +
  annotate("text", x = 13, y = 5.2, label = "De-exposure needed additional cooling", hjust = 0, colour = "#4970A5",
           size = 2.7, fontface = 3) +
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color, # amphibians
               x = phylo_x_p1, y = 4, height = 0.55, alpha = 1, horizontal = T) +    
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color, # birds
               x = phylo_x_p1, y = 1, height = 0.67, alpha = 1) +   
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color, # mammals
               x = phylo_x_p1, y = 2, height = 0.7, alpha = 1) +   
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color, # reptiles
               x = phylo_x_p1, y = 3, height = 0.75, alpha = 1) +   
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color, # fish
               x = phylo_x_p1, y = 5, height = 0.4, alpha = 1) + 
  scale_x_continuous(limits = c(0,55), expand = c(0,0)) +
  labs(x = "Proportion of exposure events (%)", y = "") +
  theme_tidybayes() +
  theme(axis.line.y = element_blank(),
        axis.title.x = element_text(size = 9, vjust = -2),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.key.size = unit(5, "pt"),
        legend.text = element_text(size = 8.5),
        plot.margin = margin(r = 20, t = 15, b = 30)); 
p1

p2 <- data %>% 
  filter(overshoot_phase %in% c("warming_phase", "cooling_phase"),
         code == "more_cooling") %>% 
  group_by(group, model) %>% 
  summarise(med = median(gwl_diff),
            p80 = quantile(gwl_diff, probs = 0.2),
            p95 = quantile(gwl_diff, probs = 0.05), 
            .groups = "drop") %>% 
  group_by(group) %>% 
  summarise(mid_med = median(med),
            upper_med = max(med),
            lower_med = min(med),
            mid_p80 = median(p80),
            upper_p80 = max(p80),
            lower_p80 = min(p80),
            mid_p95 = median(p95),
            upper_p95 = max(p95),
            lower_p95 = min(p95),
            .groups = "drop") %>% 
  mutate(group = factor(group, levels = c("Birds", "Mammals", "Reptiles", "Amphibians", "Fishes"))) %>% 
  ggplot(aes(y = group)) +
  geom_pointrange(aes(x = mid_med, xmin = lower_med, xmax = upper_med, colour = "50%"), 
                  position = position_nudge(y = 0.23), fatten = 2) +
  geom_pointrange(aes(x = mid_p80, xmin = lower_p80, xmax = upper_p80, colour = "80%"),
                  fatten = 2) +
  geom_pointrange(aes(x = mid_p95, xmin = lower_p95, xmax = upper_p95, colour = "95%"), 
                  position = position_nudge(y = -0.23), fatten = 2) +
  scale_x_reverse(limits = c(-0.08, -0.85), expand = c(0,0), breaks = seq(-0.1,-0.8,-0.1)) +
  labs(x = "Additional cooling needed for de-exposure (°C)", y = "") +
  scale_colour_manual(values = c("#88C0D0","#4970A5","#01294B"), name = "% of populations de-exposed") +
  coord_cartesian(clip = "off", ylim = c(1,5)) +
  annotate("text", x = -0.59, y = 6.25, label = "Proportion of\npopulations de-exposed", hjust = 0,
           size = 2.7, colour = "grey50", fontface = 3) +
  annotate("text", x = -0.245, y = 5.4, label = "50%", color = "#90B3BE", size = 3) +
  annotate("text", x = -0.435, y = 5.15, label = "80%", color = "#4970A5", size = 3) +
  annotate("text", x = -0.655, y = 4.95, label = "95%", color = "#01294B", size = 3) +
  annotate("curve", x = -0.57, y = 6.3, xend = -0.27, yend = 5.7, 
           curvature = 0.2, arrow = arrow(angle = 20, type = "closed", length = unit(4, "pt")), 
           colour = "grey70", linewidth = 0.3) +
  annotate("curve", x = -0.57, y = 6.2, xend = -0.46, yend = 5.4, 
           curvature = 0.3, arrow = arrow(angle = 20, type = "closed", length = unit(4, "pt")), 
           colour = "grey70", linewidth = 0.3) +
  annotate("curve", x = -0.57, y = 6.1, xend = -0.62, yend = 5.2, 
           curvature = 0.3, arrow = arrow(angle = 20, type = "closed", length = unit(4, "pt")), 
           colour = "grey70", linewidth = 0.3) +
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color, # amphibians
               x = phylo_x_p2, y = 4, height = 0.55, alpha = 1, horizontal = T) +    
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color, # birds
               x = phylo_x_p2, y = 1, height = 0.67, alpha = 1) +   
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color, # mammals
               x = phylo_x_p2, y = 2, height = 0.7, alpha = 1) +   
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color, # reptiles
               x = phylo_x_p2, y = 3, height = 0.75, alpha = 1) +   
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color, # fish
               x = phylo_x_p2, y = 5, height = 0.4, alpha = 1) + 
  
  theme_tidybayes() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 9, vjust = -2),
        plot.margin = margin(r = 20, t = 25, b = 10, l = 0)); 



p <- plot_grid(p1, p2, scale = 0.95, labels = c("a", "b"), ncol = 1, align = "v", label_size = 10, label_x = 0.02)

ggsave(here("figures/Fig_04x.jpg"),
       p,
       width = 12, height = 16, units = "cm", dpi = 700)

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



bks <- c(1,5,10,20,30,50,100,1000)

fill_columns <- c("never", "less_cooling", "more_cooling")

p_map <- map(fill_columns, function(x){
  
  bound_colour <- ifelse(x == "less_cooling", "#92C9D9",
                                ifelse(x == "never", "#ED2E4C","#366297"))
  
  plot_title <- ifelse(x == "less_cooling", "Additional cooling was not needed",
                       ifelse(x == "never", "No de-exposure","De-exposure needed additional cooling"))
  
  

  ggplot() +
    geom_sf(data = bound, colour = NA, fill = "grey90") +
    geom_sf(data = countries, colour = NA, fill = "grey50",  linewidth = 0.15) +
    geom_sf(data = grid_ocean_join, aes(fill = !!sym(x)), colour = NA, show.legend = T) +
    geom_sf(data = grid_land_join, aes(fill = !!sym(x)), colour = NA, show.legend = T) +
    geom_sf(data = countries, colour = "black", fill = NA,  linewidth = 0.15) +
    # geom_sf(data = bound, colour = bound_colour, fill = NA, linewidth = 0.3) +
    scale_fill_viridis_b(option = "D", direction = 1, breaks = bks, name = "N. of species",
                         end = 0.95, begin = 0.25, na.value = NA, limits = range(bks),
                         labels = c("", bks[2:(length(bks)-1)], "")) +
    labs(subtitle = plot_title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = "center", 
          plot.margin = margin(t = 0.6, unit = "cm"),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 8),
          plot.subtitle = element_text(size = 11, hjust = 0.5, vjust = -1)) +
    guides(fill = guide_colorsteps(title.position = "top",
                                   title.hjust = .5, 
                                   barwidth = unit(13, "lines"), 
                                   barheight = unit(.4, "lines")))
  
  
  
})


# p <- plot_grid(p1, p2, scale = 0.95, labels = c("a", "b"))
# pp <- plot_grid(p_map[[1]] + theme(legend.position = "none"),
#                 p_map[[2]] + theme(legend.position = "none")) 
#   
# p_final <- plot_grid(p, 
#                      pp, 
#                      p_map[[3]] + theme(plot.margin = margin(b = 10)), 
#                      ncol = 1, rel_heights = c(1,1,1.1))


p <- plot_grid(p1, p2, scale = 0.95, labels = c("a", "b"), ncol = 1, align = "v", label_size = 11)


ggsave(here("figures/Fig_04_n.jpg"),
       p,
       width = 12, height = 15, units = "cm", dpi = 700)






p1 + p2 + plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a")


pp <- plot_grid(p_map[[1]] + theme(legend.position = "none"),
                     p_map[[2]], ncol = 1) 

p_final <- plot_grid(p, 
                     pp, 
                     ncol = 1, rel_heights = c(1,2))


p1 + p2 + p_map[[1]] + p_map[[2]] + 
  plot_layout(ncol = 2, guides = "collect", byrow = F) &
  theme(legend.position = "bottom")

p <- p1 + p2 + plot_layout(ncol = 1)
p + pp + plot_layout(ncol = 2)

p_final <- p1 + p2 + p_map[[1]]
ggsave(here("figures/Fig_04_n.jpg"),
       p_final,
       width = 21, height = 24, units = "cm", dpi = 700)






pp <- (p1 + p2) / 
  p_map[[1]] + 
  p_map[[2]] +
  p_map[[3]] + plot_layout(ncol = 1)

ggsave(here("figures/Fig_04_n.jpg"),
       pp,
       width = 21, height = 24, units = "cm", dpi = 700)

maps <-  (p_map[[1]] + p_map[[2]]) / p_map[[3]] + plot_layout(guides = "collect", heights = c(1,1))
maps

p <- p1 + p2

p <- plot_grid(p1, p2,  plotlist = p_map, ncol = 2)

p_final <- plot_grid(p, plotlist = p_map, ncol = 1)

p_final <- p + maps + plot_layout(nrow = 2)
