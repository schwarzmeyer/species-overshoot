# POTENTIAL FIGURE 1 

data_median <- readRDS(paste0(path, "Results/Risk Metric/risk_raw_median.rds"))

amph <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Amphibians.rds"))
bird <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Birds.rds"))
mamm <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Mammals.rds"))
rept <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Reptiles.rds"))
fish <- readRDS(paste0(path, "Data/Species Data/Range Maps Grid Cells/Fishes.rds"))

# load countries polygon
countries <- ne_countries(returnclass = "sf")
countries <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")


# create bounding box to plot around the maps
bound <- st_sf(geometry = st_sfc(
  st_polygon(x = list(cbind(c(-180, rep(180, 100), rep(-180, 100)),
                            c(-90, seq(-90, 90, length = 100), 
                              seq(90, -90, length = 100))))), crs = 'WGS84')) %>% 
  st_transform("+proj=robin")


myquantile <- 0.5

qrange <- quantile(data_median$range_exposed, myquantile)
qduration <- quantile(data_median$mean_local_duration, myquantile)

# qrange <- 0.5
# qduration <- 50

p1 <-  ggplot(data_median, aes(x = range_exposed*100)) +
  geom_histogram(aes(fill = range_exposed*100 >= qrange*100), binwidth = 5, boundary = 0, colour = "white") +
  geom_boxplot(aes(y = -1700), width = 1400) +
  scale_fill_manual(values = c("grey73","#99269c")) +
  labs(y = "N. of species", x = "Range exposed (%)") +
  scale_y_continuous(breaks = c(0,3000,6000,9000)) +
  # scale_x_continuous(breaks = seq(0,100, length.out = 24)) +
  annotate("segment", x = qrange*100, xend = qrange*100, y = 30, yend = 7500, color = "#721d75", linewidth = 0.8, 
           linetype = 2) +
  annotate("text", x = 33, y = 8700, label = "32%", color = "#721d75", fontface = 2, size = 3.2) +
  coord_cartesian(clip = "off", ylim = c(-3000,9000)) +
  theme_tidybayes() +
  theme(legend.position = "none",
        plot.margin = margin(t = 0, b = 15, l = 20, r = 20)); p1



p2 <- ggplot(data_median, aes(x = mean_local_duration)) +
    geom_histogram(aes(fill = mean_local_duration >= qduration),  binwidth = 10, boundary = 0, closed = "left", position = "identity", colour = "white") +
    geom_boxplot(aes(y = -1600), width = 1000, outlier.color = NA) +
    scale_fill_manual(values = c("grey73","#99269c")) +
    labs(y = "N. of species", x = "Duration (years)") +
    scale_y_continuous(breaks = c(0,2000,4000,6000)) +
    annotate("segment", x = qduration, xend = qduration, y = 30, yend = 6000, color = "#721d75", linewidth = 0.8, 
           linetype = 2) +
    annotate("text", x = 119, y = 7100, label = "109 years", color = "#721d75", fontface = 2, size = 3.2) +
    coord_cartesian(clip = "off", ylim = c(-2300,6000)) +
    theme_tidybayes() +
    theme(legend.position = "none",
        plot.margin = margin(t = 15, b = 0, l = 20, r = 20)); p2




risk_spp <- data_median %>% 
  filter(range_exposed >= qrange,
         mean_local_duration >= qduration) %>% 
  pull(species)




range_data_land <- c(amph, bird, mamm, rept) 
range_data_ocean <- fish 



length_land <- sapply(range_data_land, function(x) length(x))
length_ocean <- sapply(range_data_ocean, function(x) length(x))

range_data_land <- tibble(species = rep(names(length_land), times = length_land),
                          WorldID = unlist(range_data_land))

range_data_ocean <- tibble(species = rep(names(length_ocean), times = length_ocean),
                           WorldID = unlist(range_data_ocean))

risk_land <- range_data_land %>% 
  filter(species %in% risk_spp) %>% 
  count(WorldID, name = "spp_risk")

risk_ocean <- range_data_ocean %>% 
  filter(species %in% risk_spp) %>% 
  count(WorldID, name = "spp_risk")


richness_land <- range_data_land %>% 
  count(WorldID)


richness_ocean <- range_data_ocean %>% 
  count(WorldID)


data_land <- range_data_land %>% 
  left_join(data_median, by = "species") %>% 
  na.omit() %>% 
  group_by(WorldID) %>% 
  summarise(range_exposed = median(range_exposed),
            mean_local_duration = median(mean_local_duration),
            range_size = median(range_size)) %>% 
  left_join(richness_land, by = "WorldID") %>% 
  left_join(risk_land, by = "WorldID") %>% 
  mutate(spp_risk = ifelse(is.na(spp_risk), 0, spp_risk)) %>% 
  mutate(spp_risk_perc = spp_risk/n*100)


data_ocean <- range_data_ocean %>% 
  left_join(data_median, by = "species") %>% 
  na.omit() %>% 
  group_by(WorldID) %>% 
  summarise(range_exposed = median(range_exposed),
            mean_local_duration = median(mean_local_duration)) %>% 
  left_join(richness_ocean, by = "WorldID") %>% 
  left_join(risk_ocean, by = "WorldID") %>% 
  mutate(spp_risk = ifelse(is.na(spp_risk), 0, spp_risk)) %>% 
  mutate(spp_risk_perc = spp_risk/n*100)


grid_land <- readRDS(paste0(path, "Data/Spatial Data/terrestrial_grid_robin.rds"))
grid_ocean <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_robin.rds"))

grid_land <- left_join(grid_land, data_land, by = "WorldID")
grid_ocean <- left_join(grid_ocean, data_ocean, by = "WorldID")





grid_ocean <- grid_ocean %>% 
  mutate(spp_risk_perc = ifelse(spp_risk_perc <= 1, NA, spp_risk_perc))

grid_land <- grid_land %>% 
  mutate(spp_risk_perc = ifelse(spp_risk_perc <= 1, NA, spp_risk_perc))



bks <- c(1,5,10,20,30,40,65)
  

pmap_perc <- ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey91") +
  geom_sf(data = countries, colour = NA, fill = "grey73",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = spp_risk_perc, colour = spp_risk_perc), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = spp_risk_perc, colour = spp_risk_perc), show.legend = T) +
  geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.3) +
  geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_b(option = "C", direction = 1, breaks = bks, name = "% of species at high risk",
                       end = 0.98, begin = 0.27, na.value = NA, limits = c(1,max(bks))) +
  scale_colour_viridis_b(option = "C", direction = 1, breaks = bks, name = "% of species at high risk",
                         end = 0.98, begin = 0.27, na.value = NA, limits = c(1,max(bks))) +
  # coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position.inside = c(0.28,0.98),
        legend.direction = "horizontal",
        plot.margin = margin(t=20,0,0,0),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5, 
                                 barwidth = unit(18, 'lines'), barheight = unit(.65, 'lines'))); pmap_perc




richness <- tibble(group = c("Amphibians", "Birds", "Fishes", "Mammals", "Reptiles"),
                   richness = c(length(unique(names(amph))),
                                length(unique(names(bird))),
                                length(unique(names(fish))),
                                length(unique(names(mamm))),
                                length(unique(names(rept)))))
p3 <- data_median %>% 
  filter(species %in% risk_spp) %>% 
  count(group, name = "exposed") %>% 
  left_join(richness, by = "group") %>% 
  mutate(not_exposed = richness - exposed) %>% 
  mutate(group = fct_reorder(group, exposed)) %>%
  pivot_longer(cols = c(exposed, not_exposed)) %>% 
  mutate(perc = value/richness*100) %>%
  # mutate(group = factor(group, levels = rev(c("Amphibians","Reptiles","Mammals","Birds","Fishes")))) %>%
  ggplot(aes(x = value, y = group, fill = fct_rev(name))) +
  geom_col(position = "stack", width = 0.72) +
  scale_fill_manual(values = c("grey73", "#99269c")) +
  scale_x_continuous(limits = c(0,11000), breaks = c(0,2500,5000,7500,10000), expand = c(0,0)) +
  labs(x = "No. of species", y = "") +
  theme_tidybayes() +
  geom_text(aes(label = paste0(round(perc, 0), "%")),
            position = position_stack(vjust = 0.5), colour = rep(c("white","black"), 5), fontface = 2, size = 2.8, family = "Tahoma") +
  theme(plot.title = element_text(hjust = 0.5, family = "Tahoma"),
        plot.subtitle = element_text(hjust = 0.5, family = "Tahoma"),
        legend.text = element_text(family = "Tahoma"),
        plot.margin = margin(b = 0, l = 0, r = 15),
        legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.line.y = element_blank()); p3
  


pp <- (((p1 / p2)  | p3 ) + plot_layout(widths = c(0.6,1))) / pmap_perc + 
  plot_layout(heights = c(1,2)) &
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 12, margin = margin(r = 10)),
        plot.tag.position = "topleft")


ggsave(paste0(path, "Figures/_Fig_01_v1.jpg"),
       pp,
       width = 24, height = 25, units = "cm", dpi = 700)



