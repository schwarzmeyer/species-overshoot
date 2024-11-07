# POTENTIAL FIGURE 1 
 
data_median <- readRDS(here("results/risk/risk_raw_median.rds"))

amph <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Amphibians.rds"))
bird <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Birds.rds"))
mamm <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Mammals.rds"))
rept <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Reptiles.rds"))
fish <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Fishes.rds"))


# load countries polygon
countries <- ne_countries(returnclass = "sf")
countries <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")


# create bounding box to plot around the maps
bound <- readRDS(here("raw_data/random/bbox.rds"))


qrange <- 0.8
qduration <- 100

theme_colour <- lighten("#150f35", 0.1)

median_qrange <- median(data_median$range_exposed)
median_qduration <- median(data_median$mean_local_duration)

p1 <-  ggplot(data_median, aes(x = range_exposed*100)) +
  geom_histogram(aes(fill = range_exposed*100 >= qrange*100), binwidth = 5, boundary = 0, colour = "white") +
  geom_boxplot(aes(y = -1700), width = 1400) +
  scale_fill_manual(values = c("grey73",theme_colour)) +
  labs(y = "N. of species", x = "Range exposed (%)") +
  scale_y_continuous(breaks = c(0,3000,6000,9000)) +
  # annotate("segment", x = median_qrange*100, xend = median_qrange*100, y = 30, yend = 7500, color = "black", linewidth = 0.8, 
  #          linetype = 2) +
  # annotate("text", x = median_qrange*100, y = 8700, label = "32%", color = "black", fontface = 2, size = 3.2) +
  coord_cartesian(clip = "off", ylim = c(-3000,9000)) +
  theme_tidybayes() +
  theme(legend.position = "none",
        plot.margin = margin(t = 10, b = 0, l = 30, r = 15))



p2 <- ggplot(data_median, aes(x = mean_local_duration)) +
    geom_histogram(aes(fill = mean_local_duration >= qduration),  binwidth = 10, boundary = 0, closed = "left", position = "identity", colour = "white") +
    geom_boxplot(aes(y = -1600), width = 1000, outlier.color = NA) +
    scale_fill_manual(values = c("grey73",theme_colour)) +
    labs(y = "N. of species", x = "Duration (years)") +
    scale_y_continuous(breaks = c(0,2000,4000,6000)) +
    # annotate("segment", x = median_qduration, xend = median_qduration, y = 30, yend = 6000, color = "grey40", linewidth = 0.8, 
    #        linetype = 2) +
    # annotate("text", x = 119, y = 7100, label = "109 years", color = "black", fontface = 2, size = 3.2) +
    coord_cartesian(clip = "off", ylim = c(-2300,6000)) +
    theme_tidybayes() +
    theme(legend.position = "none",
        plot.margin = margin(t = 25, b = -20, l = 30, r = 15))




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


grid_land <- readRDS(here("raw_data/spatial_data/terrestrial_grid_robin.rds"))
grid_ocean <- readRDS(here("raw_data/spatial_data/ocean_grid_robin.rds"))

grid_land <- left_join(grid_land, data_land, by = "WorldID")
grid_ocean <- left_join(grid_ocean, data_ocean, by = "WorldID")


# grid_ocean_perc <- grid_ocean %>% 
#   mutate(spp_risk_perc = ifelse(spp_risk_perc <= 1, NA, spp_risk_perc))
# 
# grid_land_perc <- grid_land %>% 
#   mutate(spp_risk_perc = ifelse(spp_risk_perc <= 1, NA, spp_risk_perc))
# 
# 
# 
# bks <- c(1,2, 5,10,20,30,40,65)
#   
# 
# ggplot() +
#   geom_sf(data = bound, colour = NA, fill = "grey91") +
#   geom_sf(data = countries, colour = NA, fill = "grey73",  linewidth = 0.2) +
#   geom_sf(data = grid_ocean_perc, aes(fill = spp_risk_perc, colour = spp_risk_perc), show.legend = T) +
#   geom_sf(data = grid_land_perc, aes(fill = spp_risk_perc, colour = spp_risk_perc), show.legend = T) +
#   geom_sf(data = countries, colour = "grey22", fill = NA,  linewidth = 0.3) +
#   geom_sf(data = bound, colour = "grey22", fill = NA, linewidth = 0.5) +
#   scale_fill_viridis_b(option = "C", direction = 1, breaks = bks, name = "% of species at risk of global extinction",
#                        end = 0.98, begin = 0.27, na.value = NA, limits = c(min(bks),max(bks))) +
#   scale_colour_viridis_b(option = "C", direction = 1, breaks = bks, name = "% of species at risk of global extinction",
#                          end = 0.98, begin = 0.27, na.value = NA, limits = c(min(bks),max(bks))) +
#   # coord_sf(ylim = c(-5500000,8000000)) +
#   theme_map() +
#   theme(legend.position.inside = c(0.28,0.98),
#         legend.direction = "horizontal",
#         plot.margin = margin(t=20,0,0,0),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10)) +
#   guides(fill = guide_colorsteps(title.position = 'top',
#                                  title.hjust = .5, 
#                                  barwidth = unit(18, 'lines'), barheight = unit(.65, 'lines')))


grid_ocean <- grid_ocean %>% 
  mutate(spp_risk = ifelse(spp_risk <= 1, NA, spp_risk))

grid_land <- grid_land %>% 
  mutate(spp_risk = ifelse(spp_risk <= 1, NA, spp_risk))


bks2 <- c(1,5,10,15,20,30,50,204)


pmap <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "#150f36",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = spp_risk), colour = NA, show.legend = T) +
  geom_sf(data = grid_land, aes(fill = spp_risk), colour = NA, show.legend = T) +
  geom_sf(data = countries, colour = "grey", fill = NA,  linewidth = 0.25) +
  scale_fill_viridis_b(option = "A", direction = 1, breaks = bks2, 
                       name = "Number of species at risk of global extinction",
                       end = 0.96, begin = 0.2, na.value = NA, limits = c(min(bks2),max(bks2))) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position.inside = c(0.28,1.05),
        legend.direction = "horizontal",
        plot.margin = margin(t = 100, b = 0, l = 0, r = 0),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5, 
                                 barwidth = unit(18, 'lines'), barheight = unit(.65, 'lines')))




richness <- tibble(group = c("Amphibians", "Birds", "Fishes", "Mammals", "Reptiles"),
                   richness = c(length(unique(names(amph))),
                                length(unique(names(bird))),
                                length(unique(names(fish))),
                                length(unique(names(mamm))),
                                length(unique(names(rept)))))

phylo_color <- theme_colour

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
  scale_fill_manual(values = c("grey73", theme_colour)) +
  scale_x_continuous(limits = c(0,11500), breaks = c(0,2500,5000,7500,10000), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(x = "No. of species", y = "") +
  theme_tidybayes() +
  geom_text(aes(label = paste0("           ", round(perc, 0), "%")),
            position = position_stack(vjust = 1), colour = rep(c(theme_colour,NA), 5), 
            fontface = 2, size = 3, family = "Tahoma") +
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color,
               x = 10500, y = 5, height = 0.78, alpha = 1) +  
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color,
               x = 7800, y = 4, height = 0.6, alpha = 1, horizontal = T) +   
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color,
               x = 11500, y = 3, height = 0.7, alpha = 1) +   
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color,
               x = 6400, y = 2, height = 0.75, alpha = 1) +   
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color,
               x = 5100, y = 1, height = 0.42, alpha = 1) +   
  theme(plot.title = element_text(hjust = 0.5, family = "Tahoma"),
        plot.subtitle = element_text(hjust = 0.5, family = "Tahoma"),
        legend.text = element_text(family = "Tahoma"),
        plot.margin = margin(b = -17, l = 0, r = 25, t = 15),
        legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.line.y = element_blank())
  


pp <- plot_grid(
  plot_grid(
    plot_grid(p1, p2, ncol = 1),
    p3, ncol = 2, rel_widths = c(0.75, 1), scale = 0.95),
  pmap, ncol = 1,  rel_heights = c(1,2)) +
  draw_plot_label(letters[1:4],
                  c(0.01,0.01,0.45,0.01),
                  c(1,0.8,1,0.55),
                  size = 12)


ggsave(here("figures/Fig_01.jpg"),
       pp,
       width = 24, height = 22, units = "cm", dpi = 500)
