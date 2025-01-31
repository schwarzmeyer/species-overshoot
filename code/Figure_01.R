# FIGURE 1 
 
data_median <- readRDS(here("results/risk/risk_raw_median.rds"))

amph <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Amphibians.rds"))
bird <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Birds.rds"))
mamm <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Mammals.rds"))
rept <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Reptiles.rds"))
fish <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Fishes.rds"))

range_data <- c(amph, bird, mamm, rept, fish) 
total_species <- length(unique(names(range_data)))

# load countries polygon
countries <- ne_countries(returnclass = "sf")
countries <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")

# create bounding box to plot around the maps
bound <- readRDS(here("raw_data/random/bbox.rds"))

# thresholds for high risk
qrange <- 0.8
qduration <- 100

# % of species at high risk
n_spp <- data_median %>% 
  filter(range_exposed >= qrange,
         mean_local_duration >= qduration) %>% 
  nrow()

perc_high_risk <- paste0(round(n_spp / total_species * 100, 1), "%")

# plot par
theme_colour_1 <- lighten("#14151f",0.1)
theme_colour_2 <- "#811c51" 
theme_colour_3 <- lighten("#14151f",0.75)

# panel a ----

p1_tmp <- data_median %>% 
  mutate(f = ifelse(mean_local_duration >= 100 & range_exposed >= 0.8, "a", "b")) %>% 
  ggplot(aes(x = mean_local_duration, y = range_exposed*100, colour = f)) +
  geom_point(alpha = 0.05, show.legend = F) +
  scale_colour_manual(values = c(theme_colour_2, theme_colour_1)) +
  coord_cartesian(clip = "off", ylim = c(0,100)) +
  # annotate("label", x = 78, y = 80, colour = "#811c51", size = 2.8, fill = alpha("white", 0.5),
  #          label = glue("25% of species with
  #                        ≥80% range exposed
  #                        for ≥100 years"), hjust = 1) +  
  labs(y = "Range exposed (%)", x = "Average duration (years)") +
  theme_tidybayes() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)); p1_tmp


p1 <- ggExtra::ggMarginal(p1_tmp, type = "histogram",  groupColour = TRUE, groupFill = TRUE) 

# species at high risk
risk_spp <- data_median %>%
  filter(range_exposed >= qrange,
         mean_local_duration >= qduration) %>% 
  pull(species)


richness <- tibble(group = c("Amphibians", "Birds", "Fishes", "Mammals", "Reptiles"),
                   richness = c(length(unique(names(amph))),
                                length(unique(names(bird))),
                                length(unique(names(fish))),
                                length(unique(names(mamm))),
                                length(unique(names(rept)))))

phylo_color <- alpha("#811c51", 0.9)

p2 <- data_median %>% 
  filter(species %in% risk_spp) %>% 
  count(group, name = "exposed") %>% 
  left_join(richness, by = "group") %>% 
  mutate(not_exposed = richness - exposed) %>% 
  mutate(group = fct_reorder(group, exposed)) %>%
  pivot_longer(cols = c(exposed, not_exposed)) %>% 
  mutate(perc = value/richness*100) %>%
  # mutate(group = factor(group, levels = rev(c("Amphibians","Reptiles","Mammals","Birds","Fishes")))) %>%
  ggplot(aes(x = value, y = group, fill = fct_rev(name))) +
  geom_col(position = "stack", width = 0.72, alpha = 0.95) +
  scale_fill_manual(values = c(theme_colour_3, theme_colour_2)) +
  scale_x_continuous(limits = c(0,11700), breaks = c(0,2500,5000,7500,10000), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Number of species", y = "") +
  theme_tidybayes() +
  geom_text(aes(label = paste0("           ", round(perc, 0), "%")),
            position = position_stack(vjust = 1), colour = rep(c(theme_colour_2,NA), 5), 
            fontface = 1, size = 3.5) +
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color,
               x = 10500, y = 5, height = 0.85, alpha = 1) +  
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color,
               x = 7900, y = 4, height = 0.74, alpha = 1, horizontal = T) +   
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color,
               x = 11700, y = 3, height = 0.78, alpha = 1) +   
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color,
               x = 6400, y = 2, height = 0.8, alpha = 1) +   
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color,
               x = 5100, y = 1, height = 0.49, alpha = 1) +   
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        # plot.margin = margin(l = 5, r = 25),
        legend.position = "none",
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.line.y = element_blank())


# panel c ----

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


grid_ocean <- grid_ocean %>% 
  mutate(spp_risk = ifelse(spp_risk <= 1, NA, spp_risk))

grid_land <- grid_land %>% 
  mutate(spp_risk = ifelse(spp_risk <= 1, NA, spp_risk))


bks <- c(1,5,10,15,25,50,100,max(grid_land$spp_risk, na.rm = T))


pmap <- ggplot() +
  geom_sf(data = bound, colour = NA, fill = lighten("#04061a", 0.3)) +
  geom_sf(data = countries, colour = NA, fill = lighten("#04061a", 0.05)) +
  geom_sf(data = grid_ocean, aes(fill = spp_risk), colour = NA, show.legend = T) +
  geom_sf(data = grid_land, aes(fill = spp_risk), colour = NA, show.legend = T) +
  geom_sf(data = countries, colour = alpha("white", 0.5), fill = NA,  linewidth = 0.25) +
  scale_fill_viridis_b(option = "F", direction = 1, breaks = bks, 
                       name = "Number of species at risk of severe exposure",
                       end = 1, begin = 0.25, na.value = NA, limits = range(bks)) +
  theme_map() +
  theme(legend.position.inside = c(0.5,1.05),
        legend.justification = "centre",
        legend.direction = "horizontal",
        plot.margin = margin(t = 3, b = 1, r = 1.5, l = 1.5, unit = "cm"),
        legend.title = element_text(size = 11.5),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5, 
                                 barwidth = unit(18, 'lines'), barheight = unit(.4, 'lines')))

# pp <- 
#   plot_grid(
#     plot_grid(p1, p2, 
#               nrow = 1, 
#               align = "h"),
#     pmap, ncol = 1,  rel_heights = c(1,2), scale = c(1, 1.05)) +
#   draw_plot_label(letters[1:3],
#                   c(0.01, 0.46,0.01),
#                   c(0.95,0.95,0.56),
#                   size = 12)

pp <- ggdraw() +
  draw_plot(p1, x = -0.46, y = 0.28, scale = 0.33, width = 1.4) +
  draw_plot(p2, x = 0.08, y = 0.32, scale = 0.33, width = 1.32, height = 0.9) +
  draw_plot(pmap, x = 0, y = -0.2) +
  draw_text(glue("25% of species projected
                  to have ≥80% range 
                  exposed for ≥100 years"),
            x = 0.4, y = 0.94, size = 9, 
            fontface = 1, colour = theme_colour_2, hjust = 0) +
  draw_text(letters[1:3], 
            x = c(0.03, 0.6, 0.03), 
            y = c(0.94, 0.94, 0.52),
            fontface = 2, size = 12)


  

ggsave(here("figures/Fig_01.jpg"),
       pp,
       width = 22, height = 21, units = "cm", dpi = 300)
