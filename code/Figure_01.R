# FIGURE 1 
source("code/00_packages.R")
groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")

# FIGURE 1 

#############################################
#############################################
#############################################

risk_data <- readRDS(here("results/risk/risk_thresholds_models.rds")) 
range_data <- list.files(here("processed_data/species_data/range_maps_grid_cells/"), full.names = T) 

group_richness <- map_dfr(groups, 
                          ~ grep(.x, range_data, value = T) %>% 
                            readRDS() %>% 
                            names() %>% 
                            length() %>% 
                            unique() %>% 
                            as_tibble()) %>% 
  mutate(group = groups) %>% 
  rename(richness = value)


phylo_color <- alpha("#811c51", 0.9)


risk_data_plot <- risk_data %>% 
  filter(threshold %in% c("2w", "peak", "2c")) %>%
  filter(range_exposed >= 0.8) %>%
  group_by(model, group, threshold) %>% 
  count(name = "exposed") %>% 
  group_by(group, threshold) %>% 
  summarise(exposed_median = median(exposed),
            exposed_max = max(exposed),
            exposed_min = min(exposed),
            .groups = "drop") %>% 
  left_join(group_richness, by = "group") %>% 
  mutate(exposed_median = exposed_median / richness * 100,
         exposed_max = exposed_max / richness * 100,
         exposed_min = exposed_min / richness * 100) %>% 
  mutate(threshold = fct_recode(threshold, "2°C before peak" = "2w", "Peak" = "peak", "2°C after peak" = "2c"),
         group = factor(group, levels = c( "Amphibians", "Birds", "Mammals", "Reptiles", "Fishes"))) %>% 
  select(-richness)

risk_data_plot_all <- risk_data %>% 
  filter(threshold %in% c("2w", "peak", "2c")) %>%
  filter(range_exposed >= 0.8) %>%
  group_by(model, threshold) %>% 
  count(name = "exposed") %>% 
  group_by(threshold) %>% 
  summarise(exposed_median = median(exposed),
            exposed_max = max(exposed),
            exposed_min = min(exposed),
            .groups = "drop") %>% 
  mutate(exposed_median = exposed_median / 37688 * 100,
         exposed_max = exposed_max / 37688 * 100,
         exposed_min = exposed_min / 37688 * 100) %>% 
  mutate(threshold = fct_recode(threshold, "2°C before peak" = "2w", "Peak" = "peak", "2°C after peak" = "2c"),
         group = factor("All species")) %>% 
  relocate(group) 

all_data <- bind_rows(risk_data_plot, risk_data_plot_all)

p_bar <- ggplot(all_data, aes(x = group, y = exposed_median, fill = threshold)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = exposed_min, ymax = exposed_max), width = 0.2, position = position_dodge(width = 0.7),
                linewidth = 0.4) +
  scale_fill_viridis_d(option = "F", begin = 0.3, end = 0.8, direction = -1, name = "") +
  labs(x = "", y = "Species at risk of\nsevere exposure (%)") +
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color, # reptiles
               x = 4, y = 61, height = 15, alpha = 1) +
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color, # amphibians
               x = 1, y = 73, height = 13, alpha = 1, horizontal = T) +
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color, # birds
               x = 2, y = 33, height = 14, alpha = 1) +
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color, # mammals
               x = 3, y = 42, height = 14, alpha = 1) +
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color, # fish
               x = 5, y = 26, height = 9, alpha = 1) +
  theme_tidybayes() +
  theme(legend.position = "inside",
        legend.justification = c(0.6,1.1),
        legend.direction = "horizontal",
        legend.key.height = unit(9,  "pt"),
        legend.key.width = unit(9, "pt"),
        plot.margin = margin(t = 3, l = 2, r = 2, unit = "line"))





p_bar <- ggplot(risk_data_plot, aes(x = group, y = exposed_median, fill = threshold)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = exposed_min, ymax = exposed_max), width = 0.2, position = position_dodge(width = 0.7),
                linewidth = 0.4) +
  scale_fill_viridis_d(option = "F", begin = 0.3, end = 0.8, direction = -1, name = "") +
  labs(x = "", y = "Species at risk of\nsevere exposure (%)") +
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color, # reptiles
               x = 4, y = 61, height = 15, alpha = 1) +
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color, # amphibians
               x = 1, y = 73, height = 13, alpha = 1, horizontal = T) +
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color, # birds
               x = 2, y = 33, height = 14, alpha = 1) +
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color, # mammals
               x = 3, y = 42, height = 14, alpha = 1) +
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color, # fish
               x = 5, y = 26, height = 9, alpha = 1) +
  theme_tidybayes() +
  theme(legend.position = "inside",
        legend.justification = c(0.6,1.1),
        legend.direction = "horizontal",
        legend.key.height = unit(9,  "pt"),
        legend.key.width = unit(9, "pt"),
        plot.margin = margin(t = 3, l = 2, r = 2, unit = "line"))



# p <- plot_grid(p_bar, p_map,
#           ncol = 1, 
#           rel_heights = c(1, 3),
#           rel_widths = c(0.8, 1),
#           labels = c("a", "b"),
#           label_size = 12,
#           label_x = 0.05,
#           label_y = 0.95)

ggsave(here("figures/Fig_01_bar.jpg"),
       p_bar,
       width = 20, height = 11, units = "cm", dpi = 500)



# panel c ----

range_data_land <- c(amph, bird, mamm, rept)
range_data_ocean <- fish

length_land <- sapply(range_data_land, function(x) length(x))
length_ocean <- sapply(range_data_ocean, function(x) length(x))

range_data_land <- tibble(species = rep(names(length_land), times = length_land),
                          world_id = unlist(range_data_land))

range_data_ocean <- tibble(species = rep(names(length_ocean), times = length_ocean),
                           world_id = unlist(range_data_ocean))



risk_land <- map(c("2", "peak", "-2"), ~ {
  
  risk_spp <- data_median_gwl %>% 
    filter(group != "Fishes",
           threshold == .x,
           range_exposed >= 0.8) %>% 
    pull(species)
  
  range_data_land %>% 
    filter(species %in% risk_spp) %>% 
    count(world_id, name = "spp_risk") %>% 
    mutate(threshold = .x)
  
}) %>% 
  bind_rows() %>% 
  pivot_wider(names_from = threshold, values_from = spp_risk) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  mutate(diff = `-2` - `2`)

risk_ocean <- map(c("2", "peak", "-2"), ~ {
  
  risk_spp <- data_plot %>% 
    filter(group == "Fishes",
           threshold == .x,
           range_exposed >= 0.8) %>% 
    pull(species)
  
  
  range_data_ocean %>% 
    filter(species %in% risk_spp) %>% 
    count(world_id, name = "spp_risk") %>% 
    mutate(threshold = .x)
  
}) %>% 
  bind_rows() %>% 
  pivot_wider(names_from = threshold, values_from = spp_risk) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  mutate(diff = `-2` - `2`)

grid_land <- readRDS(here("raw_data/spatial_data/terrestrial_grid_robin.rds")) %>% 
  left_join(risk_land, by = c("WorldID" = "world_id")) %>% 
  pivot_longer(cols = c("2", "peak", "-2"), names_to = "threshold") %>% 
  mutate(threshold = fct_relevel(threshold, "2", "peak", "-2"))

grid_ocean <- readRDS(here("raw_data/spatial_data/ocean_grid_robin.rds")) %>% 
  left_join(risk_ocean, by = c("WorldID" = "world_id")) %>% 
  pivot_longer(cols = c("2", "peak", "-2"), names_to = "threshold") %>% 
  mutate(threshold = fct_relevel(threshold, "2", "peak", "-2"))



max_break <- max(risk_land$peak)
min(risk_land$peak)

bks <- c(1,5,10,15,20,30,40,50,100,150,max_break)

ggplot() +
  geom_sf(data = bound, colour = NA, fill = lighten("#04061a", 0.3)) +
  geom_sf(data = countries, colour = NA, fill = lighten("#04061a", 0.05)) +
  geom_sf(data = grid_ocean, aes(fill = value), colour = NA, show.legend = T) +
  geom_sf(data = grid_land, aes(fill = value), colour = NA, show.legend = T) +
  geom_sf(data = countries, colour = alpha("white", 0.5), fill = NA,  linewidth = 0.25) +
  scale_fill_viridis_b(option = "F", direction = 1, breaks = bks, 
                       name = "Number of species at risk of severe exposure",
                       end = 1, begin = 0.25, na.value = NA, limits = range(bks)) +
  theme_map() +
  theme(legend.position = "bottom",
        legend.justification = "centre",
        legend.direction = "horizontal",
        legend.title = element_text(size = 11.5),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.4, 'lines'))) +
  facet_wrap(~threshold, ncol = 3)

bks_diff <- c(-1, -0.1, 1, 5, 10, 20, 30, 40, 50, max(c(grid_ocean$diff, grid_land$diff), na.rm=T))
pal <- c("green", viridis::viridis(length(bks_diff) - 2, option = "A", begin = 0.25, direction = 1))

pal <- c(viridis::viridis(length(bks_diff) - 2, option = "A", begin = 0.25, direction = 1))
bks_diff <- c(2,5, 10, 15, 25, 50, 100, max(c(grid_ocean$diff, grid_land$diff), na.rm=T))

ggplot() +
  geom_sf(data = bound, colour = NA, fill = lighten("#09081b", 0.3)) +
  geom_sf(data = countries, colour = NA, fill = lighten("#09081b", 0.04)) +
  geom_sf(data = grid_ocean %>% 
            filter(threshold == "peak",
                   diff > 1), 
          aes(fill = diff), colour = NA, show.legend = T) +
  geom_sf(data = grid_land %>% 
            filter(threshold == "peak",
                   diff > 1), 
          aes(fill = diff), colour = NA, show.legend = T) +
  geom_sf(data = countries, colour = alpha("white", 0.5), fill = NA,  linewidth = 0.25) +
  scale_fill_viridis_b(option = "A", breaks = bks_diff, begin = 0.25) +
  theme_map() +
  theme(legend.position = "bottom",
        legend.justification = "centre",
        legend.direction = "horizontal",
        # plot.margin = margin(t = 3, b = 1, r = 1.5, l = 1.5, unit = "cm"),
        legend.title = element_text(size = 11.5),
        legend.text = element_text(size = 10)) &
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.4, 'lines')))




bks_diff <- c(-1, -0.1, 1, 5, 10, 20, 30, 40, 50, max(c(grid_ocean$diff, grid_land$diff), na.rm=T))
bks_diff <- c(2, 5, 10, 20, 30, 40, 50, 100, max(c(grid_ocean$diff, grid_land$diff), na.rm=T))

# pal <- c("blue", scico(length(bks_diff) - 2, palette = "vik", begin = 0.55, direction = 1))

ggplot() +
  geom_sf(data = bound, colour = NA, fill = lighten("#1e0a25", 0.5)) +
  geom_sf(data = countries, colour = NA, fill = lighten("#1e0a25", 0.05)) +
  geom_sf(data = grid_ocean, aes(fill = diff), colour = NA, show.legend = T) +
  geom_sf(data = grid_land, aes(fill = diff), colour = NA, show.legend = T) +
  geom_sf(data = countries, colour = alpha("white", 0.5), fill = NA,  linewidth = 0.25) +
  scale_fill_viridis_b(option = "H", breaks = bks_diff, begin = 0.2,
                       name = "Difference between number of") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.justification = "centre",
        legend.direction = "horizontal",
        # plot.margin = margin(t = 3, b = 1, r = 1.5, l = 1.5, unit = "cm"),
        legend.title = element_text(size = 11.5),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.4, 'lines')))







p <- wrap_plots(p1[1:3]) +
  wrap_plots(p2[1:3]) +
  wrap_plots(pmap[1:3]) +
  plot_layout(guides = "collect", nrow = 3) 

maps <-   wrap_plots(pmap[[1]]) + wrap_plots(pmap[[2]]) + wrap_plots(pmap[[3]]) +
  plot_layout(guides = "collect", ncol = 3) &
  theme(legend.position = "bottom",
        legend.justification = "centre",
        legend.direction = "horizontal",
        plot.margin = margin(t = 0, b = 0, r = 0, l = 0),
        legend.title = element_text(size = 11.5),
        legend.text = element_text(size = 10)) &
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.4, 'lines')))

maps

p <- (wrap_plots(p1[[1]]) + wrap_plots(p1[[2]]) + wrap_plots(p1[[3]]) + 
        wrap_plots(p2[[1]]) + wrap_plots(p2[[2]]) + wrap_plots(p2[[3]]) + 
        plot_layout(ncol = 3)) /
  maps 

ggsave(here("figures/Fig_01_new.jpg"),
       p,
       width = 22, height = 21, units = "cm", dpi = 500)


