data <- readRDS(here("results/gwl_exp_deexp/data.rds"))

data_median <- data %>% 
  filter(phase_2c_os == "os", 
         code != "never") %>%   
  group_by(group, species, world_id) %>% 
  summarise(gwl_diff = median(gwl_diff),
            .groups = "drop") %>% 
  mutate(world_id = as.numeric(world_id))

data %>% 
  group_by(model, phase_2c_os) %>% 
  summarise(n = n()) %>% 
  mutate(p = n/sum(n)) %>% 
  arrange(p)

# plot


p1 <- data %>% 
  filter(phase_2c_os == "os") %>%
  group_by(model, group) %>% 
  count(code) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  group_by(group, code) %>% 
  summarise(mid = median(perc),
            lower = min(perc),
            upper = max(perc), 
            .groups = "drop") %>% 
  mutate(group = factor(group, c("Mammals","Birds","Amphibians","Reptiles", "Fishes"))) %>%
  mutate(code = case_when(code == "less_cooling" ~ "Additional cooling not required",
                          code == "more_cooling" ~ "De-exposure required additional cooling",
                          code == "same_gwl" ~ "De-exposure at the same GWL",
                          code == "never" ~ "No de-exposure")) %>% 
  mutate(code = factor(code, c("No de-exposure", "Additional cooling not required", "De-exposure at the same GWL","De-exposure required additional cooling"))) %>%
  ggplot(aes(x = mid, y = group, fill = code)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.88) +
  scale_fill_manual(values = c("grey23","#b45420", "#c1c2c4", "#3882ab"), name = "") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, position = position_dodge(width = 0.88),
                colour = rep(darken(c("#7c1d05","#034c86", "grey33",  "#c1c2c4"), 0.4), 5), linewidth = 0.5) +
   coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0,0), limits = c(0,105), breaks = seq(0,100,20)) +
  labs(x = "Proportion of exposure events (%)", y = "") +
  theme_tidybayes() +
  theme(axis.line.y = element_blank(),
        axis.title.x = element_text(size = 10, vjust = 0.6),
        panel.grid.major.x = element_line(linewidth = 0.2),
        panel.grid.minor.x = element_line(linewidth = 0.2),
        legend.position = "inside",
        legend.position.inside = c(0.5,-0.3),
        legend.key.spacing.y = unit(2, "pt"),
        legend.key.height = unit(8, "pt"),
        legend.key.width = unit(6, "pt"),
        legend.text = element_text(size = 9),
        legend.background = element_blank(),
        plot.margin = margin(r = 25, t = 5, b = 40, unit = "pt")) +
  guides(fill = guide_legend(ncol = 2)); p1


phylo_x <- -1.19
phylo_color <- "#3882ab" 


p2 <- data_median %>% 
  mutate(group = factor(group, c("Mammals","Birds","Amphibians","Reptiles", "Fishes"))) %>%
  ggplot(aes(x = gwl_diff, y = group)) +
  stat_halfeye(
    aes(fill = after_stat(x)),
    fill_type = "gradient",
    adjust = 0.5,
    # width = 0,
    .width = 0,
    justification = -0.2,
    point_colour = NA,
    show.legend = F) +
  geom_boxplot(
    width = .25,
    outlier.shape = NA,
    show.legend = F,
    alpha = 0.4,
    linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color,
               x = phylo_x, y = 3.3, height = 0.6, alpha = 1, horizontal = T) +
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color,
               x = phylo_x, y = 2.3, height = 0.75, alpha = 1) +
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color,
               x = phylo_x, y = 1.3, height = 0.75, alpha = 1) +
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color,
               x = phylo_x, y = 4.3, height = 0.78, alpha = 1) +
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color,
               x = phylo_x, y = 5.3, height = 0.42, alpha = 1) +
  labs(y = "", 
       x = "Difference between global warming levels\nat exposure and de-exposure") +
  scale_fill_scico(midpoint = 0.02, palette = "vik") +
  coord_cartesian(clip = "off", ylim = c(1,5)) +
  scale_x_reverse(limits = c(0.5, -1.2), expand = c(0,0), 
                  breaks = seq(0.4, -1, by = -0.2)) +
  # scale_y_discrete(expand = c(0,0)) +
  theme_tidybayes() +
  theme(panel.grid.major.x = element_line(linewidth = 0.1),
        panel.grid.minor.x = element_line(linewidth = 0.1),
        axis.title.x = element_text(size = 10, vjust = -1),
        axis.text.x = element_text(size = 9.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(r = 30, t = 5, b = 5, unit = "pt"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        strip.background = element_blank()); p2


land_id <- data_median %>% 
  filter(group != "Fishes") %>% 
  count(world_id) %>% 
  filter(n > 4) %>% 
  pull(world_id)

ocean_id <- data_median %>% 
  filter(group == "Fishes") %>% 
  count(world_id) %>% 
  filter(n > 4) %>% 
  pull(world_id)

data_land <- data_median %>% 
  filter(group != "Fishes") %>% 
  group_by(world_id) %>% 
  summarise(median_diff = median(gwl_diff),
            .groups = "drop") %>% 
  filter(world_id %in% land_id)

data_ocean <- data_median %>% 
  filter(group == "Fishes") %>%
  group_by(world_id) %>% 
  summarise(median_diff = median(gwl_diff),
            .groups = "drop") %>% 
  filter(world_id %in% ocean_id)


data_median %>% 
  group_by(group) %>% 
  summarise(med = median(gwl_diff
                         ))
# load countries polygon
countries <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")


# create bounding box to plot around the maps
bound <- readRDS(here("raw_data/random/bbox.rds"))

grid_land <- readRDS(here("raw_data/spatial_data/terrestrial_grid_robin.rds"))
grid_ocean <- readRDS(here("raw_data/spatial_data/ocean_grid_robin.rds"))

grid_land_join <- left_join(grid_land, data_land, by = c("WorldID" = "world_id")) %>% 
  filter(!is.na(median_diff))

grid_ocean_join <- left_join(grid_ocean, data_ocean, by = c("WorldID" = "world_id")) %>% 
  filter(!is.na(median_diff))

min_lim <- min(c(data_land$median_diff, data_ocean$median_diff))
max_lim <- max(c(data_land$median_diff, data_ocean$median_diff))


pmap <- ggplot() +
  geom_sf(data = bound, colour = NA, fill = "grey94") +
  geom_sf(data = countries, colour = NA, fill = "grey82") +
  geom_sf(data = grid_land_join, aes(fill = median_diff), colour = NA) +
  geom_sf(data = grid_ocean_join, aes(fill = median_diff), colour = NA) +
  geom_sf(data = countries, colour = "grey23", fill = NA,  linewidth = 0.25) +
  scale_fill_scico(palette = "vik", 
                   values = scales::rescale(c(min_lim, 0, max_lim)),
                   breaks = round(seq(-1.2, 0.4, 0.2), 1),
                   name = "Difference between GWL at exposure and de-exposure (°C)") +
  theme_map() +
  theme(legend.position.inside = c(0.5,1.05),
        legend.justification = "centre",
        legend.direction = "horizontal",
        plot.margin = margin(t = 3.5, b = 0.5, unit = "line"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))  +
  guides(fill = guide_colorbar(title.position = 'top',
                               title.hjust = .5,
                               barwidth = unit(18, 'lines'),
                               barheight = unit(.4, 'lines'),
                               reverse = T))


pp <- (p1 + p2) / pmap + 
  plot_layout(heights = c(0.6, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 11, face = 2),
        plot.tag.position = c(0.03,1))


ggsave(here("figures/Fig_03.jpg"),
       pp,
       width = 22, height = 21, units = "cm", dpi = 700)


# 
# p <- plot_grid(p1, p2, scale = 0.95, labels = c("a", "b"), 
#                ncol = 2, align = "h", label_size = 10, label_x = 0.02, rel_heights = c(1,1))
# 
# ggsave(here("figures/Fig_04_os.jpg"),
#        p,
#        width = 20, height = 11, units = "cm", dpi = 700)

