# FIGURE 2
source("code/00_packages.R")
source("code/functions/tibble_to_raster.R")

groups <- c("Amphibians","Birds","Mammals","Reptiles","Fishes")

###### Load data  -----------------
risk_data <- readRDS(here("results/risk/risk_thresholds_models_v2.rds")) 

dat <- list.files(here("results/species_exposure_times/"), pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS) |> 
  select(species, world_id, model) |> 
  distinct() |> 
  group_by(species, world_id) |> 
  count() |> 
  ungroup()

range_data <- list.files(here("processed_data/species_data/range_maps_grid_cells/"), full.names = TRUE) |> 
  map_dfr(readRDS) |> 
  left_join(dat, by = c("species", "world_id")) |> 
  replace_na(list(n = 0))

os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds"))
world <- ne_countries(returnclass = "sf")
bbox <- readRDS(here("raw_data/random/bbox.rds"))

###### Calculate no. of species per group  -----------------

group_richness <- range_data |> 
  group_by(group) |> 
  mutate(richness = n_distinct(species)) |> 
  ungroup() |> 
  select(group, richness) |> 
  distinct() |> 
  add_row(group = "All species",
          richness = length(unique(range_data$species)))




###### Prepare data for plot "a"  -----------------
# This plot shows the proportion of species with >= 80% of the range exposed at 2°C before peak, peak, and 2°C after peak
plot_data_a <- risk_data |>
  bind_rows(risk_data |>
              mutate(group = factor("All species"))) |> 
  left_join(group_richness, by = c("group")) |> 
  filter(threshold %in% c("2w", "peak", "2c"),
         range_exposed >= 0.8) |>
  group_by(model, group, threshold) |> 
  count(name = "exposed") |> 
  group_by(group, threshold) |> 
  summarise(exposed_median = median(exposed),
            exposed_max = max(exposed),
            exposed_min = min(exposed),
            .groups = "drop") |> 
  left_join(group_richness, by = "group") |> 
  mutate(exposed_median = exposed_median / richness * 100,
         exposed_max = exposed_max / richness * 100,
         exposed_min = exposed_min / richness * 100) |> 
  select(-richness) |> 
  mutate(group = factor(group,
                        levels = c("All species", "Amphibians", "Reptiles", "Mammals", "Birds", "Fishes")),
         threshold = fct_recode(threshold, "When 2°C overshoot starts" = "2w", "Peak temperature" = "peak", "When 2°C overshoot ends" = "2c")) 



###### Prepare data for plot "b"  -----------------
# Map showing the difference in the number of species exposed at 2°C before and after peak

plot_data_b <- risk_data |>
  filter(threshold %in% c("2w", "peak", "2c")) |> 
  group_by(species, threshold) |> 
  summarise(range_exposed = median(range_exposed),
            weighted_duration = median(weighted_duration),
            .groups = "drop") |> 
  filter(range_exposed >= 0.8)


species_2w <- plot_data_b |> filter(threshold == "2w") |> pull(species)
species_peak <- plot_data_b |> filter(threshold == "peak") |> pull(species)
species_2c <- plot_data_b |> filter(threshold == "2c") |> pull(species)


risk_2w <- range_data |> 
  filter(species %in% species_2w) |> 
  filter(n > 2) |> # only grid cells where at least 3 models predicted exposure
  group_by(world_id) |> 
  count() |> 
  ungroup()


risk_2c <- range_data |> 
  filter(species %in% species_2c) |> 
  filter(n > 2) |> # only grid cells where at least 3 models predicted exposure
  group_by(world_id) |> 
  count() |> 
  ungroup()


risk_diff <- risk_2w |> 
  full_join(risk_2c, by = "world_id") |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         diff =  n.y - n.x) |>
  mutate(diff = round(diff, 0))


raster_diff <- tibble_to_raster(risk_diff, "diff") 


###### Plots -----------------

phylo_color <- alpha("#811c51", 0.8)

pal_a <- c("#c54673", "#715C80", "#4298c1")



phylopics <- data.frame(x = 1,
                        y = c(68, 57, 38, 26, 12),
                        group =  factor(c("Amphibians", "Reptiles", "Mammals", "Birds", "Fishes")),
                        uuid = c("bd80bc51-460c-4dd9-8341-e5b460372efb", # amphibians
                                 "264fa655-afd7-451c-8f27-e0a9557376e6", # reptiles
                                 "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", # mammals
                                 "157d3109-7124-413c-8362-3abcc6889a3f", # birds
                                 "c90aa49b-d9c5-44a4-a709-4f8d9a33b559")) # fish


plot_a <- plot_data_a |> 
  ggplot(aes(x = "", 
             y = exposed_median, 
             fill = threshold)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = exposed_min, ymax = exposed_max), width = 0, position = position_dodge(width = 0.7),
                linewidth = 0.4, colour = "grey10") +
  # scale_fill_viridis_d(option = "A", begin = 0.34, end = 0.8, direction = -1, name = "") +
  scale_fill_manual(values = pal_a, name = "") +
  labs(x = "", y = "Species at risk of\nsevere exposure") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75), labels = function(x) glue("{x}%")) +
  geom_phylopic(data = phylopics, aes(x = x, y = y, uuid = uuid),
                fill = phylo_color,
                height = c(7, 9, 9, 9, 5.5), alpha = 1, inherit.aes = FALSE) +
  theme_tidybayes() +
  facet_wrap(~group, nrow = 1) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, -0.1),
        legend.direction = "horizontal",
        legend.key.height = unit(10,  "pt"),
        legend.key.width = unit(10, "pt"),
        legend.key.spacing.y = unit(4, "pt"),
        legend.text = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 11),
        panel.grid.major.y = element_line(linewidth = 0.13),
        panel.grid.minor.y = element_line(linewidth = 0.13),
        strip.text = element_text(size = 9, face = "bold", colour = "white"),
        strip.background = element_rect(fill = "gray62"),
        plot.margin = margin(t = 2, l = 1.3, r = 3, b = 1, unit = "line")) +
  theme(legend.justification = c(0.9, 1.05))




# raster_diff[raster_diff == 0] <- NA


zeropal <- 0.0001
dbks <- c(-8, -5,-1, -zeropal, zeropal, 1, 5, 10, 20, 40, 62)

vals <- c(0,                                                    # -10 (lower limit)
          scales::rescale(-5, from = range(dbks)),               # -5
          scales::rescale(-1, from = range(dbks)),               # -1
          scales::rescale(-zeropal, from = range(dbks)),                # 0
          scales::rescale(zeropal, from = range(dbks)),                # 0
          
          scales::rescale(1, from = range(dbks)),                # 1
          scales::rescale(5, from = range(dbks)),                # 5
          scales::rescale(10, from = range(dbks)),               # 10
          scales::rescale(20, from = range(dbks)),   
          scales::rescale(40, from = range(dbks)),               # 20
          # 20
          1)   


my_pal <- c(
  "#861e5b",  # -10 to -5
  "#d4347a",  # -5 to -1
  "#F15F86",  # -1 to -0.0001
  "grey75",    # -0.0001 to 0.0001
  "#b8e8f0",  # 0 to 1
  "#56cfe1",  # 1 to 5
  "#2196c4",  # 5 to 10
  "#1565a0",  # 10 to 20
  "#004C94",   # 20 to 40
  "#05254b"   # 41 to 60
  
)




plot_b <- ggplot() +
  geom_sf(data = bbox, fill = alpha("grey90", 0.25), colour = NA) +
  geom_sf(data = world, fill = "white", colour = NA) +
  geom_spatraster(data = raster_diff, aes(fill = diff)) +
  geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) +
  scale_fill_stepsn(
    colours = my_pal,
    breaks = dbks,
    values = vals,
    labels = c(c(-8, -5,-1, "", "0          ", 1, 5, 10, 20, 40, 62)),
    limits = range(dbks),
    na.value = NA,
    name = "Change in the number species at risk of severe exposure\nbefore and after 2°C overshoot") +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.5, -0.1),
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_text(size = 11.5, vjust = 1),
        legend.text = element_text(size = 10),
        plot.margin = margin(b = 4, t = 1, unit = "line")) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = 0.5,
                                 barwidth = unit(19, 'lines'), barheight = unit(0.5, 'lines'))); plot_b




p <- plot_grid(plot_a, plot_b, 
               ncol = 1, 
               rel_heights = c(1, 1.7), 
               align = "none",
               labels = c("a", "b"))



ggsave(here("figures/Figure_02.jpg"),
       p,
       width = 18, height = 20, units = "cm", dpi = 800)



# Stats

risk_stats <- risk_data |>
  bind_rows(risk_data |>
              mutate(group = factor("All species"))) |> 
  left_join(group_richness, by = c("group")) |> 
  filter(threshold %in% c("2w", "peak", "2c"),
         range_exposed >= 0.8) |>
  group_by(model, group, threshold) |> 
  count(name = "exposed") |> 
  pivot_wider(names_from = threshold, values_from = exposed) |> 
  mutate(diff = `2c` - `2w`) |>
  pivot_longer(cols = -c(model, group), names_to = "threshold", values_to = "exposed") |>
  group_by(group, threshold) |>
  summarise(exposed_median = median(exposed),
            exposed_min = min(exposed),
            exposed_max = max(exposed),
            .groups = "drop") |> 
  left_join(group_richness, by = "group") |>
  mutate(exposed_median = exposed_median / richness,
         exposed_max = exposed_max / richness,
         exposed_min = exposed_min / richness) |> 
  mutate(across(c(exposed_median, exposed_max, exposed_min), ~ percent(.x, accuracy = 1))) |>
  mutate(threshold = fct_recode(threshold, "2°C overshoot begins" = "2w", "Peak temperature" = "peak", "2°C overshoot ends" = "2c")) 


risk_data |>
  bind_rows(risk_data |>
              mutate(group = factor("All species"))) |> 
  filter(threshold %in% c("2w", "peak", "2c")) |> 
  group_by(model, group, threshold) |> 
  summarise(range_exposed = mean(range_exposed),
            .groups = "drop") |> 
  pivot_wider(names_from = threshold, values_from = range_exposed) |> 
  mutate(diff = `2c` - `2w`) |> 
  group_by(group) |> 
  summarise(median = median(diff),
            min = min(diff),
            max = max(diff)) |> 
  mutate(across(where(is.numeric), ~scales::percent(.x, accuracy = 1)))

