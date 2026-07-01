# FIGURE 1 
source("code/00_packages.R")


# load data
risk_data <- readRDS(here("results/risk/risk_thresholds_models_v2.rds"))
gwl_thresholds <- readRDS(here("processed_data/climate_data/global_averages/gwl_thresholds.rds"))

group_richness <- list.files(here("processed_data/species_data/range_maps_grid_cells/"), full.names = TRUE) |> 
  map_dfr(readRDS) |> 
  group_by(group) |> 
  mutate(richness = n_distinct(species)) |> 
  ungroup() |> 
  select(group, richness) |> 
  distinct() 

group_richness <- group_richness |> 
  add_row(group = "All species",
          richness = sum(group_richness$richness))

############# plot a -----

plot_data <- risk_data |> 
  bind_rows(risk_data |> mutate(group = "All species")) |>
  group_by(group, model, threshold) |> 
  summarise(p_20 = sum(range_exposed >= 0.2),
            p_50 = sum(range_exposed >= 0.5),
            p_80 = sum(range_exposed >= 0.8),
            .groups = "drop") |> 
  pivot_longer(cols = c(p_20, p_50, p_80),
               names_to = "range_exposed",
               values_to = "n_species") |> 
  left_join(group_richness, by = "group") |>
  mutate(perc_species = n_species / richness,
         threshold = factor(threshold, levels = c(paste0(seq(1.5, 3, 0.1), "w"),
                                                  "peak",
                                                  paste0(seq(3, 1.5, -0.1), "c")))) |> 
  left_join(gwl_thresholds |> select(model, threshold, gwl), by = c("model", "threshold"))




# Define which models reach each cooling threshold
valid_models_by_threshold <- plot_data |>
  filter(str_detect(threshold, "c")) |>
  mutate(threshold_val = as.numeric(str_remove(threshold, "c"))) |>
  select(model, threshold_val) |>
  distinct()

# Now filter plot_a_data to only keep model x threshold combinations
# where the model has BOTH warming and cooling data for that threshold
plot_data_filtered <- plot_data |>
  mutate(threshold_val = as.numeric(str_remove(threshold, "[wc]"))) |> 
  inner_join(valid_models_by_threshold, by = c("model", "threshold_val")) |> 
  bind_rows(plot_data |> filter(threshold == "peak")) |> 
  mutate(range_exposed = factor(range_exposed, 
                                levels = c("p_20", "p_50", "p_80"),
                                labels = c("Range exposed ≥20%", "Range exposed ≥50%", "Range exposed ≥80%")),
         group = factor(group, 
                        levels = c("All species", "Amphibians", "Reptiles", "Mammals", "Birds", "Fishes"))) |> 
  filter(threshold %in% c(paste0(seq(1.5, 2, 0.1), "w"),
                          "peak",
                          paste0(seq(2, 1.5, -0.1), "c"))) |> 
  group_by(group, threshold, range_exposed) |>
  summarise(exposed_median = median(perc_species),
            exposed_max = max(perc_species),
            exposed_min = min(perc_species),
            .groups = "drop") |>
  mutate(threshold_val = case_when(str_detect(threshold, "w") ~ str_replace(threshold, "w", ""),
                                   str_detect(threshold, "c") ~ str_replace(threshold, "c", ""),
                                   threshold == "peak" ~ "peak")) 


plot_data_filtered |> 
  mutate(across(c(exposed_median, exposed_max, exposed_min), ~ percent(.x, accuracy = 1))) 

p <- plot_data_filtered |> 
  ggplot(aes(x = threshold, 
             y = exposed_median, 
             fill = threshold_val)) +
  geom_col(position = "dodge", width = 0.7, show.legend = FALSE) +
  geom_errorbar(aes(ymin = exposed_min, ymax = exposed_max), 
                width = 0, position = position_dodge(width = 0.7),
                linewidth = 0.3, colour = "grey10") +
  scale_fill_viridis_d(option = "C", end = 0.85, begin = 0.17, alpha = 0.85) +
  labs(x = "Global warming level °C", y = "Percentage of species exposed") +
  scale_y_continuous(expand = c(0,0), labels = percent_format()) +
  scale_x_discrete(labels = c(seq(1.5, 2, 0.1),
                              "Peak",
                              seq(2, 1.5, -0.1))) +
  theme_tidybayes() +
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
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7, angle = 0, vjust = 0.5),
        axis.title = element_text(size = 10),
        panel.grid.major.y = element_line(linewidth = 0.13),
        panel.grid.minor.y = element_line(linewidth = 0.13),
        panel.spacing.y = unit(1.2, "lines"),
        strip.text = element_text(size = 9, face = "bold", colour = "white"),
        strip.background = element_rect(fill = "gray62"),
        plot.margin = margin(b = 1, r = 1, t = 1.5, l = 1, unit = "line")) +
  theme(legend.justification = c(0.9, 1.05)) +
  facet_grid(group ~ range_exposed)

p

ggsave(filename = here("figures/ED_Figure_02.png"),
       p, 
       width = 8, height = 8, dpi = 800)
