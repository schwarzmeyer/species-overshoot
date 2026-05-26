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

plot_a_data <- risk_data |> 
  group_by(model, threshold) |> 
  summarise(p_20 = sum(range_exposed >= 0.2),
            p_50 = sum(range_exposed >= 0.5),
            p_80 = sum(range_exposed >= 0.8),
            .groups = "drop") |> 
  pivot_longer(cols = c(p_20, p_50, p_80),
               names_to = "range_exposed",
               values_to = "n_species") |> 
  mutate(range_exposed = factor(range_exposed, 
                                levels = c("p_20", "p_50", "p_80"),
                                labels = c("≥20%", "≥50%", "≥80%")),
         perc_species = n_species / 37073,
         threshold = factor(threshold, levels = c(paste0(seq(1.5, 3, 0.1), "w"),
                                                  "peak",
                                                  paste0(seq(3, 1.5, -0.1), "c")))) |> 
  left_join(gwl_thresholds |> select(model, threshold, gwl), by = c("model", "threshold")) |> 
  group_by(model, range_exposed) |>
  filter(!(threshold != "peak" & gwl == gwl[threshold == "peak"] & str_detect(threshold, "c"))) |>
  ungroup() |>
  group_by(model, range_exposed) |>
  mutate(step = row_number()) |>
  ungroup()

models <- unique(plot_a_data$model)

model_labels <- list(
  "ACCESS-ESM1-5" = c("1.5", "2", "peak"),
  "CNRM-ESM2-1"   = c("1.5", "2", "peak"),
  "GISS-E2-1-G"   = c("1.5", "2", "peak"),
  "IPSL-CM6A-LR"  = c("1.5", "2", "2.5", "peak"),
  "MRI-ESM2-0"    = c("1.5", "2", "peak")
)



x_scales <- imap(models, ~{
  sub <- plot_a_data |> 
    filter(model == .x, range_exposed == "≥20%")
  
  peak_temp <- sub |> 
    filter(threshold == "peak") |> 
    pull(gwl) |> 
    unique() |> 
    round(1) |> 
    as.character()
  
  raw_labels <- unique(sub$threshold) |>
    as.character() |>
    str_replace("w", "") |>
    str_replace("c", "")
  
  keep_labels <- model_labels[[.x]]
  display_labels <- ifelse(raw_labels %in% keep_labels, raw_labels, "")
  label_vec <- setNames(display_labels, unique(as.character(sub$threshold)))
  label_vec[which(names(label_vec) == "peak")] <- peak_temp
  
  keep_steps <- sub$step[sub$threshold %in% names(label_vec[label_vec != ""])]
  keep_labels_display <- label_vec[label_vec != ""]
  
  scale_x_continuous(breaks = keep_steps,
                     labels = keep_labels_display)
})


############# plot b -----


risk_summary <- risk_data |> 
  bind_rows(risk_data |> mutate(group = "All species")) |>
  group_by(model, group, threshold) |> 
  summarise(p_20 = sum(range_exposed >= 0.2),
            p_50 = sum(range_exposed >= 0.5),
            p_80 = sum(range_exposed >= 0.8),
            .groups = "drop") |> 
  arrange(model, group, threshold) |> 
  left_join(gwl_thresholds |> select(model, threshold, gwl), by = c("model", "threshold")) |> 
  group_by(model, group) |>
  mutate(rate_20 = p_20 - lag(p_20),
         rate_50 = p_50 - lag(p_50),
         rate_80 = p_80 - lag(p_80),
         gwl_diff = abs(gwl - lag(gwl))) |>
  ungroup() |>
  mutate(rate_20_std = rate_20 * (0.1 / gwl_diff), 
         rate_50_std = rate_50 * (0.1 / gwl_diff), 
         rate_80_std = rate_80 * (0.1 / gwl_diff)) |> 
  left_join(group_richness, by = "group") |> 
  # select(model, threshold, gwl, rate_20_std, rate_50_std, rate_80_std) |> 
  mutate(perc_20_std = rate_20_std / richness,
         perc_50_std = rate_50_std / richness,
         perc_80_std = rate_80_std / richness)



risk_rate <- risk_summary |> 
  mutate(phase = case_when(
    str_detect(threshold, "w") ~ "Warming",
    str_detect(threshold, "p") ~ "Warming",
    str_detect(threshold, "c") ~ "Cooling")) |>
  pivot_longer(cols = c(perc_20_std, perc_50_std, perc_80_std),
               names_to = "range_exposed",
               values_to = "rate") |>
  select(model, group, threshold, range_exposed, rate, phase) |> 
  filter(!is.infinite(rate),
         !is.na(rate)) 


risk_rate_combined <- bind_rows(
  
  risk_rate |> 
    mutate(dataset = "Full overshoot"),
  
  risk_rate |> 
    filter(str_detect(threshold, "2") | str_detect(threshold, "peak"), !str_detect(threshold, "^2w$")) |> 
    mutate(dataset = "2°C overshoot")
  
) 




plot_b_data <- risk_rate_combined |>
  group_by(dataset, model, group, range_exposed, phase) |>
  summarise(avg = mean(rate),
            .groups = "drop") |> 
  mutate(range_exposed = case_when(
    range_exposed == "perc_20_std" ~ "≥20%",
    range_exposed == "perc_50_std" ~ "≥50%",
    range_exposed == "perc_80_std" ~ "≥80%"),
    phase = factor(phase, levels = c("Warming", "Cooling"))) |> 
  mutate(group = factor(group, levels = c("All species", "Amphibians", "Reptiles", "Mammals", "Birds", "Fishes")),
         realm = ifelse(group == "Fishes", "Marine", "Terrestrial"),
         realm = factor(realm, levels = c("Terrestrial", "Marine"))) |> 
  group_by(dataset, range_exposed, group, phase) |>
  summarise(median_avg = median(avg),
            min_avg = min(avg),
            max_avg = max(avg),
            .groups = "drop")


####### plots -------

viridis_option <- "A"
viridis_begin <- 0.4
viridis_end <- 0.75

plot_a <- plot_a_data |>
  ggplot(aes(x = step, y = perc_species, group = range_exposed, colour = range_exposed)) +
  geom_segment(data = plot_a_data |> filter(threshold == "peak") |> group_by(model) |> slice(1),
               aes(x = step, xend = step, y = 0, yend = 0.64),
               linetype = "dashed", colour = "black", linewidth = 0.2, inherit.aes = FALSE) +
  geom_line() +
  facet_wrap(~model, nrow = 1, scales = "free_x") +
  facetted_pos_scales(x = x_scales) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.7), expand = c(0, 0)) +
  scale_colour_viridis_d(option = viridis_option, begin = viridis_begin, end = viridis_end, name = "% of the geographic\nrange exposed") +
  labs(x = "Global warming level °C", y = "% of species exposed") +
  theme_tidybayes() +
  theme(plot.margin = margin(b = 1, r = 0, l = 0, t = 2, unit = "lines"),
        panel.grid.minor.y = element_line(linewidth = 0.25),
        panel.grid.major.y = element_line(linewidth = 0.25),
        axis.title.y = element_text(size = 10, vjust = 1),
        axis.title.x = element_text(size = 9, vjust = -1),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8.5, face = "bold", colour = "white"),
        strip.background = element_rect(fill = "gray62"),
        legend.position = "right",
        legend.title = element_text(size = 9, vjust = 1)); plot_a



plot_b <- plot_b_data  |>
  filter(dataset == "Full overshoot",
         range_exposed == "≥80%") |> 
  mutate(phase_num = ifelse(phase == "Warming", 1, 2.5)) |>  # adjust 2.5 for spacing
  ggplot(aes(x = phase_num, group = range_exposed, fill = phase)) +
  
  # geom_point(aes(y = median_avg), size = 2.7, position = position_dodge(width = 0.3)) +
  geom_col(aes(y = median_avg), position = position_dodge(width = 1), width = 0.9) +
  geom_errorbar(aes(ymin = min_avg, ymax = max_avg), linewidth = 0.6,
                width = 0, position = position_dodge(width = 1), 
                colour = "grey10") +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = c("#c54673", "#4298c1"), name = "",
                    labels = c("per 0.1°C warming","per 0.1°C cooling")) +
  scale_y_continuous(labels = percent_format(), limits = c(-0.025, 0.05), breaks = seq(-0.02, 0.05, 0.02)) +
  scale_x_continuous(breaks = c(1, 2.5),
                     limits = c(0.3, 3.2)) +
  labs(y = "Change in % of species exposed", x = "") +
  theme_tidybayes() +
  facet_wrap(~group, nrow = 1) +
  theme(plot.margin = margin(b = 1, r = 0, l = 2, t = 1, unit = "lines"),
        panel.grid.major.y = element_line(linewidth = 0.2),
        panel.grid.minor.y = element_line(linewidth = 0.2),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 11, vjust = 1),
        strip.text = element_text(size = 9, face = "bold", colour = "white"),
        strip.background = element_rect(fill = "gray62")); plot_b


p <- plot_a + plot_b + plot_layout(nrow = 2, heights = c(0.7, 1)) + 
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 11, face = "bold"), 
        axis.title.y = element_text(size = 10))


ggsave(filename = here("figures/Figure_01.jpg"), 
       p, 
       width = 8, height = 5.5, dpi = 800)


