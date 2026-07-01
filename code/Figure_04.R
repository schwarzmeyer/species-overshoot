source("code/00_packages.R")

# this script calculates the proportion of range exposed and the duration of exposure
# the script is also divided in two parts:
# one calculate risk using the whole data time series.
# the other spliting the data at different global warming levels.



# load data
files <- list.files(here("results/species_exposure_times"), full.names = TRUE, rec = FALSE, pattern = ".rds")
ranges <- list.files(here("processed_data/species_data/range_maps_grid_cells/"), full.names = TRUE, rec = FALSE, pattern = ".rds") |> 
  map_dfr(readRDS) |> 
  group_by(species, world_id) |> 
  summarise(range_proportion = sum(range_proportion),
            coverage_fraction = sum(coverage_fraction),
            .groups = "drop") 

os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds")) 

exposure_data <- files |> 
  map_dfr(readRDS) |> 
  left_join(os |> select(-duration), by = "model") |> 
  left_join(ranges, by = c("species", "world_id"), relationship = "many-to-many") |>
  select(model, group, species, world_id, begin_os, end_os, exposure, deexposure, duration, range_proportion) |> 
  filter(exposure < end_os) |> # we only keep exposures that start before the end of overshoot
  mutate(range_proportion = round(range_proportion, 4)) |> 
  mutate(phase = ifelse(exposure < begin_os, "before_os", "during_os")) 


# range per grid cell (deduplicated)
range_summary <- exposure_data |>
  distinct(model, group, species, world_id, phase, range_proportion) |> # this step is to deduplicate grid cells that are exposed multiple times
  group_by(model, group, species, phase) |> 
  summarise(range_exposed = sum(range_proportion), .groups = "drop") |>
  pivot_wider(names_from = phase, values_from = range_exposed, values_fill = 0) |>
  rename(range_exp_before = before_os,
         range_exp_during = during_os)


# duration (all events summed)
duration_summary <- exposure_data |>
  mutate(duration = case_when(phase == "before_os" ~ begin_os - exposure,
                              phase == "during_os" & deexposure < end_os ~ deexposure - exposure,
                              phase == "during_os" & deexposure >= end_os ~ end_os - exposure)) |>
  group_by(model, group, species, world_id, range_proportion, phase) |>
  summarise(total_duration = sum(duration, na.rm = TRUE), .groups = "drop") |>
  group_by(model, group, species, phase) |>
  summarise(weighted_duration = weighted.mean(total_duration, range_proportion, na.rm = TRUE),
            .groups = "drop") |>
  pivot_wider(names_from = phase, values_from = weighted_duration, values_fill = 0) |>
  rename(duration_before = before_os,
         duration_during = during_os)

# still exposed after os (deduplicated)
still_exposed_summary <- exposure_data |> 
  # filter(species == "Phyzelaphryne miriamae", model == "IPSL-CM6A-LR") |> 
  filter(phase == "during_os") |> 
  distinct(model, group, species, world_id, phase, range_proportion, .keep_all = TRUE) |> # this step is to deduplicate grid cells that are exposed multiple times
  mutate(status_end_os = ifelse(deexposure < end_os, "deexposed", "still_exposed")) |>
  group_by(model, group, species, status_end_os) |>
  summarise(range_exposed = sum(range_proportion), 
            .groups = "drop") |> 
  pivot_wider(names_from = status_end_os, values_from = range_exposed, values_fill = 0) |> 
  mutate(range_exposure = deexposed + still_exposed,
         reversibility = ifelse(range_exposure > 0, 1 - still_exposed / range_exposure, NA)) |> 
  select(-range_exposure)


# join all together
exposure_summary <- range_summary |>
  left_join(duration_summary, by = c("model", "group", "species")) |>
  left_join(still_exposed_summary, by = c("model", "group", "species")) 


all_species <- ranges |>
  distinct(species) |>
  left_join(exposure_summary |> distinct(species, group), by = "species")


# get all model/species combinations
all_combinations <- expand_grid(
  model = unique(exposure_summary$model),
  species = unique(all_species$species)) |>
  left_join(all_species, by = "species")



amph <- read_csv2(here("processed_data/species_data/attribute_tables/Amphibians.csv")) |> 
  select(sci_name, category, SHAPE_Area) |> 
  rename(range_size = SHAPE_Area)

birds_cat <- read_csv("/Users/andreasschwarzmeyer/Downloads/species-filter-results/species-filter-results.csv") |> 
  select(`Scientific name`, `RL Category`) |> 
  rename(sci_name = `Scientific name`,
          category = `RL Category`)

bird <- read_csv2(here("processed_data/species_data/attribute_tables/Birds.csv")) |> 
  select(sci_name, Shape_Area) |> 
  rename(range_size = Shape_Area) |> 
  left_join(birds_cat, by = "sci_name") |> 
  na.omit() 
  

mamm <- read_csv2(here("processed_data/species_data/attribute_tables/Mammals.csv")) |> 
  select(sci_name, category, SHAPE_Area) |> 
  rename(range_size = SHAPE_Area)

rept <- read_csv2(here("processed_data/species_data/attribute_tables/Reptiles.csv")) |> 
  select(sci_name, category, SHAPE_Area) |> 
  rename(range_size = SHAPE_Area)

fish <- read_csv2(here("processed_data/species_data/attribute_tables/Fishes.csv")) |> 
  select(sci_name, category, Shape_Area) |> 
  rename(range_size = Shape_Area)


df <- bind_rows(amph, bird, mamm, rept, fish) |> 
  group_by(sci_name, category) |>
  summarise(range_size = sum(range_size), .groups = "drop") |> 
  filter(category %in% c("CR","EN","LC","VU","NT","DD")) |> 
  mutate(category = factor(category,
                           levels = c("LC","NT","VU","EN","CR","DD"))) 


exposure_summary_complete <- all_combinations |>
  left_join(exposure_summary, by = c("model", "species", "group")) |>
  replace_na(list(range_exp_before = 0,
                  range_exp_during = 0,
                  duration_before = 0,
                  duration_during = 0,
                  range_still_exposed = 0)) |> 
  left_join(df, by = c("species" = "sci_name"), relationship = "many-to-many") |> 
  drop_na(category) 



cat_colours <- c("LC" = "#50bc1e", "NT" = "#06b491", "VU" = "#f9e814",
                 "EN" = "#f4900e", "CR" = "#D72104", "DD" = "grey60")
cat_fills <- lighten(cat_colours, amount = 0.3)
cat_labels <- c("LC" = "Least concern", "NT" = "Near threatened", 
                "VU" = "Vulnerable", "EN" = "Endangered",
                "CR" = "Critically endangered", "DD" = "Data deficient")

# shared theme
my_theme <- theme_tidybayes() +
  theme(axis.text.x = element_markdown(size = 9, face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.1),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 10, face = "bold", vjust = 2),
        plot.margin = margin(t = 1.5, unit = "line"),
        legend.position = "none")

x_scale <- scale_x_discrete(
  labels = function(x) glue::glue("<span style='color:{cat_colours[x]}'>{x}</span>"))

# panel a - range exposed before OS
p_before <- exposure_summary_complete |>
  filter(category %in% c("LC", "NT", "VU", "EN", "CR", "DD")) |>
  ggplot(aes(x = category, y = range_exp_before, 
             fill = category, colour = category)) +
  geom_violin(scale = "width", adjust = 1.2, linewidth = 0, alpha = 0.4) +
  geom_boxplot(outliers = FALSE, fill = NA, width = 0.2,
               show.legend = FALSE, colour = alpha("black", 0.5), linewidth = 0.3) +
  scale_fill_manual(values = cat_fills) +
  scale_colour_manual(values = cat_colours) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0), limits = c(0, 1.05)) +
  x_scale +
  labs(y = "Range exposed", 
       title = "Range exposed before the 2°C overshoot starts") +
  my_theme

# panel b - range exposed during OS
p_during <- exposure_summary_complete |>
  filter(category %in% c("LC", "NT", "VU", "EN", "CR", "DD")) |>
  ggplot(aes(x = category, y = range_exp_during,
             fill = category, colour = category)) +
  geom_violin(scale = "width", adjust = 1.2, linewidth = 0, alpha = 0.4) +
  geom_boxplot(outliers = FALSE, fill = NA, width = 0.2,
               show.legend = FALSE, colour = alpha("black", 0.5), linewidth = 0.3) +
  scale_fill_manual(values = cat_fills) +
  scale_colour_manual(values = cat_colours) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0), limits = c(0, 1.05)) +
  x_scale +
  labs(y = "Range exposed",
       title = "Range newly exposed during the 2°C overshoot") +
  my_theme

# panel c - reversibility
p_rev <- exposure_summary_complete |>
  filter(category %in% c("LC", "NT", "VU", "EN", "CR", "DD"),
         range_exp_during > 0) |>
  ggplot(aes(x = category, y = reversibility,
             fill = category, colour = category)) +
  geom_violin(scale = "width", adjust = 1.2, linewidth = 0, alpha = 0.4) +
  geom_boxplot(outliers = FALSE, fill = NA, width = 0.2,
               show.legend = FALSE, colour = alpha("black", 0.5), linewidth = 0.3) +
  scale_fill_manual(values = cat_fills) +
  scale_colour_manual(values = cat_colours) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0), limits = c(0, 1.05)) +
  x_scale +
  labs(y = "Reversibility of exposure",
       x = "IUCN Red List category",
       title = "Reversibility of exposure by end of overshoot") +
  my_theme +
  theme(axis.title.x = element_text(size = 10, vjust = -1))

p <- p_before / p_during / p_rev +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 12, face = "bold"))



ggsave(here("figures/Figure_04.jpg"), 
       p, 
       width = 4, height = 8, dpi = 800)


# Kruskal test