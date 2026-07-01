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
  filter(category %in% c("LC", "NT", "VU", "EN", "CR", "DD"))

exposure_median <- exposure_summary_complete |> 
  group_by(group, species, category) |> 
  summarise(range_exp_before = median(range_exp_before, na.rm = TRUE),
            range_exp_during = median(range_exp_during, na.rm = TRUE),
            reversibility = median(reversibility, na.rm = TRUE),
            range_size = median(range_size),
            .groups = "drop") |> 
  mutate(log_range_size = log(range_size))

# range_exp_before
# range_exp_during
# reversibility


residuals_range_exp_before <- lm(range_exp_before ~ log_range_size, data = exposure_median) |> 
  residuals()

residuals_range_exp_during <- lm(range_exp_during ~ log_range_size, data = exposure_median) |> 
  residuals()

residuals_reversibility <- lm(reversibility ~ log_range_size, data = exposure_median) |> 
  residuals()


exposure_median$residuals_range_exp_before <- residuals_range_exp_before
exposure_median$residuals_range_exp_during <- residuals_range_exp_during
exposure_median$residuals_reversibility <- NA
exposure_median$residuals_reversibility[as.numeric(names(residuals_reversibility))] <- residuals_reversibility

kw_range_exp_before_residuals <- kruskal.test(residuals_range_exp_before ~ category, data = exposure_median)
kw_range_exp_during_residuals <- kruskal.test(residuals_range_exp_during ~ category, data = exposure_median)
kw_reversibility_residuals <- kruskal.test(residuals_reversibility ~ category, data = exposure_median)


epsilon_squared <- function(kw_result, n) {
  (kw_result$statistic - kw_result$parameter) / (n - kw_result$parameter)
}
n <- nrow(exposure_median)

epsilon_squared(kw_range_exp_before_residuals, n)
epsilon_squared(kw_range_exp_during_residuals, n)
epsilon_squared(kw_reversibility_residuals, n)

# pairwise dunn test

dunn_before <- dunn_test(exposure_median, residuals_range_exp_before ~ category, p.adjust.method = "bonferroni") |>
  mutate(variable = "Range exposed before the 2°C overshoot")

dunn_during <- dunn_test(exposure_median, residuals_range_exp_during ~ category, p.adjust.method = "bonferroni") |>
  mutate(variable = "Range newly exposed during the 2°C overshoot")

dunn_reversibility <- dunn_test(exposure_median, residuals_reversibility ~ category, p.adjust.method = "bonferroni") |>
  mutate(variable = "Reversibility of exposure by the end of the overshoot")

table_s2 <- bind_rows(dunn_before, dunn_during, dunn_reversibility) |>
  mutate(comparison = paste(group1, "vs.", group2),
         p.adj = round(p.adj, 4),
         p.adj = ifelse(p.adj < 0.001, "< 0.001", as.character(p.adj))) |>
  select(variable, comparison, statistic, p.adj, p.adj.signif) |>
  rename(Variable = variable,
         Comparison = comparison,
         Statistic = statistic,
         `Adjusted p-value` = p.adj,
         Significance = p.adj.signif)

write_xlsx(table_s2, here("tables/table_s2.xlsx"))
