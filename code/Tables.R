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


# Extended data table s1

table_s1 <- risk_rate_combined |>
  group_by(dataset, model, group, range_exposed, phase) |>
  summarise(mean_rate = mean(rate),
            .groups = "drop") |> 
  mutate(range_exposed = case_when(
    range_exposed == "perc_20_std" ~ "≥20%",
    range_exposed == "perc_50_std" ~ "≥50%",
    range_exposed == "perc_80_std" ~ "≥80%"),
    phase = factor(phase, levels = c("Warming", "Cooling"))) |> 
  pivot_wider(names_from = phase, values_from = mean_rate) |> 
  mutate(ratio = round(-Warming / Cooling, 1),
         Warming =  round(Warming * 100, 2),
         Cooling =  round(Cooling* 100, 2)) |>
  rename(Data = dataset,
         Group = group,
         Model = model,
         `Range exposure` = range_exposed,
         `Change per 0.1°C warming` = Warming,
         `Change per 0.1°C cooling` = Cooling,
         Asymmetry = ratio) 

write_xlsx(table_s1, here("tables/table_s1.xlsx"))


table_s2 <- table_s1 |>
  group_by(dataset, range_exposed, group) |>
  summarise(median_rate_warming = median(Warming),
            median_rate_cooling = median(Cooling),
            median_ratio = median(ratio), 
            .groups = "drop") |> 
  mutate(median_rate_warming = percent(median_rate_warming, accuracy = 0.1),
         median_rate_cooling = percent(median_rate_cooling, accuracy = 0.1),
         median_ratio = round(median_ratio, 1)) 

  
table_s3 <- table_s1 |> 
    select(-ratio) |>
    pivot_wider(names_from = dataset, values_from = c(Warming, Cooling)) |> 
    mutate(diff_warming = `Warming_2°C overshoot` / `Warming_Full overshoot`,
           diff_cooling =  `Cooling_2°C overshoot` / `Cooling_Full overshoot`) |> 
    select(model, group, range_exposed, diff_warming, diff_cooling) |> 
    group_by(group, range_exposed) |> 
    summarise(median_diff_warming = round(median(diff_warming), 2),
              min_diff_warming = round(min(diff_warming), 2),
              max_diff_warming = round(max(diff_warming), 2),
              median_diff_cooling = round(median(diff_cooling), 2),
              min_diff_cooling = round(min(diff_cooling), 2),
              max_diff_cooling = round(max(diff_cooling), 2),
              .groups = "drop") 
  