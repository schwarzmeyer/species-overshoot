source("code/00_packages.R")
source("code/functions/tibble_to_raster.R")


files <- list.files(here("results/species_exposure_times"), pattern = ".rds", full.names = TRUE)
models <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "GISS-E2-1-G", "IPSL-CM6A-LR", "MRI-ESM2-0")
os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds"))

diff_threshold <- 0.05

data <- map_dfr(files, readRDS) |> 
  select(model, group, species, world_id, exposure, exposure_gwl, deexposure_gwl) |> 
  left_join(os |> select(model, begin_os, end_os), by = "model") |> 
  filter(exposure >= begin_os & exposure <= end_os) |> 
  select(-c(exposure, begin_os, end_os)) |> 
  mutate(gwl_diff = exposure_gwl - deexposure_gwl) |> 
  mutate(code = case_when(gwl_diff <= diff_threshold ~ "Additional cooling not required",
                          gwl_diff > diff_threshold ~ "De-exposure required additional cooling")) |> 
  mutate(code = ifelse(is.na(deexposure_gwl), "No de-exposure", code)) 


data <- data |> 
  bind_rows(data |>
              mutate(group = factor("All species"))) |> 
  bind_rows(data |> 
              filter(group != "Fishes") |>
              mutate(group = factor("Terrestrial species"))) 

plot_data_a <- data |> 
  group_by(model, group) |> 
  count(code) |> 
  mutate(n_perc = n/sum(n)) |> 
  group_by(group, code) |> 
  summarise(min = min(n_perc),
            median = median(n_perc),
            max = max(n_perc),
            .groups = "drop") |> 
  mutate(code = factor(code, 
                       levels = c("No de-exposure", "Additional cooling not required",  "De-exposure required additional cooling")),
         group = factor(group,
                        levels = c("All species", "Amphibians", "Reptiles", "Mammals", "Birds", "Fishes", "Terrestrial species")))

plot_a <- plot_data_a |> 
  filter(group != "Terrestrial species") |> 
  ggplot(aes(y = median, x = group, fill = code)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("grey", "#e9942f","#42b2e6"), name = "") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0, position = position_dodge(width = 0.7),
                colour = "black", linewidth = 0.4) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05), labels = percent_format(), breaks = seq(0, 1, 0.2)) +
  labs(y = "% of exposure events", x = "") +
  theme_tidybayes() +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_text(size = 9, vjust = 0.6),
        axis.text.x = element_text(size = 7.5, angle = 30, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(linewidth = 0.2),
        legend.position = "inside",
        legend.position.inside = c(0.2,-0.4),
        legend.key.spacing.y = unit(2, "pt"),
        legend.key.height = unit(8, "pt"),
        legend.key.width = unit(6, "pt"),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        plot.margin = margin(r = 25, t = 0, b = 0, unit = "pt")) +
  guides(fill = guide_legend(ncol = 1, reverse = TRUE)); plot_a



phylo_x <- -1.19
phylo_color <- "#2c1f47" 

phylopics <- data.frame(x = -1.2,
                        y = 5:1 + 0.5,
                        group =  factor(c("Amphibians", "Reptiles", "Mammals", "Birds", "Fishes")),
                        uuid = c("bd80bc51-460c-4dd9-8341-e5b460372efb", # amphibians
                                 "264fa655-afd7-451c-8f27-e0a9557376e6", # reptiles
                                 "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", # mammals
                                 "157d3109-7124-413c-8362-3abcc6889a3f", # birds
                                 "c90aa49b-d9c5-44a4-a709-4f8d9a33b559")) # fish


boxplot_stats <- data |>
  filter(group == "All species") |>
  mutate(model = factor(model, levels = c("MRI-ESM2-0", "GISS-E2-1-G", "ACCESS-ESM1-5", "IPSL-CM6A-LR", "CNRM-ESM2-1"))) |>
  drop_na(gwl_diff) |>
  group_by(model) |>
  summarise(lower = quantile(gwl_diff * -1, 0.25),
            upper = quantile(gwl_diff * -1, 0.75),
            middle = median(gwl_diff * -1),
            xmin = quantile(gwl_diff * -1, 0.05),
            xmax = quantile(gwl_diff * -1, 0.95),
            .groups = "drop")




plot_b <- data |> 
  filter(group %in% c("All species")) |>
  mutate(model = factor(model, levels = c("MRI-ESM2-0", "GISS-E2-1-G", "ACCESS-ESM1-5", "IPSL-CM6A-LR", "CNRM-ESM2-1"))) |>
  drop_na(gwl_diff) |>
  ggplot(aes(x = gwl_diff * -1, y = model)) +
  stat_halfeye(
    aes(fill = after_stat(x)),
    fill_type = "gradient",
    adjust = 3,
    .width = 0,
    justification = -0.2,
    point_colour = NA,
    show.legend = FALSE) +
  geom_boxplot(data = boxplot_stats,
               aes(xmin = xmin, xlower = lower, xmiddle = middle,
                   xupper = upper, xmax = xmax, y = model),
               stat = "identity",
               inherit.aes = FALSE,
               width = 0.25,
               show.legend = FALSE,
               alpha = 0.4,
               linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  labs(y = "", 
       x = "Difference in global warming levels\nat exposure and de-exposure (°C)") +
  scale_fill_gradientn(colors = c("#3a2666", "#3a2666", "#4295fc", "#19d5cb", "#F7F7F7", "#e9962f", "#ee5c13", "#981105"),
                       values = scales::rescale(c(-2, -1.2, -0.6, -0.5, -0.2,  0, 0.1, 0.2, 0.4, 0.6)),
                       limits = c(-2, 0.6)) +
  coord_cartesian(clip = "off") +
  scale_x_reverse(expand = c(0, 0), limits = c(0.7, -1.5)) +
  theme_tidybayes() +
  theme(panel.grid.major.x = element_line(linewidth = 0.1),
        panel.grid.minor.x = element_line(linewidth = 0.1),
        axis.title.x = element_text(size = 9, vjust = 9),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8.5),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(r = 0.4, t = 0, b = 1, unit = "line"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        strip.background = element_blank()); plot_b



id_filter <- data |> 
  drop_na(deexposure_gwl) |> 
  filter(group %in% c("Fishes", "Terrestrial species")) |> 
  group_by(model, group, world_id) |>
  summarise(n_species = n_distinct(species), 
            .groups = "drop")


data_land <- data |> 
  drop_na(deexposure_gwl) |> 
  filter(group == "Terrestrial species") |> 
  left_join(id_filter, by = c("model","group", "world_id")) |> 
  filter(n_species >= 5) |>
  group_by(model, world_id) |>
  summarise(n_med = median(gwl_diff, na.rm = TRUE),
            n_95 = quantile(gwl_diff, 0.95, na.rm = TRUE), 
            .groups = "drop") 

data_ocean <- data |> 
  drop_na(deexposure_gwl) |> 
  filter(group == "Fishes") |> 
  left_join(id_filter, by = c("model","group", "world_id")) |> 
  filter(n_species >= 5) |>
  group_by(model, world_id) |>
  summarise(n_med = median(gwl_diff, na.rm = TRUE),
            n_95 = quantile(gwl_diff, 0.95, na.rm = TRUE), 
            .groups = "drop") 


r_land_med_models <- map(models, ~ {
  data_model <- data_land |> filter(model == .x)
  r <- tibble_to_raster(data_model, "n_med", robin = TRUE)
  names(r) <- .x
  -r
}) |> 
  rast()

r_land_95_models <- map(models, ~ {
  data_model <- data_land |> filter(model == .x)
  r <- tibble_to_raster(data_model, "n_95", robin = TRUE)
  names(r) <- .x
  -r
}) |> 
  rast()

r_ocean_med_models <- map(models, ~ {
  data_model <- data_ocean |> filter(model == .x)
  r <- tibble_to_raster(data_model, "n_med", robin = TRUE)
  names(r) <- .x
  -r
}) |> 
  rast()

r_ocean_95_models <- map(models, ~ {
  data_model <- data_ocean |> filter(model == .x)
  r <- tibble_to_raster(data_model, "n_95", robin = TRUE)
  names(r) <- .x
  -r
}) |> 
  rast()

r_land_med <- median(r_land_med_models, na.rm = TRUE)
r_ocean_med <- median(r_ocean_med_models, na.rm = TRUE)
r_land_95 <- median(r_land_95_models, na.rm = TRUE)
r_ocean_95 <- median(r_ocean_95_models, na.rm = TRUE)

r_land_med[r_land_med == 0] <- NA
# create bounding box to plot around the maps
bbox <- readRDS(here("raw_data/random/bbox.rds"))
# load countries polygon
world <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")

seq(-0.4, 1.2, 0.4)

bks <- c(-0.8, -0.3,  -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 0.3, 0.6, 0.9)


theme_maps <- theme(legend.position = "inside",
                    legend.position.inside = c(0.5, -0.2),
                    legend.direction = "horizontal",
                    plot.margin = margin(t = 0, b = 0, unit = "line"),
                    legend.title = element_text(size = 8),
                    legend.text = element_text(size = 7))  

guide_maps <-   guides(fill = guide_colorsteps(title.position = "top",
                                              reverse = TRUE,
                                              even.steps = TRUE,
                                              title.hjust = .5,
                                              barwidth = unit(13, "lines"),
                                              barheight = unit(.4, "lines")))


bks <- c(-1.4, -0.8, -0.4, -0.2, -0.1, -0.05, 0, 0.05, 0.1,  0.2 , 0.4, 0.8)
my_pal <- c(viridis(6, option = "H", end = 0.28), 
            viridis(5, option = "H", direction = 1, begin = 0.66))

plot_med <- ggplot() +
  geom_sf(data = bbox, fill = alpha("grey", 0.2), colour = NA) +
  geom_sf(data = world, fill = "white", colour = NA) +
  geom_spatraster(data = r_ocean_med, aes(fill = median)) +
  geom_spatraster(data = r_land_med, aes(fill = median)) +
  geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) +
  scale_fill_stepsn(colours = my_pal,
                    breaks = bks,
                    values = scales::rescale(bks),
                    limits = range(bks),
                    na.value = NA,
                    labels = scales::label_number(drop0trailing = TRUE),
                    name = "Difference in global warming levels\nat exposure and de-exposure (°C) — median") +
  theme_minimal() +
  theme_maps +
  guide_maps; plot_med


plot_95 <- ggplot() +
  geom_sf(data = bbox, fill = alpha("grey", 0.2), colour = NA) +
  geom_sf(data = world, fill = "white", colour = NA) +
  geom_spatraster(data = r_ocean_95, aes(fill = median)) +
  geom_spatraster(data = r_land_95, aes(fill = median)) +
  geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) +
  scale_fill_stepsn(
    colours = my_pal,
    breaks = bks,
    values = scales::rescale(bks),
    limits = range(bks),
    na.value = NA,
    labels = scales::label_number(drop0trailing = TRUE),
    name = "Difference in global warming levels\nat exposure and de-exposure (°C) — 95th percentile") +
  theme_minimal() +
  theme_maps +
  guide_maps




r_template <- rast()
values(r_template) <- 1:ncell(r_template)
names(r_template) <- "world_id"

template_df <- as.data.frame(r_template, xy = TRUE) |> 
  as_tibble() |> 
  rename(lat = y, lon = x)

data_lat <- data |> 
  filter(group %in% c("Fishes", "Terrestrial species")) |>
  drop_na(deexposure_gwl) |> 
  mutate(realm = ifelse(group == "Fishes", "Ocean", "Land")) |>
  select(model, group, species, world_id, gwl_diff, realm) |> 
  left_join(template_df, by = "world_id") |> 
  mutate(lat_bin = round(lat, 0)) |> 
  mutate(lat_bin = round(lat_bin / 2) * 2) |>  # 2-degree bins
  group_by(model, realm, lat_bin) |>
  summarise(med_gwl_diff = -median(gwl_diff, na.rm = TRUE),
            n_pops = n(),
            .groups = "drop") |>
  group_by(realm, lat_bin) |>
  summarise(med_gwl_diff = median(med_gwl_diff, na.rm = TRUE),
            med_n_pops = median(n_pops, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(realm = factor(realm, levels = c("Land", "Ocean"))) 


plot_lat <- data_lat |> 
  filter(med_n_pops >= 1) |>
  ggplot(aes(x = med_gwl_diff, y = lat_bin, colour = realm)) +
  geom_point(aes(size = med_n_pops), alpha= 0.08, show.legend = FALSE) +  # Point transparency shows density
  geom_smooth(aes(weight = med_n_pops), method = "loess", se = FALSE, 
              orientation = "y", linewidth = 0.7, span = 2) +
  scale_y_continuous(breaks = seq(-80, 80, 20), limits = c(-80,80), labels = c(-80, "", -40, "", 0, "", 40, "", 80)) +
  scale_x_reverse(limits = c(0.3, -0.66)) +
  scale_colour_manual(values = c("Land" = "#FF6347", "Ocean" = "turquoise4"),
                      name = "") +
  coord_cartesian(expand = FALSE)+
  labs(x = "Difference in global\nwarming level (°C)", y = "Latitude (°)") +
  theme_tidybayes() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 1),
        legend.text = element_text(size = 7),
        legend.key.size = unit(6, "pt"),
        legend.background = element_blank(),
        axis.title = element_text(size = 8),
        axis.title.y = element_text(vjust = -3),
        axis.text = element_text(size = 7)) +
  guides(colour = guide_legend(override.aes = list(fill = NA)))




p1 <- plot_grid(plot_a, plot_b, ncol = 2, align = "h", rel_widths = c(1, 1))

p2 <- plot_grid(plot_med, plot_95, ncol = 2)


p <- ggdraw() +
  draw_plot(p1, x = 0.07, , y = 0.55, height = 0.4, width = 0.85) +
  draw_plot(plot_lat, x = 0.8, y = 0.15, width = 0.18, height = 0.295) +
  draw_plot(p2, x = -0.09, y = -0.16, scale = 0.8) +
  draw_text("a", x = 0.05, y = 0.97, size = 11, fontface = "bold") +
  draw_text("b", x = 0.5, y = 0.97, size = 11, fontface = "bold") +
  draw_text("c", x = 0.05, y = 0.49, size = 11, fontface = "bold") +
  draw_text("d", x = 0.45, y = 0.49, size = 11, fontface = "bold") +
  draw_text("e", x = 0.82, y = 0.49, size = 11, fontface = "bold") 

ggsave(here("figures/Figure_03.jpg"),
       p,
       width = 23, height = 18, units = "cm", dpi = 700)




# Stats

plot_data_a |> 
  mutate(across(where(is.numeric), ~ percent(.x, accuracy = 1))) 
data |> 
  group_by(group, model) |> 
  summarise(median = median(gwl_diff, na.rm = TRUE),
            p_95 = quantile(gwl_diff, 0.95, na.rm = TRUE),
            .groups = "drop") |>
  group_by(group) |>
  summarise(med = median(median, na.rm = TRUE),
            min = min(median, na.rm = TRUE),
            max = max(median, na.rm = TRUE),
            p_95_med = median(p_95, na.rm = TRUE),
            p_95_min = min(p_95, na.rm = TRUE),
            p_95_max = max(p_95, na.rm = TRUE),
            .groups = "drop") |> 
  filter(group %in% c("Fishes", "Terrestrial species", "All species")) 


boxplot_stats |> 
  mutate(across(where(is.numeric), ~ round(.x, 2))) 
  

bind_rows(values(r_land_med) |> as_tibble(), values(r_ocean_med) |> as_tibble()) |>
  na.omit() |> 
    filter(median > 0)


  bind_rows(values(r_land_95) |> as_tibble(), values(r_ocean_95) |> as_tibble()) |>
    na.omit() |> 
    filter(median >0)




ggplot() +
  geom_sf(data = bbox, fill = alpha("grey", 0.2), colour = NA) +
  geom_sf(data = world, fill = "white", colour = NA) +
  geom_spatraster(data = (median(r_land_med, na.rm = TRUE)*-1) >0, aes(fill = median)) +
  geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) 

plot_95 <- ggplot() +
  geom_sf(data = bbox, fill = alpha("grey", 0.2), colour = NA) +
  geom_sf(data = world, fill = "white", colour = NA) +
  geom_spatraster(data = r_ocean_95*-1, aes(fill = n_95)) +
  geom_spatraster(data = r_land_95*-1, aes(fill = n_95)) +
  geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) +
  scale_y_continuous(breaks = seq(-80, 80, 20)) +
  scale_fill_gradientn(colors = c("#36043a", "#8a61d8","#92dcfd", "#F7F7F7", "#fc6465", "#c10f0f"),
                       values = scales::rescale(c(-1.5, -0.7, -0.25, 0, 0.25, 0.5)),
                       limits = c(-1.5, 0.5),
                       na.value = NA,
                       name = "Cooling required to de-expose\n95% of the species in the grid cell (°C)") +
  
  theme_minimal() +
  theme_maps +
  guide_maps





r_land_med <- tibble_to_raster(data_land, "n_med")
r_land_95 <- tibble_to_raster(data_land, "n_95")

r_ocean_med <- tibble_to_raster(data_ocean, "n_med")
r_ocean_95 <- tibble_to_raster(data_ocean, "n_95")

# load countries polygon
world <- st_transform(ne_countries(returnclass = "sf"), "+proj=robin")

# create bounding box to plot around the maps
bbox <- readRDS(here("raw_data/random/bbox.rds"))

seq(-0.4, 1.2, 0.4)

bks <- c(-0.45, -0.3,  -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 0.3, 0.6, 1.3)

# plot_c <- ggplot() +
#   geom_sf(data = bbox, fill = alpha("grey", 0.2), colour = NA) +
#   geom_sf(data = world, fill = "white", colour = NA) +
#   geom_spatraster(data = data_ocean, aes(fill = n)) +
#   geom_spatraster(data = data_land, aes(fill = n)) +
#   geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) +
#   scale_fill_scico(palette = "vik", 
#                    direction = 1,
#                    begin = 0.15,
#                    end = 0.9,
#                    breaks = bks,
#                    values = scales::rescale(bks),
#                    labels = round(bks, 2),
#                    name = "Difference between global warming levels\nat exposure and de-exposure (°C)",
#                    na.value = NA) +
#   
#   theme_minimal() +
#   theme(legend.position = "inside",
#         legend.position.inside = c(0.5, -0.1),
#         legend.direction = "horizontal",
#         plot.margin = margin(t = 2, b = 4, unit = "line"),
#         legend.title = element_text(size = 11),
#         legend.text = element_text(size = 9))  +
#   guides(fill = guide_colorsteps(title.position = 'top',
#                                title.hjust = .5,
#                                barwidth = unit(20, 'lines'),
#                                barheight = unit(.4, 'lines'))); plot_c

theme_maps <- theme(legend.position = "inside",
                    legend.position.inside = c(0.5, -0.17),
                    legend.direction = "horizontal",
                    plot.margin = margin(t = 0, b = 0, unit = "line"),
                    legend.title = element_text(size = 9),
                    legend.text = element_text(size = 8))  

guide_maps <-   guides(fill = guide_colourbar(title.position = "top",
                                              reverse = TRUE,
                                              title.hjust = .5,
                                              barwidth = unit(13, "lines"),
                                              barheight = unit(.45, "lines")))

plot_med <- ggplot() +
  geom_sf(data = bbox, fill = alpha("grey", 0.2), colour = NA) +
  geom_sf(data = world, fill = "white", colour = NA) +
  geom_spatraster(data = r_ocean_med*-1, aes(fill = n_med)) +
  geom_spatraster(data = r_land_med*-1, aes(fill = n_med)) +
  geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) +
  scale_fill_gradientn(colors = c("#36043a", "#8a61d8","#92dcfd", "#F7F7F7", "#fc6465", "#c10f0f"),
                       values = scales::rescale(c(-1.5, -0.7, -0.25, 0, 0.25, 0.5)),
                       limits = c(-1.5, 0.5),
                       na.value = NA,
                       name = "Cooling required to de-expose\n50% of the species in the grid cell (°C)") +
  theme_minimal() +
  theme_maps +
  guide_maps



plot_95 <- ggplot() +
  geom_sf(data = bbox, fill = alpha("grey", 0.2), colour = NA) +
  geom_sf(data = world, fill = "white", colour = NA) +
  geom_spatraster(data = r_ocean_95*-1, aes(fill = n_95)) +
  geom_spatraster(data = r_land_95*-1, aes(fill = n_95)) +
  geom_sf(data = world, fill = NA, colour = "black", linewidth = 0.07) +
  scale_y_continuous(breaks = seq(-80, 80, 20)) +
  scale_fill_gradientn(colors = c("#36043a", "#8a61d8","#92dcfd", "#F7F7F7", "#fc6465", "#c10f0f"),
                       values = scales::rescale(c(-1.5, -0.7, -0.25, 0, 0.25, 0.5)),
                       limits = c(-1.5, 0.5),
                       na.value = NA,
                       name = "Cooling required to de-expose\n95% of the species in the grid cell (°C)") +
  
  theme_minimal() +
  theme_maps +
  guide_maps



r_template <- rast()
values(r_template) <- 1:ncell(r_template)
names(r_template) <- "world_id"

template_df <- as.data.frame(r_template, xy = TRUE) |> 
  as_tibble() |> 
  rename(lat = y, lon = x)

data_lat <- data |> 
  filter(group != "All species",
         group != "Terrestrial species",
         model == median_model) |> 
  mutate(realm = ifelse(group == "Fishes", "ocean", "land")) |>
  left_join(template_df, by = "world_id") |> 
  mutate(lat_bin = round(lat, 0)) |> 
  mutate(lat_bin = round(lat_bin / 2) * 2) |>  # 2-degree bins
  group_by(realm, lat_bin) |>
  mutate(n_points = n()) |>
  summarise(med = median(gwl_diff, na.rm = TRUE),
            mean = mean(gwl_diff, na.rm = TRUE),
            n_pops = n(),
            .groups = "drop") 

plot_lat <- data_lat |> 
  mutate(realm = factor(realm, levels = c("ocean", "land"))) |>
  # filter(n_pops >= 20) |>  # Remove polar regions with sparse data
  ggplot(aes(x = med*-1, y = lat_bin, colour = realm)) +
  geom_point(aes(size = n_pops), alpha= 0.15, show.legend = FALSE) +  # Point transparency shows density
  geom_smooth(aes(weight = n_pops), method = "loess", se = FALSE, 
              orientation = "y", level = 0.95, linewidth = 0.6) +
  scale_y_continuous(breaks = seq(-80, 80, 20), limits = c(-80,80), labels = c(-80, "", -40, "", 0, "", 40, "", 80)) +
  scale_x_reverse(limits = c(0.3, -0.66)) +
  scale_colour_manual(values = c("land" = "grey60", "ocean" = "seagreen3"),
                      labels = c("land" = "Land", "ocean" = "Ocean"),
                      name = "") +
  coord_cartesian(expand = FALSE)+
  labs(x = "Median cooling (°C)", y = "Latitude (°)") +
  theme_tidybayes() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 1),
        legend.text = element_text(size = 7),
        legend.key.size = unit(6, "pt"),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.title.y = element_text(vjust = -3),
        axis.text = element_text(size = 7)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))); plot_lat


p1 <- plot_grid(plot_a, plot_b, ncol = 2, align = "h", rel_widths = c(1, 0.85))
  
p2 <- plot_grid(plot_med, plot_95, ncol = 2)


p <- ggdraw() +
  draw_plot(p1, x = 0.07, , y = 0.58, height = 0.4, width = 0.85) +
  draw_plot(plot_lat, x = 0.8, y = 0.16, width = 0.18, height = 0.295) +
  draw_plot(p2, x = -0.09, y = -0.16, scale = 0.8) +
  draw_text("a", x = 0.05, y = 0.97, size = 11, fontface = "bold") +
  draw_text("b", x = 0.5, y = 0.97, size = 11, fontface = "bold") +
  draw_text("c", x = 0.05, y = 0.49, size = 11, fontface = "bold") +
  draw_text("d", x = 0.45, y = 0.49, size = 11, fontface = "bold") +
  draw_text("e", x = 0.82, y = 0.49, size = 11, fontface = "bold") 

ggsave(here("figures/Figure_04.jpg"),
       p,
       width = 23, height = 18, units = "cm", dpi = 700)

