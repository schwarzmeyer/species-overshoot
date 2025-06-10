# this code calculated the stats presented in the paper

amph <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Amphibians.rds"))
bird <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Birds.rds"))
mamm <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Mammals.rds"))
rept <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Reptiles.rds"))
fish <- readRDS(here("processed_data/species_data/range_maps_grid_cells/Fishes.rds"))

os <- readRDS(here("processed_data/climate_data/overshoot_times/overshoot_times.rds"))

range_data <- c(amph, bird, mamm, rept, fish) 

total_populations <- sum(sapply(range_data, function(x) length(x)))
total_species <- length(unique(names(range_data)))

risk_raw_models <- readRDS(here("results/risk/risk_raw_models.rds"))
risk_thresholds_models <- readRDS(here("results/risk/risk_thresholds_models.rds"))

# Stats from the 2nd paragraph of the results

# Total number of species analyzed:
total_species

# 1. Total number of species at risk from severe exposure at 2°C warming, peak and 2°C cooling ----
# Absolute increase from 2w to peak
# Absolute decrease from peak to 2c
# Relative difference from 2w to 2c

stats <- risk_thresholds_models %>% 
  filter(threshold %in% c("2w", "peak", "2c")) %>% 
  filter(range_exposed >= 0.8) %>%
  group_by(model, threshold) %>% 
  count(name = "exposed") %>% 
  ungroup() %>% 
  mutate(proportion = exposed/total_species * 100) %>% 
  select(-exposed) %>% 
  pivot_wider(
    names_from = threshold, 
    values_from = proportion
    ) %>% 
  mutate(
    abs_increase = peak - `2w`,
    abs_decrease = `2c` - peak,
    relative_diff = (`2w`-`2c`)/`2w`*100
    ) %>% 
  pivot_longer(
    cols = where(is.numeric),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  ) %>% 
  mutate(
    variable = factor(variable, 
                      levels = c("2w", "peak", "2c", "abs_increase", "abs_decrease", "relative_diff")
                      )) %>% 
  arrange(variable) %>% 
  mutate(across(where(is.numeric), ~round(.x, digits = 1)))
  
stats

# 3. Correlation ----

risk_thresholds_models %>% 
  filter(threshold %in% c("2w", "peak", "2c")) %>% 
  filter(range_exposed >= 0.8) %>%
  group_by(model, threshold) %>% 
  count(name = "exposed") %>% 
  ungroup() %>% 
  mutate(proportion = exposed/total_species * 100) %>% 
  select(-exposed) %>% 
  pivot_wider(
    names_from = threshold, 
    values_from = proportion
    ) %>% 
  left_join(os, by = "model") %>% 
  summarise(
    cor_results = list(tidy(cor.test(peak, magnitude, method = "spearman")))
  ) %>%
  unnest(cor_results) 




model_median %>% 
  left_join(model_range, by = "threshold") %>% 
  pivot_longer(cols = c(median, min, max, all_species)) %>% 
  pivot_wider(names_from = threshold) %>% 
  mutate(abs_increase = peak - `2w`,
         abs_decrease = `2c` - peak,
         relative_diff = (`2w`-`2c`)/`2w`)

# Absolute 

risk_thresholds_models %>% 
  filter(threshold %in% c("2w", "peak", "2c")) %>% 
  group_by(model, threshold) %>% 
  filter(range_exposed >= 0.8) %>%
  summarise(all_species = length(unique(species))) %>% 
  ungroup() %>% 
  mutate(proportion = round(all_species/total_species * 100, 1)) %>% 
  select(-all_species) %>% 
  pivot_wider(names_from = threshold, values_from = proportion) %>% 
  mutate(increase = (`2w`-`2c`) / `2w`,
         diff_peak = `peak` - `2w`,
         diff_2c = `peak` - `2c`)


risk_raw_models %>% 
  group_by(model) %>% 
  filter(range_exposed >= 0.8) %>%
  summarise(all_species = length(unique(species))) %>% 
  mutate(proportion = round(all_species/total_species*100, 1)) %>% 
  arrange(proportion)


data_models %>% 
  group_by(model) %>% 
  filter(range_exposed >= 0.8) %>%
  summarise(all_species = length(unique(species))) %>% 
  mutate(proportion = round(all_species/total_species*100, 1)) %>% 
  arrange(proportion)


data_thresh_summary %>% 
  left_join(os, by = "model") %>% 
  ggplot(aes(y = `2c`, x = magnitude)) +
  geom_point() +
  geom_smooth(method = "lm")



data_thresh_m %>% 
  filter(threshold %in% c("2w", "peak", "2c")) %>% 
  group_by(threshold) %>% 
  filter(range_exposed >= 0.8) %>%
  summarise(all_species = length(unique(species))) %>% 
  ungroup() %>% 
  mutate(proportion = round(all_species/total_species*100, 1)) %>% 
  # select(-all_species) %>% 
  pivot_wider(names_from = threshold, values_from = proportion)

data_models %>% 
  group_by(model) %>% 
  summarise(mean(range_exposed), mean(mean_local_duration))


data_median <- readRDS(here("results/risk/risk_raw_median.rds"))

data_median %>% 
  filter(range_exposed >= 0.8,
         mean_local_duration >= 100)



# median number of populations exposed
round(sum(data_median$n_cells_exposed)/total_populations*100,1)

# number of species exposed
round(length(unique(data_median$species))/total_species*100,1)

round(length(unique(data_median$species))/total_species*100,1)

qrange <- 0.1
qduration <- 50

species_at_risk <- data_median %>% 
  filter(mean_local_duration >= qduration,
         range_exposed >= qrange) %>% 
  count(species) %>% 
  nrow()

species_at_risk/total_species*100



# range across models
data_models <- readRDS(paste0(path, "Results/Risk Metric/risk_raw_models.rds"))

data_models %>% 
  group_by(model) %>% 
  summarise(all_species = length(unique(species))) %>% 
  mutate(proportion = round(all_species/total_species*100, 1))


# median duration and range exposed
round(quantile(data_median$range_exposed, myquantile), 2)*100
round(quantile(data_median$mean_local_duration, myquantile), 0)

# qrange <- 0.5
# qduration <- 50

p1 <-  ggplot(data_median, aes(x = range_exposed*100)) +
  geom_histogram(aes(fill = range_exposed*100 >= qrange*100), binwidth = 5, boundary = 0, colour = "white") +
  geom_boxplot(aes(y = 10000), width = 1200) +
  scale_fill_manual(values = c("grey","#d72547")) +
  labs(y = "Count", x = "Range exposed (%)") +
  # scale_y_continuous(expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(0,100, length.out = 24)) +
  coord_cartesian(clip = "off", ylim = c(0,10000)) +
  theme_tidybayes() +
  theme(legend.position = "none",
        plot.margin = margin(t = 15)); 


p2 <- ggplot(data_median, aes(x = mean_local_duration)) +
  geom_histogram(aes(fill = mean_local_duration >= qduration),  binwidth = 10, boundary = 0, closed = "left", position = "identity", colour = "white") +
  geom_boxplot(aes(y = 6000), width = 800, outlier.color = NA) +
  scale_fill_manual(values = c("grey","#d72547")) +
  labs(y = "Count", x = "Duration (years)") +
  # scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off", ylim = c(0,6000)) +
  theme_tidybayes() +
  theme(legend.position = "none",
        plot.margin = margin(t = 15));

p <- p1 + plot_spacer() + p2 + plot_layout(ncol = 1, heights = c(1,0.1,1))



p



risk_spp <- data_median %>% 
  filter(range_exposed >= qrange,
         mean_local_duration >= qduration) %>% 
  pull(species)





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


grid_land <- readRDS(paste0(path, "Data/Spatial Data/terrestrial_grid_robin.rds"))
grid_ocean <- readRDS(paste0(path, "Data/Spatial Data/ocean_grid_robin.rds"))

grid_land <- left_join(grid_land, data_land, by = "WorldID")
grid_ocean <- left_join(grid_ocean, data_ocean, by = "WorldID")





# bks <- c(0,1,5,10,20,30,40,50,100,200)
# 
# pmap <- ggplot() +
#   geom_sf(data = countries, fill = "grey87", alpha = 0.1, colour = NA) +
#   geom_sf(data = grid_ocean, aes(fill = spp_risk, colour = spp_risk), show.legend = T) +
#   geom_sf(data = grid_land, aes(fill = spp_risk, colour = spp_risk), show.legend = T) +
#   geom_sf(data = countries, colour = "grey30", fill = NA,  linewidth = 0.1) +
#   # coord_sf(ylim = c(-5800000,9000000)) +
#   scale_fill_viridis_b(option = "F", direction = -1, breaks = bks, name = "Number of species at high risk (top tertile)",
#                        limits = c(1,200), end = 0.98, begin = 0.03, na.value = "#f9e2d2") +
#   scale_colour_viridis_b(option = "F", direction = -1, breaks = bks, name = "Number of species at high risk (top tertile)",
#                          limits = c(1,200),end = 0.98, begin = 0.03, na.value = "#f9e2d2") +
#   theme_map() +
#   theme(legend.position = "top",
#         legend.justification = "center",
#         plot.margin = margin(0,0,0,0)) +
#   guides(fill = guide_colorsteps(title.position = 'top',
#                                  title.hjust = .5,
#                                  barwidth = unit(15, 'lines'), barheight = unit(.4, 'lines')))
# 
# pmap


grid_ocean <- grid_ocean %>% 
  mutate(spp_risk_perc = ifelse(spp_risk_perc <= 1, NA, spp_risk_perc))

grid_land <- grid_land %>% 
  mutate(spp_risk_perc = ifelse(spp_risk_perc <= 1, NA, spp_risk_perc))


if(qrange == 0.5) {
  
  bks <- c(0,1,5,10,20,30,40,70)
  
} else {
  
  bks <- c(0,1,5,10,20,30,40,55)
  
}



pmap_perc <- ggplot() +
  geom_sf(data = countries, colour = NA, fill = "grey80",  linewidth = 0.2) +
  geom_sf(data = grid_ocean, aes(fill = spp_risk_perc, colour = spp_risk_perc), show.legend = T) +
  geom_sf(data = grid_land, aes(fill = spp_risk_perc, colour = spp_risk_perc), show.legend = T) +
  geom_sf(data = countries, colour = "white", fill = NA,  linewidth = 0.2) +
  scale_fill_viridis_b(option = "F", direction = -1, breaks = bks, name = "% of species at high risk",
                       end = 0.9, begin = 0.2, na.value = NA, limits = c(1,max(bks)),
                       values = rescale(spp_risk_perc)) +
  scale_colour_viridis_b(option = "F", direction = -1, breaks = bks, name = "% of species at high risk",
                         end = 0.9, begin = 0.2, na.value = NA, limits = c(1,max(bks)),
                         values = rescale(spp_risk_perc)) +
  coord_sf(ylim = c(-5500000,8000000)) +
  theme_map() +
  theme(legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_colorsteps(title.position = 'top',
                                 title.hjust = .5,
                                 barwidth = unit(18, 'lines'), barheight = unit(.6, 'lines')))

pmap_perc


richness <- data_median %>% 
  count(group, name = "richness")

p3 <- data_median %>% 
  filter(species %in% risk_spp) %>% 
  count(group, name = "exposed") %>% 
  left_join(richness, by = "group") %>% 
  mutate(not_exposed = richness - exposed) %>% 
  mutate(perc = exposed/sum(exposed)*100,
         wperc = exposed/richness*100) %>% 
  pivot_longer(cols = c(exposed, not_exposed)) %>% 
  mutate(group = factor(group, levels = rev(c("Amphibians","Reptiles","Mammals","Birds","Fishes")))) %>% 
  ggplot(aes(x = value, y = group, fill = name)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#d72547", "grey")) +
  xlim(c(0,12000)) +
  labs(x = "No. of species", y = "") +
  # scale_fill_viridis_d(direction = 1, option = "F", begin = 0.15, end = 0.84, name = "") +
  # labs(title = "Proportion of species at the top tertile",
  #      subtitle = "Weighted by the number of species in each group") +
  theme_tidybayes() +
  geom_text(aes(label = paste0(round(wperc, 0), "%")),
            position = position_stack(vjust = 1.1), colour = rep(c("black",NA), 5), fontface = 2, size = 3.6, family = "Tahoma") +
  theme(plot.title = element_text(hjust = 0.5, family = "Tahoma"),
        plot.subtitle = element_text(hjust = 0.5, family = "Tahoma"),
        legend.text = element_text(family = "Tahoma"),
        plot.margin = margin(b = 0),
        legend.position = "none"); p3


# layout <- "
# ####BBBBB
# AAAABBBBB
# AAAABBBBB
# AAAABBBBB
# ####BBBBB"


pp <- ggdraw() +
  draw_plot(p, x = 0.01, y = 0.56, width = 0.45, height = 0.42) +
  draw_plot(p3, x = 0.53, y = 0.62, width = 0.45, height = 0.31) +
  draw_plot(pmap_perc, y = -0.2)


ggsave(paste0(path, "Figures/_Fig_01_v1.jpg"),
       pp,
       width = 24, height = 25, units = "cm", dpi = 700)



