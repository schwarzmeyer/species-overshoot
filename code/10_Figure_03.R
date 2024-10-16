data_median <- readRDS(here("results/risk/risk_raw_median.rds"))
ed <- readRDS(here("processed_data/species_data/phylogenetic/mean_evol_distinctiveness.rds"))

df <- left_join(data_median, ed, by = c("group", "species"))

df %>% 
  na.omit() %>% 
  ggplot(aes(y = log(meanED), x = log(range_exposed*100))) +
  ggpointdensity::geom_pointdensity(alpha = 0.4) +
  geom_smooth(method = "lm") +
  facet_wrap(~group, ncol = 1, scale = "free_y") +
  theme_tidybayes()

df %>% 
  na.omit() %>% 
  ggplot(aes(y = log(meanED), x = log(mean_local_duration))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  facet_wrap(~group, ncol = 1, scale = "free_y") +
  theme_tidybayes()

df %>% 
  na.omit() %>% 
  mutate(range_exposed = range_exposed * 100) %>% 
  pivot_longer(cols = c(range_exposed, mean_local_duration)) %>% 
  ggplot(aes(y = log(meanED), x = log(value))) +
  ggpointdensity::geom_pointdensity(alpha = 0.4) +
  scale_colour_viridis_c(option = "G", begin = 0.1) +
  geom_smooth(method = "lm", colour = "maroon") +
  facet_grid(group ~ name, scale = "free") +
  theme_tidybayes()
