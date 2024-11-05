data_median <- readRDS(here("results/risk/risk_raw_median.rds"))
ed <- readRDS(here("processed_data/species_data/phylogenetic/mean_evol_distinctiveness.rds"))
 
df <- left_join(data_median, ed, by = c("group", "species")) %>% 
  select(-c(n_cells_exposed, total_duration, range_size)) %>% 
  na.omit() %>% 
  mutate(range_exposed = (range_exposed + 0.001) * 100,
         group = factor(group, levels = c("Amphibians", "Birds", "Mammals",
                                          "Reptiles", "Fishes")))

df_plot <- df %>% 
  pivot_longer(cols = c(range_exposed, mean_local_duration), names_to = "risk_metric") %>% 
  mutate(risk_metric = ifelse(risk_metric == "range_exposed", "Range exposed", "Duration"),
         # risk_metric = factor(risk_metric, levels = c("Range exposed", "Duration")),
         value = log(value)) 
  
  
df_cor <- df %>% 
  group_by(group) %>% 
  summarise(cor_range = cor(log(meanED), log(range_exposed), method = "spearman"),
            cor_duration = cor(log(meanED), log(mean_local_duration), method = "spearman")) %>%
  pivot_longer(cols = c(cor_range, cor_duration)) %>% 
  mutate(x = rep(c(-0.1, 2.1), 5), y = rep(c(5, 4), 5), value = round(value, 2))

p <- df_plot %>% 
  ggplot(aes(y = log(meanED), x = value)) +
  ggpointdensity::geom_pointdensity(alpha = 0.2) +
  scale_colour_viridis_c(option = "G", begin = 0.1, name = "Density") +
  geom_smooth(method = "lm", colour = "maroon") +
  labs(y = "log(Evolutionary distinctiveness)", x = "log(value)") +
  scale_y_continuous(limits = c(-0.5,5.5)) +
  facet_grid(group ~ risk_metric, scales = "free_x", axes = "all", axis.labels = "margins") +
  theme_tidybayes() +
  # annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  # annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 7.5),
        legend.key.height = unit(1.2, "line"),
        legend.key.width = unit(1, "line"),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(l = 1, b = 1, t = 1, unit = "line"),
        strip.text = element_text(face = "bold"))

phylo_color <- "#4d3e54"


x1 <- 0.17
x2 <- 0.517 
x3 <- 0.15

p_final <- ggdraw(p) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Amphibians", name == "cor_range")$value),
            x = x1, y = 0.9, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Birds", name == "cor_range")$value),
            x = x1, y = 0.74, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Mammals", name == "cor_range")$value),
            x = x1, y = 0.565, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Reptiles", name == "cor_range")$value),
            x = x1, y = 0.40, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Fishes", name == "cor_range")$value),
            x = x1, y = 0.23, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Amphibians", name == "cor_duration")$value),
            x = x2, y = 0.9, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Birds", name == "cor_duration")$value),
            x = x2, y = 0.74, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Mammals", name == "cor_duration")$value),
            x = x2, y = 0.565, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Reptiles", name == "cor_duration")$value),
            x = x2, y = 0.40, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Fishes", name == "cor_duration")$value),
            x = x2, y = 0.23, size = 8) +
  add_phylopic(uuid = "bd80bc51-460c-4dd9-8341-e5b460372efb", fill = phylo_color, # amphibians
               x = x3, y = 0.793 , height = 0.035, alpha = 1) +
  add_phylopic(uuid = "157d3109-7124-413c-8362-3abcc6889a3f", fill = phylo_color, # birds
               x = x3, y = 0.625, height = 0.035, alpha = 1, horizontal = T) +
  add_phylopic(uuid = "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", fill = phylo_color, # mammals
               x = x3, y = 0.46, height = 0.035, alpha = 1, horizontal = T) +
  add_phylopic(uuid = "264fa655-afd7-451c-8f27-e0a9557376e6", fill = phylo_color, # reptiles
               x = x3, y = 0.29, height = 0.035, alpha = 1, horizontal = T) +
  add_phylopic(uuid = "c90aa49b-d9c5-44a4-a709-4f8d9a33b559", fill = phylo_color, # fishes
               x = x3, y = 0.12, height = 0.02, alpha = 1, horizontal = T)
  

ggsave(here("figures/Fig_03.jpg"),
       p_final,
       width = 13, height = 16, units = "cm", dpi = 1000)

