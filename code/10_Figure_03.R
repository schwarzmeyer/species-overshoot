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
         risk_metric = factor(risk_metric, levels = c("Range exposed", "Duration")),
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
  facet_grid(group ~ risk_metric, scales = "free_x") +
  theme_tidybayes() +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 7.5),
        legend.key.height = unit(1.2, "line"),
        legend.key.width = unit(1, "line"),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(l = 1, b = 1, t = 1, unit = "line"),
        strip.text = element_text(face = "bold"))


p_final <- ggdraw(p) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Amphibians", name == "cor_range")$value),
            x = 0.18, y = 0.9, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Birds", name == "cor_range")$value),
            x = 0.18, y = 0.74, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Mammals", name == "cor_range")$value),
            x = 0.18, y = 0.57, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Reptiles", name == "cor_range")$value),
            x = 0.18, y = 0.40, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Fishes", name == "cor_range")$value),
            x = 0.18, y = 0.23, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Amphibians", name == "cor_duration")$value),
            x = 0.52, y = 0.9, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Birds", name == "cor_duration")$value),
            x = 0.513, y = 0.74, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Mammals", name == "cor_duration")$value),
            x = 0.52, y = 0.57, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Reptiles", name == "cor_duration")$value),
            x = 0.52, y = 0.40, size = 8) +
  draw_text(text = paste("ρ =", filter(df_cor, group == "Fishes", name == "cor_duration")$value),
            x = 0.52, y = 0.23, size = 8)


ggsave(here("figures/Fig_03.jpg"),
       p_final,
       width = 13, height = 16, units = "cm", dpi = 1000)



# pylopics ----
phylopics <- c("bd80bc51-460c-4dd9-8341-e5b460372efb", # amphibian
               "157d3109-7124-413c-8362-3abcc6889a3f", # birds
               "1e606dbc-f881-4bd7-aaa5-01130f1fc6cc", # mammals
               "264fa655-afd7-451c-8f27-e0a9557376e6", # reptiles
               "c90aa49b-d9c5-44a4-a709-4f8d9a33b559") # fish


phylosize <- c(0.75,0.85,0.7,0.9,0.8,0.7,0.5,0.55,0.75,0.7,0.85)

group_barplot <- plot_risk_data_group %>% 
  mutate(group = factor(group, levels = plot_order)) %>% 
  ggplot(aes(x = perc, y = group, fill = risk_category)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(direction = 1, option = "F", begin = 0.15, end = 0.84,
                       name = "", breaks = c("Low risk","Moderate risk","High risk","Very high risk")) +
  labs(x = "Proportion of species (%)", y = "") +
  scale_x_continuous(expand = c(0,0)) +
  theme_tidybayes() +
  theme(axis.line.y = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(r = 50),
        axis.title.x = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 9)) +
  add_phylopic(uuid = phylopics, color = phylocols,
               x = 104, y = 11:1, ysize = phylosize, alpha = 1) +
  coord_cartesian(clip = "off", xlim = c(0,100)) 



cor.test(log(df$meanED), log(df$mean_local_duration), method = "spearman")$p.value
cor.test(log(df$meanED), log(df$mean_local_duration), method = "spearman")$estimate

