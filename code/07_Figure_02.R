
amph <- read_csv2(paste0(path, "Data/Species Data/Attribute Tables/Amphibians.csv"))
bird <- read_csv2(paste0(path, "Data/Species Data/Attribute Tables/Birds.csv"))
mamm <- read_csv2(paste0(path, "Data/Species Data/Attribute Tables/Mammals.csv"))
rept <- read_csv2(paste0(path, "Data/Species Data/Attribute Tables/Reptiles.csv"))
fish <- read_csv2(paste0(path, "Data/Species Data/Attribute Tables/Fishes.csv"))

amph <- select(amph, sci_name, category)
bird <- select(bird, sci_name)
mamm <- select(mamm, sci_name, category)
rept <- select(rept, sci_name, category)
fish <- select(fish, sci_name, category)



birds_cat <- read_excel(paste0(path, "Data/Species Data/Range Maps IUCN/BIRDS/HBW-BirdLife_Checklist_v6b_Jul22/Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_6b.xlsx"))
colnames(birds_cat) <- birds_cat[2,]

birds_cat <- birds_cat %>% 
  slice(-c(1,2)) %>% 
  select(`Scientific name`, `2022 IUCN Red List category`) %>% 
  rename(sci_name = `Scientific name`,
         category = `2022 IUCN Red List category`) %>% 
  na.omit() %>% 
  filter(category != "NR") %>% 
  distinct()


bird <- left_join(bird, birds_cat, by = "sci_name")

df <- bind_rows(amph, bird, mamm, rept, fish)

risk_data <- readRDS(paste0(path, "Results/Risk Metric/risk_raw_median.rds"))

data_iucn <- left_join(risk_data, df, by = c("species" = "sci_name"))

data_iucn <- distinct(data_iucn)

p1 <- data_iucn %>% 
  # mutate(realm = ifelse(group %in% c("Amphibians","Birds","Mammals","Reptiles"), "Terrestrial species","Marine species")) %>% 
  filter(category %in% c("CR","EN","LC","VU","NT","DD")) %>% 
  mutate(category = factor(category,
                           levels = c("LC","NT","VU","EN","CR","DD"))) %>% 
  group_by(category) %>% 
  ggplot(aes(y = round(range_exposed*100,0), x = category, fill = category, colour = category)) +
  ggdist::stat_halfeye(
    adjust = .8, 
    width = .6, 
    .width = 0, 
    justification = -.4,
    point_colour = NA,
    show.legend = F) + 
  geom_point(
    aes(colour = category),
    size = 1.1,
    alpha = 0.01,
    position = position_jitter(seed = 1, width = .1),
    show.legend = F
  ) + 
  geom_boxplot(
    width = .35, 
    outlier.shape = NA,
    show.legend = F,
    alpha = 0,
    linewidth = 0.5
  ) +
  # stat_summary(fun="median", 
  #              show.legend = F, 
  #              geom =  "text", 
  #              aes(label = format(round(after_stat(y), digits = 2), nsmall = 0)),
  #              vjust = 1.7,
  #              colour = "black", family = "Tahoma", size = 2.7, fontface = "bold") +
  labs(x = "", y = "Range exposed (%)", title = "") +
  scale_fill_manual(values = c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7")) +
  scale_colour_manual(values = darken(c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7"),
                                      amount = 0.1)) +  
  theme_tidybayes() +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(family = "Tahoma"),
        axis.text = element_text(size = 9, family = "Tahoma"),
        panel.grid.major.y = element_line(linewidth = 0.3))


p2 <- data_iucn %>% 
  # mutate(realm = ifelse(group %in% c("Amphibians","Birds","Mammals","Reptiles"), "Terrestrial species","Marine species")) %>% 
  filter(category %in% c("CR","EN","LC","VU","NT","DD")) %>% 
  mutate(category = factor(category,
                           levels = c("LC","NT","VU","EN","CR","DD"))) %>% 
  group_by(category) %>% 
  ggplot(aes(y = mean_local_duration, x = category, fill = category, colour = category)) +
  ggdist::stat_halfeye(
    adjust = .7, 
    width = .5, 
    .width = 0, 
    justification = -.55,
    point_colour = NA,
    show.legend = F) + 
  geom_point(
    aes(colour = category),
    size = 1.1,
    alpha = 0.01,
    position = position_jitter(seed = 1, width = .1),
    show.legend = F
  ) + 
  geom_boxplot(
    width = .35, 
    outlier.shape = NA,
    show.legend = F,
    alpha = 0,
    linewidth = 0.5
  ) +
  # stat_summary(fun="median", 
  #              show.legend = F, 
  #              geom =  "text", 
  #              aes(label = format(round(after_stat(y), digits = 0), nsmall = 0)),
  #              vjust = 1.7,
  #              colour = "black", family = "Tahoma", size = 2.7, fontface = "bold") +
  labs(x = "", y = "Duration (years)", title = "") +
  scale_fill_manual(values = c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7")) +
  scale_colour_manual(values = darken(c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7"),
                                      amount = 0.1)) +
  theme_tidybayes() +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(family = "Tahoma"),
        axis.text = element_text(size = 9, family = "Tahoma"),
        panel.grid.major.y = element_line(linewidth = 0.3)); p2

p <- p1 + p2 + plot_layout(ncol = 1)



ggsave(paste0(path, "Figures/_Fig_03.jpg"),
       p,
       width = 8.9, height = 15, units = "cm", dpi = 700)


p <- p1 + p2 + plot_layout(ncol = 2)



ggsave(paste0(path, "Figures/_Fig_03_rescue.jpg"),
       p,
       width = 20, height = 9, units = "cm", dpi = 700)
