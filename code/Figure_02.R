 
amph <- read_csv2(here("processed_data/species_data/attribute_tables/Amphibians.csv"))
bird <- read_csv2(here("processed_data/species_data/attribute_tables/Birds.csv"))
mamm <- read_csv2(here("processed_data/species_data/attribute_tables/Mammals.csv"))
rept <- read_csv2(here("processed_data/species_data/attribute_tables/Reptiles.csv"))
fish <- read_csv2(here("processed_data/species_data/attribute_tables/Fishes.csv"))

amph <- select(amph, sci_name, category)
bird <- select(bird, sci_name)
mamm <- select(mamm, sci_name, category)
rept <- select(rept, sci_name, category)
fish <- select(fish, sci_name, category)


birds_cat <- read_excel(here("raw_data/species_data/range_maps_iucn/BIRDS/HBW-BirdLife_Checklist_v6b_Jul22/Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_6b.xlsx"))
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
risk_data <- readRDS(here("results/risk/risk_raw_median.rds"))
data_iucn <- left_join(risk_data, df, by = c("species" = "sci_name"))
data_iucn <- distinct(data_iucn)

p1 <- data_iucn %>% 
  filter(category %in% c("CR","EN","LC","VU","NT","DD")) %>% 
  mutate(category = factor(category,
                           levels = c("LC","NT","VU","EN","CR","DD"))) %>% 
  group_by(category) %>% 
  ggplot(aes(y = round(range_exposed*100,0), x = category, fill = category, colour = category)) +
  stat_halfeye(
    adjust = 1.1, 
    width = .6, 
    .width = 0, 
    justification = -.5,
    point_colour = NA,
    show.legend = F) + 
  geom_boxplot(
    width = .35, 
    outlier.shape = NA,
    show.legend = F,
    alpha = 0.3,
    linewidth = 0.35
  ) +
  labs(x = "", y = "Range exposed (%)", title = "") +
  scale_fill_manual(values = darken(c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7"),
                                    amount = 0.08)) +
  scale_colour_manual(values = darken(c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7"),
                                      amount = 0.08)) +
  theme_tidybayes() +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 7.4),
        axis.title = element_text(size = 9))

p2 <- data_iucn %>% 
  filter(category %in% c("CR","EN","LC","VU","NT","DD")) %>% 
  mutate(category = factor(category,
                           levels = c("LC","NT","VU","EN","CR","DD"))) %>% 
  group_by(category) %>% 
  ggplot(aes(y = mean_local_duration, x = category, fill = category, colour = category)) +
  stat_halfeye(
    adjust = .7, 
    width = .5, 
    .width = 0, 
    justification = -.55,
    point_colour = NA,
    show.legend = F) + 
  geom_boxplot(
    width = .35, 
    outlier.shape = NA,
    show.legend = F,
    alpha = 0.3,
    linewidth = 0.35
  ) +
  labs(x = "", y = "Duration (years)", title = "") +
  scale_fill_manual(values = darken(c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7"),
                                      amount = 0.08)) +
  scale_colour_manual(values = darken(c("#50bc1e","#06b491","#f9e814","#f4900e","#D72104","#d1d1c7"),
                                      amount = 0.08)) +
  theme_tidybayes() +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 7.4),
        axis.title = element_text(size = 9))


pp <- plot_grid(p1, p2, ncol = 2, 
                labels = "auto", 
                label_size = 9, 
                label_x = 0.02,
                scale = 0.95)

ggsave(here("figures/Fig_02.jpg"),
       pp,
       width = 16, height = 6.5, units = "cm", dpi = 700)
