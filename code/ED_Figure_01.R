# Currently Figure S1

# calculate pre-industrial averages for each model
global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))


gwl <- global_temp_avg |> 
  group_by(model) |>
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) |> 
  mutate(gwl = temperature - pre_industrial_avg,
         gwl_rolling = temperature_rolling - pre_industrial_avg) |> 
  mutate(os = ifelse(gwl_rolling > 2, gwl_rolling, NA)) |> 
  mutate(os = ifelse(model == "CNRM-ESM2-1" & year > 2179, NA, os)) |> 
  filter(year >= 2014,
         year <= 2220) 

peak_gwl <- gwl |> 
  group_by(model) |> 
  slice_max(gwl_rolling) |> 
  select(model, year, gwl_rolling) |> 
  mutate(label = glue("{format(round(gwl_rolling, 2), nsmall = 2)}°C"))

low_gwl <- gwl |> 
  filter(year > 2100) |> 
  group_by(model) |> 
  slice_min(gwl_rolling) |> 
  select(model, year, gwl_rolling) |> 
  mutate(label = glue("{format(round(gwl_rolling, 2), nsmall = 2)}°C"))

begin_os <- gwl |> 
  group_by(model) |> 
  filter(gwl_rolling > 2) |> 
  slice_min(year) |> 
  select(model, year, gwl_rolling)

end_os <- gwl |> 
  filter(year > 2050) |> 
  group_by(model) |> 
  filter(gwl_rolling <= 2) |> 
  slice(1) |> 
  select(model, year, gwl_rolling)



length_os <- begin_os |> 
  left_join(end_os, by = "model") |> 
  mutate(duration = year.y - year.x)

models <- unique(gwl$model)


p <- map(models, function(x){
  
  data <- gwl |> 
    filter(model == x)
  
  text_low <- low_gwl |> 
    filter(model == x) |> 
    slice(1)
  
  text_peak <- peak_gwl |> 
    filter(model == x) |> 
    slice(1)
  
  l_os <- length_os |> 
    filter(model == x) |> 
    slice(1)
  
  
  text_low_position <- 5
  y_title <- ifelse(x %in% c(models[3], models[2]), "Global warming level (°C)", "")
  text_size <- 2.6
  
  ggplot(data, aes(x = year)) +
    geom_hline(yintercept = 2, colour = "#FF5D6F", linetype = 2, linewidth = 0.3) +
    geom_ribbon(aes(ymin = 2, ymax = os), fill = "#FF5D6F", alpha = 0.5) +
    geom_line(aes(y = gwl_rolling), linewidth = 0.4) +
    # geom_bracket(data = l_os, aes(xmin = 2015, xmax = year.y), label = "",
    #              y.position = 0.7, tip.length = -0.03, colour = "indianred4") +
    # geom_bracket(data = l_os, aes(xmin = year.x, xmax = 2220), label = "",
    #              y.position = 0.3, tip.length = -0.03, colour = "steelblue4") +
    
    coord_cartesian(expand = F, clip = "off") +
    scale_y_continuous(limits = c(0.2, 3.5)) +
    scale_x_continuous(limits = c(2013,2225)) +
    labs(y = y_title, title = x) +
    geom_text(data = text_peak, 
              aes(x = year, 
                  y = gwl_rolling + 0.20, 
                  label = label),
              size = text_size) +
    geom_text(data = text_low, 
              aes(x = year + text_low_position, 
                  y = gwl_rolling - 0.15, 
                  label = label),
              size = text_size) +
    geom_text(data = l_os, 
              aes(x = mean(c(year.x, year.y)), 
                  y = 1.8, 
                  label = glue("{duration} yrs")),
              size = text_size) +
    # geom_text(data = l_os, 
    #           aes(x = mean(c(2015, year.y)), 
    #               y = 0.9, 
    #               label = "exposure"),
    #           colour = "indianred4",
    #           size = text_size) +
    # geom_text(data = l_os, 
    #           aes(x = mean(c(year.x, 2220)), 
    #               y = 0.5, 
    #               label = "deexposure"),
    #           colour = "steelblue4",
    #           size = text_size) +
    theme_tidybayes() +
    theme(plot.title = element_text(size = 9, vjust = 2),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 7),
          plot.background = element_blank(),
          panel.background = element_blank())
  
})


(p[[3]] + p[[5]] + p[[1]] + plot_layout(nrow = 1)) /
  (plot_spacer() + p[[2]] + p[[4]] + plot_spacer() + 
     plot_layout(nrow = 1, widths = c(0.5, 1, 1, 0.5))) 


ggsave(here("figures/ED_Figure_01.jpg"),
       height = 12, width = 18, unit = "cm", dpi = 800)

