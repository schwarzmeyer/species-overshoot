# this script computes global warming levels at exposure and de-exposure.
# the data is used in figures 04 and 05. 

files <- list.files(here("results/species_exposure_times"), full.names = T)

data_raw <- bind_rows(map(files, readRDS))

# calculate pre-industrial averages for each model
global_temp_avg <- readRDS(here("processed_data/climate_data/global_averages/global_averages.rds"))

gwl <- global_temp_avg %>% 
  group_by(model) %>%
  mutate(pre_industrial_avg = mean(temperature[year <= 1900])) %>% 
  mutate(gwl = temperature_rolling - pre_industrial_avg) %>% 
  select(model, year, gwl)

# binding the lowest temp data to the main data
# gwl_diff the difference in temperature at exposure and deexposure
# if the value is negative, deexposure required further cooling

data_raw <- data_raw %>% 
  left_join(gwl, by = c("model", "exposure" = "year")) %>% 
  rename("gwl_exposure" = gwl) %>% 
  left_join(gwl, by = c("model", "deexposure" = "year")) %>% 
  rename("gwl_deexposure" = gwl) %>% 
  mutate(gwl_diff = gwl_deexposure - gwl_exposure) 


# select the year of peak warming
year_peak_warming <- gwl %>% 
  group_by(model) %>% 
  slice_max(gwl) %>%
  rename("year_peak" = year,
         "peak_gwl" = gwl) %>% 
  ungroup()

# here we analyse two overshoot periods
# the threshold for the general overshoot is based on the minimum global warming level after the peak
# for example, if the minimum warming level after the peak is 1.3°C, we consider 1.3°C the overshoot threshold
# the 2°C overshoot is based on the 2°C threshold

# 2°C overshoot
# select the year 2°C OS starts 
begin_os <- gwl %>% 
  filter(gwl > 2) %>% 
  group_by(model) %>% 
  slice_min(year) %>% 
  rename("begin_os" = year) %>% 
  select(model, begin_os)

# select the year 2°C OS ends
end_os <- gwl %>% 
  left_join(begin_os, by = "model") %>% 
  filter(year > begin_os,
         gwl < 2) %>% 
  group_by(model) %>% 
  slice_head() %>% 
  rename("end_os" = year) %>% 
  select(model, end_os)

os <- left_join(begin_os, end_os, by = "model")

# this df will be used to classify each phase of the OS
code_df <- left_join(os, year_peak_warming, by = "model")

# classify exposure events according to the gwl 
data <- data_raw %>% 
  left_join(code_df, by ="model") %>% 
  mutate(phase_2c_os = case_when(exposure < begin_os ~ "pre_os",
                                 exposure >= begin_os & exposure <= end_os ~ "os",
                                 exposure > end_os ~ "post_os")) %>% 
  mutate(code = case_when(gwl_diff > 0.04 ~ "less_cooling",
                          gwl_diff <= 0.04 & gwl_diff >= -0.04 ~ "same_gwl",
                          gwl_diff < -0.04 ~ "more_cooling")) %>% 
  mutate(code = ifelse(deexposure == 2221, "never", code)) 
# %>% 
#   select(-c(species, duration, year_os_starts, year_os_ends, year_2c_os_starts, year_2c_os_ends)) 



saveRDS(data, 
        here("results/gwl_exp_deexp/data.rds"))

