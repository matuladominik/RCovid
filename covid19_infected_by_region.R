

library("tidyverse")
library("gganimate")
library("transformr")  # sf + gganimate needs this
library("RCzechia")

source("data_utils.R")



ggplot2::theme_set(ggplot2::theme_void())

# get data
infected <- load_data_individuals()
regions <- 
  RCzechia::kraje("low") %>% 
  set_names(c("region_code", "region_nuts3", "region_name", "geometry")) %>% 
  select(-region_code)


# transform data
daily_stats <- 
  infected %>% 
  filter(date_reported > "2020-03-05") %>% 
  group_by(date_reported, region) %>% 
  arrange(date_reported) %>% 
  mutate(
    running_mean_age = dplyr::cummean(age),
    running_pct_male = dplyr::cummean(gender == "Male"),
    tmp = 1,
    running_count = cumsum(tmp)
  ) %>% 
  select(-tmp)



# PLOTS -------------------------------------------------------------------

daily_stats <- 
  daily_stats %>% 
  left_join(regions, by = c("region" = "region_nuts3"))


# Gender percentage of infected (by date)

daily_stats %>% 
  filter(date_reported > "2020-03-25") %>% 
  ggplot() + 
  geom_sf(aes(fill = running_pct_male, geometry = geometry)) + 
  scale_x_continuous(breaks = NULL) + 
  scale_fill_continuous(limits = c(0, 1), labels = scales::percent) + 
  labs(
    title = "Covid19:  % of male patients",
    subtitle = "Date reported: 2020-03-25, cummulative stats",
    fill = "% of MALE\npatients"
  ) + 
  transition_time(date_reported)


# Age

