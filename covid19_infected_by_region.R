

library("tidyverse")
library("gganimate")
library("transformr")  # sf + gganimate needs this
library("RCzechia")
library("tibbletime")  # because of rollify()

source("data_utils.R")
ggplot2::theme_set(ggplot2::theme_minimal())


# get data
infected_people <- load_data_individuals()
regions <- get_czech_regions()


# transform data
daily_stats <- prepare_daily_stats(infected_people, since = "2020-03-01")

running_daily_stats <- 
  add_running_vars(daily_stats) %>% 
  left_join(regions, by = c("region" = "region_nuts3"))




# PLOTS -------------------------------------------------------------------


# CASES reported by region and date

running_daily_stats %>% 
  ggplot(aes(x = date_reported, y = n_infected)) + 
  geom_histogram(stat = "identity", alpha = .3) + 
  geom_line(aes(y = rolling_mean_infected, col = "Moving avg (5 days window)"), size = 1) + 
  facet_wrap("region_name") + 
  labs(
    x = "Date reported",
    y = "# infected",
    title = "Covid19: CASES reported by region",
    col = NULL
  ) + 
  theme(legend.position = "bottom")



# Gender percentage of infected (by date)

p <- running_daily_stats %>% 
  filter(date_reported >= "2020-03-15") %>% 
  ggplot() + 
  geom_hline(yintercept = .5, linetype = "dashed", alpha = .6, col = "firebrick3", size = 1) + 
  geom_point(aes(x = region_name, y=running_pct_male, size = running_count), stat = "identity") + 
  scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent, limits = 0:1) + 
  labs(
    x = NULL,
    y = "% of male patients",
    title = "Covid19 - Proportion of male patients, by region",
    subtitle = "Date till: {frame_time}"
  ) + 
  coord_flip() + 
  transition_time(date_reported)

gganimate::animate(p, end_pause = 100, fps = 25, duration = 15, width=400, height=600)


# The same date, but plotted as a map

running_daily_stats %>% 
  filter(date_reported == "2020-03-15") %>% 
  mutate(running_pct_male = ifelse(running_count < 10, NA, running_pct_male)) %>% 
  ggplot() + 
  geom_sf(aes(fill = running_pct_male, geometry = geometry)) + 
  geom_sf_text(aes(geometry = label_pos, label = n_infected), size = 2.5)



tol_min_cases <- 10
info_text <- str_c("% of MALE patients, iff # CASES > ", tol_min_cases)

p <- running_daily_stats %>% 
  filter(date_reported >= "2020-03-01") %>% 
  mutate(running_pct_male = ifelse(running_count < tol_min_cases, NA, running_pct_male)) %>% 
  ggplot() + 
  geom_sf(aes(fill = running_pct_male, geometry = geometry)) + 
  scale_fill_gradient2(low = "#E63900", mid = "gray85", high = "deepskyblue3", midpoint = 0.5, na.value = "#FFFFFF00", limits = c(0, 1), labels = scales::percent) + 
  labs(
    title = "Covid19: GENDER of patients by region (CZE)",
    subtitle = str_c(info_text, "\nReported till: {frame_time}"),
    fill = info_text
  ) + 
  theme(legend.position = "bottom", legend.title.align = 1, axis.text = element_blank()) +
  transition_time(date_reported)

gganimate::animate(p, end_pause = 10, fps = 5, duration = 10, width=600, height=400)



# Age

p <- 
  running_daily_stats %>% 
  filter(date_reported >= "2020-03-01") %>% 
  mutate(running_mean_age = ifelse(running_count <= 10, NA, running_mean_age)) %>% 
  ggplot() + 
  geom_sf(aes(fill = running_mean_age, geometry = geometry), col = "gray77") + 
  # geom_sf_text(aes(geometry = label_pos, label = n_infected), size = 2.5) + 
  scale_fill_gradient(low = "gray33", high = "firebrick", na.value = "#FFFFFF00") + 
  labs(
    title = "Covid19 - Average age of patients",
    subtitle = "Reported till: {frame_time}",
    fill = "Average age of patients\niff # CASES > 10"
  ) + 
  theme(legend.position = "bottom", legend.title.align = 1, axis.text = element_blank()) +
  transition_time(date_reported)

gganimate::animate(p, end_pause = 10, fps = 5, duration = 10, width=600, height=400)




# do dat je potřeba přidat 0
# přidat info, že data nejsou k dispozici
# přidat informaci o počtu nakažených (druhá osa y)
