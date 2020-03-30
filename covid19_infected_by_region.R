

# functions ---------------------------------------------------------------

library("tidyverse")
library("gganimate")
library("transformr")  # sf + gganimate needs this
library("RCzechia")
library("tibbletime")  # because of rollify()
library("ggpomological")

source("data_utils.R")
ggplot2::theme_set(ggplot2::theme_minimal())


animate_me <- function(p, filename = NULL) {
  
  start_pause <- 1
  end_pause <- 5
  n_frames <- dplyr::n_distinct(p$data$date_reported)
  duration <- start_pause + n_frames/2 + end_pause
  
  anim <- gganimate::animate(
    plot = p, 
    start_pause = start_pause, 
    end_pause = end_pause, 
    duration = duration, 
    fps = 4, 
    width = 800, 
    height = 600
  )
  if (is.null(filename)) {
    return(anim)
  }
  dir.create("gifs/", showWarnings = FALSE)
  gganimate::anim_save(filename, anim, "gifs/")
  
  invisible(anim)
}



# data --------------------------------------------------------------------


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

caption_data_author <- "linkedin.com/in/dominik-matula\nDATA: bit.ly/2WRJaIL"
min_date_reported <- "2020-03-01"

running_daily_stats %>% 
  filter(date_reported >= min_date_reported) %>% 
  ggplot(aes(x = date_reported, y = n_infected)) + 
  geom_histogram(stat = "identity", alpha = .3) + 
  geom_line(aes(y = rolling_mean_infected, col = "Moving avg (5 days window)"), size = 1) + 
  facet_wrap("region_name") + 
  labs(
    x = "Date reported",
    y = "# infected",
    title = "Covid19: CASES reported by region",
    col = NULL,
    subtitle = str_c("Since ", min_date_reported),
    caption = caption_data_author
  ) + 
  theme(legend.position = "bottom")




# RELATIVE counts of Covid19-infected by region (CZE)

p_relative_counts <- 
  running_daily_stats %>% 
  mutate(
    running_count_per100k = running_count * 1e5 / popul
  ) %>% 
  filter(date_reported >= min_date_reported) %>% 
  ggplot() + 
  geom_sf(aes(fill = running_count_per100k, geometry = geometry)) + 
  scale_fill_gradient(low = "white", high = "firebrick") + 
  scale_x_continuous(breaks = seq(10, 20, .5)) + 
  labs(
    title = as.expression(bquote(bold("Covid19") ~"- CASES per 100k ihabitans (CZE)")),
    subtitle = "\nReported till: {frame_time}",
    fill = "CASES per 100k ihabitans",
    caption = caption_data_author
  ) + 
  theme(
    legend.position = "bottom", 
    legend.title.align = 1, 
    legend.title = element_text(vjust = 0.7, lineheight = 1.1, size = 13),  
    axis.text = element_blank(),
    plot.caption = element_text(color = "gray50"),
    plot.subtitle = element_text(lineheight = 1.1),
  ) +
  transition_time(date_reported)
  
anim <- animate_me(p_relative_counts, "relative_counts.gif")
anim


# GENDER percentage of infected (by date)

overall_male_pct <- infected_people %>% pull(gender) %>% `==`("Male") %>% mean() %>% round(1)

p_gender_of_cases <- 
  running_daily_stats %>% 
  filter(date_reported >= min_date_reported) %>% 
  ggplot() + 
  geom_hline(yintercept = overall_male_pct, linetype = "dashed", alpha = .6, col = "firebrick3", size = 1) + 
  geom_point(aes(x = region_name, y = running_pct_male, size = running_count), stat = "identity") + 
  scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent, limits = 0:1) + 
  labs(
    x = NULL,
    y = "% of male patients",
    title = as.expression(bquote(bold("Covid19") ~"- GENDER of patients by region (CZE)")),
    subtitle = "Proportion of male patients, by region\nReported till: {frame_time}"
  ) + 
  coord_flip() + 
  transition_time(date_reported)

animate_me(p_gender_of_cases, NULL)



# The same date, but plotted as a map
tol_min_cases <- 10
info_text <- str_c("% of MALE patients, iff # CASES > ", tol_min_cases)

p_map_gender <- running_daily_stats %>% 
  filter(date_reported >= min_date_reported) %>% 
  mutate(running_pct_male = ifelse(running_count < tol_min_cases, NA, running_pct_male)) %>% 
  ggplot() + 
  geom_sf(aes(fill = running_pct_male, geometry = geometry)) + 
  scale_fill_gradient2(low = "#E63900", mid = "gray85", high = "deepskyblue3", midpoint = overall_male_pct, na.value = "#FFFFFF00", limits = c(0, 1), labels = scales::percent) + 
  scale_x_continuous(breaks = seq(10, 20, .5)) + 
  labs(
    title = as.expression(bquote(bold("Covid19  -  GENDER") ~" of patients by region (CZE)")),
    subtitle = str_c(info_text, "\nReported till: {frame_time}"),
    fill = str_c(info_text,"\nMiddle: ", overall_male_pct, " (overall % male patients)"),
    caption = caption_data_author
  ) + 
  theme(
    legend.position = "bottom", 
    legend.title.align = 1, 
    legend.title = element_text(vjust = 0.7, lineheight = 1.1, size = 13),  
    axis.text = element_blank(),
    plot.caption = element_text(color = "gray50"),
    plot.subtitle = element_text(lineheight = 1.1),
  ) + 
  transition_time(date_reported)

anim <- animate_me(p_map_gender, "gender_of_infected.gif")
anim



# AGE

tol_min_cases <- 10
info_text <- str_c("Average AGE of patients, iff # CASES > ", tol_min_cases)

overall_mean_age <- infected_people %>% pull(age) %>% mean() %>% round(1)

p_map_age <- 
  running_daily_stats %>% 
  filter(date_reported >= min_date_reported) %>% 
  mutate(running_mean_age = ifelse(running_count < tol_min_cases, NA, running_mean_age)) %>% 
  ggplot() + 
  geom_sf(aes(fill = running_mean_age, geometry = geometry), col = "gray33") + 
  # geom_sf_text(aes(geometry = label_pos, label = n_infected), size = 2.5) + 
  scale_fill_gradient2(low = "goldenrod2", mid = "gray75", midpoint = overall_mean_age, high = "firebrick", na.value = "#FFFFFF00", breaks = seq(0, 100, 5)) + 
  scale_x_continuous(breaks = seq(10, 20, .5)) + 
  labs(
    title = as.expression(bquote(bold("Covid19  -  AGE") ~" of patients by region (CZE)")),
    subtitle = str_c(info_text, "\nReported till: {frame_time}"),
    fill = str_c(info_text,"\nMiddle: ", overall_mean_age, " (overall mean age)"),
    caption = caption_data_author
  ) + 
  theme(
    legend.position = "bottom", 
    legend.title.align = 1, 
    legend.title = element_text(vjust = 0.7, lineheight = 1.1, size = 13),  
    axis.text = element_blank(),
    plot.caption = element_text(color = "gray40"),
    plot.subtitle = element_text(lineheight = 1.1),
  ) +
  transition_time(date_reported)

anim <- animate_me(p_map_age, "age_of_infected.gif")
anim






# cummulative density of infected in region compared to global density (in CZE)

popul_total <- regions %>% pull(popul) %>% sum()
total_running_daily_stats <- 
  running_daily_stats %>% 
  group_by(date_reported) %>%
  summarise(running_count_total = sum(running_count)) %>%
  mutate(running_dens_total = running_count_total/popul_total)


running_dense_ratio <- 
  running_daily_stats %>%
  mutate(running_dens = running_count/popul) %>% 
  left_join(total_running_daily_stats, by = "date_reported") %>% 
  mutate(running_dens_ratio = running_dens/running_dens_total)

last_date_reported <- running_dense_ratio$date_reported %>% max(na.rm = TRUE)

selected_regions <- 
  running_dense_ratio %>% 
  group_by(region_name_short) %>% 
  summarise(
    last_dens_ratio = last(running_dens_ratio), 
    popul = first(popul)
  ) %>% 
  mutate(
    region_col = ifelse(last_dens_ratio > 1, as.character(region_name_short), NA),
    region_col = fct_reorder(region_col, popul, .desc = TRUE)
  )

p <- 
  running_dense_ratio %>% 
  filter(date_reported > "2020-03-08") %>% 
  left_join(selected_regions, by = "region_name_short") %>% 
  ggplot(aes(x = date_reported, y = running_dens_ratio, col = region_col, group = region_name_short)) + 
  geom_hline(yintercept = 1, linetype = "dotted", col = "firebrick3", size = .9) + 
  geom_line(key_glyph = "point") +
  geom_point(size = 1.8) +
  geom_segment(data = . %>% filter(!is.na(region_col)), aes(xend = last_date_reported, yend = running_dens_ratio), linetype = 2, col = 'grey') + 
  geom_text(data = . %>% filter(!is.na(region_col)), aes(x = last_date_reported + 0.3, label = region_col), hjust = 0, key_glyph = "point") + 
  scale_color_pomological(na.value = "gray77") + 
  scale_x_date(date_breaks = "week", date_labels = "%d/%m\nMon", minor_breaks = NULL) +
  scale_y_continuous(labels = function(x) { str_c(x,"x")}) + 
  labs(
    x = "Time",
    y = "Ratio of CASES Density:  region / whole country",
    col = as.expression(bquote("Regions with currently" ~ italic(abnormal) ~ " density of Covid19 CASES")),
    title = as.expression(bquote(bold("Covid19  -  Region density comparision") ~"(CZE)")),
    subtitle = "Comparision of REGION density of patients to COUNTRY density \nReported till: {frame_along}",
    caption = caption_data_author
  ) + 
  theme(
    legend.position = "bottom", 
    legend.title.align = 1, 
    legend.title = element_text(vjust = 0.7, lineheight = 1.1, size = 13),  
    plot.caption = element_text(color = "gray40"),
    plot.subtitle = element_text(lineheight = 1.1),
    plot.margin = margin(5.5, 40, 5.5, 5.5)
  ) + 
  transition_reveal(date_reported)

animate_me(p, "regions_density.gif")  

