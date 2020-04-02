

# load Covid19 data -------------------------------------------------------

is_recent <- function(file_path, max_age_secs = 3600) {
  
  mtime <- file.mtime(file_path)
  now <- Sys.time()
  
  age_secs <- as.numeric(now - mtime, units = "secs")
  age_secs < max_age_secs
}


download_data_individuals <- function(url = "https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/osoby.csv") {
  
  file_name <- base::basename(url)
  dest_path <- stringr::str_c("data/", file_name)
  
  if (file.exists(dest_path) && is_recent(dest_path)) {
    return(invisible(dest_path))
  }
  
  curl::curl_download(url, destfile = dest_path, quiet = TRUE)   # TODO: add check if outdated
  invisible(dest_path)
}


load_data_individuals <- function() {
  
  path_data <- download_data_individuals()
  
  col_types <- cols(
    "datum_hlaseni" = col_date(format = "%Y-%m-%d"),
    "vek" = col_double(),
    "pohlavi" = col_character(),
    "kraj" = col_character(),
    "nakaza_v_zahranici" = col_double(),
    "nakaza_zeme_csu_kod" = col_character()
  )
  individuals <- read_csv(path_data, col_types = col_types)
  individuals %>% 
    rename(
      date_reported = datum_hlaseni, 
      age = vek,
      gender = pohlavi,
      region = kraj,
      infected_abroad = nakaza_v_zahranici,
      country_of_infection = nakaza_zeme_csu_kod
    ) %>% 
    mutate(
      gender = fct_recode(gender, Female = "Z", Male = "M"),
      infected_abroad = 
        infected_abroad %>% 
        replace_na(0) %>% 
        as.logical(),
      country_of_infection = 
        ifelse(infected_abroad, replace_na(country_of_infection, "?"), replace_na(country_of_infection, "CZ")) %>% 
        fct_infreq()
    )
}


# load other data ---------------------------------------------------------


get_czech_regions <- function(resolution = "low") {
  stopifnot(resolution %in% c("low", "high"))
  regions <- 
    RCzechia::kraje("low") %>% 
    set_names(c("region_code", "region_nuts3", "region_name", "geometry")) %>% 
    select(-region_code)
  
  adjust_label_pos_by <- function(regions, nuts3_code, by = c(0, 0)) {
    which_point <- which(regions$region_nuts3 == nuts3_code)
    regions$label_pos[which_point] <- regions$label_pos[which_point] + by
    invisible(regions)    
  }
  
  regions %>% 
    add_czech_regions_population() %>% 
    mutate(label_pos = sf::st_centroid(geometry)) %>% 
    adjust_label_pos_by("CZ020", c(0.5, 0)) %>% 
    adjust_label_pos_by("CZ071", c(0, -0.2))
}



add_czech_regions_population <- function(regions) {
  # https://www.czso.cz/documents/10180/91917344/1300721901.xlsx/a0e0c274-c1d0-4e78-bf77-99a177538d3a?version=1.0
  
  region_popul <- tribble(
    ~region_nuts3, ~region_name_short, ~popul, ~popul_male, ~popul_female, ~mean_age, ~mean_age_male, ~mean_age_female,
    "CZ010", "PHA", 1308632,	638009,	670623,	41.9,	40.5,	43.3,
    "CZ020", "STC", 1369332,	676696,	692636,	41.2,	40.0,	42.4,
    "CZ031", "JHC", 642133,	317268,	324865,	42.7,	41.4,	43.9,
    "CZ032", "PLK", 584672,	290226,	294446,	42.7,	41.5,	43.9,
    "CZ041", "KVK", 294896,	145674,	149222,	42.9,	41.5,	44.2,
    "CZ042", "ULK", 820789,	407395,	413394,	42.0,	40.7,	43.4,
    "CZ051", "LBK", 442356,	217791,	224565,	42.1,	40.7,	43.5,
    "CZ052", "HKK", 551021,	271591,	279430,	43.1,	41.6,	44.5,
    "CZ053", "PAK", 520316,	257948,	262368,	42.4,	41.0,	43.8,
    "CZ063", "VYS", 509274,	253061,	256213,	42.8,	41.4,	44.1,
    "CZ064", "JHM", 1187667,	582516,	605151,	42.4,	40.9,	43.8,
    "CZ071", "OLK", 632492,	309620,	322872,	42.8,	41.2,	44.3,
    "CZ072", "ZLK", 582921,	285883,	297038,	43.1,	41.4,	44.7,
    "CZ080", "MSK", 1203299,	590516,	612783,	42.7,	41.1,	44.2
  )
  regions %>% 
    left_join(region_popul, by = "region_nuts3") %>% 
    mutate_at(vars(region_nuts3, region_name_short), ~fct_reorder(., popul, .desc = TRUE))
}



# preparing daily stats ---------------------------------------------------


add_empty_days <- function(daily_stats) {
  days <- unique(daily_stats$date_reported)
  regions <- unique(daily_stats$region)
  
  crossing(date_reported = days, region = regions) %>% 
    left_join(daily_stats, by = c("date_reported", "region")) %>% 
    mutate_at(vars(-region, -date_reported), ~replace_na(., 0))
}


prepare_daily_stats <- function(infected_people, since = "2020-03-01") {
  daily_stats <- 
    infected_people %>% 
    filter(date_reported > since) %>% 
    group_by(date_reported, region) %>% 
    summarise(
      n_infected = n(),
      n_male = sum(gender == "Male"),
      n_eldest60 = sum(age >= 60),
      n_eldest65 = sum(age >= 65),
      n_youth18 = sum(age <= 18),
      n_youth30 = sum(age <= 30),
      sum_age = sum(age),
      n_infected_abroad = sum(infected_abroad)
    )
  daily_stats %>% 
    add_empty_days()
}


add_running_vars <- function(daily_stats) {
  daily_stats %>% 
    group_by(region) %>% 
    arrange(date_reported) %>% 
    mutate(
      running_count = cumsum(n_infected),
      running_mean_age = cumsum(sum_age)/running_count,
      running_pct_male = cumsum(n_male)/running_count,
      running_pct_infected_abroad = cumsum(n_infected_abroad)/running_count,
      running_count_eldest60 = cumsum(n_eldest60),
      running_count_eldest65 = cumsum(n_eldest65),
      running_count_youth18 = cumsum(n_youth18),
      running_count_youth30 = cumsum(n_youth30),
      running_pct_eldest60 = running_count_eldest60/running_count,
      running_pct_eldest65 = running_count_eldest65/running_count,
      running_pct_youth18 = running_count_youth18/running_count,
      running_pct_youth30 = running_count_youth30/running_count,
      rolling_mean_infected = rollify(mean, window = 5)(n_infected)
    ) %>% 
    select(date_reported, region, running_count, n_infected, everything())
}
