


download_data_individuals <- function(url = "https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/osoby.csv") {
  
  file_name <- base::basename(url)
  dest_path <- stringr::str_c("data/", file_name)
  
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

