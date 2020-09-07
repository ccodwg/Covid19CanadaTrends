# load data
load_data <- function() {

  prov_map <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/other/prov_map.csv", stringsAsFactors = FALSE)
  hr_map <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/other/hr_map.csv", stringsAsFactors = FALSE)
  cases_ts <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/cases_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
    inner_join(prov_map, by = "province") %>%
    rename(
      date = date_report,
      incidence = cases
    ) %>%
    mutate(
      date = as.Date(date, "%d-%m-%Y")
    ) %>%
    select(province_short, date, incidence)
  mortality_ts <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/mortality_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
    inner_join(prov_map, by = "province") %>%
    rename(
      date = date_death_report,
      incidence = deaths
    ) %>%
    mutate(
      date = as.Date(date, "%d-%m-%Y")
    ) %>%
    select(province_short, date, incidence)
  active_ts <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/active_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
    inner_join(prov_map, by = "province") %>%
    rename(
      date = date_active,
      incidence = active_cases
    ) %>%
    mutate(
      date = as.Date(date, "%d-%m-%Y")
    ) %>%
    select(province_short, date, incidence)
  testing_ts <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/testing_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
    inner_join(prov_map, by = "province") %>%
    rename(
      date = date_testing,
      incidence = testing
    ) %>%
    mutate(
      date = as.Date(date, "%d-%m-%Y")
    ) %>%
    select(province_short, date, incidence)
  cases_ts_hr <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_hr/cases_timeseries_hr.csv", stringsAsFactors = FALSE) %>%
    inner_join(prov_map, by = "province") %>%
    inner_join(hr_map, by = c("province", "health_region")) %>%
    rename(
      date = date_report,
      incidence = cases
    ) %>%
    mutate(
      date = as.Date(date, "%d-%m-%Y")
    ) %>%
    select(province_short, health_region, HR_UID, date, incidence)
  mortality_ts_hr <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_hr/mortality_timeseries_hr.csv", stringsAsFactors = FALSE) %>%
    inner_join(prov_map, by = "province") %>%
    inner_join(hr_map, by = c("province", "health_region")) %>%
    rename(
      date = date_death_report,
      incidence = deaths
    ) %>%
    mutate(
      date = as.Date(date, "%d-%m-%Y")
    ) %>%
    select(province_short, health_region, HR_UID, date, incidence)

  ## return everything to parent environment
  invisible(list2env(mget(ls()), envir = parent.frame()))

}
