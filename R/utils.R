#' Load data from the COVID-19 Canada Open Data Working Group dataset
#' @name load_Covid19Canada
#'
#' @param x Character vector of the datasets to load: "all" (all time series), "all_prov" (all provincial time series), "all_hr" (all health region time series), or a vector of individual file names.
#' @details
#' Valid datasets include:
#' * cases_ts_prov
#' * mortality_ts_prov
#' * active_ts_prov
#' * testing_ts_prov
#' * cases_ts_hr
#' * mortality_ts_hr
#' @importFrom dplyr inner_join rename mutate select
#' @export

# load data
load_Covid19Canada <- function(x = c("all", "all_prov", "all_hr")) {

  ## must select one mode: prov or hr
  if (identical(x, c("all", "all_prov", "all_hr"))) {
    stop("Please select one of the following: all, all_prov, all_hr, or a vector of individual files.")
  }

  ## load province and/or health region name files
  if (any(c("all", "all_prov", "cases_ts_prov", "mortality_ts_prov", "active_ts_prov", "testing_ts_prov") %in% x)) {
    prov_map <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/other/prov_map.csv", stringsAsFactors = FALSE)
  }
  if (any(c("all", "all_prov", "cases_ts_hr", "mortality_ts_hr") %in% x)) {
    hr_map <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/other/hr_map.csv", stringsAsFactors = FALSE)
  }

  ## load time series: cases by province
  if (any(c("all", "all_prov", "cases_ts_prov") %in% x)) {
    cases_ts_prov <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/cases_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
      inner_join(prov_map, by = "province") %>%
      rename(
        date = date_report,
        val = cases
      ) %>%
      mutate(
        date = as.Date(date, "%d-%m-%Y")
      ) %>%
      select(province_short, date, val)
    }
  ## load time series: mortality by province
  if (any(c("all", "all_prov", "mortality_ts_prov") %in% x)) {
    mortality_ts_prov <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/mortality_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
      inner_join(prov_map, by = "province") %>%
      rename(
        date = date_death_report,
        val = deaths
      ) %>%
      mutate(
        date = as.Date(date, "%d-%m-%Y")
      ) %>%
      select(province_short, date, val)
    }
  ## load time series: active cases by province
  if (any(c("all", "all_prov", "active_ts_prov") %in% x)) {
    active_ts_prov <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/active_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
      inner_join(prov_map, by = "province") %>%
      rename(
        date = date_active,
        val = active_cases
      ) %>%
      mutate(
        date = as.Date(date, "%d-%m-%Y")
      ) %>%
      select(province_short, date, val)
    }
  ## load time series: testing by province
  if (any(c("all", "all_prov", "testing_ts_prov") %in% x)) {
    testing_ts_prov <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_prov/testing_timeseries_prov.csv", stringsAsFactors = FALSE) %>%
      inner_join(prov_map, by = "province") %>%
      rename(
        date = date_testing,
        val = testing
      ) %>%
      mutate(
        date = as.Date(date, "%d-%m-%Y")
      ) %>%
      select(province_short, date, val)
    }
  if (any(c("all", "all_hr", "cases_ts_hr") %in% x)) {
    cases_ts_hr <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_hr/cases_timeseries_hr.csv", stringsAsFactors = FALSE) %>%
      inner_join(prov_map, by = "province") %>%
      inner_join(hr_map, by = c("province", "health_region")) %>%
      rename(
        date = date_report,
        val = cases
      ) %>%
      mutate(
        date = as.Date(date, "%d-%m-%Y")
      ) %>%
      select(province_short, health_region, HR_UID, date, val)
    }
  if (any(c("all", "all_hr", "mortality_ts_hr") %in% x)) {
    mortality_ts_hr <- read.csv("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_hr/mortality_timeseries_hr.csv", stringsAsFactors = FALSE) %>%
      inner_join(prov_map, by = "province") %>%
      inner_join(hr_map, by = c("province", "health_region")) %>%
      rename(
        date = date_death_report,
        val = deaths
      ) %>%
      mutate(
        date = as.Date(date, "%d-%m-%Y")
      ) %>%
      select(province_short, health_region, HR_UID, date, val)
    }

  ## return everything to parent environment
  invisible(list2env(mget(ls()), envir = parent.frame()))

}

#' Get most recent update date of the COVID-19 Canada Open Data Working Group dataset
#' @name get_update_date
#'
#' @export

get_update_date <- function() {

  as.Date(readLines("https://github.com/ishaberry/Covid19Canada/raw/master/update_time.txt"))

}
