# calculate 7-day rolling averages
rolling_average <- function(dat) {

  dat %>%
    {if ("health_region" %in% names(.)) group_by(., province_short, health_region) else group_by(., province_short)} %>%
    mutate(
      avg_7_day = rollapply(incidence, 7, mean, na.rm = TRUE, align = "right", partial = TRUE)
    )

}

# report trends in 7-day rolling averages by province or health region
trends_rolling_average <- function(dat, stat = c("cases", "mortality", "active", "testing"), newest_date, loc = c("all_prov", "all_hr"), threshold = 10, print_abs = FALSE, name) {

  ## must select one mode: prov or hr
  if (identical(loc, c("all_prov", "all_hr"))) {
    error("Please select one of the following: all_prov, all_hr, a vector of province codes, or a vector of health region codes.")
  }

  ## set mode: prov or hr
  if (loc[1] %in% c("all_prov", prov_map$province_short)) {
    match.arg(stat, choices = c("cases", "mortality", "active", "testing"), several.ok = FALSE)
    m <- "prov"
  } else if (loc[1] %in% c("all_hr", hr_map$HR_UID)) {
    match.arg(stat, choices = c("cases", "mortality"), several.ok = FALSE)
    m <- "hr"
  }

  ## filter provinces or health regions
  if (m == "prov") {
    if (!identical(loc, "all_prov")) {
      dat <- dat %>%
        filter(province_short %in% loc)
    }
  } else if (m == "hr") {
    if (!identical(loc, "all_hr")) {
      dat <- dat %>%
        filter(HR_UID %in% loc)
    }
  }

  ## calculate 7-day rolling average
  dat <- rolling_average(dat)

  ## calculate percent change from 7 days ago
  if (m == "prov") {
    loc <- unique(dat$province_short)
  } else if (m == "hr") {
    loc <- distinct(dat[, c("province_short", "health_region")])
  }
  a_newest_date <- dat %>%
    filter(date == newest_date) %>%
    pull("avg_7_day")
  a_7_days_ago <- dat %>%
    filter(date == newest_date - 7) %>%
    pull("avg_7_day")
  dat <- {if (m == "prov") {
    data.frame(
      prov = loc,
      old = a_7_days_ago,
      new = a_newest_date,
      change = (a_newest_date - a_7_days_ago) / a_7_days_ago * 100
    )
  } else {
    data.frame(
      prov = loc$province_short,
      hr = loc$health_region,
      old = a_7_days_ago,
      new = a_newest_date,
      change = (a_newest_date - a_7_days_ago) / a_7_days_ago * 100
    )
  }
  } %>%
    arrange(desc(change)) %>%
    mutate(
      emoji = case_when(
        abs(change) < threshold ~ as.character(emo::ji("heavy_minus_sign")),
        change > threshold ~ as.character(emo::ji("chart_with_upwards_trend")),
        change < -threshold ~ as.character(emo::ji("chart_with_downwards_trend"))
      ),
      old = paste0(formatC(old, digits = 1, format = "f", big.mark = ","), "/day"),
      new = paste0(formatC(new, digits = 1, format = "f", big.mark = ","), "/day"),
      change = paste0(formatC(change, digits = 1, format = "f", big.mark = ",", flag = "+"), "%"),
    )
  if (stat == "cases") {
    topline <- paste0("Percent change in 7-day rolling average of cases compared to one week ago (threshold: ", threshold, "% change)")
  } else if (stat == "mortality") {
    topline <- paste0("Percent change in 7-day rolling average of deaths compared to one week ago (threshold: ", threshold, "% change)")
  } else if (stat == "active") {
    topline <- paste0("Percent change in 7-day rolling average of active cases compared to one week ago (threshold: ", threshold, "% change)")
  } else if (stat == "testing") {
    topline <- paste0("Percent change in 7-day rolling average of completed tests compared to one week ago (threshold: ", threshold, "% change)")
  }
  if (m == "prov") {
    if (print_abs) {
      dat <- data.frame(line = c(topline, paste0(dat$prov, ": ", dat$change, " ", dat$emoji, " (", dat$old, " ", as.character(emo::ji("right_arrow")), " ", dat$new, ")")))
    } else {
      dat <- data.frame(line = c(topline, paste0(dat$prov, ": ", dat$change, " ", dat$emoji)))
    }
  } else if (m == "hr") {
    if (print_abs) {
      dat <- data.frame(line = c(topline, paste0(dat$hr, " (", dat$prov, "): ", dat$change, " ", dat$emoji, " (", dat$old, " ", as.character(emo::ji("right_arrow")), " ", dat$new, ")")))
    } else {
      dat <- data.frame(line = c(topline, paste0(dat$hr, " (", dat$prov, "): ", dat$change, " ", dat$emoji)))
    }
  }
  write.table(dat, file = file, sep = "\n", row.names = FALSE, quote = FALSE, col.names = FALSE)

}
