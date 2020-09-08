#' Calculate rolling averages
#'
#' @name rolling_average
#' @param x The time series for which to calculate the rolling average (for column "val").
#' @param window_days The length of the window in days for the rolling average (default = 7).
#' @return The time series with the rolling average added in new column "rolling_avg".
#' @importFrom dplyr group_by mutate
#' @importFrom zoo rollapply
#' @export

rolling_average <- function(x, window_days = 7) {

  x %>%
    {if ("health_region" %in% names(.)) group_by(., province_short, health_region) else group_by(., province_short)} %>%
    mutate(
      rolling_avg = rollapply(val, window_days, mean, na.rm = TRUE, align = "right", partial = TRUE)
    )

}

#' Calculate and summarize percent change in rolling average in tweetable form
#'
#' @name trends_rolling_average
#' @param x The time series for which to calculate the rolling average.
#' @param stat The stat of the time series. One of "cases", "mortality", "active', or "testing".
#' @param new_date The date for which to calculate the percent change since the previous date (by default, the most recent date the dataset was updated).
#' @param loc The locations for which to calculate trends. One of "all_prov" (all provinces), "all_hr" (all health regions), a vector of 2-letter province codes, or a vector of 4-number health region codes.
#' @param before_days The number of days before `new_date` to compare with to calculate the percent change (default: 7).
#' @param window_days The length of the window in days for the rolling average (default = 7).
#' @param threshold The percent threshold (in absolute value) for delineating a positive/negative trend from no change (default = 10).
#' @param change_digits The number of digits to print for the percent change (defaults to 1).
#' @param print_val Logical. Also print absolute changes in value from before date to new date? (Default = FALSE)
#' @param print_val_digits The number of digits to print for absolute values (defaults to 1). Used if `print_val` is TRUE and when a location goes from 0/day on the before date to > 0/day on the new date.
#' @param min_val_before Optional. If set, remove locations with values below the stated number on the before date (see Details).
#' @param min_val_new Optional. If set, remove locations with values below the stated number on the new date (see Details).
#' @param file A character string for the path and name of the output file.
#' @details
#' The arguments `min_val_before` and `min_val_new` can be used to censor small absolute changes that correspond to huge percent changes. For example, suppose a health region goes from 0.1 cases/day -> 0.6 cases/day represents a 500% increase but a small absolute change. Setting either `min_val_before` to a value > 0.1 or `min_val_new` to a value > 0.6 would censor this health region.
#' @return A text file summarizing the trends in the selected value over a particular time range and set of locations. Includes emojis.
#' @importFrom dplyr filter distinct pull arrange mutate case_when
#' @export

# report trends in rolling averages by province or health region
trends_rolling_average <- function(x, stat = c("cases", "mortality", "active", "testing"), new_date = get_update_date(), loc = c("all_prov", "all_hr"), before_days = 7, window_days = 7, threshold = 10, change_digits = 1, print_val = FALSE, print_val_digits = 1, min_val_before = NULL, min_val_new = NULL, file) {

  ## must select one mode: prov or hr
  if (identical(loc, c("all_prov", "all_hr"))) {
    stop("Please select one of the following: all_prov, all_hr, a vector of province codes, or a vector of health region codes.")
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
      x <- x %>%
        filter(province_short %in% loc)
    }
  } else if (m == "hr") {
    if (!identical(loc, "all_hr")) {
      x <- x %>%
        filter(HR_UID %in% loc)
    }
  }

  ## calculate rolling average
  x <- rolling_average(x = x, window_days = window_days)

  ## calculate percent change from before_days ago
  if (m == "prov") {
    loc <- unique(x$province_short)
  } else if (m == "hr") {
    loc <- distinct(x[, c("province_short", "health_region")])
  }
  avg_new <- x %>%
    filter(date == new_date) %>%
    pull("rolling_avg")
  avg_before <- x %>%
    filter(date == new_date - before_days) %>%
    pull("rolling_avg")
  x <- {if (m == "prov") {
    data.frame(
      prov = loc,
      before = avg_before,
      new = avg_new,
      change = (avg_new - avg_before) / avg_before * 100
    )
  } else {
    data.frame(
      prov = loc$province_short,
      hr = loc$health_region,
      before = avg_before,
      new = avg_new,
      change = (avg_new - avg_before) / avg_before * 100
    )
  }
  } %>%
    {if (!is.null(min_val_before)) filter(., before >= min_val_before) else .} %>%
    {if (!is.null(min_val_new)) filter(., new >= min_val_new) else .} %>%
    mutate(
      order_results = case_when(
        is.infinite(change) ~ 2,
        is.nan(change) ~ 3,
        TRUE ~ 1
      )
    ) %>%
    arrange(order_results, desc(change), desc(new), desc(before)) %>%
    mutate(
      name = {if (m == "hr") paste0(hr, " (", prov, ")") else prov},
      emoji = case_when(
        abs(change) < threshold ~ as.character(emo::ji("heavy_minus_sign")),
        change > threshold ~ as.character(emo::ji("chart_with_upwards_trend")),
        change < -threshold ~ as.character(emo::ji("chart_with_downwards_trend"))
      ),
      before = paste0(formatC(before, digits = print_val_digits, format = "f", big.mark = ","), "/day"),
      new = paste0(formatC(new, digits = print_val_digits, format = "f", big.mark = ","), "/day"),
      percent = ifelse(!is.infinite(change) & !is.nan(change), paste0(formatC(change, digits = change_digits, format = "f", big.mark = ",", flag = "+"), "%"), change),
    )

  ## convert to text output
  if (stat == "cases") {
    topline <- paste0("Percent change in 7-day rolling average of cases compared to one week ago (threshold: ", threshold, "% change)")
  } else if (stat == "mortality") {
    topline <- paste0("Percent change in 7-day rolling average of deaths compared to one week ago (threshold: ", threshold, "% change)")
  } else if (stat == "active") {
    topline <- paste0("Percent change in 7-day rolling average of active cases compared to one week ago (threshold: ", threshold, "% change)")
  } else if (stat == "testing") {
    topline <- paste0("Percent change in 7-day rolling average of completed tests compared to one week ago (threshold: ", threshold, "% change)")
  }
  x <- data.frame(line = c(topline,
                           case_when(
                             is.infinite(x$change) ~ paste0(x$name, ": 0/day ", as.character(emo::ji("right_arrow")), " ", x$new),
                             is.nan(x$change) ~ paste0(x$name, ": 0/day ", as.character(emo::ji("right_arrow")), " 0/day"),
                             print_val ~ paste0(x$name, ": ", x$percent, " ", x$emoji, " (", x$before, " ", as.character(emo::ji("right_arrow")), " ", x$new, ")"),
                             TRUE ~ paste0(x$name, ": ", x$percent, " ", x$emoji)
                           )))
  write.table(x, file = file, sep = "\n", row.names = FALSE, quote = FALSE, col.names = FALSE)

}
