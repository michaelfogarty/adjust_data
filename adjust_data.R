adjust_data <- function(df) {
  #' adjust_data
  #'
  #' @param df A data frame/tibble with daily spending data. The data frame 
  #' should have a column called date that is \code{Date} type and a column 
  #' called spend that is numeric and has the daily spend data.
  #'
  #' @return a data frame with the weekly aggregated NSA and SA spending series
  
  # Get SA and NSA retail sales data from Haver, calculate seasonal factors
  census <-
    import_haver(series = c("NRSTXM@USECON", "NRSTXMN@USECON"),
                 start = ymd("2012-01-01")) %>%
    mutate(sf_t = nrstxm / nrstxmn,
           month = month(date),
           year = year(date)) %>%
    select(year, month, sf_t)
  
  # Regress retail sales ex auto factor on retail sales factor
  census_fit <- 
    import_haver(series = "NRSTF@USECON", start = ymd("2018-01-01")) %>%
    mutate(month = month(date), year = year(date), sf_census = 1/nrstf) %>%
    left_join(census, by = c("year", "month")) %>%
    arrange(year, month) %>%
    gen_lags(sf_t) %>% 
    select(-date, -nrstf) %>%
    lm(data = ., sf_t ~ . -month -year)
  
  # data for forecasting retail ex. auto seasonal factors
  census_seasonal <-
    import_haver(series = "NRSTF@USECON", start = ymd("2018-01-01")) %>%
    mutate(month = month(date), year = year(date), sf_census = 1/nrstf) %>%
    left_join(census, by = c("year", "month")) %>%
    arrange(year, month) %>%
    gen_lags(sf_t) %>%
    filter(date >=  ymd("2018-01-01"))
  
  # forecast seasonal factor
  sf_hat <-
    import_haver(series = "NRSTF@USECON", start = ymd("2018-01-01")) %>%
    mutate(sf_hat = predict(census_fit, newdata = census_seasonal)) %>%
    mutate(month = month(date), year = year(date)) %>%
    select(year, month, sf_hat)
  
  # aggregate data to weekly
  data_weekly <- 
    df %>%
    mutate(
      month = month(date),
      year = year(date),
      mday = mday(date),
      week = ((mday - 1) %/% 7) + 1,
      week = replace(week, week == 5, 4)
    ) %>%
    group_by(year, month, week) %>%
    summarize(
      spend_it = sum(spend),
      d_it = n()
    ) %>%
    mutate(
      spend_t = sum(spend_it),
      d_t = days_in_month(ymd(glue::glue("{year}-{month}-{week*7}")))
    ) %>%
    ungroup() %>%
    filter(d_it >= 7) %>%
    ungroup() %>% 
    mutate(n_month = group_indices(., year, month))
  
  this_month <- max(data_weekly$n_month)
  
  weekly_factors <-
    data_weekly %>%
    left_join(census, by = c("year", "month")) %>%
    left_join(sf_hat, by = c("year", "month")) %>%
    mutate(sf_t = if_else(is.na(sf_t), sf_hat, sf_t)) %>%
    ungroup() %>%
    mutate(week_share = (spend_it / d_it) / (spend_t / d_t)) %>%
    group_by(month, week) %>%
    mutate(
      avg_i = mean(week_share[n_month != this_month]) *(d_it / d_t)
    ) %>%
    ungroup() %>% 
    mutate(
      sf_it = (0.25 / avg_i) * sf_t,
      spend_sa = spend_it * sf_it,
      date = ymd(glue::glue("{year}-{month}-{week*7}"))
    )
  
  weekly_factors %>%
    select(date, year, month, week, spend_sa, spend_nsa = spend_it)
  
}