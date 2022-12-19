# This gets historical prices (at least back to year 2000)
get_price_history <- function(isin,
                              limit = 50,
                              mic = "XFRA",
                              min_date = lubridate::today() - lubridate::days(51),
                              max_date = lubridate::today() - 1) {
  date_delta <- max_date - min_date
  params <- list(
    "limit" = limit,
    `offset` = "0",
    "isin" = isin,
    "mic" = mic,
    "minDate" = format(min_date, "%Y-%m-%d"),
    "maxDate" = format(max_date, "%Y-%m-%d"),
    "cleanSplit" = "false",
    "cleanPayout" = "false",
    "cleanSubscriptionRights" = "false"
  )

  response <- get_data(encode_url("price_history", params))

  parsed_response <- tibble::tibble(jsonlite::fromJSON(rawToChar(response$content))$data)
  parsed_response$date <- lubridate::as_date(parsed_response$date)

  return(parsed_response)
}

db <- get_price_history(isin = "DE0005140008")
