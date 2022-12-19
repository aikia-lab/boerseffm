# Helper functions

encode_url <- function(endpoint, params, type = "data") {
  if (type == "data") {
    url_base <- paste0("https://api.boerse-frankfurt.de/v1/data/", endpoint)
  } else if (type == "bond_api") {
    url_base <- paste0("https://api.boerse-frankfurt.de/v1/bonds/api/v1/", endpoint)
  } else if (type == "search") {
    url_base <- paste0("https://api.boerse-frankfurt.de/v1/search/", endpoint)
  } else if (type == "instrument_search") {
    url_base <- paste0("https://api.boerse-frankfurt.de/v1/global_search/pagedsearch/", endpoint)
  } else if (type == "esg") {
    url_base <- paste0("https://api.boerse-frankfurt.de/v1/esg/", endpoint)
  }

  full_url <- paste0(url_base, "?", paste0(names(params), "=", params,
    collapse = "&"
  ))

  return(full_url)
}


# Helper Function for correct request headers
get_ids <- function(url) {
  # Static from the API
  salt <- "w4ivc1ATTGta6njAZzMbkL3kJwxMfEAKDa3MNr"

  timestamp <- lubridate::now()
  # Timestamps for encrypted headers
  options(digits.secs = 5)
  timelocal <- strftime(lubridate::now(), "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  options(digits.secs = 3)
  timestr <- strftime(lubridate::now(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  options(digits.secs = 0)
  traceidbase <- stringr::str_c(timestr, url, salt)
  encoded <- enc2utf8(traceidbase)
  traceid <- digest::digest(encoded, algo = "md5", serialize = FALSE)

  xsecuritybase <- strftime(timelocal, "%Y%m%d%H%M")
  encoded <- enc2utf8(xsecuritybase)
  xsecurity <- digest::digest(encoded, algo = "md5", serialize = FALSE)

  return(list(
    timestr = timestr,
    traceid = traceid,
    xsecurity = xsecurity
  ))
}

# Function for creating headers to pass to cURL
create_headers <- function(url) {
  ids <- get_ids(url)

  headers <- c(
    `authority` = "api.boerse-frankfurt.de",
    `accept` = "application/json, text/plain, */*",
    `accept-language` = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7",
    `client-date` = ids$timestr,
    `content-type` = "application/json; charset=UTF-8",
    `origin` = "https://www.boerse-frankfurt.de",
    `referer` = "https://www.boerse-frankfurt.de/",
    `sec-ch-ua` = '"Not?A_Brand";v="8", "Chromium";v="108", "Google Chrome";v="108"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"Windows"',
    `sec-fetch-dest` = "empty",
    `sec-fetch-mode` = "cors",
    `sec-fetch-site` = "same-site",
    `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36",
    `x-client-traceid` = ids$traceid,
    `x-security` = ids$xsecurity
  )
  return(headers)
}

# Gets the Data per curl GET.
# Does this work with post? It may not
get_data <- function(url, ...) {
  response <- httr::GET(
    url,
    httr::add_headers(
      .headers = create_headers(url),
      ...
    )
  )

  return(response)
}

unnest_json <- function(data) {
  # Unnest all nested Columns
  while (any(sapply(data, class) == "data.frame")) {
    data <- data |>
      tidyr::unnest_wider(tidyselect::where(is.data.frame), names_repair = "minimal")
    # If doubled colnames occur loop will break
    data <- subset(data, select = which(!duplicated(names(data))))
  }

  return(data)
}
