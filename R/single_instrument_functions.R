
# Some basic information for single isins
get_short_static <- function(isin) {
  url_base <- "https://api.boerse-frankfurt.de/v1/data/price_information/shortStatic"

  response <- httr::GET(url_base, query = list(isinsWithOptionalMic = isin))

  content <- httr::content(response, as = "parsed")

  tibble::enframe(unlist(content)) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    )
}

get_short_static("DE000DL40SR8")

get_instrument_statics <- function(isin) {
  params <- list("isin" = isin)

  response <- get_data(encode_url("instrument_information", params))

  tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(response$content))))
}

get_company_information <- function(isin) {
  params <- list("isin" = isin)

  response <- get_data(encode_url("corporate_information", params))

  tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(response$content))))
}

db_info <- get_company_information(isin = "DE0005140008")

get_related_indices <- function(isin) {
  params <- list("isin" = isin)

  response <- get_data(encode_url("related_indices", params))

  tibble::tibble(jsonlite::fromJSON(rawToChar(response$content))$data)
}

db_indices <- get_related_indices(isin = "DE0005140008")


get_instrument_information <- function(isin) {
  instrument_type <- get_instrument_statics(isin) |>
    dplyr::filter(name == "instrumentTypeKey") |>
    dplyr::pull(value)

  if (instrument_type == "bond") { # Who did this???
    suburl <- "master_data_bond"
  } else if (instrument_type == "equity") {
    suburl <- "equity_master_data"
  }
  params <- list("isin" = isin)

  response <- get_data(encode_url(suburl, params))

  res <- jsonlite::fromJSON(rawToChar(response$content))

  output <- tibble::enframe(unlist(res)) |>
    janitor::clean_names()

  return(output)
}


bond_information <- get_instrument_information(isin = "DE000DL40SR8")


get_market_data <- function(isin, mic = "XFRA", content = "all", match_equity = FALSE) {
  bulk_isin <- URLencode(stringr::str_c(isin, collapse = ","))

  params <- list(
    "isin" = bulk_isin,
    "mic" = mic,
    "status.ormode" = 1
  )

  instrument_type <- get_instrument_statics(isin[1]) |>
    dplyr::filter(name == "instrumentTypeKey") |>
    dplyr::pull(value)


  price_res <- get_data(encode_url("quote_box/single", params))
  price <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(price_res$content)))) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    ) |>
    janitor::clean_names()

  # Performance
  performance_res <- get_data(encode_url("performance", params))
  performance <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(performance_res$content)))) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(isin = isin, .before = 1)

  if (instrument_type == "bond") {
    metrics_res <- get_data(encode_url("data_for_datasheet", params, type = "bond_api"))
    metrics <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(metrics_res$content)))) |>
      tidyr::pivot_wider(
        names_from = name,
        values_from = value
      ) |>
      janitor::clean_names() |>
      dplyr::mutate(isin = isin, .before = 1)


    interest_res <- get_data(encode_url("interest_rate_widget", params))
    interest <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(interest_res$content)))) |>
      tidyr::pivot_wider(
        names_from = name,
        values_from = value
      ) |>
      janitor::clean_names()
  } else if (instrument_type == "equity") {
    metrics_res <- get_data(encode_url("equity_key_data", params))
    metrics <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(metrics_res$content)))) |>
      tidyr::pivot_wider(
        names_from = name,
        values_from = value
      ) |>
      janitor::clean_names() |>
      dplyr::mutate(isin = isin, .before = 1)
  }



  if (content == "all") {
    if (instrument_type == "bond") {
      output <- purrr::reduce(list(price, metrics, performance, interest), dplyr::left_join, by = "isin") |>
        dplyr::mutate(
          dplyr::across(tidyselect::contains("time"), lubridate::ymd_hms),
          dplyr::across(c(first_annual_pay_date, start_interest_payment), lubridate::ymd),
          suppressWarnings(dplyr::across(
            c(
              tidyselect::where(is.character),
              -c(isin, nominal, trading_status, instrument_status),
              -tidyselect::contains("interest_payment_cycle")
            ),
            as.numeric
          ))
        )
    } else if (instrument_type == "equity") {
      output <- purrr::reduce(list(price, metrics, performance), dplyr::left_join, by = "isin") |>
        dplyr::mutate(
          dplyr::across(tidyselect::contains("time"), lubridate::ymd_hms),
          suppressWarnings(dplyr::across(
            c(
              tidyselect::where(is.character),
              -c(isin, nominal, trading_status, instrument_status)
            ),
            as.numeric
          ))
        )
    }
  } else if (content == "performance") {
    output <- performance
  } else if (content == "metrics") {
    output <- metrics
  } else if (content == "price") {
    output <- price
  } else if (content == "interest") {
    output <- interest
  }

  # Equity Matching
  if (match_equity == TRUE) {
    issuer <- get_instrument_information(isin) |>
      dplyr::filter(name == "issuer") |>
      dplyr::pull(value)
    
    issuer_slug <- gsub("[^A-Za-z0-9-]", " ", issuer)
    issuer_slug <- gsub("\\s+", " ", issuer_slug)
    issuer_slug <- tolower(gsub("\\s", "-", issuer_slug))
  

    corresponding_equity <- search_instrument(issuer) |>
      dplyr::select(isin_equity = isin, symbol, slug) |>
      dplyr::mutate(isin = isin)

    if (nrow(corresponding_equity) > 1 && any(corresponding_equity$slug == issuer_slug)) {
      
      corresponding_equity <- corresponding_equity[corresponding_equity$slug == issuer_slug,]

      cat(cli::col_br_cyan("More than one equity found, but one has an identic slug:\n"))
      cat(cli::col_br_green(corresponding_equity$isin_equity), "with slug",
          cli::col_br_green(corresponding_equity$slug), "\n")

    } else if (nrow(corresponding_equity) > 1) {
      
    string_distances <- as.vector(adist(issuer_slug, corresponding_equity$slug))
    nearest_match_id <- which(string_distances == min(string_distances))
    corresponding_equity <- corresponding_equity[nearest_match_id, ]   
    
    cat(cli::col_br_cyan("More than one equity found, returning the nearest match:"))
    cat(cli::col_br_green(corresponding_equity$isin_equity), "with match distance",
        cli::col_br_green(min(string_distances)))

    } else if (nrow(corresponding_equity) == 0) {

    cat(cli::cli_alert_warning("No corresponding equities found, returning NA"))

    corresponding_equity <- tibble::tibble(isin_equity = NA_character_,
                                           symbol = NA_character_,
                                           slug = NA_character_,
                                           isin = NA_character_)

    } 
    
    output <- output |>
      dplyr::left_join(corresponding_equity, by = "isin")
  }

  output <- subset(output, select = which(!duplicated(names(output))))
  return(output)
}

market_data <- get_market_data(isin = "AT0000A2N7T2", match_equity = TRUE)

isin = "AT0000A2N7T2"

get_esg <- function(isin) {
  params <- list("isin" = isin)

  response <- get_data(encode_url("instrument/get", params, type = "esg"))

  esg <- jsonlite::fromJSON(rawToChar(response$content))
  esg$researchDe
  esg$reportingDe
  if (is.null(esg$ratings)) {
    cli::cli_alert_warning(
      paste0("There are no ESG scores for ", isin, ". Please select another security.")
    )
    return()
  }

  ratings <- tibble::tibble(esg$ratings) |>
    dplyr::mutate(isin = isin) |>
    janitor::clean_names() |>
    dplyr::select(
      isin,
      ratings_provider_name,
      ratings_score
    ) |>
    tidyr::pivot_wider(
      names_from = ratings_provider_name,
      values_from = ratings_score
    ) |>
    janitor::clean_names()

  ratings$research <- unlist(esg["researchDe"])
  ratings$reporting <- unlist(esg["reportingDe"])

  return(ratings)
}




esg <- get_esg(isin = "DE000ENAG999")

purrr::map_df(c("DE000ENAG999", "DE000DL40SR8"), get_esg)
