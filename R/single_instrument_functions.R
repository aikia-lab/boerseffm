
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

get_instrument_information(isin = "DE0005140008")


get_market_data <- function(isin, mic = "XFRA", content = "all") {
    params <- list(
        "isin" = isin,
        "mic" = mic
    )

    instrument_type <- get_instrument_statics(isin) |>
        dplyr::filter(name == "instrumentTypeKey") |>
        dplyr::pull(value)

    if (instrument_type == "bond") {
        # Performance
        performance_res <- get_data(encode_url("performance", params))
        performance <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(performance_res$content)))) |>
            tidyr::pivot_wider(
                names_from = name,
                values_from = value
            ) |>
            janitor::clean_names() |>
            dplyr::mutate(isin = isin, .before = 1)

        metrics_res <- get_data(encode_url("data_for_datasheet", params, type = "bond_api"))
        metrics <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(metrics_res$content)))) |>
            tidyr::pivot_wider(
                names_from = name,
                values_from = value
            ) |>
            janitor::clean_names() |>
            dplyr::mutate(isin = isin, .before = 1)

        price_res <- get_data(encode_url("quote_box/single", params))
        price <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(price_res$content)))) |>
            tidyr::pivot_wider(
                names_from = name,
                values_from = value
            ) |>
            janitor::clean_names()

        interest_res <- get_data(encode_url("interest_rate_widget", params))
        interest <- tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(interest_res$content)))) |>
            tidyr::pivot_wider(
                names_from = name,
                values_from = value
            ) |>
            janitor::clean_names()

        if (content == "all") {
            output <- purrr::reduce(list(price, metrics, performance, interest), dplyr::left_join, by = "isin")
        } else if (content == "performance") {
            output <- performance
        } else if (content == "metrics") {
            output <- metrics
        } else if (content == "price") {
            output <- price
        } else if (content == "interest") {
            output <- interest
        }
    }

    output <- subset(output, select = which(!duplicated(names(output))))
    return(output)
}

market_data <- get_market_data(isin = "DE000DL40SR8")

# TODO
# Equity Summary like bond
# Single Security breakdown