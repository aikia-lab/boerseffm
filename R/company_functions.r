# Get a companies financial key data
# Retrieve actual and historical values
# get_company_financials(isin = "US0378331005")
# get_company_financials(isin = "US0378331005", historical_financials = TRUE, histo_years = 5)


get_company_financials <- function(isin, historical_financials = FALSE, histo_years = 5) {
    
    params <- list("isin" = isin)

    response <- get_data(encode_url("equity_key_data", params))

    content <- jsonlite::fromJSON(rawToChar(response$content)) 

    for (i in seq_len(length(content))) {
        if(is.null(content[i])) {
            content[i] <- NULL
        }
    }

    equity_key_data <- content |> 
        unlist() |> 
        tibble::enframe() |> 
        tidyr::pivot_wider(names_from = name,
                           values_from = value) |> 
        janitor::clean_names() |> 
        dplyr::mutate(dplyr::across(-c(isin, performance_keys_reference_time), as.numeric),
                      performance_keys_reference_time = lubridate::as_datetime(performance_keys_reference_time),
                      isin = isin) |> 
        dplyr::relocate(isin)

    if (historical_financials == TRUE) {
    
    params_histo <- list("isin" = isin,
                         "limit" = histo_years)

    response_histo <- get_data(encode_url("historical_key_data", params_histo))

    content_histo <- jsonlite::fromJSON(rawToChar(response_histo$content))$data |> 
        dplyr::as_tibble()
    
    return(list(actuals = equity_key_data,
                historical = content_histo))

    }

    return(equity_key_data)
}

