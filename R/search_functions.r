
search_instrument <- function(search_term, instrument_type = "equity", n_results = 50) {
    
    valid_instrument_type <- c("equity", "bond", "derivative", "future")

    params <- list("searchTerms" = URLencode(search_term),
                  page = 1,
                  pageSize = n_results)

    search_helper <- function(instrument_type, params) {
        
        response <- suppressWarnings(get_data(encode_url(paste0(instrument_type, "/de"), params, type = "instrument_search")))
        result <- jsonlite::fromJSON(rawToChar(response$content))$result |> 
        dplyr::as_tibble() |> 
        unnest_json()

        return(result)

    }

    if (instrument_type != "all") {
        
        result <- search_helper(params = params, instrument_type = instrument_type)

    } else if (instrument_type == "all") {
        
        result <- purrr::map_df(valid_instrument_type, search_helper, params = params)

    }
    
    return(result)
    
}

#search_instrument(search_term = "Deutsche Bank", instrument_type = "all", n_results = 50)



