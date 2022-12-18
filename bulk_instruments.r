
# This function delivers bulk data for *ALL* available instruments in one asset class. 
# Set limit with n_instruments and select one of bond, equity, etp or fund
# get_bulk_instruments("equity")
# get_bulk_instruments("bond")
# get_bulk_instruments("etp")
# get_bulk_instruments("fund")
get_bulk_instruments <- function(asset_class = "bond", 
                                 n_instruments = 25) {
    base_url <- "https://api.boerse-frankfurt.de/v1/search/"

    valid_ac <- c("bond",
                  "equity",
                  "etp",
                  "fund")

    if (!asset_class %in% valid_ac) {
        stop(paste0("The provided asset class is not valid!\n",
                    "Valid asset classes are:\n",
                    paste0(valid_ac, collapse = "\n")))
    }
    
    url <- paste0(base_url, asset_class, "_search")
    
    # Body for POST request, maybe check other possible options like sort by performance
    data <- stringr::str_c('{
                "lang":"de",
                "offset":0,
                "limit":', n_instruments, ',
                "sorting":"TURNOVER",
                "sortOrder":"DESC"}')

    # Request POST with headers 
    res <- httr::POST(url,
        httr::add_headers(.headers = create_headers(url)),
        body = data
    )
    
    # Parse content
    content <- jsonlite::fromJSON(rawToChar(res$content))$data |> 
        dplyr::as_tibble() 

    # Unnest all nested Columns
    while (any(sapply(content, class) == "data.frame")) {
        content <- content |> 
            tidyr::unnest_wider(tidyselect::where(is.data.frame), names_repair = "minimal") 
        # If doubled colnames occur loop will break
        content <- subset(content, select=which(!duplicated(names(content))))
    }
    
    # Remove dots from names and remove duplicate columns
    names(content) <- stringr::str_remove(names(content), "\\.\\d")
    content_subsetted <- subset(content, select=which(!duplicated(names(content)))) |> 
        janitor::clean_names()  |> 
        dplyr::mutate(dplyr::across(tidyselect::contains("date"), lubridate::ymd_hms)) |> 
        dplyr::relocate(isin,
                         full_name = original_value,
                         wkn)

    return(content_subsetted)
}

### Doesnt work for now or just needs hours to run. But data is available on website
get_index_realtime <- function(index_isin = "DE0008469008") {

    params  <- list(isin = index_isin)

    response <- get_data(encode_url("equities_frankfurt_realtime_quotes", params))

}
