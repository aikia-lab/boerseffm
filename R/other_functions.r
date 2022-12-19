get_market_indications <- function() {
    response <- get_data(encode_url("market_indications_bonds", params = NULL))

    jsonlite::fromJSON(rawToChar(response$content)) |> 
        dplyr::as_tibble() |> 
        unnest_json()
}


get_new_issues <- function(n_instruments = 5) {
    
    params <- list(limit = n_instruments)
    
    response <- get_data(encode_url("bond_new_issues", params = params, type = "search"))

    total <- jsonlite::fromJSON(rawToChar(response$content))$recordsTotal

    cat(cli::col_cyan(
        stringr::str_c("There are ", total, " new issues. Showing the first ", n_instruments, " instruments.\n"),
        stringr::str_c("Please run get_new_issues(n_instruments = ", total, ") to see all.")
                   ))

    jsonlite::fromJSON(rawToChar(response$content))$data |> 
        dplyr::as_tibble() |> 
        unnest_json() |> 
        janitor::clean_names()
}

get_new_issues(20)

