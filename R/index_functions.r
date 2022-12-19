# Index Types:
# 1 Main German Indices
# 2 Stoxx Subsectors
# 3 Main International Indices
# 4 Volatility Indices
# 5 ESG Indices
# 6 DAX Supersector

get_indices <- function(type = 5, n_indices = 25) {
  params <- list(
    "type" = type,
    "limit" = n_indices,
    "offset" = 0,
    "lang" = "en"
  )

  response <- get_data(encode_url("index_by_type", params))

  if (!type %in% seq_len(6)) {
    warning("The index type number of ", type, " is not valid.")
    cli::col_cyan(paste("Valid Index Types are", paste0(seq_len(6), collapse = ", ")))
  }

  indices <- jsonlite::fromJSON(rawToChar(response$content))$result |>
    dplyr::as_tibble() |>
    unnest_json() |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(tidyselect::contains("date"), lubridate::as_datetime))

  return(indices)
}
