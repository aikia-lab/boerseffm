
# This function delivers bulk data for *ALL* available instruments in one asset class.
# Set limit with n_instruments and select one of bond, equity, etp or fund
# get_bulk_instruments("equity")
# get_bulk_instruments("bond")
# get_bulk_instruments("etp")
# get_bulk_instruments("fund")
# Get Index members with
# get_bulk_instruments(asset_class = "equity", index_isin = "DE0008469008", n_instruments = 40)
# It is also possible to retrieve members for more than one index
# But it seems that you get no index information with the securities so I guess it doesnt really make sense
# get_bulk_instruments(asset_class = "equity", index_isin = c("DE0008469008", "DE0007203275"))
get_bulk_instruments <- function(asset_class = "bond",
                                 n_instruments = 25,
                                 green_bond = FALSE,
                                 index_isin = NULL) {
  base_url <- "https://api.boerse-frankfurt.de/v1/search/"

  valid_ac <- c(
    "bond",
    "equity",
    "etp",
    "fund"
  )

  if (!asset_class %in% valid_ac) {
    stop(paste0(
      "The provided asset class is not valid!\n",
      "Valid asset classes are:\n",
      paste0(valid_ac, collapse = "\n")
    ))
  }

  url <- paste0(base_url, asset_class, "_search")

  if (green_bond & asset_class != "bond") {
    cli::cli_alert_info(
      paste0("green_bond = TRUE with asset class other than bond is superfluous.")
    )
  }

  green_bond_indicator <- tolower(green_bond)
  # Body for POST request, maybe check other possible options like sort by performance
  data <- paste0('{
                "indices":["', paste0(index_isin, collapse = '","'), '"],
                "greenBond":', green_bond_indicator, ',
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
    dplyr::as_tibble() |>
    unnest_json()

  # Remove dots from names and remove duplicate columns
  names(content) <- stringr::str_remove(names(content), "\\.\\d")
  content_subsetted <- subset(content, select = which(!duplicated(names(content)))) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(tidyselect::contains("date"), lubridate::ymd_hms)) |>
    dplyr::relocate(isin,
      full_name = original_value,
      wkn
    )

  return(content_subsetted)
}

get_bulk_instruments(asset_class = "equity", index_isin = c("DE0008469008", "DE0007203275"))

all_index_constituents <- get_bulk_instruments(asset_class = "equity", index_isin = all_index_types$isin, n_instruments = 1000000)

all_index_constituents |>
  dplyr::group_by(isin)

# Testing
# index_with_constituents <- function(index_isin) {
#    tryCatch({
#        get_bulk_instruments(asset_class = "equity", index = index_isin)
#        tibble::tibble(isin = index_isin,
#                       constituents = TRUE)
#    },
#    error = function(e) {
#        paste("No constituents available for", index_isin)
#        tibble::tibble(isin = index_isin,
#                       constituents = FALSE)
#    }
#    )
# }
#
# all_indices <- purrr::map_df(seq_len(6), get_indices, n_indices = 1000)
#
# has_constituents <- purrr::map_df(all_indices$isin, index_with_constituents)
#
# has_constituents |>
#    dplyr::filter(constituents == TRUE) |>
#    View()
#
