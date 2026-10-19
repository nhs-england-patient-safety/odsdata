# ODS API
# Shared logic for fetching, wrangling, and joining organisation data from ODS API

#' Get ODS organisation data
#'
#' @param org_code A list of organisation codes
#'
#' @return Dataframe with organisation data
#'
#' @export
get_ods_orgs <- function(org_code) {

  api_key <- Sys.getenv("ods_key")

  tryCatch({
    response <-
      httr2::request("https://api.service.nhs.uk/organisation-data-terminology-api/fhir") |>
      httr2::req_url_path_append("Organization", org_code) |>
      httr2::req_headers(apikey = api_key) |>
      httr2::req_throttle(rate = 5) |>
      httr2::req_retry(max_tries = 3, backoff = \(x) 2) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    dplyr::tibble(
      org_code = purrr::pluck(response, "id",      .default = NA),
      org_name = purrr::pluck(response, "name",    .default = NA),
      org_type = purrr::pluck(response, "extension", 1, "extension", 2,
                              "valueCodeableConcept", "coding", 1, "display",
                              .default = NA),
      org_country  = purrr::pluck(response, "address", 1, "country", .default = NA)
    )

  }, error = function(e) {
    message(glue::glue("No info available for {org_code}: {e$message}"))
    dplyr::tibble(org_code = org_code, org_name = NA_character_,
           org_type = NA_character_, org_country = NA_character_)
  })
}

#' Fetch and join ODS organisation data to a dataset
#'
#' @param data A dataframe containing organisation codes
#' @param org_code_col Column containing organisation code (e.g., 'ReporterOrganisationCode', 'RP07', 'reporting_organisation_code')
#' @param english_only Logical. Filter to include English (and N/A) organisations
#' @param include_cols Character. ODS columns to include (default includes `org_name`, `org_type`, and `org_country`)
#'
#' @return Original dataframe with organisation data joined on
#'
#' @export
fetch_and_join_ods <- function(data, org_code_col,
                               english_only = TRUE,
                               include_cols = '') {


  # check the column exists in the dataframe
  if (!org_code_col %in% names(data)) {
    stop(glue::glue("Column '{org_code_col}' not found in data."))
  }

  # extract unique, non-NA org codes
  org_codes <- data |>
    dplyr::pull(dplyr::all_of(org_code_col)) |>
    stats::na.omit() |>
    unique()

  n_codes <- length(org_codes)
  message(glue::glue("{n_codes} unique org codes to process."))

  # use furrr if over threshold, otherwise purrr
  if (n_codes >= 250) {
    workers <- max(1, parallel::detectCores() - 1)
    future::plan(future::multisession, workers = workers)

    orgs <- furrr::future_map(org_codes, get_ods_orgs, .progress = TRUE) |>
      purrr::list_rbind()

    future::plan(future::sequential)

  } else {
    orgs <- purrr::map(org_codes, get_ods_orgs, .progress = TRUE) |>
      purrr::list_rbind()
  }

  # join org data back to original dataframe
  result <- data |>
    dplyr::left_join(
      orgs, by = dplyr::join_by(!!rlang::sym(org_code_col) == y$org_code)
    )

  # optionally filter to English and unknown organisations
  if (english_only) {

    rows_before <- nrow(result)
    orgs_before <- dplyr::n_distinct(result[[org_code_col]], na.rm = TRUE)

    result <- result |>
      dplyr::filter(.data$org_country == "ENGLAND" | is.na(.data$org_country))

    rows_removed <- rows_before - nrow(result)
    orgs_removed <- orgs_before - dplyr::n_distinct(result[[org_code_col]], na.rm = TRUE)

    if (rows_removed > 0 | orgs_removed > 0) {
      message(glue::glue(
        "{rows_removed} rows and {orgs_removed} non-English organisations removed."
      ))
    }
  }

  # drop org columns not requested
  cols_to_drop <- setdiff(c("org_name", "org_type", "org_country"), include_cols)

  result <- result |>
    dplyr::select(-dplyr::any_of(cols_to_drop))

  result
}
