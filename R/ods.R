# ODS Data
# Shared logic for fetching, wrangling, and joining ODS organisation data from ODT API

#' Get ODS organisation data
#'
#' @param org_code A single organisation code
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
      # key on the input code, not response$id, so the join can't miss if the API normalises the code
      org_code = org_code,
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

# Perform a CodeSystem $lookup
#
# @param system URI of the CodeSystem
# @param code Code to look up within the CodeSystem
# @param property Character vector of properties to return (NULL returns none)
#
# @return Parsed JSON response
#
# @noRd
ods_lookup <- function(system, code, property = NULL) {

  api_key <- Sys.getenv("ods_key")

  req <-
    httr2::request("https://api.service.nhs.uk/organisation-data-terminology-api/fhir") |>
    httr2::req_url_path_append("CodeSystem", "$lookup") |>
    httr2::req_url_query(system = system, code = code)

  # .multi = "explode" repeats the parameter: property=country&property=primaryRole&...
  if (!is.null(property)) {
    req <- httr2::req_url_query(req, property = property, .multi = "explode")
  }

  req |>
    httr2::req_headers(apikey = api_key) |>
    httr2::req_throttle(rate = 5) |>
    httr2::req_retry(max_tries = 3, backoff = \(x) 2) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# Pull the top-level display text out of a $lookup response
#
# @noRd
extract_ods_display <- function(response) {
  purrr::detect(response$parameter, \(x) identical(x$name, "display")) |>
    purrr::pluck("valueString", .default = NA_character_)
}

# Pull a property's value out of a $lookup response
# Coded properties (NHSER, primaryRole) come back as valueCoding, others (country) as valueString
#
# @noRd
extract_ods_property <- function(response, property) {
  for (p in response$parameter) {
    if (!identical(p$name, "property")) next
    prop_name <- purrr::detect(p$part, \(x) identical(x$name, "code"))$valueCode
    if (identical(prop_name, property)) {
      val <- purrr::detect(p$part, \(x) identical(x$name, "value"))
      return(purrr::pluck(val, "valueCoding", "code",
                          .default = purrr::pluck(val, "valueString",
                                                  .default = NA_character_)))
    }
  }
  NA_character_
}

#' Get ODS organisation details in a single call
#'
#' @param org_code A single organisation code
#'
#' @return Dataframe with organisation name, primary role code, country, and NHS England region code
#'
#' @export
get_ods_org_details <- function(org_code) {

  tryCatch({
    response <- ods_lookup(
      system = "https://fhir.nhs.uk/Id/ods-organization-code",
      code = org_code,
      property = c("country", "primaryRole", "NHSER")
    )

    dplyr::tibble(
      org_code = org_code,
      # the organisation name is the top-level "display" parameter
      org_name = extract_ods_display(response),
      org_type_code = extract_ods_property(response, "primaryRole"),
      org_country = extract_ods_property(response, "country"),
      org_region_code = extract_ods_property(response, "NHSER")
    )

  }, error = function(e) {
    message(glue::glue("No info available for {org_code}: {e$message}"))
    dplyr::tibble(org_code = org_code, org_name = NA_character_,
                  org_type_code = NA_character_, org_country = NA_character_,
                  org_region_code = NA_character_)
  })
}

#' Get the name of an ODS organisation role
#'
#' @param role_code A single role code (e.g., 'RO197')
#'
#' @return Dataframe with role code and role name
#'
#' @export
get_ods_role_name <- function(role_code) {

  tryCatch({
    response <- ods_lookup(
      system = "https://digital.nhs.uk/services/organisation-data-service/CodeSystem/ODSOrganisationRole",
      code = role_code
    )

    dplyr::tibble(org_type_code = role_code, org_type = extract_ods_display(response))

  }, error = function(e) {
    message(glue::glue("No role name available for {role_code}: {e$message}"))
    dplyr::tibble(org_type_code = role_code, org_type = NA_character_)
  })
}

# Map a per-code fetch function over codes
#
# @param org_codes Character vector of unique, non-NA codes
# @param fn Function taking one code and returning a dataframe
#
# @return Dataframe of the row-bound results
#
# @noRd
map_org_codes <- function(org_codes, fn) {

  # use furrr if over threshold, otherwise purrr
  if (length(org_codes) >= 250) {
    # plan() returns the previous plan; restore it however the function exits
    oplan <- future::plan(future::multisession,
                          workers = max(1, future::availableCores() - 1))
    on.exit(future::plan(oplan), add = TRUE)

    furrr::future_map(org_codes, fn, .progress = TRUE) |>
      purrr::list_rbind()

  } else {
    purrr::map(org_codes, fn, .progress = TRUE) |>
      purrr::list_rbind()
  }
}

#' Fetch and join ODS organisation data to a dataset
#'
#' @param data A dataframe containing organisation codes
#' @param org_code_col Column containing organisation code (e.g., 'ReporterOrganisationCode', 'RP07', 'reporting_organisation_code')
#' @param english_only Logical. Filter to include English (and N/A) organisations
#' @param include_cols Character. ODS columns to include (`org_name`, `org_type`, `org_country`, `org_region_code`, `org_region`)
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

  # fail once up front if the API key is missing, rather than once per code
  if (!nzchar(Sys.getenv("ods_key"))) {
    stop("Environment variable 'ods_key' is not set.")
  }

  # extract unique, non-NA org codes
  org_codes <- data[[org_code_col]]
  org_codes <- unique(org_codes[!is.na(org_codes)])

  n_codes <- length(org_codes)
  message(glue::glue("{n_codes} unique org codes to process."))

  orgs <- map_org_codes(org_codes, get_ods_org_details)

  # list_rbind() on an empty list returns a tibble with no columns, so give it the expected shape
  if (nrow(orgs) == 0) {
    orgs <- dplyr::tibble(org_code = character(), org_name = character(),
                          org_type_code = character(), org_country = character(),
                          org_region_code = character())
  }

  # join org data back to original dataframe
  # orgs has one row per unique code, so match() can't duplicate rows
  # row i of `info` is the ODS data for data[[org_code_col]][i], and NA codes give NA rows
  info <- orgs[match(data[[org_code_col]], orgs$org_code),
               c("org_name", "org_type_code", "org_country", "org_region_code")]
  result <- dplyr::bind_cols(data, info)

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

  # optionally resolve role codes to names
  if ("org_type" %in% include_cols) {

    role_codes <- unique(result$org_type_code[!is.na(result$org_type_code)])
    role_names <- map_org_codes(role_codes, get_ods_role_name)

    # give an empty result the expected shape
    if (nrow(role_names) == 0) {
      role_names <- dplyr::tibble(org_type_code = character(), org_type = character())
    }

    result$org_type <-
      role_names$org_type[match(result$org_type_code, role_names$org_type_code)]
  }

  # optionally resolve region codes to names
  if ("org_region" %in% include_cols) {

    region_codes <- unique(result$org_region_code[!is.na(result$org_region_code)])
    region_names <- map_org_codes(region_codes, get_ods_orgs)

    if (nrow(region_names) == 0) {
      region_names <- dplyr::tibble(org_code = character(), org_name = character())
    }

    result$org_region <-
      region_names$org_name[match(result$org_region_code, region_names$org_code)]
  }

  # drop org columns not requested (org_type_code is only used internally)
  cols_to_drop <- c(
    "org_type_code",
    setdiff(c("org_name", "org_type", "org_country", "org_region_code", "org_region"),
            include_cols)
  )

  result <- result |>
    dplyr::select(-dplyr::any_of(cols_to_drop))

  result
}
