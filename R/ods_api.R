# ODS API
# Transport layer for interacting with the ODS API

ods_base_url <- "https://api.service.nhs.uk/organisation-data-terminology-api/fhir"

# run in parallel at or above this many codes
ods_parallel_threshold <- 250

#' Get the API key, stopping if it isn't set
#'
#' @noRd
ods_api_key <- function() {
  api_key <- Sys.getenv("ods_key")

  if (!nzchar(api_key)) {
    stop("Environment variable 'ods_key' is not set.", call. = FALSE)
  }

  api_key
}

#' Look up many codes in one CodeSystem
#'
#' @param system URI of the CodeSystem
#' @param codes Character vector of codes (NA and duplicates are dropped)
#' @param properties Named character vector of properties to return. Names are the
#'   output column names, values are the API property names, e.g. c(org_country = "country")
#' @param multi Character. Names from `properties` whose column should hold every value as a
#'   list column, for properties that can repeat (e.g. successors). Other columns hold the
#'   first value only.
#' @param label What the codes are, for messages about codes that could not be retrieved
#'   (e.g. "org code")
#' @param on_missing One of "warn", "error" or "silent"
#'
#' @return Dataframe with one row per unique code: `code`, `name`, and one column per property.
#'   Codes that could not be retrieved have NA in every column except `code` (and an empty
#'   vector in list columns)
#'
#' @noRd
ods_lookup_codes <- function(
  system,
  codes,
  properties = character(),
  multi = character(),
  label = "code",
  on_missing = "warn"
) {
  stopifnot(all(multi %in% names(properties)))

  # read the key once here so a missing key fails once, not once per code
  api_key <- ods_api_key()

  codes <- unique(codes[!is.na(codes)])

  lookup_one <- \(code) ods_lookup_one(api_key, system, code, properties, multi)

  # use furrr if over threshold, otherwise purrr
  if (length(codes) >= ods_parallel_threshold) {
    # plan() returns the previous plan; restore it however the function exits
    oplan <- future::plan(
      future::multisession,
      workers = max(1, future::availableCores() - 1)
    )
    on.exit(future::plan(oplan), add = TRUE)

    results <- furrr::future_map(codes, lookup_one, .progress = TRUE)
  } else {
    results <- purrr::map(codes, lookup_one, .progress = TRUE)
  }

  results <- purrr::list_rbind(results)

  # list_rbind() on an empty list returns a tibble with no columns, so give it the expected shape
  if (nrow(results) == 0) {
    results <- ods_blank_row("", properties, multi, "ok")[0, ]
  }

  report_missing_lookups(results, label, on_missing)

  # status and detail are internal to this file
  dplyr::select(results, -dplyr::all_of(c("status", "detail")))
}

#' Look up one code
#'
#' @noRd
ods_lookup_one <- function(api_key, system, code, properties, multi) {
  tryCatch(
    {
      req <- httr2::request(ods_base_url) |>
        httr2::req_url_path_append("CodeSystem", "$lookup") |>
        httr2::req_url_query(system = system, code = code)

      # .multi = "explode" repeats the parameter: property=country&property=primaryRole&...
      if (length(properties) > 0) {
        req <- httr2::req_url_query(
          req,
          property = unname(properties),
          .multi = "explode"
        )
      }

      response <- req |>
        httr2::req_headers(apikey = api_key) |>
        httr2::req_throttle(rate = 5) |>
        httr2::req_retry(max_tries = 3, backoff = \(x) 2) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      values <- purrr::imap(properties, \(property, column) {
        found <- extract_ods_property(response, property)

        # a property that can repeat keeps every value as a list column; others keep the first
        if (column %in% multi) list(found) else found[1]
      })

      dplyr::tibble(
        code = code,
        # the organisation name is the top-level "display" parameter
        name = extract_ods_display(response),
        !!!values,
        status = "ok",
        detail = NA_character_
      )
    },
    # handlers are tried in order, so a 404 is caught before the general error
    httr2_http_404 = function(e) {
      ods_blank_row(code, properties, multi, "not_found")
    },
    error = function(e) {
      ods_blank_row(code, properties, multi, "failed", conditionMessage(e))
    }
  )
}

#' One row with every field missing, for codes that could not be retrieved
#'
#' @noRd
ods_blank_row <- function(
  code,
  properties,
  multi,
  status,
  detail = NA_character_
) {
  blanks <- purrr::imap(properties, \(property, column) {
    if (column %in% multi) list(character()) else NA_character_
  })

  dplyr::tibble(
    code = code,
    name = NA_character_,
    !!!blanks,
    status = status,
    detail = detail
  )
}

#' Handle codes that could not be retrieved, according to on_missing
#'
#' @param results Dataframe from ods_lookup_one() calls, including status and detail columns
#' @param label What the codes are, for messages (e.g. "org code")
#' @param on_missing One of "warn", "error" or "silent"
#'
#' @noRd
report_missing_lookups <- function(results, label, on_missing) {
  # every request failed: points to a key or connection problem, so stop whatever on_missing is
  if (nrow(results) > 0 && all(results$status == "failed")) {
    stop(
      glue::glue(
        "All ODS requests failed ({results$detail[[1]]}). ",
        "Check 'ods_key' and your connection."
      ),
      call. = FALSE
    )
  }

  if (on_missing == "silent") {
    return(invisible(NULL))
  }

  labels <- c(not_found = "not found in ODS", failed = "failed (request error)")

  # build one message per failure type, listing up to 10 codes
  msgs <- character()
  for (status in names(labels)) {
    codes <- results$code[results$status == status]
    if (length(codes) == 0) {
      next
    }

    shown <- utils::head(codes, 10)
    extra <- length(codes) - length(shown)
    suffix <- if (extra > 0) paste0(" and ", extra, " more") else ""

    msgs[[status]] <- paste0(
      length(codes),
      " ",
      label,
      "(s) ",
      labels[[status]],
      ": ",
      paste(shown, collapse = ", "),
      suffix
    )
  }

  if (length(msgs) == 0) {
    return(invisible(NULL))
  }

  if (on_missing == "error") {
    rlang::abort(
      c(
        "Some codes could not be retrieved.",
        stats::setNames(unname(msgs), rep("x", length(msgs))),
        i = "Use `on_missing = \"warn\"` to continue with missing values."
      ),
      class = "odsdata_missing"
    )
  }

  # on_missing == "warn": one classed warning per failure type
  for (status in names(msgs)) {
    rlang::warn(msgs[[status]], class = paste0("odsdata_", status))
  }

  invisible(NULL)
}

# Pull the top-level display text out of a $lookup response
#
# @noRd
extract_ods_display <- function(response) {
  purrr::detect(response$parameter, \(x) identical(x$name, "display")) |>
    purrr::pluck("valueString", .default = NA_character_)
}

#' Pull a property's value out of a $lookup response
#' Coded properties (NHSER, primaryRole) come back as valueCoding, others (country) as valueString
#'
#' @noRd
extract_ods_property <- function(response, property) {
  for (p in response$parameter) {
    if (!identical(p$name, "property")) {
      next
    }
    prop_name <- purrr::detect(p$part, \(x) identical(x$name, "code"))$valueCode
    if (identical(prop_name, property)) {
      val <- purrr::detect(p$part, \(x) identical(x$name, "value"))
      return(purrr::pluck(
        val,
        "valueCoding",
        "code",
        .default = purrr::pluck(val, "valueString", .default = NA_character_)
      ))
    }
  }
  NA_character_
}

#' Pull every value of a property out of a $lookup response
#' A property can appear more than once (e.g. predecessors, successors), and its value
#' sits under a type-specific name (valueCode, valueString, valueDateTime, valueCoding, ...)
#'
#' @return Character vector, one element per occurrence; length 0 if the property is absent
#'
#' @noRd
extract_ods_property <- function(response, property) {
  matches <- purrr::keep(response$parameter, \(p) {
    identical(p$name, "property") &&
      identical(
        purrr::detect(p$part, \(x) identical(x$name, "code"))$valueCode,
        property
      )
  })

  purrr::map_chr(matches, \(p) {
    val <- purrr::detect(p$part, \(x) identical(x$name, "value"))
    v <- purrr::pluck(val[setdiff(names(val), "name")], 1, .default = NA_character_)

    # coded values (valueCoding) hold the code
    as.character(
      if (is.list(v)) purrr::pluck(v, "code", .default = NA_character_) else v
    )
  })
}

