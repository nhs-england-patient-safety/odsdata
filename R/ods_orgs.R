# ODS Organisations
# Navigate ODS CodeSystems and properties for pulling organisation data from the ODS API

ods_org_system <- "https://fhir.nhs.uk/Id/ods-organization-code"
ods_role_system <- "https://digital.nhs.uk/services/organisation-data-service/CodeSystem/ODSOrganisationRole"

# fields that can be requested
ods_org_fields <- c(
  "org_name",
  "org_type",
  "org_country",
  "org_region_code",
  "org_region",
  "org_legal_start",
  "org_legal_end",
  "org_operational_start",
  "org_operational_end",
  "org_successors"
)

# fields that come back from the API as text but are dates
ods_date_fields <- c(
  "org_legal_start",
  "org_legal_end",
  "org_operational_start",
  "org_operational_end"
)

#' Look up organisation details for a vector of ODS codes
#'
#' @param org_codes Character vector of ODS codes (NA and duplicates are dropped)
#' @param fields Character. Which of `ods_org_fields` to return
#' @param on_missing One of "warn", "error" or "silent"
#'
#' @return Dataframe with `org_code` and one column per requested field,
#'   one row per unique code
#'
#' @noRd
ods_lookup_orgs <- function(
  org_codes,
  fields = ods_org_fields,
  on_missing = "warn"
) {
  n_codes <- dplyr::n_distinct(org_codes, na.rm = TRUE)
  message(glue::glue("{n_codes} unique org codes to process."))

  # everything except the role and region names comes from one call per code
  orgs <- ods_lookup_codes(
    ods_org_system,
    org_codes,
    properties = c(
      org_type_code = "primaryRole",
      org_country = "country",
      org_region_code = "NHSER",
      org_legal_start = "legStartDate",
      org_legal_end = "legEndDate",
      org_operational_start = "opStartDate",
      org_operational_end = "opEndDate",
      org_successors = "successor"
    ),
    multi = "org_successors",
    label = "org code",
    on_missing = on_missing
  )

  orgs <- dplyr::rename(orgs, org_code = "code", org_name = "name")

  # dates come back as text (YYYY-MM-DD)
  orgs[ods_date_fields] <- purrr::map(orgs[ods_date_fields], as.Date)

  # successor names: one lookup per distinct successor code, only when asked for
  if ("org_successors" %in% fields) {
    successor_names <- ods_lookup_codes(
      ods_org_system,
      unlist(orgs$org_successors),
      label = "successor code",
      on_missing = on_missing
    )

    # each row's successors as a small table of code and name
    orgs$org_successors <- purrr::map(orgs$org_successors, \(codes) {
      dplyr::tibble(
        code = codes,
        name = successor_names$name[match(codes, successor_names$code)]
      )
    })
  }

  # role and region names are separate lookups, one call per distinct code, only when asked for
  if ("org_type" %in% fields) {
    orgs$org_type <- ods_names_for(
      orgs$org_type_code,
      ods_role_system,
      "role code",
      on_missing
    )
  }

  # regions are organisations in ODS, so their names come from the organisation CodeSystem
  if ("org_region" %in% fields) {
    orgs$org_region <- ods_names_for(
      orgs$org_region_code,
      ods_org_system,
      "region code",
      on_missing
    )
  }

  dplyr::select(orgs, "org_code", dplyr::all_of(fields))
}

#' Turn a vector of codes into their names, one lookup per distinct code
#'
#' @return Character vector the same length as `codes`; NA codes give NA
#'
#' @noRd
ods_names_for <- function(codes, system, label, on_missing) {
  looked_up <- ods_lookup_codes(
    system,
    codes,
    label = label,
    on_missing = on_missing
  )

  looked_up$name[match(codes, looked_up$code)]
}
