# ODS Fetch
# Add ODS organisation fields to a dataframe, fetching from the ODS API

#' Fetch and join ODS organisation data to a dataset
#'
#' @param data A dataframe containing organisation codes
#' @param org_code_col Column containing organisation code (e.g., 'ReporterOrganisationCode', 'RP07', 'reporting_organisation_code')
#' @param english_only Logical. Filter to include English (and N/A) organisations
#' @param include_cols Character. ODS columns to add (`org_name`, `org_type`, `org_country`, `org_region_code`, `org_region`).
#'   Default adds none.
#' @param on_missing Character. What to do when codes are not found in ODS or the request fails:
#'   `"warn"` (default) warns and carries on, `"error"` stops, `"silent"` does nothing.
#'   Missing codes get NA values unless `"error"` is used. If every request fails, the function stops regardless.
#'
#' @return Original dataframe with organisation data joined on
#'
#' @export
fetch_and_join_ods <- function(
  data,
  org_code_col,
  english_only = TRUE,
  include_cols = character(),
  on_missing = c("warn", "error", "silent")
) {
  # validate before any API calls are made
  on_missing <- match.arg(on_missing)
  include_cols <- setdiff(include_cols, "")

  # check the column exists in the dataframe
  if (!org_code_col %in% names(data)) {
    stop(glue::glue("Column '{org_code_col}' not found in data."))
  }

  unknown <- setdiff(include_cols, ods_org_fields)
  if (length(unknown) > 0) {
    stop(glue::glue(
      "Unknown include_cols: {toString(unknown)}. ",
      "Choose from: {toString(ods_org_fields)}."
    ))
  }

  # nothing to add and nothing to filter on, so no need to call the API
  if (length(include_cols) == 0 && !english_only) {
    return(data)
  }

  # the English filter needs org_country even if it wasn't asked for
  fields <- union(include_cols, if (english_only) "org_country")
  orgs <- ods_lookup_orgs(data[[org_code_col]], fields, on_missing)

  # row i of data matches row idx[i] of orgs; NA codes and unmatched codes give NA
  # orgs has one row per unique code, so this can't duplicate rows
  idx <- match(data[[org_code_col]], orgs$org_code)

  # optionally filter to English and unknown organisations
  if (english_only) {
    country <- orgs$org_country[idx]
    keep <- is.na(country) | country == "ENGLAND"

    rows_removed <- sum(!keep)
    orgs_removed <- dplyr::n_distinct(data[[org_code_col]], na.rm = TRUE) -
      dplyr::n_distinct(data[[org_code_col]][keep], na.rm = TRUE)

    if (rows_removed > 0 | orgs_removed > 0) {
      message(glue::glue(
        "{rows_removed} rows and {orgs_removed} non-English organisations removed."
      ))
    }

    data <- data[keep, , drop = FALSE]
    idx <- idx[keep]
  }

  # add the requested columns, warning if any already exist
  clashes <- intersect(include_cols, names(data))
  if (length(clashes) > 0) {
    warning(
      glue::glue(
        "Column(s) {toString(clashes)} already exist in data and will be overwritten."
      ),
      call. = FALSE
    )
  }

  for (col in include_cols) {
    data[[col]] <- orgs[[col]][idx]
  }

  data
}
