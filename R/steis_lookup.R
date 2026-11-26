# StEIS Lookup
# Plug the StEIS mapping table into map_to_ods_codes()

#' Map StEIS organisation codes to ODS codes and fetch ODS organisation data
#'
#' A StEIS code can cover several organisations. Supply `org_name_col` so the organisation
#' name picks the right ODS code. Without it, those codes can't be resolved and are
#' reported according to `on_missing`.
#'
#' @param data A dataframe containing StEIS organisation codes
#' @param org_code_col Column containing organisation code (e.g., 'Reporting Organisation Code')
#' @param org_name_col Column containing the organisation name (e.g., 'Reporting Organisation'), or NULL
#' @inheritParams fetch_and_join_ods
#' @param on_missing Character. What to do when codes are ambiguous, not found in ODS, or the
#'   request fails: `"warn"` (default) warns and carries on, `"error"` stops, `"silent"` does nothing.
#'
#' @return Original dataframe with an `ods_org_code` column and organisation data joined on
#'
#' @export
map_steis_org_codes <- function(
  data,
  org_code_col,
  org_name_col = NULL,
  english_only = TRUE,
  include_cols = c(
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
  ),
  on_missing = c("warn", "error", "silent")
) {
  on_missing <- match.arg(on_missing)

  # names are columns in the user's data, values are columns in steis_mapping_table
  by <- stats::setNames("steis_org_code", org_code_col)
  if (!is.null(org_name_col)) {
    by <- c(by, stats::setNames("steis_org_name", org_name_col))
  }

  map_to_ods_codes(
    data,
    mapping = steis_mapping_table,
    by = by,
    to = "mapped_org_code",
    english_only = english_only,
    include_cols = include_cols,
    on_missing = on_missing
  )
}
