# ODS code mapping
# Map codes from another source to ODS codes using a lookup table, then fetch ODS data

#' Map organisation codes to ODS codes and fetch ODS organisation data
#'
#' Each row is matched to an ODS code using the mapping table, in this order:
#' 1. the full key in `by` (e.g. code and name together)
#' 2. the source code alone, if the table gives only one ODS code
#'
#' A code that is in the table but still has more than one possible ODS code is given `NA`
#' (and reported according to `on_missing`). Codes not in the table are assumed to be ODS
#' codes already.
#'
#' @param data A dataframe containing the codes to map
#' @param mapping A dataframe linking source codes to ODS codes
#' @param by Character. Links columns in `data` to columns in `mapping`: names are columns in
#'   `data`, values are columns in `mapping`, and an unnamed value means the column has the
#'   same name in both. The first entry is the source code. Extra entries (e.g. an organisation
#'   name) separate codes that cover more than one organisation. Codes and names are compared
#'   ignoring case and surrounding spaces.
#' @param to Character. Column in `mapping` holding the ODS codes (default: the first
#'   column of `mapping` not used in `by`)
#' @param ods_col Character. Name of the column added to `data` to hold the resulting ODS codes
#' @inheritParams fetch_and_join_ods
#' @param on_missing Character. What to do when codes are ambiguous, not found in ODS, or the
#'   request fails: `"warn"` (default) warns and carries on, `"error"` stops, `"silent"` does nothing.
#'
#' @return Original dataframe with an ODS code column (`ods_col`) and organisation data joined on
#'
#' @export
map_to_ods_codes <- function(
  data,
  mapping,
  by,
  to = setdiff(names(mapping), by)[1],
  ods_col = "ods_org_code",
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
  # resolve to a single value before passing on
  on_missing <- match.arg(on_missing)

  if (!is.data.frame(mapping)) {
    stop("`mapping` must be a dataframe.")
  }

  # names of `by` are data columns, values are mapping columns; unnamed means the same name
  map_cols <- unname(by)
  data_cols <- if (is.null(names(by))) {
    map_cols
  } else {
    ifelse(names(by) == "", map_cols, names(by))
  }

  missing_data_cols <- setdiff(data_cols, names(data))
  if (length(missing_data_cols) > 0) {
    stop(glue::glue(
      "Column(s) {toString(missing_data_cols)} not found in data."
    ))
  }

  missing_map_cols <- setdiff(c(map_cols, to), names(mapping))
  if (length(missing_map_cols) > 0) {
    stop(glue::glue(
      "Column(s) {toString(missing_map_cols)} not found in `mapping`."
    ))
  }

  if (identical(ods_col, data_cols[1])) {
    stop("`ods_col` must be different from the source code column.")
  }

  # warn before overwriting an existing column
  if (ods_col %in% names(data)) {
    warning(
      glue::glue(
        "Column '{ods_col}' already exists in data and will be overwritten."
      ),
      call. = FALSE
    )
  }

  resolved <- resolve_ods_codes(data, mapping, data_cols, map_cols, to)
  report_ambiguous_codes(resolved$ambiguous, on_missing)

  data[[ods_col]] <- resolved$ods_code

  fetch_and_join_ods(
    data,
    ods_col,
    english_only = english_only,
    include_cols = include_cols,
    on_missing = on_missing
  )
}

#' Build a comparison key from one or more columns: trimmed, upper case,
#' NA if any part is NA
#'
#' @noRd
make_key <- function(df, cols) {
  parts <- purrr::map(df[cols], \(x) toupper(trimws(as.character(x))))
  key <- purrr::exec(paste, !!!unname(parts), sep = "\r")
  key[purrr::reduce(purrr::map(parts, is.na), `|`)] <- NA_character_
  key
}

#' Resolve source codes to ODS codes
#'
#' @param data Dataframe containing the codes
#' @param mapping Mapping dataframe
#' @param data_cols Columns in `data` to match on; the first is the source code
#' @param map_cols Columns in `mapping` matching `data_cols`, in the same order
#' @param to Column in `mapping` holding the ODS codes
#'
#' @return List with `ods_code` (character vector, one per row of data) and `ambiguous`
#'   (the distinct `data_cols` values that are in the table but could not be resolved)
#'
#' @noRd
resolve_ods_codes <- function(data, mapping, data_cols, map_cols, to) {
  # rows without a source code or ODS code can't map anything
  mapping <- mapping[!is.na(mapping[[map_cols[1]]]) & !is.na(mapping[[to]]), ]

  # look up rows by key, using only keys that give one ODS code
  lookup_single <- function(map_key_cols, data_key_cols) {
    pairs <- unique(mapping[c(map_key_cols, to)])
    keys <- make_key(pairs, map_key_cols)
    keep <- !is.na(keys) & !keys %in% keys[duplicated(keys)]

    pairs[[to]][keep][match(
      make_key(data, data_key_cols),
      keys[keep],
      incomparables = NA
    )]
  }

  # step 1: full key (code + extra columns); step 2: code alone
  ods <- lookup_single(map_cols, data_cols)
  ods <- dplyr::coalesce(ods, lookup_single(map_cols[1], data_cols[1]))

  # step 3: in the table but unresolved = ambiguous; not in the table = assume it is ODS
  in_table <- make_key(data, data_cols[1]) %in% make_key(mapping, map_cols[1])
  ambiguous <- is.na(ods) & in_table

  assume_ods <- is.na(ods) & !in_table
  ods[assume_ods] <- as.character(data[[data_cols[1]]])[assume_ods]

  list(
    ods_code = ods,
    ambiguous = unique(data[ambiguous, data_cols, drop = FALSE])
  )
}

#' Handle source codes that could not be resolved to one ODS code, according to on_missing
#'
#' @param ambiguous Dataframe of the distinct code (and extra column) values
#' @param on_missing One of "warn", "error" or "silent"
#'
#' @noRd
report_ambiguous_codes <- function(ambiguous, on_missing) {
  if (on_missing == "silent" || nrow(ambiguous) == 0) {
    return(invisible(NULL))
  }

  # list up to 10 codes, with their extra columns where there are any
  shown <- utils::head(ambiguous, 10)
  lines <- purrr::exec(paste, !!!unname(as.list(shown)), sep = " / ")
  extra <- nrow(ambiguous) - nrow(shown)
  suffix <- if (extra > 0) paste0("; and ", extra, " more") else ""

  msg <- paste0(
    nrow(ambiguous),
    " source code(s) have more than one possible ODS code and were not mapped: ",
    paste(lines, collapse = "; "),
    suffix
  )
  hint <- "Add the missing rows to the mapping table."

  if (on_missing == "error") {
    rlang::abort(c(msg, i = hint), class = "odsdata_ambiguous")
  }

  # on_missing == "warn"
  rlang::warn(c(msg, i = hint), class = "odsdata_ambiguous")
}
