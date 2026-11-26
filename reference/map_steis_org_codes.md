# Map StEIS organisation codes to ODS codes and fetch ODS organisation data

A StEIS code can cover several organisations. Supply `org_name_col` so
the organisation name picks the right ODS code. Without it, those codes
can't be resolved and are reported according to `on_missing`.

## Usage

``` r
map_steis_org_codes(
  data,
  org_code_col,
  org_name_col = NULL,
  english_only = TRUE,
  include_cols = c("org_name", "org_type", "org_country", "org_region_code",
    "org_region", "org_legal_start", "org_legal_end", "org_operational_start",
    "org_operational_end", "org_successors"),
  on_missing = c("warn", "error", "silent")
)
```

## Arguments

- data:

  A dataframe containing StEIS organisation codes

- org_code_col:

  Column containing organisation code (e.g., 'Reporting Organisation
  Code')

- org_name_col:

  Column containing the organisation name (e.g., 'Reporting
  Organisation'), or NULL

- english_only:

  Logical. Filter to include English (and N/A) organisations

- include_cols:

  Character. ODS columns to add. Default adds all. Choose from:
  `org_name`, `org_type`, `org_country`, `org_region_code`,
  `org_region`; the dates `org_legal_start`, `org_legal_end`,
  `org_operational_start` and `org_operational_end`; and
  `org_successors`, a list column holding the codes of the
  organisation's immediate successors (empty if none). A legal end date
  shows that an organisation has closed. ODS often gives start dates a
  default of 1 April or 1 October.

- on_missing:

  Character. What to do when codes are ambiguous, not found in ODS, or
  the request fails: `"warn"` (default) warns and carries on, `"error"`
  stops, `"silent"` does nothing.

## Value

Original dataframe with an `ods_org_code` column and organisation data
joined on
