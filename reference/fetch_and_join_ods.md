# Fetch and join ODS organisation data to a dataset

Fetch and join ODS organisation data to a dataset

## Usage

``` r
fetch_and_join_ods(
  data,
  org_code_col,
  english_only = TRUE,
  include_cols = character(),
  on_missing = c("warn", "error", "silent")
)
```

## Arguments

- data:

  A dataframe containing organisation codes

- org_code_col:

  Column containing organisation code (e.g., 'ReporterOrganisationCode',
  'RP07', 'reporting_organisation_code')

- english_only:

  Logical. Filter to include English (and N/A) organisations

- include_cols:

  Character. ODS columns to add (`org_name`, `org_type`, `org_country`,
  `org_region_code`, `org_region`). Default adds none.

- on_missing:

  Character. What to do when codes are not found in ODS or the request
  fails: `"warn"` (default) warns and carries on, `"error"` stops,
  `"silent"` does nothing. Missing codes get NA values unless `"error"`
  is used. If every request fails, the function stops regardless.

## Value

Original dataframe with organisation data joined on
