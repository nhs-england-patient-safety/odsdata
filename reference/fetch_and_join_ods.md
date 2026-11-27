# Fetch and join ODS organisation data to a dataset

Fetch and join ODS organisation data to a dataset

## Usage

``` r
fetch_and_join_ods(
  data,
  org_code_col,
  english_only = TRUE,
  include_cols = c("org_name", "org_type", "org_country", "org_region_code",
    "org_region", "org_legal_start", "org_legal_end", "org_operational_start",
    "org_operational_end", "org_successors"),
  on_missing = c("warn", "error", "silent")
)
```

## Arguments

- data:

  A dataframe containing organisation codes

- org_code_col:

  Column containing organisation code (e.g., 'ReporterOrganisationCode',
  'RP07')

- english_only:

  Logical. Filter to include English (and N/A) organisations

- include_cols:

  Character. ODS columns to add. Default adds all. Choose from:
  `org_name`, `org_type`, `org_country`, `org_region_code`,
  `org_region`; the dates `org_legal_start`, `org_legal_end`,
  `org_operational_start` and `org_operational_end`; and
  `org_successors`, a list column holding, for each row, a tibble with
  the code and name of each immediate successor (empty if none). A legal
  end date shows that an organisation has closed. ODS often gives start
  dates a default of 1 April or 1 October.

- on_missing:

  Character. What to do when codes are not found in ODS or the request
  fails: `"warn"` (default) warns and carries on, `"error"` stops,
  `"silent"` does nothing. Missing codes get NA values unless `"error"`
  is used. If every request fails, the function stops regardless.

## Value

Original dataframe with organisation data joined on
