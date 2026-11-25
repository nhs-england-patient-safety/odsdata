# Map organisation codes to ODS codes and fetch ODS organisation data

Each row is matched to an ODS code using the mapping table, in this
order:

1.  the full key in `by` (e.g. code and name together)

2.  the source code alone, if the table gives only one ODS code

## Usage

``` r
map_to_ods_codes(
  data,
  mapping,
  by,
  to = setdiff(names(mapping), by)[1],
  ods_col = "ods_org_code",
  english_only = TRUE,
  include_cols = character(),
  on_missing = c("warn", "error", "silent")
)
```

## Arguments

- data:

  A dataframe containing the codes to map

- mapping:

  A dataframe linking source codes to ODS codes

- by:

  Character. Links columns in `data` to columns in `mapping`: names are
  columns in `data`, values are columns in `mapping`, and an unnamed
  value means the column has the same name in both. The first entry is
  the source code. Extra entries (e.g. an organisation name) separate
  codes that cover more than one organisation. Codes and names are
  compared ignoring case and surrounding spaces.

- to:

  Character. Column in `mapping` holding the ODS codes (default: the
  first column of `mapping` not used in `by`)

- ods_col:

  Character. Name of the column added to `data` to hold the resulting
  ODS codes

- english_only:

  Logical. Filter to include English (and N/A) organisations

- include_cols:

  Character. ODS columns to add (`org_name`, `org_type`, `org_country`,
  `org_region_code`, `org_region`). Default adds none.

- on_missing:

  Character. What to do when codes are ambiguous, not found in ODS, or
  the request fails: `"warn"` (default) warns and carries on, `"error"`
  stops, `"silent"` does nothing.

## Value

Original dataframe with an ODS code column (`ods_col`) and organisation
data joined on

## Details

A code that is in the table but still has more than one possible ODS
code is given `NA` (and reported according to `on_missing`). Codes not
in the table are assumed to be ODS codes already.
