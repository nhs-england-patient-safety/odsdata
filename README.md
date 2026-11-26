# odsdata

Look up NHS organisation details (name, type, country, region) from a column of ODS codes in your data.

## Quick Start

To install the package using `renv`:

```r
renv::install("nhs-england-patient-safety/odsdata")
```

To fetch ODS organisation details from the API:

```r
# the API key is read from an environment variable
Sys.setenv(ods_key = "your-key-here")

orgs <- dplyr::tibble(org_code = c("RR8", "RX1", "NQV", "RWX", NA))

fetch_and_join_ods(data = orgs, org_code_col = "org_code")
```

You get your original data back with the extra columns added on the right. 

If your data holds codes from another source, map them to ODS codes with a table first:

```r
mapping <- dplyr::tibble(
  local_code = c("A1", "A2", "B1", "A3"),
  ods_code   = c("RR8", "RX1", "NQV", "RWX")
)

sites <- dplyr::tibble(code = c("A1", "A2", "B1", "A3", NA))

map_to_ods_codes(data = sites, mapping = mapping, by = c(code = "local_code"))
```

## Organisation Data Terminology API

The Organisation Data Terminology (ODT) API is a FHIR API: a REST API with a fixed set of "resources", each with its own path. It holds all the Organisation Data Service (ODS) data. The base URL is:

```
https://api.service.nhs.uk/organisation-data-terminology-api/fhir
```

```
/fhir
├── /Organization/{code}        one organisation: name, address, roles, status
├── /Organization?...           search for organisations
├── /OrganizationAffiliation    relationships between organisations
├── /CodeSystem/$lookup         look up one code, with extra properties  <- used by odsdata
├── /ValueSet/$expand           pull a filtered batch of codes
├── /List/{id}                  records that have been deleted
└── /metadata                   what the server supports
```

`odsdata` only uses `/CodeSystem/$lookup`. A CodeSystem is a table of codes with properties attached to each one. We use two:

- the organisation CodeSystem, for name, country, primary role code and NHS England region code (region names are organisations too, so they come from here as well)
- the role CodeSystem, to turn a role code like `RO197` into `NHS TRUST`

The API holds a lot more than this, which could be added later.

## Functions

| Function                | Purpose                                                   |
|-------------------------|-----------------------------------------------------------|
| `fetch_and_join_ods()`  | Add ODS organisation columns to data that holds ODS codes |
| `map_to_ods_codes()`    | Map ODS codes from another system                         |
| `map_steis_org_codes()` | `map_to_ods_codes()` using a StEIS mapping table          |

All three share these arguments:

- `include_cols`: which ODS columns to add (`org_name`, `org_type`, `org_country`, `org_region_code`, `org_region`).
- `english_only`: drop non-English organisations, keeping those with no country.
- `on_missing`: what to do with codes that can't be resolved. `"warn"` (default) warns and carries on, `"error"` stops, `"silent"` says nothing. It covers codes ODS can't find, codes the mapping table can't resolve (see below), and failed requests. If every request fails, the function stops whatever `on_missing` is set to.

If your data already holds ODS codes, start at `fetch_and_join_ods()`.

`fetch_and_join_ods()` does the following, in order:

1. Looks up each unique, non-missing code once, however often it appears in your data. Codes that can't be found are reported according to `on_missing`.
2. Adds role and region names if you asked for them, with one lookup per distinct code.
3. If `english_only = TRUE`, drops rows for organisations whose country is not England.
4. Joins the columns you asked for back to your data. Rows with a missing code get missing values.

Lookups run in parallel above 250 codes. The package waits between requests to stay inside the API's rate limit.

## Resources

- [ODT API Documentation](https://digital.nhs.uk/developer/api-catalogue/organisation-data-terminology)
- [ODS Data Coverage](https://digital.nhs.uk/services/organisation-data-service/ods-data-coverage)