# rebuild internal mapping table from raw csv
steis_mapping_table <- readr::read_csv(
  "data-raw/steis_mapping_table.csv",
  show_col_types = FALSE
)

# internal = TRUE writes R/sysdata.rda
usethis::use_data(steis_mapping_table, internal = TRUE, overwrite = TRUE)
