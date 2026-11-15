get_script_path <- function() {
  file_arguments <- commandArgs(trailingOnly = FALSE)
  script_arguments <- file_arguments[stringr::str_detect(
    string = file_arguments,
    pattern = "^--file="
  )]

  if (length(script_arguments) != 1L) {
    rlang::abort(message = "Unable to resolve the script path.")
  }

  stringr::str_remove(
    string = script_arguments,
    pattern = "^--file="
  )
}

script_path <- fs::path_abs(path = get_script_path())
project_root <- fs::path_dir(path = fs::path_dir(path = script_path))
source(
  file = fs::path(project_root, "R", "icu_duckdb_transform.R"),
  local = TRUE
)

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!length(args) %in% c(2L, 3L)) {
    rlang::abort(
      message = stringr::str_c(
        "Usage: Rscript scripts/transform_icu_workbook.R",
        " <input.xlsx> <output.duckdb> [diagnostics.csv]"
      )
    )
  }

  diagnostic_log_path <- if (length(args) == 3L) args[[3L]] else NULL
  row_counts <- transform_icu_workbook(
    input_path = args[[1L]],
    database_path = args[[2L]],
    diagnostic_log_path = diagnostic_log_path
  )
  count_summary <- purrr::imap_chr(
    .x = row_counts,
    .f = function(row_count, table_name) {
      stringr::str_c(table_name, ": ", row_count)
    }
  ) |>
    stringr::str_c(collapse = ", ")
  base::message(
    "Created DuckDB database with rows: ",
    count_summary
  )
}

main()
