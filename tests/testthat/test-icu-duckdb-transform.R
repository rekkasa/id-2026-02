testthat::skip_if_not_installed(pkg = "readxl")
testthat::skip_if_not_installed(pkg = "openxlsx")

testthat::test_that(
  desc = "the transformer creates a paired name-free DuckDB database",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    write_synthetic_workbook(workbook_path = workbook_path)

    row_counts <- transform_icu_workbook(
      input_path = workbook_path,
      database_path = database_path
    )
    connection <- DBI::dbConnect(
      drv = duckdb::duckdb(),
      dbdir = database_path,
      read_only = TRUE
    )
    on.exit(
      expr = DBI::dbDisconnect(conn = connection, shutdown = TRUE),
      add = TRUE
    )

    testthat::expect_equal(row_counts$patients, 1L)
    testthat::expect_equal(row_counts$studySessions, 2L)
    testthat::expect_equal(row_counts$physiologicalAssessments, 4L)
    testthat::expect_equal(row_counts$sleepAssessments, 2L)

    table_columns <- purrr::map(
      .x = DBI::dbListTables(conn = connection),
      .f = function(table_name) {
        DBI::dbListFields(conn = connection, name = table_name)
      }
    ) |>
      unlist(use.names = FALSE)
    testthat::expect_false(
      any(stringr::str_detect(
        string = table_columns,
        pattern = stringr::regex(pattern = "name", ignore_case = TRUE)
      ))
    )

    fraction_value <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "fractionInspiredOxygen"',
        'FROM "physiologicalAssessments"',
        'WHERE "assessmentPhase" = \'afterPhysiotherapy\'',
        collapse = " "
      )
    )
    testthat::expect_identical(fraction_value[[1L, 1L]], "0,40")
  }
)

testthat::test_that(
  desc = "the transformer rejects a missing paired control session",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    write_synthetic_workbook(
      workbook_path = workbook_path,
      include_control = FALSE
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = "exactly one session"
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects duplicate treatment records",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    write_synthetic_workbook(
      workbook_path = workbook_path,
      duplicate_physiotherapy = TRUE
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = "exactly one session"
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects an altered source header",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    write_synthetic_workbook(
      workbook_path = workbook_path,
      invalid_physio_header = TRUE
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = "does not match the expected template"
    )
  }
)
