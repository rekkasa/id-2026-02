testthat::skip_if_not_installed(pkg = "readxl")
testthat::skip_if_not_installed(pkg = "openxlsx")

testthat::test_that(
  desc = "the date parser normalizes flexible clinical date formats",
  code = {
    parsed_dates <- parse_ddmmyyyy(
      values = c(
        "1/2/25",
        "01/2/25",
        "1/02/2025",
        "01/02/2025",
        " 1/1/00 ",
        "1/1/99"
      ),
      column_name = "admission_date",
      source_sheet = "crf data",
      source_row_numbers = 2:7
    )

    testthat::expect_identical(
      parsed_dates,
      as.Date(x = c(
        "2025-02-01",
        "2025-02-01",
        "2025-02-01",
        "2025-02-01",
        "2000-01-01",
        "2099-01-01"
      ))
    )
  }
)

testthat::test_that(
  desc = "the date parser rejects implausible text serials",
  code = {
    parsed_cell <- parse_date_cell(value = "25")

    testthat::expect_false(parsed_cell$valid)
    testthat::expect_identical(
      parsed_cell$raw_kind,
      "text_serial_out_of_range"
    )
  }
)

testthat::test_that(
  desc = "the time parser normalizes durations and AM/PM clock times",
  code = {
    clock_times <- purrr::map_chr(
      .x = list(
        as.difftime(tim = 82800, units = "secs"),
        82800,
        23 / 24,
        as.POSIXct(x = "2026-01-01 23:00:00", tz = "UTC"),
        "11:00:00 PM",
        "23:00:00"
      ),
      .f = function(value) parse_time_cell(value = value)$time
    )

    testthat::expect_identical(clock_times, rep(x = "23:00:00", times = 6L))
    testthat::expect_identical(
      parse_time_cell(value = "12:00:00 AM")$time,
      "00:00:00"
    )
    testthat::expect_identical(
      parse_time_cell(value = "12:00:00 PM")$time,
      "12:00:00"
    )

    next_day_duration <- parse_time_cell(
      value = as.difftime(tim = 86400, units = "secs")
    )
    testthat::expect_false(next_day_duration$valid)
    testthat::expect_identical(
      next_day_duration$raw_kind,
      "duration_seconds_out_of_range"
    )
  }
)

testthat::test_that(
  desc = "the transformer creates a paired name-free DuckDB database",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    diagnostic_log_path <- fs::file_temp(ext = ".csv")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path, diagnostic_log_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    write_synthetic_workbook(
      workbook_path = workbook_path,
      admission_times = "11:00:00 PM"
    )

    row_counts <- transform_icu_workbook(
      input_path = workbook_path,
      database_path = database_path,
      diagnostic_log_path = diagnostic_log_path
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
    testthat::expect_equal(row_counts$study_sessions, 2L)
    testthat::expect_equal(row_counts$physiological_assessments, 4L)
    testthat::expect_equal(row_counts$sleep_assessments, 2L)

    table_names <- DBI::dbListTables(conn = connection)
    table_columns <- purrr::map(
      .x = table_names,
      .f = function(table_name) {
        DBI::dbListFields(conn = connection, name = table_name)
      }
    ) |>
      unlist(use.names = FALSE)
    testthat::expect_true(
      all(stringr::str_detect(
        string = table_names,
        pattern = "^[a-z][a-z0-9_]*$"
      ))
    )
    testthat::expect_true(
      all(stringr::str_detect(
        string = table_columns,
        pattern = "^[a-z][a-z0-9_]*$"
      ))
    )
    testthat::expect_false(
      any(stringr::str_detect(
        string = table_columns,
        pattern = stringr::regex(pattern = "name", ignore_case = TRUE)
      ))
    )

    fraction_value <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "fraction_inspired_oxygen"',
        'FROM "physiological_assessments"',
        'WHERE "assessment_phase" = \'afterPhysiotherapy\'',
        collapse = " "
      )
    )
    testthat::expect_identical(fraction_value[[1L, 1L]], "0,40")

    patient_values <- DBI::dbGetQuery(
      conn = connection,
      statement = 'SELECT "patient_id", "eama_reference" FROM "patients"'
    )
    testthat::expect_identical(patient_values$patient_id, "1")
    testthat::expect_identical(patient_values$eama_reference, "EAMA-001")

    admission_dates <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "admission_date", "mechanical_ventilation_start_date",',
        '"icu_discharge_date" FROM "icu_admissions"'
      )
    )
    testthat::expect_s3_class(admission_dates$admission_date, "Date")
    testthat::expect_identical(
      admission_dates$admission_date,
      as.Date(x = "2026-01-01")
    )
    testthat::expect_identical(
      admission_dates$mechanical_ventilation_start_date,
      as.Date(x = "2026-01-02")
    )
    testthat::expect_identical(
      admission_dates$icu_discharge_date,
      as.Date(x = "2026-01-03")
    )

    admission_times <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT CAST("admission_time" AS VARCHAR) AS "admission_time",',
        'CAST("mechanical_ventilation_start_time" AS VARCHAR)',
        'AS "mechanical_ventilation_start_time" FROM "icu_admissions"',
        sep = " "
      )
    )
    time_types <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT typeof("admission_time") AS "admission_time_type",',
        'typeof("mechanical_ventilation_start_time")',
        'AS "mechanical_ventilation_start_time_type"',
        'FROM "icu_admissions"',
        sep = " "
      )
    )
    testthat::expect_identical(admission_times$admission_time, "23:00:00")
    testthat::expect_identical(
      admission_times$mechanical_ventilation_start_time,
      "15:45:00"
    )
    testthat::expect_identical(time_types$admission_time_type, "TIME")
    testthat::expect_identical(
      time_types$mechanical_ventilation_start_time_type,
      "TIME"
    )

    connection_dates <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "connection_date" FROM "study_sessions"',
        'WHERE "condition" = \'withPhysiotherapy\''
      )
    )
    testthat::expect_s3_class(connection_dates$connection_date, "Date")
    testthat::expect_identical(
      connection_dates$connection_date,
      as.Date(x = "2026-01-04")
    )
    testthat::expect_false(fs::file_exists(path = diagnostic_log_path))
  }
)

testthat::test_that(
  desc = "the transformer permits duplicate EAMA references",
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
      patient_ids = c("1", "2"),
      eama_references = c("EAMA-001", "EAMA-001")
    )

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

    patient_values <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "patient_id", "eama_reference" FROM "patients"',
        'ORDER BY "patient_id"'
      )
    )
    testthat::expect_equal(row_counts$patients, 2L)
    testthat::expect_equal(row_counts$study_sessions, 4L)
    testthat::expect_identical(patient_values$patient_id, c("1", "2"))
    testthat::expect_identical(
      patient_values$eama_reference,
      c("EAMA-001", "EAMA-001")
    )
  }
)

testthat::test_that(
  desc = "the transformer retains blank dates as database nulls",
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
      admission_dates = NA_character_
    )

    transform_icu_workbook(
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

    admission_dates <- DBI::dbGetQuery(
      conn = connection,
      statement = 'SELECT "admission_date" FROM "icu_admissions"'
    )
    testthat::expect_s3_class(admission_dates$admission_date, "Date")
    testthat::expect_true(is.na(admission_dates$admission_date))
  }
)

testthat::test_that(
  desc = "the transformer normalizes flexible dates in every date field",
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
      patient_ids = c("1", "2", "3", "4"),
      admission_dates = c(
        "1/2/25",
        "01/2/25",
        "1/02/2025",
        "01/02/2025"
      ),
      mechanical_ventilation_start_dates = c(
        "3/4/25",
        "03/4/25",
        "3/04/2025",
        "03/04/2025"
      ),
      icu_discharge_dates = c(
        "5/6/25",
        "05/6/25",
        "5/06/2025",
        "05/06/2025"
      ),
      connection_dates = c(
        "7/8/25",
        "07/8/25",
        "7/08/2025",
        "07/08/2025"
      )
    )

    transform_icu_workbook(
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

    admission_dates <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "admission_date", "mechanical_ventilation_start_date",',
        '"icu_discharge_date" FROM "icu_admissions"',
        'ORDER BY "patient_id"'
      )
    )
    connection_dates <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "connection_date" FROM "study_sessions"',
        'WHERE "condition" = \'withPhysiotherapy\'',
        'ORDER BY "patient_id"'
      )
    )
    testthat::expect_identical(
      admission_dates$admission_date,
      rep(x = as.Date(x = "2025-02-01"), times = 4L)
    )
    testthat::expect_identical(
      admission_dates$mechanical_ventilation_start_date,
      rep(x = as.Date(x = "2025-04-03"), times = 4L)
    )
    testthat::expect_identical(
      admission_dates$icu_discharge_date,
      rep(x = as.Date(x = "2025-06-05"), times = 4L)
    )
    testthat::expect_identical(
      connection_dates$connection_date,
      rep(x = as.Date(x = "2025-08-07"), times = 4L)
    )
  }
)

testthat::test_that(
  desc = "the transformer converts Excel date cells and serials",
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

    admission_date <- as.Date(x = "2025-02-01")
    ventilation_date <- as.Date(x = "2025-03-02")
    discharge_date <- as.Date(x = "2025-04-03")
    connection_datetime <- as.POSIXct(
      x = "2025-05-04 14:30:00",
      tz = "UTC"
    )
    admission_serial <- as.numeric(
      admission_date - as.Date(x = "1899-12-30")
    )
    ventilation_serial <- as.numeric(
      ventilation_date - as.Date(x = "1899-12-30")
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "crf data",
      value = stringr::str_c(admission_serial, ".5"),
      column_number = 9L,
      row_number = 2L
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "crf data",
      value = ventilation_serial + 0.5,
      column_number = 21L,
      row_number = 2L
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "crf data",
      value = discharge_date,
      column_number = 26L,
      row_number = 2L
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "crf data",
      value = 0.5,
      column_number = 10L,
      row_number = 2L
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "crf data",
      value = "23:15",
      column_number = 22L,
      row_number = 2L
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ",
      value = connection_datetime,
      column_number = 3L,
      row_number = 3L
    )

    transform_icu_workbook(
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

    admission_dates <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "admission_date", "mechanical_ventilation_start_date",',
        '"icu_discharge_date" FROM "icu_admissions"'
      )
    )
    connection_dates <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "connection_date" FROM "study_sessions"',
        'WHERE "condition" = \'withPhysiotherapy\''
      )
    )
    admission_times <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT CAST("admission_time" AS VARCHAR) AS "admission_time",',
        'CAST("mechanical_ventilation_start_time" AS VARCHAR)',
        'AS "mechanical_ventilation_start_time" FROM "icu_admissions"',
        sep = " "
      )
    )
    testthat::expect_identical(admission_dates$admission_date, admission_date)
    testthat::expect_identical(
      admission_dates$mechanical_ventilation_start_date,
      ventilation_date
    )
    testthat::expect_identical(
      admission_dates$icu_discharge_date,
      discharge_date
    )
    testthat::expect_identical(
      connection_dates$connection_date,
      as.Date(x = connection_datetime)
    )
    testthat::expect_identical(admission_times$admission_time, "12:00:00")
    testthat::expect_identical(
      admission_times$mechanical_ventilation_start_time,
      "23:15:00"
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects invalid 24-hour admission times",
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
      admission_times = "24:00"
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = stringr::str_c(
        "invalid 24-hour HH:MM values in 'admission_time' at ",
        "Excel row\\(s): ",
        "2 \\(types: unsupported_text=1\\)"
      )
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects malformed and impossible dates",
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
      patient_ids = c("1", "2", "3", "4", "5"),
      admission_dates = c(
        "2026-01-01",
        "31/02/2026",
        "1/2/2",
        "1/2/20255",
        "1-2-25"
      )
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = stringr::str_c(
        "invalid date values in 'admission_date' at Excel row\\(s\\): ",
        "2, 3, 4, 5, 6 \\(types: unsupported_text=5\\)"
      )
    )
  }
)

testthat::test_that(
  desc = "the transformer logs a missing CRF patient identifier safely",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    diagnostic_log_path <- fs::file_temp(ext = ".csv")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path, diagnostic_log_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    write_synthetic_workbook(
      workbook_path = workbook_path,
      missing_crf_patient_id = TRUE
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path,
        diagnostic_log_path = diagnostic_log_path
      ),
      regexp = "missing patient_id"
    )

    diagnostic_lines <- readLines(
      con = diagnostic_log_path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    testthat::expect_identical(
      diagnostic_lines,
      c(
        "\"event\",\"sheet\",\"excel_row\",\"missing_column\"",
        "\"missing_patient_id_in_crf\",\"crf data\",2,\"patient_id\""
      )
    )
    testthat::expect_false(
      any(stringr::str_detect(
        string = diagnostic_lines,
        pattern = "Synthetic Name"
      ))
    )
  }
)

testthat::test_that(
  desc = "the transformer logs an unmatched source record safely",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    diagnostic_log_path <- fs::file_temp(ext = ".csv")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path, diagnostic_log_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    write_synthetic_workbook(
      workbook_path = workbook_path,
      physiotherapy_patient_ids = "not-linked"
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path,
        diagnostic_log_path = diagnostic_log_path
      ),
      regexp = "missing patient_id"
    )

    diagnostic_lines <- readLines(
      con = diagnostic_log_path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    testthat::expect_true(any(stringr::str_detect(
      string = diagnostic_lines,
      pattern = "unmatched_patient_record"
    )))
    testthat::expect_true(any(stringr::str_detect(
      string = diagnostic_lines,
      pattern = "ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ"
    )))
    testthat::expect_true(any(stringr::str_detect(
      string = diagnostic_lines,
      pattern = ",3,"
    )))
    testthat::expect_false(any(stringr::str_detect(
      string = diagnostic_lines,
      pattern = "not-linked"
    )))
  }
)

testthat::test_that(
  desc = "a diagnostic log write failure preserves the validation error",
  code = {
    workbook_path <- fs::file_temp(ext = ".xlsx")
    database_path <- fs::file_temp(ext = ".duckdb")
    log_parent_path <- fs::file_temp()
    diagnostic_log_path <- fs::path(log_parent_path, "diagnostics.csv")
    on.exit(
      expr = {
        paths <- c(workbook_path, database_path, log_parent_path)
        existing_paths <- paths[fs::file_exists(path = paths)]
        if (length(existing_paths) > 0L) {
          fs::file_delete(path = existing_paths)
        }
      },
      add = TRUE
    )
    fs::file_create(path = log_parent_path)
    write_synthetic_workbook(
      workbook_path = workbook_path,
      missing_crf_patient_id = TRUE
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path,
        diagnostic_log_path = diagnostic_log_path
      ),
      regexp = "missing patient_id"
    )
    testthat::expect_false(fs::file_exists(path = diagnostic_log_path))
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
