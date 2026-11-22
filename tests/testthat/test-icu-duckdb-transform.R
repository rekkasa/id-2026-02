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
  desc = "the sleep duration parser converts supported source formats",
  code = {
    hours_minutes <- parse_sleep_duration_cell(
      value = "2h 5m",
      input_format = "hours_minutes"
    )
    minute_suffix <- parse_sleep_duration_cell(
      value = "15.5m",
      input_format = "minute_suffix"
    )
    blank_value <- parse_sleep_duration_cell(
      value = NA_character_,
      input_format = "minute_suffix"
    )
    invalid_hours_minutes <- parse_sleep_duration_cell(
      value = "2h 60m",
      input_format = "hours_minutes"
    )
    invalid_minutes <- parse_sleep_duration_cell(
      value = "15",
      input_format = "minute_suffix"
    )

    testthat::expect_identical(hours_minutes$minutes, 125)
    testthat::expect_true(hours_minutes$valid)
    testthat::expect_identical(minute_suffix$minutes, 15.5)
    testthat::expect_true(minute_suffix$valid)
    testthat::expect_identical(blank_value$minutes, NA_real_)
    testthat::expect_true(blank_value$valid)
    testthat::expect_false(invalid_hours_minutes$valid)
    testthat::expect_identical(
      invalid_hours_minutes$raw_kind,
      "minutes_out_of_range"
    )
    testthat::expect_false(invalid_minutes$valid)
    testthat::expect_identical(invalid_minutes$raw_kind, "unsupported_text")
  }
)

testthat::test_that(
  desc = "the arousal index parser normalizes optional per-hour suffixes",
  code = {
    per_hour <- parse_arousal_index_cell(value = "18.5/h")
    numeric_value <- parse_arousal_index_cell(value = "12")
    blank_value <- parse_arousal_index_cell(value = NA_character_)
    invalid_value <- parse_arousal_index_cell(value = "18/day")

    testthat::expect_identical(per_hour$value, 18.5)
    testthat::expect_true(per_hour$valid)
    testthat::expect_identical(numeric_value$value, 12)
    testthat::expect_true(numeric_value$valid)
    testthat::expect_identical(blank_value$value, NA_real_)
    testthat::expect_true(blank_value$valid)
    testthat::expect_false(invalid_value$valid)
    testthat::expect_identical(invalid_value$raw_kind, "unsupported_text")
  }
)

testthat::test_that(
  desc = "the sleep percentage parser normalizes source percentages",
  code = {
    percentage <- parse_sleep_percentage_cell(value = "87.5%")
    comma_percentage <- parse_sleep_percentage_cell(value = "42,25 %")
    fraction_percentage <- parse_sleep_percentage_cell(value = "0.875")
    numeric_percentage <- parse_sleep_percentage_cell(value = "87.5")
    blank_value <- parse_sleep_percentage_cell(value = NA_character_)
    invalid_value <- parse_sleep_percentage_cell(value = "not available")

    testthat::expect_identical(percentage$value, 87.5)
    testthat::expect_true(percentage$valid)
    testthat::expect_identical(comma_percentage$value, 42.25)
    testthat::expect_true(comma_percentage$valid)
    testthat::expect_identical(fraction_percentage$value, 87.5)
    testthat::expect_true(fraction_percentage$valid)
    testthat::expect_identical(numeric_percentage$value, 87.5)
    testthat::expect_true(numeric_percentage$valid)
    testthat::expect_identical(blank_value$value, NA_real_)
    testthat::expect_true(blank_value$valid)
    testthat::expect_false(invalid_value$valid)
    testthat::expect_identical(invalid_value$raw_kind, "unsupported_text")
  }
)

testthat::test_that(
  desc = "the hand-grip parser splits right and left strengths",
  code = {
    strengths <- parse_hand_grip_strength_cell(value = "27.5 (R) 24 (L)")
    comma_strengths <- parse_hand_grip_strength_cell(
      value = "27,5(R)24,25(L)"
    )
    blank_strengths <- parse_hand_grip_strength_cell(value = NA_character_)
    invalid_strengths <- parse_hand_grip_strength_cell(value = "27 (L) 24 (R)")

    testthat::expect_identical(strengths$right, 27.5)
    testthat::expect_identical(strengths$left, 24)
    testthat::expect_true(strengths$valid)
    testthat::expect_identical(comma_strengths$right, 27.5)
    testthat::expect_identical(comma_strengths$left, 24.25)
    testthat::expect_true(comma_strengths$valid)
    testthat::expect_identical(blank_strengths$right, NA_real_)
    testthat::expect_identical(blank_strengths$left, NA_real_)
    testthat::expect_true(blank_strengths$valid)
    testthat::expect_false(invalid_strengths$valid)
    testthat::expect_identical(invalid_strengths$raw_kind, "unsupported_text")
  }
)

testthat::test_that(
  desc = "the blood pressure parser splits diastolic and systolic values",
  code = {
    blood_pressure <- parse_blood_pressure_cell(value = "70/120")
    comma_blood_pressure <- parse_blood_pressure_cell(
      value = "70,5 / 120,5"
    )
    blank_blood_pressure <- parse_blood_pressure_cell(value = NA_character_)
    invalid_blood_pressure <- parse_blood_pressure_cell(value = "70-120")

    testthat::expect_identical(blood_pressure$diastolic, 70)
    testthat::expect_identical(blood_pressure$systolic, 120)
    testthat::expect_true(blood_pressure$valid)
    testthat::expect_identical(comma_blood_pressure$diastolic, 70.5)
    testthat::expect_identical(comma_blood_pressure$systolic, 120.5)
    testthat::expect_true(comma_blood_pressure$valid)
    testthat::expect_identical(blank_blood_pressure$diastolic, NA_real_)
    testthat::expect_identical(blank_blood_pressure$systolic, NA_real_)
    testthat::expect_true(blank_blood_pressure$valid)
    testthat::expect_false(invalid_blood_pressure$valid)
    testthat::expect_identical(
      invalid_blood_pressure$raw_kind,
      "unsupported_text"
    )
  }
)

testthat::test_that(
  desc = "the transformer stores numeric sleep measurements",
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

    sleep_assessment_columns <- DBI::dbListFields(
      conn = connection,
      name = "sleep_assessments"
    )
    sleep_values <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "assessment_id", "patient_id", "assessment", "time_in_bed",',
        '"total_sleep_time", "sleep_efficiency",',
        '"wake_after_sleep_onset", "sleep_latency", "stage1_duration",',
        '"stage1_percent_tst", "stage2_duration", "stage2_percent_tst",',
        '"slow_wave_sleep_duration", "arousal_index"',
        'FROM "sleep_assessments"',
        'ORDER BY "assessment_id"',
        sep = " "
      )
    )
    sleep_types <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT typeof("assessment_id") AS "assessment_id_type",',
        'typeof("time_in_bed") AS "time_in_bed_type",',
        'typeof("total_sleep_time") AS "total_sleep_time_type",',
        'typeof("sleep_efficiency") AS "sleep_efficiency_type",',
        'typeof("wake_after_sleep_onset") AS "wake_after_sleep_onset_type",',
        'typeof("sleep_latency") AS "sleep_latency_type",',
        'typeof("stage1_duration") AS "stage1_duration_type",',
        'typeof("stage1_percent_tst") AS "stage1_percent_tst_type",',
        'typeof("stage2_duration") AS "stage2_duration_type",',
        'typeof("stage2_percent_tst") AS "stage2_percent_tst_type",',
        'typeof("slow_wave_sleep_duration")',
        'AS "slow_wave_sleep_duration_type",',
        'typeof("arousal_index") AS "arousal_index_type"',
        'FROM "sleep_assessments" LIMIT 1',
        sep = " "
      )
    )

    testthat::expect_false(
      "sleep_assessment_id" %in% sleep_assessment_columns
    )
    testthat::expect_false("session_id" %in% sleep_assessment_columns)
    testthat::expect_true(all(c(
      "assessment_id",
      "patient_id",
      "assessment"
    ) %in% sleep_assessment_columns))
    testthat::expect_equal(sleep_values$assessment_id, 1L:2L)
    testthat::expect_identical(
      sleep_values$patient_id,
      c("1", "1")
    )
    testthat::expect_identical(
      sleep_values$assessment,
      c("with_physiotherapy", "without_physiotherapy")
    )
    testthat::expect_identical(sleep_values$time_in_bed, c(525, 495))
    testthat::expect_identical(sleep_values$total_sleep_time, c(465, 450))
    testthat::expect_identical(sleep_values$sleep_efficiency, c(91, 88))
    testthat::expect_identical(
      sleep_values$wake_after_sleep_onset,
      c(12, 15.5)
    )
    testthat::expect_identical(sleep_values$sleep_latency, c(8, 10))
    testthat::expect_identical(sleep_values$stage1_duration, c(25, 30))
    testthat::expect_identical(
      sleep_values$stage1_percent_tst,
      c(4, 5)
    )
    testthat::expect_identical(sleep_values$stage2_duration, c(250, 240))
    testthat::expect_identical(
      sleep_values$stage2_percent_tst,
      c(52, 50)
    )
    testthat::expect_identical(
      sleep_values$slow_wave_sleep_duration,
      c(85, 90)
    )
    testthat::expect_identical(sleep_values$arousal_index, c(12, 18.5))
    testthat::expect_identical(
      as.character(sleep_types[1L, ]),
      c("INTEGER", rep(x = "DOUBLE", times = 11L))
    )
  }
)

testthat::test_that(
  desc = "zero total sleep time clears derived sleep measurements",
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
      patient_ids = c("1", "2")
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "sleep data",
      value = "0h 0m",
      column_number = 4L,
      row_number = 3L
    )
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "sleep data",
      value = "0h 0m",
      column_number = 20L,
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

    sleep_values <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "patient_id", "assessment", "time_in_bed",',
        '"total_sleep_time", "sleep_efficiency",',
        '"wake_after_sleep_onset", "sleep_latency", "stage_shifts",',
        '"stage1_duration", "stage1_percent_tst", "stage2_duration",',
        '"stage2_percent_tst", "slow_wave_sleep_duration",',
        '"slow_wave_sleep_percent_tst", "rem_sleep_duration",',
        '"rem_sleep_percent_tst", "arousal_index", "rcsq_score"',
        'FROM "sleep_assessments"',
        'ORDER BY "patient_id", "assessment"',
        sep = " "
      )
    )
    zero_sleep_values <- sleep_values |>
      dplyr::filter(.data$patient_id == "1")
    nonzero_sleep_values <- sleep_values |>
      dplyr::filter(.data$patient_id == "2")
    zero_total_time_columns <- c(
      "sleep_latency",
      "wake_after_sleep_onset",
      "stage_shifts",
      "stage1_duration",
      "stage1_percent_tst",
      "stage2_duration",
      "stage2_percent_tst",
      "slow_wave_sleep_duration",
      "slow_wave_sleep_percent_tst",
      "rem_sleep_duration",
      "rem_sleep_percent_tst",
      "arousal_index"
    )

    testthat::expect_identical(
      zero_sleep_values$total_sleep_time,
      c(0, 0)
    )
    testthat::expect_true(all(purrr::map_lgl(
      .x = zero_sleep_values[zero_total_time_columns],
      .f = function(values) all(is.na(values))
    )))
    testthat::expect_identical(
      zero_sleep_values$time_in_bed,
      c(525, 495)
    )
    testthat::expect_identical(
      zero_sleep_values$sleep_efficiency,
      c(91, 88)
    )
    testthat::expect_identical(
      zero_sleep_values$wake_after_sleep_onset,
      c(NA_real_, NA_real_)
    )
    testthat::expect_identical(
      zero_sleep_values$rcsq_score,
      c("value_34", "value_18")
    )
    testthat::expect_identical(
      nonzero_sleep_values$sleep_latency,
      c(8, 10)
    )
    testthat::expect_identical(
      nonzero_sleep_values$wake_after_sleep_onset,
      c(12, 15.5)
    )
    testthat::expect_identical(
      nonzero_sleep_values$stage1_duration,
      c(25, 30)
    )
    testthat::expect_identical(
      nonzero_sleep_values$stage2_duration,
      c(250, 240)
    )
    testthat::expect_identical(
      nonzero_sleep_values$slow_wave_sleep_duration,
      c(85, 90)
    )
    testthat::expect_identical(
      nonzero_sleep_values$arousal_index,
      c(12, 18.5)
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects malformed sleep durations",
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
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "sleep data",
      value = "8h 60m",
      column_number = 3L,
      row_number = 3L
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = stringr::str_c(
        "Worksheet 'sleep data' contains invalid sleep duration values in ",
        "'without_time_in_bed' at Excel row\\(s\\): 3 ",
        "\\(types: minutes_out_of_range=1\\)"
      )
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects malformed arousal indices",
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
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "sleep data",
      value = "18/day",
      column_number = 17L,
      row_number = 3L
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = stringr::str_c(
        "Worksheet 'sleep data' contains invalid arousal index values in ",
        "'without_arousal_index' at Excel row\\(s\\): 3 ",
        "\\(types: unsupported_text=1\\)"
      )
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects malformed hand-grip strengths",
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
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ",
      value = "27 (R) invalid (L)",
      column_number = 9L,
      row_number = 3L
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = stringr::str_c(
        "Worksheet 'ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ' contains invalid hand grip strength ",
        "values in 'handgrip_strength' at Excel row\\(s\\): 3 ",
        "\\(types: unsupported_text=1\\)"
      )
    )
  }
)

testthat::test_that(
  desc = "the transformer rejects malformed blood pressure values",
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
    overwrite_excel_cell(
      workbook_path = workbook_path,
      sheet_name = "ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ",
      value = "70-120",
      column_number = 20L,
      row_number = 3L
    )

    testthat::expect_error(
      object = transform_icu_workbook(
        input_path = workbook_path,
        database_path = database_path
      ),
      regexp = stringr::str_c(
        "Worksheet 'ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ' contains invalid blood pressure ",
        "values in 'pre_blood_pressure' at Excel row\\(s\\): 3 ",
        "\\(types: unsupported_text=1\\)"
      )
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
    testthat::expect_equal(row_counts$patient_clinical_status, 1L)
    testthat::expect_equal(row_counts$physiological_assessments, 4L)
    testthat::expect_equal(row_counts$sleep_assessments, 2L)
    testthat::expect_null(row_counts$session_clinical_status)

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
    testthat::expect_true("patient_clinical_status" %in% table_names)
    testthat::expect_false("session_clinical_status" %in% table_names)

    fraction_value <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "fraction_inspired_oxygen"',
        'FROM "physiological_assessments"',
        'WHERE "phase" = \'after_physiotherapy\'',
        collapse = " "
      )
    )
    testthat::expect_identical(fraction_value[[1L, 1L]], "0,40")

    physiological_assessment_columns <- DBI::dbListFields(
      conn = connection,
      name = "physiological_assessments"
    )
    physiological_assessment_values <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "assessment_id", "patient_id", "assessment", "phase",',
        '"blood_pressure_diastolic",',
        '"blood_pressure_systolic", "noradrenaline_rate"',
        'FROM "physiological_assessments"',
        'ORDER BY "assessment_id"',
        sep = " "
      )
    )
    physiological_assessment_types <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT typeof("assessment_id") AS "assessment_id_type",',
        'typeof("blood_pressure_diastolic")',
        'AS "blood_pressure_diastolic_type",',
        'typeof("blood_pressure_systolic")',
        'AS "blood_pressure_systolic_type"',
        'FROM "physiological_assessments" LIMIT 1',
        sep = " "
      )
    )
    testthat::expect_false(
      "blood_pressure" %in% physiological_assessment_columns
    )
    testthat::expect_false("session_id" %in% physiological_assessment_columns)
    testthat::expect_false(
      "assessment_phase" %in% physiological_assessment_columns
    )
    testthat::expect_false(
      "norepinephrine_rate" %in% physiological_assessment_columns
    )
    testthat::expect_true(all(c(
      "assessment_id",
      "patient_id",
      "assessment",
      "phase",
      "blood_pressure_diastolic",
      "blood_pressure_systolic",
      "noradrenaline_rate"
    ) %in% physiological_assessment_columns))
    testthat::expect_equal(
      physiological_assessment_values$assessment_id,
      1L:4L
    )
    testthat::expect_identical(
      physiological_assessment_values$patient_id,
      rep(x = "1", times = 4L)
    )
    testthat::expect_identical(
      physiological_assessment_values$assessment,
      c(
        "with_physiotherapy",
        "with_physiotherapy",
        "without_physiotherapy",
        "without_physiotherapy"
      )
    )
    testthat::expect_identical(
      physiological_assessment_values$phase,
      c(
        "before_physiotherapy",
        "after_physiotherapy",
        "recording_start",
        "recording_end"
      )
    )
    testthat::expect_identical(
      physiological_assessment_values$blood_pressure_diastolic,
      c(70, 72, 68, 70)
    )
    testthat::expect_identical(
      physiological_assessment_values$blood_pressure_systolic,
      c(120, 118, 116, 115)
    )
    testthat::expect_identical(
      physiological_assessment_values$noradrenaline_rate,
      c("1.5", "2.5", "3.5", "4.5")
    )
    testthat::expect_identical(
      as.character(physiological_assessment_types[1L, ]),
      c("INTEGER", "DOUBLE", "DOUBLE")
    )

    patient_values <- DBI::dbGetQuery(
      conn = connection,
      statement = 'SELECT "patient_id", "eama_reference" FROM "patients"'
    )
    testthat::expect_identical(patient_values$patient_id, "1")
    testthat::expect_identical(patient_values$eama_reference, "EAMA-001")

    clinical_status_columns <- DBI::dbListFields(
      conn = connection,
      name = "patient_clinical_status"
    )
    patient_clinical_status <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "patient_id", "rass_score", "vas_score", "cam_icu_result",',
        '"borg_score", "borg_rpe_score", "hand_grip_strength_right",',
        '"hand_grip_strength_left", "mrc_score", "icu_mobility_score"',
        'FROM "patient_clinical_status"',
        sep = " "
      )
    )
    hand_grip_strength_types <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT typeof("hand_grip_strength_right")',
        'AS "hand_grip_strength_right_type",',
        'typeof("hand_grip_strength_left")',
        'AS "hand_grip_strength_left_type"',
        'FROM "patient_clinical_status"',
        sep = " "
      )
    )
    testthat::expect_true("patient_id" %in% clinical_status_columns)
    testthat::expect_false("session_id" %in% clinical_status_columns)
    testthat::expect_false("handgrip_strength" %in% clinical_status_columns)
    testthat::expect_true(all(c(
      "hand_grip_strength_right",
      "hand_grip_strength_left"
    ) %in% clinical_status_columns))
    testthat::expect_identical(patient_clinical_status$patient_id, "1")
    testthat::expect_identical(patient_clinical_status$rass_score, "value_4")
    testthat::expect_identical(patient_clinical_status$vas_score, "value_5")
    testthat::expect_identical(
      patient_clinical_status$cam_icu_result,
      "value_6"
    )
    testthat::expect_identical(patient_clinical_status$borg_score, "value_7")
    testthat::expect_identical(
      patient_clinical_status$borg_rpe_score,
      "value_8"
    )
    testthat::expect_identical(
      patient_clinical_status$hand_grip_strength_right,
      27.5
    )
    testthat::expect_identical(
      patient_clinical_status$hand_grip_strength_left,
      24
    )
    testthat::expect_identical(
      as.character(hand_grip_strength_types[1L, ]),
      c("DOUBLE", "DOUBLE")
    )
    testthat::expect_identical(patient_clinical_status$mrc_score, "value_10")
    testthat::expect_identical(
      patient_clinical_status$icu_mobility_score,
      "value_11"
    )

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
  desc = "the transformer stores admission types",
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
      admission_types = c("MEDICAL", "SCEDULED MEDICAL")
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

    admission_columns <- DBI::dbListFields(
      conn = connection,
      name = "icu_admissions"
    )
    admission_values <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT "patient_id", "admission_type"',
        'FROM "icu_admissions" ORDER BY "patient_id"',
        sep = " "
      )
    )
    admission_type_types <- DBI::dbGetQuery(
      conn = connection,
      statement = stringr::str_c(
        'SELECT typeof("admission_type") AS "admission_type_type"',
        'FROM "icu_admissions" LIMIT 1',
        sep = " "
      )
    )

    testthat::expect_true("admission_type" %in% admission_columns)
    testthat::expect_identical(
      admission_values$patient_id,
      c("1", "2")
    )
    testthat::expect_identical(
      admission_values$admission_type,
      c("MEDICAL", "SCEDULED MEDICAL")
    )
    testthat::expect_identical(
      admission_type_types$admission_type_type,
      "VARCHAR"
    )
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
