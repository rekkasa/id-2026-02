runtime_packages <- c(
  "readxl",
  "DBI",
  "duckdb",
  "dplyr",
  "tidyr",
  "tibble",
  "purrr",
  "stringr",
  "fs",
  "glue",
  "rlang"
)

workbook_layout <- function() {
  list(
    crf = list(
      sheet_name = "crf data",
      column_count = 29L,
      header_row = 1L,
      first_data_row = 2L,
      leaf_headers = c(
        "Α/Α",
        "Ονοματεπώνυμο",
        "Ηλικία",
        "Ύψος",
        "Βάρος",
        "APACHE ΙΙ",
        "Φύλο",
        "ΕΑΜΑ",
        "Ημερομηνία εισαγωγής",
        "Ώρα",
        "Αιτία εισόδου",
        "ΤΥΠΟΣ ΕΙΣΑΓΩΓΗΣ",
        "ΧΑΠ",
        "Περιοριστική πνευμονοπάθεια",
        "Νευρομυική νόσος",
        "Καρδιακή ανεπάρκεια",
        "Σακχαρώδης διαβήτης",
        "παλαιό ΑΕΕ",
        "Γνωστή αγγειακή νόσος (ΣΝ ή PAD)",
        "Χρόνια νεφρική νόσος",
        "Άλλη υποκείμενη νόσος",
        "Μέρα έναρξης μηχανικού αερισμού",
        "Ώρα έναρξης μηχανικού αερισμού",
        "Ένδειξη",
        "Ημέρες παραμονής σε μηχανικό αερισμό",
        "Ημέρες παραμονής στη ΜΕΘ",
        "Ημερομηνία εξόδου από τη ΜΕΘ",
        "Έκβαση ασθενούς ΜΕΘ",
        "Ενδονοσοκομειακή έκβαση"
      ),
      group_headers = NULL,
      internal_names = c(
        "patient_id",
        "age_years",
        "height_cm",
        "weight_kg",
        "apache_ii_score",
        "sex",
        "eama_reference",
        "admission_date",
        "admission_time",
        "admission_reason",
        "admission_type",
        "copd",
        "restrictive_lung_disease",
        "neuromuscular_disease",
        "heart_failure",
        "diabetes",
        "previous_stroke",
        "vascular_disease",
        "chronic_kidney_disease",
        "other_underlying_condition",
        "mechanical_ventilation_start_date",
        "mechanical_ventilation_start_time",
        "mechanical_ventilation_indication",
        "mechanical_ventilation_days",
        "icu_stay_days",
        "icu_discharge_date",
        "icu_outcome",
        "hospital_outcome"
      )
    ),
    physiotherapy = list(
      sheet_name = "ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ",
      column_count = 39L,
      header_row = 2L,
      first_data_row = 3L,
      leaf_headers = c(
        "Α/Α",
        "Ονοματεπώνυμο",
        "Ημερομηνία σύνδεσης",
        "Κλίμακα RASS",
        "Κλίμακα VAS",
        "Cam ICU",
        "Κλίμακα BORG",
        "Borg RPE",
        "HGS hand grip",
        "MRC score",
        "ICU mobility score",
        "Psup",
        "PEEP",
        "Tidal volume",
        "VEtot",
        "FiO 2",
        "Αναπν. συχνότητα",
        "T piece",
        "SatO2",
        "Αρτηριακή πίεση",
        "Σφίξεις",
        "ΝΟΡΑΔΡΕΝΑΛΙΝΗ ml/h απλού δ/τος",
        "PH",
        "PaO 2",
        "PCO 2",
        "Psup",
        "PEEP",
        "Tidal volume",
        "VEtot",
        "FiO2",
        "Αριθμός αναπνοών",
        "T piece",
        "Sat O2",
        "Αρτηριακή Πίεση (συστ/διαστ)",
        "Σφύξεις",
        "ΝΟΡΑΔΡΕΝΑΛΙΝΗ ml/h απλού δ/τος",
        "PH",
        "PaO 2",
        "PCO 2"
      ),
      group_headers = c(
        "12" = "ΠΡΙΝ ΤΗ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ",
        "26" = "ΜΕΤΑ ΤΗ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ"
      ),
      internal_names = c(
        "source_patient_id",
        "connection_date",
        "rass_score",
        "vas_score",
        "cam_icu_result",
        "borg_score",
        "borg_rpe_score",
        "handgrip_strength",
        "mrc_score",
        "icu_mobility_score",
        "pre_pressure_support",
        "pre_positive_end_expiratory_pressure",
        "pre_tidal_volume",
        "pre_total_minute_ventilation",
        "pre_fraction_inspired_oxygen",
        "pre_respiratory_rate",
        "pre_t_piece",
        "pre_oxygen_saturation",
        "pre_blood_pressure",
        "pre_heart_rate",
        "pre_noradrenaline_rate",
        "pre_arterial_ph",
        "pre_arterial_oxygen_pressure",
        "pre_arterial_carbon_dioxide_pressure",
        "post_pressure_support",
        "post_positive_end_expiratory_pressure",
        "post_tidal_volume",
        "post_total_minute_ventilation",
        "post_fraction_inspired_oxygen",
        "post_respiratory_rate",
        "post_t_piece",
        "post_oxygen_saturation",
        "post_blood_pressure",
        "post_heart_rate",
        "post_noradrenaline_rate",
        "post_arterial_ph",
        "post_arterial_oxygen_pressure",
        "post_arterial_carbon_dioxide_pressure"
      )
    ),
    control = list(
      sheet_name = "ΧΩΡΙΣ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ",
      column_count = 37L,
      header_row = 2L,
      first_data_row = 3L,
      leaf_headers = c(
        "Α/Α",
        "Ονοματεπώνυμο",
        "Κλίμακα RASS",
        "Κλίμακα VAS",
        "Cam ICU",
        NA_character_,
        "Psup",
        "PEEP",
        "Tidal volume",
        "VEtot",
        "FiO2",
        "Αριθμός αναπνοών",
        "T piece",
        "Sat O2",
        "Αρτηριακή Πίεση (συστ/διαστ)",
        "Σφύξεις",
        "ΝΟΡΑΔΡΕΝΑΛΙΝΗ ml/h απλού δ/τος",
        "PH",
        "PaO 2",
        "PCO 2",
        "Psup",
        "V’sens / Psens",
        "Esens %",
        "PEEP",
        "Rising time %",
        "Tidal volume",
        "VEtot",
        "FiO2",
        "Αριθμός αναπνοών",
        "T piece",
        "Sat O2",
        "Αρτηριακή Πίεση (συστ/διαστ)",
        "Σφύξεις",
        "ΝΟΡΑΔΡΕΝΑΛΙΝΗ ml/h απλού δ/τος",
        "PH",
        "PaO 2",
        "PCO 2"
      ),
      group_headers = c(
        "7" = "ΑΡΧΗ ΤΗΣ ΚΑΤΑΓΡΑΦΗΣ",
        "21" = "ΤΕΛΟΣ ΤΗΣ ΚΑΤΑΓΡΑΦΗΣ"
      ),
      internal_names = c(
        "source_patient_id",
        "rass_score",
        "vas_score",
        "cam_icu_result",
        "unused_control_field",
        "start_pressure_support",
        "start_positive_end_expiratory_pressure",
        "start_tidal_volume",
        "start_total_minute_ventilation",
        "start_fraction_inspired_oxygen",
        "start_respiratory_rate",
        "start_t_piece",
        "start_oxygen_saturation",
        "start_blood_pressure",
        "start_heart_rate",
        "start_noradrenaline_rate",
        "start_arterial_ph",
        "start_arterial_oxygen_pressure",
        "start_arterial_carbon_dioxide_pressure",
        "end_pressure_support",
        "end_ventilator_sensitivity",
        "end_expiratory_sensitivity",
        "end_positive_end_expiratory_pressure",
        "end_rising_time",
        "end_tidal_volume",
        "end_total_minute_ventilation",
        "end_fraction_inspired_oxygen",
        "end_respiratory_rate",
        "end_t_piece",
        "end_oxygen_saturation",
        "end_blood_pressure",
        "end_heart_rate",
        "end_noradrenaline_rate",
        "end_arterial_ph",
        "end_arterial_oxygen_pressure",
        "end_arterial_carbon_dioxide_pressure"
      )
    ),
    sleep = list(
      sheet_name = "sleep data",
      column_count = 34L,
      header_row = 2L,
      first_data_row = 3L,
      leaf_headers = c(
        "Α/Α",
        "Ονοματεπώνυμο",
        "TIB",
        "TST=S1+S2+SWS+REM",
        "Sleep efficency 1",
        "WASO",
        "Sleep latency",
        "Stage shifts",
        "S1",
        "% of TST",
        "S2",
        "% of TST",
        "SWS",
        "% of TST",
        "REM",
        "% of TST",
        "Arousal index in TST",
        "RCSQ",
        "TIB",
        "TST=S1+S2+SWS+REM",
        "Sleep efficency 1(100 X TST/TIB)",
        "WASO",
        "Sleep latency",
        "Stage shifts",
        "S1 MIN",
        "S1% of TST",
        "S2 MIN",
        "S2% of TST",
        "SWS MIN",
        "SWS% of TST",
        "REM MIN",
        "REM% of TST",
        "Arousal index",
        "RCSQ"
      ),
      group_headers = c(
        "3" = "ΧΩΡΙΣ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ",
        "19" = "ΜΕ ΦΥΣΙΚΟΘΕΡΑΠΕΙΑ"
      ),
      internal_names = c(
        "source_patient_id",
        "without_time_in_bed",
        "without_total_sleep_time",
        "without_sleep_efficiency",
        "without_wake_after_sleep_onset",
        "without_sleep_latency",
        "without_stage_shifts",
        "without_stage1_duration",
        "without_stage1_percent_tst",
        "without_stage2_duration",
        "without_stage2_percent_tst",
        "without_slow_wave_sleep_duration",
        "without_slow_wave_sleep_percent_tst",
        "without_rem_sleep_duration",
        "without_rem_sleep_percent_tst",
        "without_arousal_index",
        "without_rcsq_score",
        "with_time_in_bed",
        "with_total_sleep_time",
        "with_sleep_efficiency",
        "with_wake_after_sleep_onset",
        "with_sleep_latency",
        "with_stage_shifts",
        "with_stage1_duration",
        "with_stage1_percent_tst",
        "with_stage2_duration",
        "with_stage2_percent_tst",
        "with_slow_wave_sleep_duration",
        "with_slow_wave_sleep_percent_tst",
        "with_rem_sleep_duration",
        "with_rem_sleep_percent_tst",
        "with_arousal_index",
        "with_rcsq_score"
      )
    )
  )
}

database_table_names <- c(
  patients = "patients",
  icu_admissions = "icu_admissions",
  patient_comorbidities = "patient_comorbidities",
  study_sessions = "study_sessions",
  patient_clinical_status = "patient_clinical_status",
  physiological_assessments = "physiological_assessments",
  sleep_assessments = "sleep_assessments"
)

time_of_day_column_names <- c(
  "admission_time",
  "mechanical_ventilation_start_time"
)

sleep_duration_column_formats <- c(
  without_time_in_bed = "hours_minutes",
  without_total_sleep_time = "hours_minutes",
  without_wake_after_sleep_onset = "minute_suffix",
  without_sleep_latency = "minute_suffix",
  without_stage1_duration = "minute_suffix",
  without_stage2_duration = "minute_suffix",
  without_slow_wave_sleep_duration = "minute_suffix",
  with_time_in_bed = "hours_minutes",
  with_total_sleep_time = "hours_minutes",
  with_wake_after_sleep_onset = "minute_suffix",
  with_sleep_latency = "minute_suffix",
  with_stage1_duration = "minute_suffix",
  with_stage2_duration = "minute_suffix",
  with_slow_wave_sleep_duration = "minute_suffix"
)

sleep_duration_column_names <- c(
  "time_in_bed",
  "total_sleep_time",
  "wake_after_sleep_onset",
  "sleep_latency",
  "stage1_duration",
  "stage2_duration",
  "slow_wave_sleep_duration"
)

arousal_index_source_column_names <- c(
  "without_arousal_index",
  "with_arousal_index"
)

sleep_percentage_source_column_names <- c(
  "without_sleep_efficiency",
  "without_stage1_percent_tst",
  "without_stage2_percent_tst",
  "with_sleep_efficiency",
  "with_stage1_percent_tst",
  "with_stage2_percent_tst"
)

sleep_numeric_column_names <- c(
  sleep_duration_column_names,
  "sleep_efficiency",
  "stage1_percent_tst",
  "stage2_percent_tst",
  "arousal_index"
)

sleep_zero_total_time_na_column_names <- c(
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

patient_clinical_status_numeric_column_names <- c(
  "hand_grip_strength_right",
  "hand_grip_strength_left"
)

physiological_assessment_numeric_column_names <- c(
  "blood_pressure_diastolic",
  "blood_pressure_systolic"
)

database_column_names <- list(
  patients = c(
    patient_id = "patient_id",
    eama_reference = "eama_reference",
    source_row_number = "source_row_number"
  ),
  icu_admissions = c(
    admission_id = "admission_id",
    patient_id = "patient_id",
    age_years = "age_years",
    height_cm = "height_cm",
    weight_kg = "weight_kg",
    apache_ii_score = "apache_ii_score",
    sex = "sex",
    admission_date = "admission_date",
    admission_time = "admission_time",
    admission_reason = "admission_reason",
    admission_type = "admission_type",
    mechanical_ventilation_start_date = "mechanical_ventilation_start_date",
    mechanical_ventilation_start_time = "mechanical_ventilation_start_time",
    mechanical_ventilation_indication = "mechanical_ventilation_indication",
    mechanical_ventilation_days = "mechanical_ventilation_days",
    icu_stay_days = "icu_stay_days",
    icu_discharge_date = "icu_discharge_date",
    icu_outcome = "icu_outcome",
    hospital_outcome = "hospital_outcome"
  ),
  patient_comorbidities = c(
    patient_id = "patient_id",
    comorbidity_type = "comorbidity_type",
    source_value = "source_value"
  ),
  study_sessions = c(
    session_id = "session_id",
    patient_id = "patient_id",
    admission_id = "admission_id",
    condition = "condition",
    connection_date = "connection_date",
    source_sheet = "source_sheet",
    source_row_number = "source_row_number"
  ),
  patient_clinical_status = c(
    patient_id = "patient_id",
    rass_score = "rass_score",
    vas_score = "vas_score",
    cam_icu_result = "cam_icu_result",
    borg_score = "borg_score",
    borg_rpe_score = "borg_rpe_score",
    hand_grip_strength_right = "hand_grip_strength_right",
    hand_grip_strength_left = "hand_grip_strength_left",
    mrc_score = "mrc_score",
    icu_mobility_score = "icu_mobility_score"
  ),
  physiological_assessments = c(
    assessment_id = "assessment_id",
    patient_id = "patient_id",
    assessment = "assessment",
    phase = "phase",
    pressure_support = "pressure_support",
    positive_end_expiratory_pressure = "positive_end_expiratory_pressure",
    tidal_volume = "tidal_volume",
    total_minute_ventilation = "total_minute_ventilation",
    fraction_inspired_oxygen = "fraction_inspired_oxygen",
    respiratory_rate = "respiratory_rate",
    t_piece = "t_piece",
    oxygen_saturation = "oxygen_saturation",
    blood_pressure_diastolic = "blood_pressure_diastolic",
    blood_pressure_systolic = "blood_pressure_systolic",
    heart_rate = "heart_rate",
    noradrenaline_rate = "noradrenaline_rate",
    arterial_ph = "arterial_ph",
    arterial_oxygen_pressure = "arterial_oxygen_pressure",
    arterial_carbon_dioxide_pressure = "arterial_carbon_dioxide_pressure",
    ventilator_sensitivity = "ventilator_sensitivity",
    expiratory_sensitivity = "expiratory_sensitivity",
    rising_time = "rising_time",
    source_row_number = "source_row_number"
  ),
  sleep_assessments = c(
    assessment_id = "assessment_id",
    patient_id = "patient_id",
    assessment = "assessment",
    time_in_bed = "time_in_bed",
    total_sleep_time = "total_sleep_time",
    sleep_efficiency = "sleep_efficiency",
    wake_after_sleep_onset = "wake_after_sleep_onset",
    sleep_latency = "sleep_latency",
    stage_shifts = "stage_shifts",
    stage1_duration = "stage1_duration",
    stage1_percent_tst = "stage1_percent_tst",
    stage2_duration = "stage2_duration",
    stage2_percent_tst = "stage2_percent_tst",
    slow_wave_sleep_duration = "slow_wave_sleep_duration",
    slow_wave_sleep_percent_tst = "slow_wave_sleep_percent_tst",
    rem_sleep_duration = "rem_sleep_duration",
    rem_sleep_percent_tst = "rem_sleep_percent_tst",
    arousal_index = "arousal_index",
    rcsq_score = "rcsq_score",
    source_row_number = "source_row_number"
  )
)

check_runtime_packages <- function() {
  missing_packages <- runtime_packages[!rlang::is_installed(runtime_packages)]

  if (length(missing_packages) > 0L) {
    rlang::abort(
      message = glue::glue(
        "Install the required R packages before running this script: ",
        "{stringr::str_c(missing_packages, collapse = ', ')}."
      )
    )
  }
}

normalize_text <- function(values) {
  values |>
    stringr::str_replace_all(
      pattern = "\\s+",
      replacement = " "
    ) |>
    stringr::str_trim()
}

is_missing_text <- function(values) {
  is.na(values) | normalize_text(values) == ""
}

read_header_values <- function(
    input_path,
    sheet_name,
    row_number,
    column_count) {
  header_range <- readxl::cell_limits(
    ul = c(row_number, 1L),
    lr = c(row_number, column_count)
  )

  header <- readxl::read_excel(
    path = input_path,
    sheet = sheet_name,
    range = header_range,
    col_names = FALSE,
    col_types = rep(x = "text", times = column_count),
    .name_repair = "minimal"
  )

  as.character(unlist(header, use.names = FALSE))
}

assert_matching_headers <- function(expected, actual, sheet_name, row_number) {
  expected_normalized <- normalize_text(expected)
  actual_normalized <- normalize_text(actual)

  matches <- (is.na(expected_normalized) & is.na(actual_normalized)) |
    (!is.na(expected_normalized) & !is.na(actual_normalized) &
      expected_normalized == actual_normalized)
  matches[is.na(matches)] <- FALSE
  mismatches <- which(!matches)

  if (length(mismatches) > 0L) {
    rlang::abort(
      message = glue::glue(
        "Worksheet '{sheet_name}' does not match the expected template at ",
        "header row {row_number}, column positions: ",
        "{stringr::str_c(mismatches, collapse = ', ')}."
      )
    )
  }
}

validate_workbook_layout <- function(input_path) {
  layout <- workbook_layout()
  worksheet_names <- readxl::excel_sheets(path = input_path)
  required_sheets <- purrr::map_chr(
    .x = layout,
    .f = function(sheet_layout) sheet_layout$sheet_name
  )
  missing_sheets <- setdiff(required_sheets, worksheet_names)

  if (length(missing_sheets) > 0L) {
    rlang::abort(
      message = glue::glue(
        "The workbook is missing required worksheets: ",
        "{stringr::str_c(missing_sheets, collapse = ', ')}."
      )
    )
  }

  purrr::walk(
    .x = layout,
    .f = function(sheet_layout) {
      leaf_headers <- read_header_values(
        input_path = input_path,
        sheet_name = sheet_layout$sheet_name,
        row_number = sheet_layout$header_row,
        column_count = sheet_layout$column_count
      )
      assert_matching_headers(
        expected = sheet_layout$leaf_headers,
        actual = leaf_headers,
        sheet_name = sheet_layout$sheet_name,
        row_number = sheet_layout$header_row
      )

      if (!is.null(sheet_layout$group_headers)) {
        group_headers <- read_header_values(
          input_path = input_path,
          sheet_name = sheet_layout$sheet_name,
          row_number = sheet_layout$header_row - 1L,
          column_count = sheet_layout$column_count
        )

        purrr::walk2(
          .x = names(sheet_layout$group_headers),
          .y = unname(sheet_layout$group_headers),
          .f = function(column_position, expected_header) {
            actual_header <- group_headers[[as.integer(column_position)]]
            assert_matching_headers(
              expected = expected_header,
              actual = actual_header,
              sheet_name = sheet_layout$sheet_name,
              row_number = sheet_layout$header_row - 1L
            )
          }
        )
      }
    }
  )

  layout
}

parse_dmy_text <- function(values) {
  normalized_values <- normalize_text(values)
  date_components <- stringr::str_match(
    string = normalized_values,
    pattern = "^([0-9]{1,2})/([0-9]{1,2})/([0-9]{2}|[0-9]{4})$"
  )
  matches_format <- !is.na(date_components[, 1L])
  day_values <- date_components[, 2L]
  month_values <- date_components[, 3L]
  year_values <- date_components[, 4L]
  short_years <- matches_format & nchar(year_values) == 2L
  # Short years in these clinical event dates represent 2000 through 2099.
  year_values[short_years] <- stringr::str_c("20", year_values[short_years])
  canonical_values <- stringr::str_c(
    stringr::str_pad(
      string = day_values,
      width = 2L,
      side = "left",
      pad = "0"
    ),
    "/",
    stringr::str_pad(
      string = month_values,
      width = 2L,
      side = "left",
      pad = "0"
    ),
    "/",
    year_values
  )
  parsed_values <- as.Date(
    x = rep(x = NA_character_, times = length(values)),
    format = "%d/%m/%Y"
  )
  parsed_values[matches_format] <- as.Date(
    x = canonical_values[matches_format],
    format = "%d/%m/%Y"
  )

  list(
    dates = parsed_values,
    matches_format = matches_format
  )
}

parse_ddmmyyyy <- function(
    values,
    column_name,
    source_sheet,
    source_row_numbers) {
  missing_values <- is_missing_text(values)
  parsed_dmy <- parse_dmy_text(values = values)
  invalid_values <- !missing_values & (
    !parsed_dmy$matches_format | is.na(parsed_dmy$dates)
  )

  if (any(invalid_values)) {
    invalid_rows <- source_row_numbers[invalid_values]
    rlang::abort(
      message = glue::glue(
        "Worksheet '{source_sheet}' contains invalid d/m/yyyy values with a ",
        "2- or 4-digit year in ",
        "'{column_name}' at Excel row(s): ",
        "{stringr::str_c(invalid_rows, collapse = ', ')}."
      )
    )
  }

  parsed_dmy$dates
}

is_missing_source_value <- function(value) {
  if (length(value) == 0L || all(is.na(value))) {
    return(TRUE)
  }

  if (is.character(value)) {
    return(all(is_missing_text(value)))
  }

  FALSE
}

is_missing_source_values <- function(values) {
  if (is.list(values)) {
    return(purrr::map_lgl(
      .x = values,
      .f = is_missing_source_value
    ))
  }

  is_missing_text(values)
}

parse_excel_serial <- function(serial, raw_kind) {
  if (!is.finite(serial) || serial < 20000 || serial > 80000) {
    return(list(
      date = as.Date(x = NA),
      raw_kind = stringr::str_c(raw_kind, "_out_of_range"),
      valid = FALSE
    ))
  }

  list(
    date = as.Date(
      x = floor(serial),
      origin = "1899-12-30"
    ),
    raw_kind = raw_kind,
    valid = TRUE
  )
}

parse_date_cell <- function(value) {
  if (is_missing_source_value(value)) {
    return(list(
      date = as.Date(x = NA),
      raw_kind = "blank",
      valid = TRUE
    ))
  }

  if (inherits(value, what = "Date")) {
    return(list(
      date = as.Date(value),
      raw_kind = "excel_date",
      valid = TRUE
    ))
  }

  if (inherits(value, what = "POSIXt")) {
    return(list(
      date = as.Date(value),
      raw_kind = "excel_datetime",
      valid = TRUE
    ))
  }

  if (is.numeric(value)) {
    return(parse_excel_serial(
      serial = value,
      raw_kind = "numeric_serial"
    ))
  }

  if (!is.character(value)) {
    return(list(
      date = as.Date(x = NA),
      raw_kind = "unsupported_type",
      valid = FALSE
    ))
  }

  normalized_value <- normalize_text(value)
  if (stringr::str_detect(
    string = normalized_value,
    pattern = "^[0-9]+(?:\\.[0-9]+)?$"
  )) {
    return(parse_excel_serial(
      serial = as.numeric(normalized_value),
      raw_kind = "text_serial"
    ))
  }

  parsed_dmy <- parse_dmy_text(values = normalized_value)
  valid_date <- parsed_dmy$matches_format && !is.na(parsed_dmy$dates)
  list(
    date = parsed_dmy$dates,
    raw_kind = if (valid_date) "text_dmy" else "unsupported_text",
    valid = valid_date
  )
}

parse_date_column <- function(
    values,
    column_name,
    source_sheet,
    source_row_numbers) {
  parsed_cells <- purrr::map(
    .x = values,
    .f = parse_date_cell
  )
  valid_values <- purrr::map_lgl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$valid
  )

  if (any(!valid_values)) {
    invalid_rows <- source_row_numbers[!valid_values]
    invalid_kinds <- purrr::map_chr(
      .x = parsed_cells[!valid_values],
      .f = function(parsed_cell) parsed_cell$raw_kind
    ) |>
      tibble::tibble(raw_kind = _) |>
      dplyr::count(.data$raw_kind, name = "row_count") |>
      dplyr::transmute(
        summary = stringr::str_c(.data$raw_kind, "=", .data$row_count)
      ) |>
      dplyr::pull(var = "summary") |>
      stringr::str_c(collapse = ", ")
    rlang::abort(
      message = glue::glue(
        "Worksheet '{source_sheet}' contains invalid date values in ",
        "'{column_name}' at Excel row(s): ",
        "{stringr::str_c(invalid_rows, collapse = ', ')} ",
        "(types: {invalid_kinds})."
      )
    )
  }

  as.Date(
    x = purrr::map_dbl(
      .x = parsed_cells,
      .f = function(parsed_cell) as.numeric(parsed_cell$date)
    ),
    origin = "1970-01-01"
  )
}

format_clock_time <- function(hours, minutes, seconds) {
  stringr::str_c(
    stringr::str_pad(
      string = hours,
      width = 2L,
      side = "left",
      pad = "0"
    ),
    ":",
    stringr::str_pad(
      string = minutes,
      width = 2L,
      side = "left",
      pad = "0"
    ),
    ":",
    stringr::str_pad(
      string = seconds,
      width = 2L,
      side = "left",
      pad = "0"
    )
  )
}

parse_time_seconds <- function(seconds, raw_kind) {
  rounded_seconds <- round(seconds)
  if (!is.finite(seconds) || seconds < 0 || rounded_seconds >= 86400) {
    return(list(
      time = NA_character_,
      raw_kind = stringr::str_c(raw_kind, "_out_of_range"),
      valid = FALSE
    ))
  }

  hours <- rounded_seconds %/% 3600
  remaining_seconds <- rounded_seconds %% 3600
  minutes <- remaining_seconds %/% 60
  seconds <- remaining_seconds %% 60
  list(
    time = format_clock_time(
      hours = hours,
      minutes = minutes,
      seconds = seconds
    ),
    raw_kind = raw_kind,
    valid = TRUE
  )
}

parse_excel_time_serial <- function(serial, raw_kind) {
  if (!is.finite(serial) || serial < 0 || serial >= 1) {
    return(list(
      time = NA_character_,
      raw_kind = stringr::str_c(raw_kind, "_out_of_range"),
      valid = FALSE
    ))
  }

  parse_time_seconds(
    seconds = serial * 86400,
    raw_kind = raw_kind
  )
}

parse_time_cell <- function(value) {
  if (is_missing_source_value(value)) {
    return(list(
      time = NA_character_,
      raw_kind = "blank",
      valid = TRUE
    ))
  }

  if (inherits(value, what = "POSIXt")) {
    return(list(
      time = format(value, format = "%H:%M:%S", tz = "UTC"),
      raw_kind = "excel_datetime",
      valid = TRUE
    ))
  }

  if (inherits(value, what = "difftime")) {
    return(parse_time_seconds(
      seconds = as.numeric(value, units = "secs"),
      raw_kind = "duration_seconds"
    ))
  }

  if (is.numeric(value)) {
    if (value < 1) {
      return(parse_excel_time_serial(
        serial = value,
        raw_kind = "numeric_serial"
      ))
    }

    return(parse_time_seconds(
      seconds = value,
      raw_kind = "numeric_seconds"
    ))
  }

  if (!is.character(value)) {
    return(list(
      time = NA_character_,
      raw_kind = "unsupported_type",
      valid = FALSE
    ))
  }

  normalized_value <- normalize_text(value)
  if (stringr::str_detect(
    string = normalized_value,
    pattern = "^[0-9]+(?:\\.[0-9]+)?$"
  )) {
    numeric_value <- as.numeric(normalized_value)
    if (numeric_value < 1) {
      return(parse_excel_time_serial(
        serial = numeric_value,
        raw_kind = "text_serial"
      ))
    }

    return(parse_time_seconds(
      seconds = numeric_value,
      raw_kind = "text_seconds"
    ))
  }

  twenty_four_hour_components <- stringr::str_match(
    string = normalized_value,
    pattern = "^([01][0-9]|2[0-3]):([0-5][0-9])(?::([0-5][0-9]))?$"
  )
  matches_twenty_four_hour <- !is.na(twenty_four_hour_components[, 1L])
  if (matches_twenty_four_hour) {
    time <- format_clock_time(
      hours = as.integer(twenty_four_hour_components[, 2L]),
      minutes = as.integer(twenty_four_hour_components[, 3L]),
      seconds = if (is.na(twenty_four_hour_components[, 4L])) {
        0L
      } else {
        as.integer(twenty_four_hour_components[, 4L])
      }
    )
    return(list(
      time = time,
      raw_kind = "text_hhmmss",
      valid = TRUE
    ))
  }

  twelve_hour_components <- stringr::str_match(
    string = normalized_value,
    pattern = "^([1-9]|1[0-2]):([0-5][0-9])(?::([0-5][0-9]))? ([AaPp][Mm])$"
  )
  matches_twelve_hour <- !is.na(twelve_hour_components[, 1L])
  if (matches_twelve_hour) {
    hours <- as.integer(twelve_hour_components[, 2L])
    meridiem <- stringr::str_to_upper(twelve_hour_components[, 5L])
    if (meridiem == "AM" && hours == 12L) {
      hours <- 0L
    } else if (meridiem == "PM" && hours < 12L) {
      hours <- hours + 12L
    }

    return(list(
      time = format_clock_time(
        hours = hours,
        minutes = as.integer(twelve_hour_components[, 3L]),
        seconds = if (is.na(twelve_hour_components[, 4L])) {
          0L
        } else {
          as.integer(twelve_hour_components[, 4L])
        }
      ),
      raw_kind = "text_ampm",
      valid = TRUE
    ))
  }

  list(
    time = NA_character_,
    raw_kind = "unsupported_text",
    valid = FALSE
  )
}

parse_time_column <- function(
    values,
    column_name,
    source_sheet,
    source_row_numbers) {
  parsed_cells <- purrr::map(
    .x = values,
    .f = parse_time_cell
  )
  valid_values <- purrr::map_lgl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$valid
  )

  if (any(!valid_values)) {
    invalid_rows <- source_row_numbers[!valid_values]
    invalid_kinds <- purrr::map_chr(
      .x = parsed_cells[!valid_values],
      .f = function(parsed_cell) parsed_cell$raw_kind
    ) |>
      tibble::tibble(raw_kind = _) |>
      dplyr::count(.data$raw_kind, name = "row_count") |>
      dplyr::transmute(
        summary = stringr::str_c(.data$raw_kind, "=", .data$row_count)
      ) |>
      dplyr::pull(var = "summary") |>
      stringr::str_c(collapse = ", ")
    rlang::abort(
      message = glue::glue(
        "Worksheet '{source_sheet}' contains invalid 24-hour HH:MM values ",
        "in '{column_name}' at Excel row(s): ",
        "{stringr::str_c(invalid_rows, collapse = ', ')} ",
        "(types: {invalid_kinds})."
      )
    )
  }

  purrr::map_chr(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$time
  )
}

parse_sleep_duration_cell <- function(value, input_format) {
  if (is_missing_source_value(value)) {
    return(list(
      minutes = NA_real_,
      raw_kind = "blank",
      valid = TRUE
    ))
  }

  if (!is.character(value)) {
    return(list(
      minutes = NA_real_,
      raw_kind = "unsupported_type",
      valid = FALSE
    ))
  }

  normalized_value <- normalize_text(values = value)
  if (input_format == "hours_minutes") {
    components <- stringr::str_match(
      string = normalized_value,
      pattern = "^([0-9]+)h ([0-9]+)m$"
    )
    if (is.na(components[, 1L])) {
      return(list(
        minutes = NA_real_,
        raw_kind = "unsupported_text",
        valid = FALSE
      ))
    }

    hours <- as.numeric(components[, 2L])
    minutes <- as.numeric(components[, 3L])
    if (minutes > 59) {
      return(list(
        minutes = NA_real_,
        raw_kind = "minutes_out_of_range",
        valid = FALSE
      ))
    }

    return(list(
      minutes = hours * 60 + minutes,
      raw_kind = "text_hours_minutes",
      valid = TRUE
    ))
  }

  if (input_format == "minute_suffix") {
    components <- stringr::str_match(
      string = normalized_value,
      pattern = "^([0-9]+(?:\\.[0-9]+)?)m$"
    )
    if (is.na(components[, 1L])) {
      return(list(
        minutes = NA_real_,
        raw_kind = "unsupported_text",
        valid = FALSE
      ))
    }

    return(list(
      minutes = as.numeric(components[, 2L]),
      raw_kind = "text_minutes",
      valid = TRUE
    ))
  }

  rlang::abort(
    message = glue::glue("Unsupported sleep duration format: {input_format}.")
  )
}

parse_sleep_duration_column <- function(
    values,
    input_format,
    column_name,
    source_sheet,
    source_row_numbers) {
  parsed_cells <- purrr::map(
    .x = values,
    .f = function(value) {
      parse_sleep_duration_cell(
        value = value,
        input_format = input_format
      )
    }
  )
  valid_values <- purrr::map_lgl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$valid
  )

  if (any(!valid_values)) {
    invalid_rows <- source_row_numbers[!valid_values]
    invalid_kinds <- purrr::map_chr(
      .x = parsed_cells[!valid_values],
      .f = function(parsed_cell) parsed_cell$raw_kind
    ) |>
      tibble::tibble(raw_kind = _) |>
      dplyr::count(.data$raw_kind, name = "row_count") |>
      dplyr::transmute(
        summary = stringr::str_c(.data$raw_kind, "=", .data$row_count)
      ) |>
      dplyr::pull(var = "summary") |>
      stringr::str_c(collapse = ", ")
    rlang::abort(
      message = glue::glue(
        "Worksheet '{source_sheet}' contains invalid sleep duration values ",
        "in '{column_name}' at Excel row(s): ",
        "{stringr::str_c(invalid_rows, collapse = ', ')} ",
        "(types: {invalid_kinds})."
      )
    )
  }

  purrr::map_dbl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$minutes
  )
}

parse_sleep_percentage_cell <- function(value) {
  if (is_missing_source_value(value)) {
    return(list(
      value = NA_real_,
      raw_kind = "blank",
      valid = TRUE
    ))
  }

  if (!is.character(value)) {
    return(list(
      value = NA_real_,
      raw_kind = "unsupported_type",
      valid = FALSE
    ))
  }

  components <- stringr::str_match(
    string = normalize_text(values = value),
    pattern = "^([0-9]+(?:[.,][0-9]+)?)(?:\\s*(%))?$"
  )
  if (is.na(components[, 1L])) {
    return(list(
      value = NA_real_,
      raw_kind = "unsupported_text",
      valid = FALSE
    ))
  }

  percentage_value <- as.numeric(stringr::str_replace(
    string = components[, 2L],
    pattern = ",",
    replacement = "."
  ))
  has_percent_suffix <- !is.na(components[, 3L])
  if (!has_percent_suffix && percentage_value <= 1) {
    percentage_value <- percentage_value * 100
  }

  list(
    value = percentage_value,
    raw_kind = if (has_percent_suffix) {
      "text_percentage"
    } else {
      "text_percentage_number"
    },
    valid = TRUE
  )
}

parse_sleep_percentage_column <- function(
    values,
    column_name,
    source_sheet,
    source_row_numbers) {
  parsed_cells <- purrr::map(
    .x = values,
    .f = parse_sleep_percentage_cell
  )
  valid_values <- purrr::map_lgl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$valid
  )

  if (any(!valid_values)) {
    invalid_rows <- source_row_numbers[!valid_values]
    invalid_kinds <- purrr::map_chr(
      .x = parsed_cells[!valid_values],
      .f = function(parsed_cell) parsed_cell$raw_kind
    ) |>
      tibble::tibble(raw_kind = _) |>
      dplyr::count(.data$raw_kind, name = "row_count") |>
      dplyr::transmute(
        summary = stringr::str_c(.data$raw_kind, "=", .data$row_count)
      ) |>
      dplyr::pull(var = "summary") |>
      stringr::str_c(collapse = ", ")
    rlang::abort(
      message = glue::glue(
        "Worksheet '{source_sheet}' contains invalid sleep percentage ",
        "values in '{column_name}' at Excel row(s): ",
        "{stringr::str_c(invalid_rows, collapse = ', ')} ",
        "(types: {invalid_kinds})."
      )
    )
  }

  purrr::map_dbl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$value
  )
}

parse_sleep_percentage_columns <- function(source_rows, source_sheet) {
  percentage_columns <- intersect(
    x = sleep_percentage_source_column_names,
    y = names(source_rows)
  )

  for (column_name in percentage_columns) {
    source_rows[[column_name]] <- parse_sleep_percentage_column(
      values = source_rows[[column_name]],
      column_name = column_name,
      source_sheet = source_sheet,
      source_row_numbers = source_rows$source_row_number
    )
  }

  source_rows
}

parse_arousal_index_cell <- function(value) {
  if (is_missing_source_value(value)) {
    return(list(
      value = NA_real_,
      raw_kind = "blank",
      valid = TRUE
    ))
  }

  if (!is.character(value)) {
    return(list(
      value = NA_real_,
      raw_kind = "unsupported_type",
      valid = FALSE
    ))
  }

  components <- stringr::str_match(
    string = normalize_text(values = value),
    pattern = "^([0-9]+(?:\\.[0-9]+)?)(?:\\s*/\\s*[Hh])?$"
  )
  if (is.na(components[, 1L])) {
    return(list(
      value = NA_real_,
      raw_kind = "unsupported_text",
      valid = FALSE
    ))
  }

  list(
    value = as.numeric(components[, 2L]),
    raw_kind = "text_arousal_index",
    valid = TRUE
  )
}

parse_arousal_index_column <- function(
    values,
    column_name,
    source_sheet,
    source_row_numbers) {
  parsed_cells <- purrr::map(
    .x = values,
    .f = parse_arousal_index_cell
  )
  valid_values <- purrr::map_lgl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$valid
  )

  if (any(!valid_values)) {
    invalid_rows <- source_row_numbers[!valid_values]
    invalid_kinds <- purrr::map_chr(
      .x = parsed_cells[!valid_values],
      .f = function(parsed_cell) parsed_cell$raw_kind
    ) |>
      tibble::tibble(raw_kind = _) |>
      dplyr::count(.data$raw_kind, name = "row_count") |>
      dplyr::transmute(
        summary = stringr::str_c(.data$raw_kind, "=", .data$row_count)
      ) |>
      dplyr::pull(var = "summary") |>
      stringr::str_c(collapse = ", ")
    rlang::abort(
      message = glue::glue(
        "Worksheet '{source_sheet}' contains invalid arousal index values in ",
        "'{column_name}' at Excel row(s): ",
        "{stringr::str_c(invalid_rows, collapse = ', ')} ",
        "(types: {invalid_kinds})."
      )
    )
  }

  purrr::map_dbl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$value
  )
}

parse_hand_grip_strength_cell <- function(value) {
  if (is_missing_source_value(value)) {
    return(list(
      right = NA_real_,
      left = NA_real_,
      raw_kind = "blank",
      valid = TRUE
    ))
  }

  if (!is.character(value)) {
    return(list(
      right = NA_real_,
      left = NA_real_,
      raw_kind = "unsupported_type",
      valid = FALSE
    ))
  }

  components <- stringr::str_match(
    string = normalize_text(values = value),
    pattern = stringr::str_c(
      "^([0-9]+(?:[.,][0-9]+)?)\\s*\\([Rr]\\)\\s*",
      "([0-9]+(?:[.,][0-9]+)?)\\s*\\([Ll]\\)$"
    )
  )
  if (is.na(components[, 1L])) {
    return(list(
      right = NA_real_,
      left = NA_real_,
      raw_kind = "unsupported_text",
      valid = FALSE
    ))
  }

  list(
    right = as.numeric(stringr::str_replace(
      string = components[, 2L],
      pattern = ",",
      replacement = "."
    )),
    left = as.numeric(stringr::str_replace(
      string = components[, 3L],
      pattern = ",",
      replacement = "."
    )),
    raw_kind = "text_hand_grip_strength",
    valid = TRUE
  )
}

parse_hand_grip_strength_columns <- function(source_rows, source_sheet) {
  source_column <- "handgrip_strength"
  if (!source_column %in% names(source_rows)) {
    return(source_rows)
  }

  parsed_cells <- purrr::map(
    .x = source_rows[[source_column]],
    .f = parse_hand_grip_strength_cell
  )
  valid_values <- purrr::map_lgl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$valid
  )

  if (any(!valid_values)) {
    invalid_rows <- source_rows$source_row_number[!valid_values]
    invalid_kinds <- purrr::map_chr(
      .x = parsed_cells[!valid_values],
      .f = function(parsed_cell) parsed_cell$raw_kind
    ) |>
      tibble::tibble(raw_kind = _) |>
      dplyr::count(.data$raw_kind, name = "row_count") |>
      dplyr::transmute(
        summary = stringr::str_c(.data$raw_kind, "=", .data$row_count)
      ) |>
      dplyr::pull(var = "summary") |>
      stringr::str_c(collapse = ", ")
    rlang::abort(
      message = glue::glue(
        "Worksheet '{source_sheet}' contains invalid hand grip strength ",
        "values in '{source_column}' at Excel row(s): ",
        "{stringr::str_c(invalid_rows, collapse = ', ')} ",
        "(types: {invalid_kinds})."
      )
    )
  }

  source_rows$hand_grip_strength_right <- purrr::map_dbl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$right
  )
  source_rows$hand_grip_strength_left <- purrr::map_dbl(
    .x = parsed_cells,
    .f = function(parsed_cell) parsed_cell$left
  )
  source_rows[[source_column]] <- NULL
  source_rows
}

parse_blood_pressure_cell <- function(value) {
  if (is_missing_source_value(value)) {
    return(list(
      diastolic = NA_real_,
      systolic = NA_real_,
      raw_kind = "blank",
      valid = TRUE
    ))
  }

  if (!is.character(value)) {
    return(list(
      diastolic = NA_real_,
      systolic = NA_real_,
      raw_kind = "unsupported_type",
      valid = FALSE
    ))
  }

  components <- stringr::str_match(
    string = normalize_text(values = value),
    pattern = "^([0-9]+(?:[.,][0-9]+)?)\\s*/\\s*([0-9]+(?:[.,][0-9]+)?)$"
  )
  if (is.na(components[, 1L])) {
    return(list(
      diastolic = NA_real_,
      systolic = NA_real_,
      raw_kind = "unsupported_text",
      valid = FALSE
    ))
  }

  list(
    diastolic = as.numeric(stringr::str_replace(
      string = components[, 2L],
      pattern = ",",
      replacement = "."
    )),
    systolic = as.numeric(stringr::str_replace(
      string = components[, 3L],
      pattern = ",",
      replacement = "."
    )),
    raw_kind = "text_blood_pressure",
    valid = TRUE
  )
}

parse_blood_pressure_columns <- function(source_rows, source_sheet) {
  blood_pressure_columns <- names(source_rows)[stringr::str_detect(
    string = names(source_rows),
    pattern = "_blood_pressure$"
  )]

  for (column_name in blood_pressure_columns) {
    parsed_cells <- purrr::map(
      .x = source_rows[[column_name]],
      .f = parse_blood_pressure_cell
    )
    valid_values <- purrr::map_lgl(
      .x = parsed_cells,
      .f = function(parsed_cell) parsed_cell$valid
    )

    if (any(!valid_values)) {
      invalid_rows <- source_rows$source_row_number[!valid_values]
      invalid_kinds <- purrr::map_chr(
        .x = parsed_cells[!valid_values],
        .f = function(parsed_cell) parsed_cell$raw_kind
      ) |>
        tibble::tibble(raw_kind = _) |>
        dplyr::count(.data$raw_kind, name = "row_count") |>
        dplyr::transmute(
          summary = stringr::str_c(.data$raw_kind, "=", .data$row_count)
        ) |>
        dplyr::pull(var = "summary") |>
        stringr::str_c(collapse = ", ")
      rlang::abort(
        message = glue::glue(
          "Worksheet '{source_sheet}' contains invalid blood pressure values ",
          "in '{column_name}' at Excel row(s): ",
          "{stringr::str_c(invalid_rows, collapse = ', ')} ",
          "(types: {invalid_kinds})."
        )
      )
    }

    column_prefix <- stringr::str_remove(
      string = column_name,
      pattern = "_blood_pressure$"
    )
    source_rows[[stringr::str_c(
      column_prefix,
      "_blood_pressure_diastolic"
    )]] <- purrr::map_dbl(
      .x = parsed_cells,
      .f = function(parsed_cell) parsed_cell$diastolic
    )
    source_rows[[stringr::str_c(
      column_prefix,
      "_blood_pressure_systolic"
    )]] <- purrr::map_dbl(
      .x = parsed_cells,
      .f = function(parsed_cell) parsed_cell$systolic
    )
    source_rows[[column_name]] <- NULL
  }

  source_rows
}

parse_date_columns <- function(source_rows, source_sheet) {
  date_columns <- names(source_rows)[stringr::str_detect(
    string = names(source_rows),
    pattern = "date"
  )]

  for (column_name in date_columns) {
    source_rows[[column_name]] <- parse_date_column(
      values = source_rows[[column_name]],
      column_name = column_name,
      source_sheet = source_sheet,
      source_row_numbers = source_rows$source_row_number
    )
  }

  source_rows
}

parse_time_columns <- function(source_rows, source_sheet) {
  time_columns <- intersect(
    x = time_of_day_column_names,
    y = names(source_rows)
  )

  for (column_name in time_columns) {
    source_rows[[column_name]] <- parse_time_column(
      values = source_rows[[column_name]],
      column_name = column_name,
      source_sheet = source_sheet,
      source_row_numbers = source_rows$source_row_number
    )
  }

  source_rows
}

parse_sleep_duration_columns <- function(source_rows, source_sheet) {
  duration_columns <- intersect(
    x = names(sleep_duration_column_formats),
    y = names(source_rows)
  )

  for (column_name in duration_columns) {
    source_rows[[column_name]] <- parse_sleep_duration_column(
      values = source_rows[[column_name]],
      input_format = sleep_duration_column_formats[[column_name]],
      column_name = column_name,
      source_sheet = source_sheet,
      source_row_numbers = source_rows$source_row_number
    )
  }

  source_rows
}

parse_arousal_index_columns <- function(source_rows, source_sheet) {
  arousal_index_columns <- intersect(
    x = arousal_index_source_column_names,
    y = names(source_rows)
  )

  for (column_name in arousal_index_columns) {
    source_rows[[column_name]] <- parse_arousal_index_column(
      values = source_rows[[column_name]],
      column_name = column_name,
      source_sheet = source_sheet,
      source_row_numbers = source_rows$source_row_number
    )
  }

  source_rows
}

read_source_rows <- function(input_path, sheet_layout) {
  data_range <- readxl::cell_limits(
    ul = c(sheet_layout$first_data_row, 1L),
    lr = c(NA_integer_, sheet_layout$column_count)
  )
  column_types <- rep(x = "text", times = sheet_layout$column_count)
  column_types[[2L]] <- "skip"
  source_column_positions <- setdiff(
    x = seq_len(sheet_layout$column_count),
    y = 2L
  )
  typed_column_positions <- source_column_positions[
    stringr::str_detect(
      string = sheet_layout$internal_names,
      pattern = "date"
    ) | sheet_layout$internal_names %in% time_of_day_column_names
  ]
  column_types[typed_column_positions] <- "list"

  source_rows <- readxl::read_excel(
    path = input_path,
    sheet = sheet_layout$sheet_name,
    range = data_range,
    col_names = FALSE,
    col_types = column_types,
    .name_repair = "minimal"
  )

  if (nrow(source_rows) == 0L) {
    empty_columns <- purrr::map(
      .x = sheet_layout$internal_names,
      .f = function(column_name) character()
    )
    names(empty_columns) <- sheet_layout$internal_names
    source_rows <- tibble::as_tibble(empty_columns)
  } else if (ncol(source_rows) != length(sheet_layout$internal_names)) {
    rlang::abort(
      message = glue::glue(
        "Worksheet '{sheet_layout$sheet_name}' returned an unexpected ",
        "number of non-name columns."
      )
    )
  }

  names(source_rows) <- sheet_layout$internal_names

  source_rows <- source_rows |>
    dplyr::mutate(
      source_row_number = as.integer(dplyr::row_number()) +
        sheet_layout$first_data_row - 1L
    ) |>
    dplyr::filter(
      dplyr::if_any(
        .cols = dplyr::all_of(sheet_layout$internal_names),
        .fns = function(values) !is_missing_source_values(values)
      )
    ) |>
    dplyr::mutate(
      source_sheet = sheet_layout$sheet_name
    )

  source_rows <- parse_date_columns(
    source_rows = source_rows,
    source_sheet = sheet_layout$sheet_name
  )
  source_rows <- parse_time_columns(
    source_rows = source_rows,
    source_sheet = sheet_layout$sheet_name
  )
  source_rows <- parse_sleep_duration_columns(
    source_rows = source_rows,
    source_sheet = sheet_layout$sheet_name
  )
  source_rows <- parse_sleep_percentage_columns(
    source_rows = source_rows,
    source_sheet = sheet_layout$sheet_name
  )
  source_rows <- parse_arousal_index_columns(
    source_rows = source_rows,
    source_sheet = sheet_layout$sheet_name
  )
  source_rows <- parse_hand_grip_strength_columns(
    source_rows = source_rows,
    source_sheet = sheet_layout$sheet_name
  )
  parse_blood_pressure_columns(
    source_rows = source_rows,
    source_sheet = sheet_layout$sheet_name
  )
}

write_diagnostic_log <- function(
    diagnostic_log_path,
    event,
    rows,
    missing_column) {
  if (is.null(diagnostic_log_path) || length(diagnostic_log_path) != 1L ||
      is_missing_text(diagnostic_log_path)) {
    return(invisible(NULL))
  }

  diagnostic_rows <- rows |>
    dplyr::transmute(
      event = event,
      sheet = .data$source_sheet,
      excel_row = .data$source_row_number,
      missing_column = missing_column
    )

  tryCatch(
    expr = {
      log_directory <- fs::path_dir(path = diagnostic_log_path)
      if (!fs::dir_exists(path = log_directory)) {
        fs::dir_create(path = log_directory, recurse = TRUE)
      }

      log_exists <- fs::file_exists(path = diagnostic_log_path)
      utils::write.table(
        x = diagnostic_rows,
        file = diagnostic_log_path,
        append = log_exists,
        col.names = !log_exists,
        row.names = FALSE,
        sep = ",",
        quote = TRUE,
        fileEncoding = "UTF-8"
      )
    },
    error = function(error) invisible(NULL)
  )

  invisible(NULL)
}

assert_non_missing <- function(
    data,
    column_name,
    context,
    diagnostic_log_path = NULL,
    diagnostic_event = NULL) {
  missing_rows <- data |>
    dplyr::filter(is_missing_text(.data[[column_name]]))
  missing_count <- nrow(missing_rows)

  if (missing_count > 0L) {
    if (!is.null(diagnostic_event)) {
      write_diagnostic_log(
        diagnostic_log_path = diagnostic_log_path,
        event = diagnostic_event,
        rows = missing_rows,
        missing_column = column_name
      )
    }

    rlang::abort(
      message = glue::glue(
        "{context} contains {missing_count} missing {column_name} value(s)."
      )
    )
  }
}

assert_unique <- function(data, column_name, context) {
  duplicate_count <- data |>
    dplyr::filter(!is_missing_text(.data[[column_name]])) |>
    dplyr::count(
      .data[[column_name]],
      name = "row_count"
    ) |>
    dplyr::filter(.data$row_count > 1L) |>
    nrow()

  if (duplicate_count > 0L) {
    rlang::abort(
      message = glue::glue(
        "{context} contains duplicated {column_name} value(s)."
      )
    )
  }
}

normalize_record_key <- function(values) {
  values |>
    normalize_text()
}

build_crf_tables <- function(crf_rows, diagnostic_log_path = NULL) {
  if (nrow(crf_rows) == 0L) {
    rlang::abort(message = "The CRF worksheet contains no data rows.")
  }

  assert_non_missing(
    data = crf_rows,
    column_name = "patient_id",
    context = "The CRF worksheet",
    diagnostic_log_path = diagnostic_log_path,
    diagnostic_event = "missing_patient_id_in_crf"
  )
  assert_unique(
    data = crf_rows,
    column_name = "patient_id",
    context = "The CRF worksheet"
  )

  patients <- crf_rows |>
    dplyr::transmute(
      patient_id = .data$patient_id,
      eama_reference = .data$eama_reference,
      source_row_number = .data$source_row_number
    )

  icu_admissions <- crf_rows |>
    dplyr::transmute(
      admission_id = stringr::str_c(.data$patient_id, "::icuAdmission"),
      patient_id = .data$patient_id,
      age_years = .data$age_years,
      height_cm = .data$height_cm,
      weight_kg = .data$weight_kg,
      apache_ii_score = .data$apache_ii_score,
      sex = .data$sex,
      admission_date = .data$admission_date,
      admission_time = .data$admission_time,
      admission_reason = .data$admission_reason,
      admission_type = .data$admission_type,
      mechanical_ventilation_start_date =
        .data$mechanical_ventilation_start_date,
      mechanical_ventilation_start_time =
        .data$mechanical_ventilation_start_time,
      mechanical_ventilation_indication =
        .data$mechanical_ventilation_indication,
      mechanical_ventilation_days = .data$mechanical_ventilation_days,
      icu_stay_days = .data$icu_stay_days,
      icu_discharge_date = .data$icu_discharge_date,
      icu_outcome = .data$icu_outcome,
      hospital_outcome = .data$hospital_outcome
    )

  comorbidity_codes <- c(
    copd = "copd",
    restrictive_lung_disease = "restrictiveLungDisease",
    neuromuscular_disease = "neuromuscularDisease",
    heart_failure = "heartFailure",
    diabetes = "diabetes",
    previous_stroke = "previousStroke",
    vascular_disease = "vascularDisease",
    chronic_kidney_disease = "chronicKidneyDisease",
    other_underlying_condition = "otherUnderlyingCondition"
  )

  patient_comorbidities <- crf_rows |>
    dplyr::select(
      "patient_id",
      dplyr::all_of(names(comorbidity_codes))
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(names(comorbidity_codes)),
      names_to = "comorbidity_type",
      values_to = "source_value"
    ) |>
    dplyr::mutate(
      comorbidity_type = unname(comorbidity_codes[.data$comorbidity_type])
    )

  patient_index <- patients |>
    dplyr::transmute(
      patient_key = normalize_record_key(.data$patient_id),
      patient_id = .data$patient_id
    )

  list(
    patients = patients,
    icu_admissions = icu_admissions,
    patient_comorbidities = patient_comorbidities,
    patient_index = patient_index
  )
}

link_patient_rows <- function(
    rows,
    patient_index,
    source_description,
    diagnostic_log_path = NULL) {
  linked_rows <- rows |>
    dplyr::mutate(
      patient_key = normalize_record_key(.data$source_patient_id)
    ) |>
    dplyr::left_join(
      y = patient_index,
      by = "patient_key"
    )

  assert_non_missing(
    data = linked_rows,
    column_name = "patient_id",
    context = source_description,
    diagnostic_log_path = diagnostic_log_path,
    diagnostic_event = "unmatched_patient_record"
  )

  linked_rows
}

add_session_details <- function(rows, condition, source_sheet) {
  rows |>
    dplyr::mutate(
      session_id = stringr::str_c(.data$patient_id, "::", .env$condition),
      admission_id = stringr::str_c(.data$patient_id, "::icuAdmission"),
      condition = .env$condition,
      source_sheet = .env$source_sheet
    )
}

build_session_tables <- function(
    physiotherapy_rows,
    control_rows,
    patient_index,
    diagnostic_log_path = NULL) {
  physiotherapy_linked <- physiotherapy_rows |>
    link_patient_rows(
      patient_index = patient_index,
      source_description = "The physiotherapy worksheet",
      diagnostic_log_path = diagnostic_log_path
    ) |>
    add_session_details(
      condition = "withPhysiotherapy",
      source_sheet = "withPhysiotherapy"
    )
  control_linked <- control_rows |>
    link_patient_rows(
      patient_index = patient_index,
      source_description = "The no-physiotherapy worksheet",
      diagnostic_log_path = diagnostic_log_path
    ) |>
    dplyr::mutate(connection_date = as.Date(x = NA)) |>
    add_session_details(
      condition = "withoutPhysiotherapy",
      source_sheet = "withoutPhysiotherapy"
    )

  study_sessions <- dplyr::bind_rows(
    physiotherapy_linked |>
      dplyr::transmute(
        session_id = .data$session_id,
        patient_id = .data$patient_id,
        admission_id = .data$admission_id,
        condition = .data$condition,
        connection_date = .data$connection_date,
        source_sheet = .data$source_sheet,
        source_row_number = .data$source_row_number
      ),
    control_linked |>
      dplyr::transmute(
        session_id = .data$session_id,
        patient_id = .data$patient_id,
        admission_id = .data$admission_id,
        condition = .data$condition,
        connection_date = .data$connection_date,
        source_sheet = .data$source_sheet,
        source_row_number = .data$source_row_number
      )
  )

  patient_clinical_status <- physiotherapy_linked |>
    dplyr::transmute(
      patient_id = .data$patient_id,
      rass_score = .data$rass_score,
      vas_score = .data$vas_score,
      cam_icu_result = .data$cam_icu_result,
      borg_score = .data$borg_score,
      borg_rpe_score = .data$borg_rpe_score,
      hand_grip_strength_right = .data$hand_grip_strength_right,
      hand_grip_strength_left = .data$hand_grip_strength_left,
      mrc_score = .data$mrc_score,
      icu_mobility_score = .data$icu_mobility_score
    )

  list(
    study_sessions = study_sessions,
    patient_clinical_status = patient_clinical_status,
    physiotherapy_linked = physiotherapy_linked,
    control_linked = control_linked
  )
}

build_assessment_rows <- function(rows, assessment, phase, column_map) {
  measurement_values <- purrr::imap(
    .x = column_map,
    .f = function(source_column, target_column) {
      if (is.na(source_column)) {
        rep(x = NA_character_, times = nrow(rows))
      } else {
        rows[[source_column]]
      }
    }
  )

  dplyr::transmute(
    rows,
    patient_id = .data$patient_id,
    assessment = .env$assessment,
    phase = .env$phase,
    !!!measurement_values,
    source_row_number = .data$source_row_number
  )
}

build_physiological_assessments <- function(
    physiotherapy_rows,
    control_rows) {
  physiotherapy_before_map <- c(
    pressure_support = "pre_pressure_support",
    positive_end_expiratory_pressure =
      "pre_positive_end_expiratory_pressure",
    tidal_volume = "pre_tidal_volume",
    total_minute_ventilation = "pre_total_minute_ventilation",
    fraction_inspired_oxygen = "pre_fraction_inspired_oxygen",
    respiratory_rate = "pre_respiratory_rate",
    t_piece = "pre_t_piece",
    oxygen_saturation = "pre_oxygen_saturation",
    blood_pressure_diastolic = "pre_blood_pressure_diastolic",
    blood_pressure_systolic = "pre_blood_pressure_systolic",
    heart_rate = "pre_heart_rate",
    noradrenaline_rate = "pre_noradrenaline_rate",
    arterial_ph = "pre_arterial_ph",
    arterial_oxygen_pressure = "pre_arterial_oxygen_pressure",
    arterial_carbon_dioxide_pressure =
      "pre_arterial_carbon_dioxide_pressure",
    ventilator_sensitivity = NA_character_,
    expiratory_sensitivity = NA_character_,
    rising_time = NA_character_
  )
  physiotherapy_after_map <- c(
    pressure_support = "post_pressure_support",
    positive_end_expiratory_pressure =
      "post_positive_end_expiratory_pressure",
    tidal_volume = "post_tidal_volume",
    total_minute_ventilation = "post_total_minute_ventilation",
    fraction_inspired_oxygen = "post_fraction_inspired_oxygen",
    respiratory_rate = "post_respiratory_rate",
    t_piece = "post_t_piece",
    oxygen_saturation = "post_oxygen_saturation",
    blood_pressure_diastolic = "post_blood_pressure_diastolic",
    blood_pressure_systolic = "post_blood_pressure_systolic",
    heart_rate = "post_heart_rate",
    noradrenaline_rate = "post_noradrenaline_rate",
    arterial_ph = "post_arterial_ph",
    arterial_oxygen_pressure = "post_arterial_oxygen_pressure",
    arterial_carbon_dioxide_pressure =
      "post_arterial_carbon_dioxide_pressure",
    ventilator_sensitivity = NA_character_,
    expiratory_sensitivity = NA_character_,
    rising_time = NA_character_
  )
  control_start_map <- c(
    pressure_support = "start_pressure_support",
    positive_end_expiratory_pressure =
      "start_positive_end_expiratory_pressure",
    tidal_volume = "start_tidal_volume",
    total_minute_ventilation = "start_total_minute_ventilation",
    fraction_inspired_oxygen = "start_fraction_inspired_oxygen",
    respiratory_rate = "start_respiratory_rate",
    t_piece = "start_t_piece",
    oxygen_saturation = "start_oxygen_saturation",
    blood_pressure_diastolic = "start_blood_pressure_diastolic",
    blood_pressure_systolic = "start_blood_pressure_systolic",
    heart_rate = "start_heart_rate",
    noradrenaline_rate = "start_noradrenaline_rate",
    arterial_ph = "start_arterial_ph",
    arterial_oxygen_pressure = "start_arterial_oxygen_pressure",
    arterial_carbon_dioxide_pressure =
      "start_arterial_carbon_dioxide_pressure",
    ventilator_sensitivity = NA_character_,
    expiratory_sensitivity = NA_character_,
    rising_time = NA_character_
  )
  control_end_map <- c(
    pressure_support = "end_pressure_support",
    positive_end_expiratory_pressure =
      "end_positive_end_expiratory_pressure",
    tidal_volume = "end_tidal_volume",
    total_minute_ventilation = "end_total_minute_ventilation",
    fraction_inspired_oxygen = "end_fraction_inspired_oxygen",
    respiratory_rate = "end_respiratory_rate",
    t_piece = "end_t_piece",
    oxygen_saturation = "end_oxygen_saturation",
    blood_pressure_diastolic = "end_blood_pressure_diastolic",
    blood_pressure_systolic = "end_blood_pressure_systolic",
    heart_rate = "end_heart_rate",
    noradrenaline_rate = "end_noradrenaline_rate",
    arterial_ph = "end_arterial_ph",
    arterial_oxygen_pressure = "end_arterial_oxygen_pressure",
    arterial_carbon_dioxide_pressure =
      "end_arterial_carbon_dioxide_pressure",
    ventilator_sensitivity = "end_ventilator_sensitivity",
    expiratory_sensitivity = "end_expiratory_sensitivity",
    rising_time = "end_rising_time"
  )

  dplyr::bind_rows(
    build_assessment_rows(
      rows = physiotherapy_rows,
      assessment = "with_physiotherapy",
      phase = "before_physiotherapy",
      column_map = physiotherapy_before_map
    ),
    build_assessment_rows(
      rows = physiotherapy_rows,
      assessment = "with_physiotherapy",
      phase = "after_physiotherapy",
      column_map = physiotherapy_after_map
    ),
    build_assessment_rows(
      rows = control_rows,
      assessment = "without_physiotherapy",
      phase = "recording_start",
      column_map = control_start_map
    ),
    build_assessment_rows(
      rows = control_rows,
      assessment = "without_physiotherapy",
      phase = "recording_end",
      column_map = control_end_map
    )
  ) |>
    dplyr::mutate(
      assessment_id = as.integer(dplyr::row_number()),
      .before = 1L
    )
}

build_sleep_rows <- function(rows, assessment, column_map) {
  measurement_values <- purrr::imap(
    .x = column_map,
    .f = function(source_column, target_column) rows[[source_column]]
  )

  dplyr::transmute(
    rows,
    patient_id = .data$patient_id,
    assessment = .env$assessment,
    !!!measurement_values,
    source_row_number = .data$source_row_number
  )
}

set_zero_total_sleep_metrics_missing <- function(sleep_assessments) {
  zero_total_sleep_time <- !is.na(sleep_assessments$total_sleep_time) &
    sleep_assessments$total_sleep_time == 0

  sleep_assessments |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(sleep_zero_total_time_na_column_names),
        .fns = function(values) {
          dplyr::if_else(
            condition = zero_total_sleep_time,
            true = values[NA_integer_],
            false = values
          )
        }
      )
    )
}

build_sleep_assessments <- function(
    sleep_rows,
    patient_index,
    diagnostic_log_path = NULL) {
  sleep_linked <- sleep_rows |>
    link_patient_rows(
      patient_index = patient_index,
      source_description = "The sleep worksheet",
      diagnostic_log_path = diagnostic_log_path
    )
  sleep_measurements <- c(
    time_in_bed = "time_in_bed",
    total_sleep_time = "total_sleep_time",
    sleep_efficiency = "sleep_efficiency",
    wake_after_sleep_onset = "wake_after_sleep_onset",
    sleep_latency = "sleep_latency",
    stage_shifts = "stage_shifts",
    stage1_duration = "stage1_duration",
    stage1_percent_tst = "stage1_percent_tst",
    stage2_duration = "stage2_duration",
    stage2_percent_tst = "stage2_percent_tst",
    slow_wave_sleep_duration = "slow_wave_sleep_duration",
    slow_wave_sleep_percent_tst = "slow_wave_sleep_percent_tst",
    rem_sleep_duration = "rem_sleep_duration",
    rem_sleep_percent_tst = "rem_sleep_percent_tst",
    arousal_index = "arousal_index",
    rcsq_score = "rcsq_score"
  )
  without_map <- stringr::str_c("without_", sleep_measurements)
  names(without_map) <- names(sleep_measurements)
  with_map <- stringr::str_c("with_", sleep_measurements)
  names(with_map) <- names(sleep_measurements)

  sleep_assessments <- dplyr::bind_rows(
    build_sleep_rows(
      rows = sleep_linked,
      assessment = "with_physiotherapy",
      column_map = with_map
    ),
    build_sleep_rows(
      rows = sleep_linked,
      assessment = "without_physiotherapy",
      column_map = without_map
    )
  ) |>
    dplyr::mutate(
      assessment_id = as.integer(dplyr::row_number()),
      .before = 1L
    )

  set_zero_total_sleep_metrics_missing(
    sleep_assessments = sleep_assessments
  )
}

assert_session_pairing <- function(study_sessions) {
  invalid_pairs <- study_sessions |>
    dplyr::count(
      .data$patient_id,
      .data$condition,
      name = "session_count"
    ) |>
    dplyr::filter(.data$session_count != 1L) |>
    nrow()
  expected_conditions <- c("withPhysiotherapy", "withoutPhysiotherapy")
  missing_conditions <- study_sessions |>
    dplyr::count(
      .data$patient_id,
      name = "condition_count"
    ) |>
    dplyr::filter(.data$condition_count != length(expected_conditions)) |>
    nrow()

  if (invalid_pairs > 0L || missing_conditions > 0L) {
    rlang::abort(
      message = "Each patient must have exactly one session in both conditions."
    )
  }
}

assert_assessment_phases <- function(
    physiological_assessments,
    study_sessions) {
  phase_expectations <- tibble::tibble(
    assessment = c(
      "with_physiotherapy",
      "with_physiotherapy",
      "without_physiotherapy",
      "without_physiotherapy"
    ),
    phase = c(
      "before_physiotherapy",
      "after_physiotherapy",
      "recording_start",
      "recording_end"
    )
  )
  expected_phases <- study_sessions |>
    dplyr::transmute(
      patient_id = .data$patient_id,
      assessment = dplyr::recode(
        .data$condition,
        withPhysiotherapy = "with_physiotherapy",
        withoutPhysiotherapy = "without_physiotherapy"
      )
    ) |>
    dplyr::inner_join(
      y = phase_expectations,
      by = "assessment",
      relationship = "many-to-many"
    )
  observed_phases <- physiological_assessments |>
    dplyr::select("patient_id", "assessment", "phase")
  unexpected_phases <- dplyr::anti_join(
    x = observed_phases,
    y = expected_phases,
    by = c("patient_id", "assessment", "phase")
  )
  missing_phases <- dplyr::anti_join(
    x = expected_phases,
    y = observed_phases,
    by = c("patient_id", "assessment", "phase")
  )
  duplicate_phases <- observed_phases |>
    dplyr::count(
      .data$patient_id,
      .data$assessment,
      .data$phase,
      name = "assessment_count"
    ) |>
    dplyr::filter(.data$assessment_count != 1L) |>
    nrow()

  if (nrow(unexpected_phases) > 0L || nrow(missing_phases) > 0L ||
      duplicate_phases > 0L) {
    error_message <- stringr::str_c(
      "Each patient must contain exactly its expected ",
      "assessment phases."
    )
    rlang::abort(
      message = error_message
    )
  }
}

assert_sleep_assessments <- function(sleep_assessments, study_sessions) {
  expected_assessments <- study_sessions |>
    dplyr::transmute(
      patient_id = .data$patient_id,
      assessment = dplyr::recode(
        .data$condition,
        withPhysiotherapy = "with_physiotherapy",
        withoutPhysiotherapy = "without_physiotherapy"
      )
    )
  observed_assessments <- sleep_assessments |>
    dplyr::select("patient_id", "assessment")
  duplicate_sleep_rows <- observed_assessments |>
    dplyr::count(
      .data$patient_id,
      .data$assessment,
      name = "sleep_count"
    ) |>
    dplyr::filter(.data$sleep_count != 1L) |>
    nrow()
  missing_sleep_rows <- dplyr::anti_join(
    x = expected_assessments,
    y = observed_assessments,
    by = c("patient_id", "assessment")
  )
  orphaned_sleep_rows <- dplyr::anti_join(
    x = observed_assessments,
    y = expected_assessments,
    by = c("patient_id", "assessment")
  )

  if (duplicate_sleep_rows > 0L || nrow(missing_sleep_rows) > 0L ||
      nrow(orphaned_sleep_rows) > 0L) {
    rlang::abort(
      message = stringr::str_c(
        "Each patient must have exactly one sleep assessment in both ",
        "assessment types."
      )
    )
  }
}

assert_no_name_columns <- function(tables) {
  prohibited_columns <- tables |>
    purrr::imap(
      .f = function(data, table_name) {
        names(data)[stringr::str_detect(
          string = names(data),
          pattern = stringr::regex(pattern = "name", ignore_case = TRUE)
        )]
      }
    ) |>
    purrr::compact()

  if (length(prohibited_columns) > 0L) {
    rlang::abort(
      message = "A transformed table contains a prohibited name-related column."
    )
  }
}

assert_raw_value_types <- function(tables) {
  invalid_value_columns <- tables |>
    purrr::imap(
      .f = function(data, table_name) {
        column_names <- names(data)
        allowed_integer <- column_names == "source_row_number" |
          (
            table_name %in% c(
              "physiological_assessments",
              "sleep_assessments"
            ) &
              column_names == "assessment_id"
          )
        is_date_column <- stringr::str_detect(
          string = column_names,
          pattern = "date"
        )
        allowed_date <- purrr::map2_lgl(
          .x = data,
          .y = is_date_column,
          .f = function(values, is_date_column) {
            is_date_column && inherits(values, what = "Date")
          }
        )
        allowed_numeric <- purrr::imap_lgl(
          .x = data,
          .f = function(values, column_name) {
            ((
              table_name == "sleep_assessments" &&
                column_name %in% sleep_numeric_column_names
            ) || (
              table_name == "patient_clinical_status" &&
                column_name %in% patient_clinical_status_numeric_column_names
            ) || (
              table_name == "physiological_assessments" &&
                column_name %in% physiological_assessment_numeric_column_names
            )) && is.numeric(values)
          }
        )
        is_character_column <- purrr::map_lgl(
          .x = data,
          .f = is.character
        )
        column_names[!(
          allowed_integer | allowed_date | allowed_numeric | is_character_column
        )]
      }
    ) |>
    purrr::compact()

  if (length(invalid_value_columns) > 0L) {
    rlang::abort(
      message = stringr::str_c(
        "Retained values must be character, except source_row_number and ",
        "date columns, plus configured numeric columns."
      )
    )
  }
}

validate_transformed_tables <- function(tables) {
  assert_no_name_columns(tables = tables)
  assert_unique(
    data = tables$patients,
    column_name = "patient_id",
    context = "The patients table"
  )
  assert_session_pairing(study_sessions = tables$study_sessions)
  assert_unique(
    data = tables$patient_clinical_status,
    column_name = "patient_id",
    context = "The patient clinical-status table"
  )
  assert_assessment_phases(
    physiological_assessments = tables$physiological_assessments,
    study_sessions = tables$study_sessions
  )
  assert_sleep_assessments(
    sleep_assessments = tables$sleep_assessments,
    study_sessions = tables$study_sessions
  )
  assert_raw_value_types(tables = tables)
}

create_duckdb_schema <- function(connection) {
  statements <- c(
    'CREATE TABLE "patients" (
      "patient_id" VARCHAR PRIMARY KEY,
      "eama_reference" VARCHAR,
      "source_row_number" INTEGER NOT NULL
    )',
    'CREATE TABLE "icu_admissions" (
      "admission_id" VARCHAR PRIMARY KEY,
      "patient_id" VARCHAR NOT NULL UNIQUE,
      "age_years" VARCHAR,
      "height_cm" VARCHAR,
      "weight_kg" VARCHAR,
      "apache_ii_score" VARCHAR,
      "sex" VARCHAR,
      "admission_date" DATE,
      "admission_time" TIME,
      "admission_reason" VARCHAR,
      "admission_type" VARCHAR,
      "mechanical_ventilation_start_date" DATE,
      "mechanical_ventilation_start_time" TIME,
      "mechanical_ventilation_indication" VARCHAR,
      "mechanical_ventilation_days" VARCHAR,
      "icu_stay_days" VARCHAR,
      "icu_discharge_date" DATE,
      "icu_outcome" VARCHAR,
      "hospital_outcome" VARCHAR,
      FOREIGN KEY ("patient_id") REFERENCES "patients" ("patient_id")
    )',
    'CREATE TABLE "patient_comorbidities" (
      "patient_id" VARCHAR NOT NULL,
      "comorbidity_type" VARCHAR NOT NULL,
      "source_value" VARCHAR,
      PRIMARY KEY ("patient_id", "comorbidity_type"),
      FOREIGN KEY ("patient_id") REFERENCES "patients" ("patient_id")
    )',
    'CREATE TABLE "study_sessions" (
      "session_id" VARCHAR PRIMARY KEY,
      "patient_id" VARCHAR NOT NULL,
      "admission_id" VARCHAR NOT NULL,
      "condition" VARCHAR NOT NULL CHECK (
        "condition" IN (\'withPhysiotherapy\', \'withoutPhysiotherapy\')
      ),
      "connection_date" DATE,
      "source_sheet" VARCHAR NOT NULL,
      "source_row_number" INTEGER NOT NULL,
      UNIQUE ("patient_id", "condition"),
      FOREIGN KEY ("patient_id") REFERENCES "patients" ("patient_id"),
      FOREIGN KEY ("admission_id") REFERENCES "icu_admissions" ("admission_id")
    )',
    'CREATE TABLE "patient_clinical_status" (
      "patient_id" VARCHAR PRIMARY KEY,
      "rass_score" VARCHAR,
      "vas_score" VARCHAR,
      "cam_icu_result" VARCHAR,
      "borg_score" VARCHAR,
      "borg_rpe_score" VARCHAR,
      "hand_grip_strength_right" DOUBLE,
      "hand_grip_strength_left" DOUBLE,
      "mrc_score" VARCHAR,
      "icu_mobility_score" VARCHAR,
      FOREIGN KEY ("patient_id") REFERENCES "patients" ("patient_id")
    )',
    'CREATE TABLE "physiological_assessments" (
      "assessment_id" INTEGER PRIMARY KEY,
      "patient_id" VARCHAR NOT NULL,
      "assessment" VARCHAR NOT NULL CHECK (
        "assessment" IN (
          \'with_physiotherapy\',
          \'without_physiotherapy\'
        )
      ),
      "phase" VARCHAR NOT NULL,
      CHECK (
        (
          "assessment" = \'with_physiotherapy\' AND
          "phase" IN (\'before_physiotherapy\', \'after_physiotherapy\')
        ) OR (
          "assessment" = \'without_physiotherapy\' AND
          "phase" IN (\'recording_start\', \'recording_end\')
        )
      ),
      "pressure_support" VARCHAR,
      "positive_end_expiratory_pressure" VARCHAR,
      "tidal_volume" VARCHAR,
      "total_minute_ventilation" VARCHAR,
      "fraction_inspired_oxygen" VARCHAR,
      "respiratory_rate" VARCHAR,
      "t_piece" VARCHAR,
      "oxygen_saturation" VARCHAR,
      "blood_pressure_diastolic" DOUBLE,
      "blood_pressure_systolic" DOUBLE,
      "heart_rate" VARCHAR,
      "noradrenaline_rate" VARCHAR,
      "arterial_ph" VARCHAR,
      "arterial_oxygen_pressure" VARCHAR,
      "arterial_carbon_dioxide_pressure" VARCHAR,
      "ventilator_sensitivity" VARCHAR,
      "expiratory_sensitivity" VARCHAR,
      "rising_time" VARCHAR,
      "source_row_number" INTEGER NOT NULL,
      UNIQUE ("patient_id", "assessment", "phase"),
      FOREIGN KEY ("patient_id") REFERENCES "patients" ("patient_id")
    )',
    'CREATE TABLE "sleep_assessments" (
      "assessment_id" INTEGER PRIMARY KEY,
      "patient_id" VARCHAR NOT NULL,
      "assessment" VARCHAR NOT NULL CHECK (
        "assessment" IN (\'with_physiotherapy\', \'without_physiotherapy\')
      ),
      "time_in_bed" DOUBLE,
      "total_sleep_time" DOUBLE,
      "sleep_efficiency" DOUBLE,
      "wake_after_sleep_onset" DOUBLE,
      "sleep_latency" DOUBLE,
      "stage_shifts" VARCHAR,
      "stage1_duration" DOUBLE,
      "stage1_percent_tst" DOUBLE,
      "stage2_duration" DOUBLE,
      "stage2_percent_tst" DOUBLE,
      "slow_wave_sleep_duration" DOUBLE,
      "slow_wave_sleep_percent_tst" VARCHAR,
      "rem_sleep_duration" VARCHAR,
      "rem_sleep_percent_tst" VARCHAR,
      "arousal_index" DOUBLE,
      "rcsq_score" VARCHAR,
      "source_row_number" INTEGER NOT NULL,
      UNIQUE ("patient_id", "assessment"),
      FOREIGN KEY ("patient_id") REFERENCES "patients" ("patient_id")
    )'
  )

  purrr::walk(
    .x = statements,
    .f = function(statement) {
      DBI::dbExecute(conn = connection, statement = statement)
    }
  )
}

to_database_columns <- function(data, table_key) {
  column_map <- database_column_names[[table_key]]
  expected_columns <- names(column_map)
  missing_columns <- setdiff(expected_columns, names(data))
  extra_columns <- setdiff(names(data), expected_columns)

  if (length(missing_columns) > 0L || length(extra_columns) > 0L) {
    rlang::abort(
      message = glue::glue(
        "The {table_key} data does not match its DuckDB table definition."
      )
    )
  }

  database_data <- data |>
    dplyr::select(dplyr::all_of(expected_columns))
  names(database_data) <- column_map

  database_data
}

write_transformed_tables <- function(connection, tables) {
  DBI::dbBegin(conn = connection)
  committed <- FALSE
  on.exit(
    expr = {
      if (!committed) {
        DBI::dbRollback(conn = connection)
      }
    },
    add = TRUE
  )

  create_duckdb_schema(connection = connection)
  table_order <- names(database_table_names)
  row_counts <- purrr::map_int(
    .x = table_order,
    .f = function(table_key) {
      database_data <- to_database_columns(
        data = tables[[table_key]],
        table_key = table_key
      )
      DBI::dbAppendTable(
        conn = connection,
        name = database_table_names[[table_key]],
        value = database_data
      )
      nrow(database_data)
    }
  )
  names(row_counts) <- unname(database_table_names[table_order])

  DBI::dbCommit(conn = connection)
  committed <- TRUE
  as.list(row_counts)
}

transform_icu_workbook <- function(
    input_path,
    database_path,
    diagnostic_log_path = NULL) {
  check_runtime_packages()

  if (length(input_path) != 1L || !fs::file_exists(path = input_path)) {
    rlang::abort(message = "input_path must reference an existing workbook.")
  }
  if (fs::path_ext(path = input_path) != "xlsx") {
    rlang::abort(message = "input_path must have the .xlsx extension.")
  }
  if (length(database_path) != 1L || fs::file_exists(path = database_path)) {
    rlang::abort(
      message = "database_path must reference a new DuckDB database file."
    )
  }

  database_directory <- fs::path_dir(path = database_path)
  if (!fs::dir_exists(path = database_directory)) {
    fs::dir_create(path = database_directory, recurse = TRUE)
  }

  layout <- validate_workbook_layout(input_path = input_path)
  crf_rows <- read_source_rows(
    input_path = input_path,
    sheet_layout = layout$crf
  )
  physiotherapy_rows <- read_source_rows(
    input_path = input_path,
    sheet_layout = layout$physiotherapy
  )
  control_rows <- read_source_rows(
    input_path = input_path,
    sheet_layout = layout$control
  )
  sleep_rows <- read_source_rows(
    input_path = input_path,
    sheet_layout = layout$sleep
  )

  crf_tables <- build_crf_tables(
    crf_rows = crf_rows,
    diagnostic_log_path = diagnostic_log_path
  )
  session_tables <- build_session_tables(
    physiotherapy_rows = physiotherapy_rows,
    control_rows = control_rows,
    patient_index = crf_tables$patient_index,
    diagnostic_log_path = diagnostic_log_path
  )
  physiological_assessments <- build_physiological_assessments(
    physiotherapy_rows = session_tables$physiotherapy_linked,
    control_rows = session_tables$control_linked
  )
  sleep_assessments <- build_sleep_assessments(
    sleep_rows = sleep_rows,
    patient_index = crf_tables$patient_index,
    diagnostic_log_path = diagnostic_log_path
  )
  tables <- list(
    patients = crf_tables$patients,
    icu_admissions = crf_tables$icu_admissions,
    patient_comorbidities = crf_tables$patient_comorbidities,
    study_sessions = session_tables$study_sessions,
    patient_clinical_status = session_tables$patient_clinical_status,
    physiological_assessments = physiological_assessments,
    sleep_assessments = sleep_assessments
  )

  validate_transformed_tables(tables = tables)

  connection <- DBI::dbConnect(
    drv = duckdb::duckdb(),
    dbdir = database_path,
    read_only = FALSE
  )
  on.exit(
    expr = DBI::dbDisconnect(conn = connection, shutdown = TRUE),
    add = TRUE
  )

  write_transformed_tables(connection = connection, tables = tables)
}
