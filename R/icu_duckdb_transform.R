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
      column_count = 28L,
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
        "source_record_number",
        "age_years",
        "height_cm",
        "weight_kg",
        "apache_ii_score",
        "sex",
        "patient_id",
        "admission_date",
        "admission_time",
        "admission_reason",
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
        "source_record_number",
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
        "pre_norepinephrine_rate",
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
        "post_norepinephrine_rate",
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
        "source_record_number",
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
        "start_norepinephrine_rate",
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
        "end_norepinephrine_rate",
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
        "source_record_number",
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
  session_clinical_status = "session_clinical_status",
  physiological_assessments = "physiological_assessments",
  sleep_assessments = "sleep_assessments"
)

database_column_names <- list(
  patients = c(
    patient_id = "patient_id",
    source_record_number = "source_record_number",
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
  session_clinical_status = c(
    session_id = "session_id",
    rass_score = "rass_score",
    vas_score = "vas_score",
    cam_icu_result = "cam_icu_result",
    borg_score = "borg_score",
    borg_rpe_score = "borg_rpe_score",
    handgrip_strength = "handgrip_strength",
    mrc_score = "mrc_score",
    icu_mobility_score = "icu_mobility_score"
  ),
  physiological_assessments = c(
    assessment_id = "assessment_id",
    session_id = "session_id",
    assessment_phase = "assessment_phase",
    pressure_support = "pressure_support",
    positive_end_expiratory_pressure = "positive_end_expiratory_pressure",
    tidal_volume = "tidal_volume",
    total_minute_ventilation = "total_minute_ventilation",
    fraction_inspired_oxygen = "fraction_inspired_oxygen",
    respiratory_rate = "respiratory_rate",
    t_piece = "t_piece",
    oxygen_saturation = "oxygen_saturation",
    blood_pressure = "blood_pressure",
    heart_rate = "heart_rate",
    norepinephrine_rate = "norepinephrine_rate",
    arterial_ph = "arterial_ph",
    arterial_oxygen_pressure = "arterial_oxygen_pressure",
    arterial_carbon_dioxide_pressure = "arterial_carbon_dioxide_pressure",
    ventilator_sensitivity = "ventilator_sensitivity",
    expiratory_sensitivity = "expiratory_sensitivity",
    rising_time = "rising_time",
    source_row_number = "source_row_number"
  ),
  sleep_assessments = c(
    sleep_assessment_id = "sleep_assessment_id",
    session_id = "session_id",
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

read_source_rows <- function(input_path, sheet_layout) {
  data_range <- readxl::cell_limits(
    ul = c(sheet_layout$first_data_row, 1L),
    lr = c(NA_integer_, sheet_layout$column_count)
  )
  column_types <- rep(x = "text", times = sheet_layout$column_count)
  column_types[[2L]] <- "skip"

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

  source_rows |>
    dplyr::mutate(
      source_row_number = as.integer(dplyr::row_number()) +
        sheet_layout$first_data_row - 1L
    ) |>
    dplyr::filter(
      dplyr::if_any(
        .cols = dplyr::all_of(sheet_layout$internal_names),
        .fns = function(values) !is_missing_text(values)
      )
    ) |>
    dplyr::mutate(
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
  assert_non_missing(
    data = crf_rows,
    column_name = "source_record_number",
    context = "The CRF worksheet"
  )
  assert_unique(
    data = crf_rows,
    column_name = "source_record_number",
    context = "The CRF worksheet"
  )

  patients <- crf_rows |>
    dplyr::transmute(
      patient_id = .data$patient_id,
      source_record_number = .data$source_record_number,
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
      record_key = normalize_record_key(.data$source_record_number),
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
      record_key = normalize_record_key(.data$source_record_number)
    ) |>
    dplyr::left_join(
      y = patient_index,
      by = "record_key"
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
    dplyr::mutate(connection_date = NA_character_) |>
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

  session_clinical_status <- dplyr::bind_rows(
    physiotherapy_linked |>
      dplyr::transmute(
        session_id = .data$session_id,
        rass_score = .data$rass_score,
        vas_score = .data$vas_score,
        cam_icu_result = .data$cam_icu_result,
        borg_score = .data$borg_score,
        borg_rpe_score = .data$borg_rpe_score,
        handgrip_strength = .data$handgrip_strength,
        mrc_score = .data$mrc_score,
        icu_mobility_score = .data$icu_mobility_score
      ),
    control_linked |>
      dplyr::transmute(
        session_id = .data$session_id,
        rass_score = .data$rass_score,
        vas_score = .data$vas_score,
        cam_icu_result = .data$cam_icu_result,
        borg_score = NA_character_,
        borg_rpe_score = NA_character_,
        handgrip_strength = NA_character_,
        mrc_score = NA_character_,
        icu_mobility_score = NA_character_
      )
  )

  list(
    study_sessions = study_sessions,
    session_clinical_status = session_clinical_status,
    physiotherapy_linked = physiotherapy_linked,
    control_linked = control_linked
  )
}

build_assessment_rows <- function(rows, phase, column_map) {
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
    assessment_id = stringr::str_c(.data$session_id, "::", .env$phase),
    session_id = .data$session_id,
    assessment_phase = .env$phase,
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
    blood_pressure = "pre_blood_pressure",
    heart_rate = "pre_heart_rate",
    norepinephrine_rate = "pre_norepinephrine_rate",
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
    blood_pressure = "post_blood_pressure",
    heart_rate = "post_heart_rate",
    norepinephrine_rate = "post_norepinephrine_rate",
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
    blood_pressure = "start_blood_pressure",
    heart_rate = "start_heart_rate",
    norepinephrine_rate = "start_norepinephrine_rate",
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
    blood_pressure = "end_blood_pressure",
    heart_rate = "end_heart_rate",
    norepinephrine_rate = "end_norepinephrine_rate",
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
      phase = "beforePhysiotherapy",
      column_map = physiotherapy_before_map
    ),
    build_assessment_rows(
      rows = physiotherapy_rows,
      phase = "afterPhysiotherapy",
      column_map = physiotherapy_after_map
    ),
    build_assessment_rows(
      rows = control_rows,
      phase = "recordingStart",
      column_map = control_start_map
    ),
    build_assessment_rows(
      rows = control_rows,
      phase = "recordingEnd",
      column_map = control_end_map
    )
  )
}

build_sleep_rows <- function(rows, condition, column_map) {
  measurement_values <- purrr::imap(
    .x = column_map,
    .f = function(source_column, target_column) rows[[source_column]]
  )

  dplyr::transmute(
    rows,
    sleep_assessment_id = stringr::str_c(
      .data$patient_id,
      "::",
      .env$condition,
      "::sleep"
    ),
    session_id = stringr::str_c(.data$patient_id, "::", .env$condition),
    !!!measurement_values,
    source_row_number = .data$source_row_number
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

  dplyr::bind_rows(
    build_sleep_rows(
      rows = sleep_linked,
      condition = "withoutPhysiotherapy",
      column_map = without_map
    ),
    build_sleep_rows(
      rows = sleep_linked,
      condition = "withPhysiotherapy",
      column_map = with_map
    )
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
    condition = c(
      "withPhysiotherapy",
      "withPhysiotherapy",
      "withoutPhysiotherapy",
      "withoutPhysiotherapy"
    ),
    assessment_phase = c(
      "beforePhysiotherapy",
      "afterPhysiotherapy",
      "recordingStart",
      "recordingEnd"
    )
  )
  observed_phases <- physiological_assessments |>
    dplyr::inner_join(
      y = study_sessions |>
        dplyr::select("session_id", "condition"),
      by = "session_id"
    ) |>
    dplyr::select("session_id", "condition", "assessment_phase")
  unexpected_phases <- dplyr::anti_join(
    x = observed_phases,
    y = phase_expectations,
    by = c("condition", "assessment_phase")
  )
  missing_phases <- dplyr::anti_join(
    x = study_sessions |>
      dplyr::select("session_id", "condition") |>
      tidyr::crossing(
        assessment_phase = c(
          "beforePhysiotherapy",
          "afterPhysiotherapy",
          "recordingStart",
          "recordingEnd"
        )
      ) |>
      dplyr::inner_join(
        y = phase_expectations,
        by = c("condition", "assessment_phase")
      ),
    y = observed_phases,
    by = c("session_id", "condition", "assessment_phase")
  )
  duplicate_phases <- observed_phases |>
    dplyr::count(
      .data$session_id,
      .data$assessment_phase,
      name = "assessment_count"
    ) |>
    dplyr::filter(.data$assessment_count != 1L) |>
    nrow()

  if (nrow(unexpected_phases) > 0L || nrow(missing_phases) > 0L ||
      duplicate_phases > 0L) {
    error_message <- stringr::str_c(
      "Each session must contain exactly its expected ",
      "assessment phases."
    )
    rlang::abort(
      message = error_message
    )
  }
}

assert_sleep_sessions <- function(sleep_assessments, study_sessions) {
  duplicate_sleep_rows <- sleep_assessments |>
    dplyr::count(.data$session_id, name = "sleep_count") |>
    dplyr::filter(.data$sleep_count != 1L) |>
    nrow()
  missing_sleep_rows <- dplyr::anti_join(
    x = study_sessions |>
      dplyr::select("session_id"),
    y = sleep_assessments |>
      dplyr::select("session_id"),
    by = "session_id"
  )
  orphaned_sleep_rows <- dplyr::anti_join(
    x = sleep_assessments |>
      dplyr::select("session_id"),
    y = study_sessions |>
      dplyr::select("session_id"),
    by = "session_id"
  )

  if (duplicate_sleep_rows > 0L || nrow(missing_sleep_rows) > 0L ||
      nrow(orphaned_sleep_rows) > 0L) {
    rlang::abort(
      message = "Each session must have exactly one linked sleep assessment."
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
  non_character_columns <- tables |>
    purrr::imap(
      .f = function(data, table_name) {
        allowed_integer <- names(data) == "source_row_number"
        is_character_column <- purrr::map_lgl(
          .x = data,
          .f = is.character
        )
        names(data)[!(allowed_integer | is_character_column)]
      }
    ) |>
    purrr::compact()

  if (length(non_character_columns) > 0L) {
    rlang::abort(
      message = "All retained source values must remain character values."
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
  assert_unique(
    data = tables$patients,
    column_name = "source_record_number",
    context = "The patients table"
  )
  assert_session_pairing(study_sessions = tables$study_sessions)
  assert_unique(
    data = tables$session_clinical_status,
    column_name = "session_id",
    context = "The session clinical-status table"
  )
  assert_assessment_phases(
    physiological_assessments = tables$physiological_assessments,
    study_sessions = tables$study_sessions
  )
  assert_sleep_sessions(
    sleep_assessments = tables$sleep_assessments,
    study_sessions = tables$study_sessions
  )
  assert_raw_value_types(tables = tables)
}

create_duckdb_schema <- function(connection) {
  statements <- c(
    'CREATE TABLE "patients" (
      "patient_id" VARCHAR PRIMARY KEY,
      "source_record_number" VARCHAR NOT NULL UNIQUE,
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
      "admission_date" VARCHAR,
      "admission_time" VARCHAR,
      "admission_reason" VARCHAR,
      "mechanical_ventilation_start_date" VARCHAR,
      "mechanical_ventilation_start_time" VARCHAR,
      "mechanical_ventilation_indication" VARCHAR,
      "mechanical_ventilation_days" VARCHAR,
      "icu_stay_days" VARCHAR,
      "icu_discharge_date" VARCHAR,
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
      "connection_date" VARCHAR,
      "source_sheet" VARCHAR NOT NULL,
      "source_row_number" INTEGER NOT NULL,
      UNIQUE ("patient_id", "condition"),
      FOREIGN KEY ("patient_id") REFERENCES "patients" ("patient_id"),
      FOREIGN KEY ("admission_id") REFERENCES "icu_admissions" ("admission_id")
    )',
    'CREATE TABLE "session_clinical_status" (
      "session_id" VARCHAR PRIMARY KEY,
      "rass_score" VARCHAR,
      "vas_score" VARCHAR,
      "cam_icu_result" VARCHAR,
      "borg_score" VARCHAR,
      "borg_rpe_score" VARCHAR,
      "handgrip_strength" VARCHAR,
      "mrc_score" VARCHAR,
      "icu_mobility_score" VARCHAR,
      FOREIGN KEY ("session_id") REFERENCES "study_sessions" ("session_id")
    )',
    'CREATE TABLE "physiological_assessments" (
      "assessment_id" VARCHAR PRIMARY KEY,
      "session_id" VARCHAR NOT NULL,
      "assessment_phase" VARCHAR NOT NULL CHECK (
        "assessment_phase" IN (
          \'beforePhysiotherapy\',
          \'afterPhysiotherapy\',
          \'recordingStart\',
          \'recordingEnd\'
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
      "blood_pressure" VARCHAR,
      "heart_rate" VARCHAR,
      "norepinephrine_rate" VARCHAR,
      "arterial_ph" VARCHAR,
      "arterial_oxygen_pressure" VARCHAR,
      "arterial_carbon_dioxide_pressure" VARCHAR,
      "ventilator_sensitivity" VARCHAR,
      "expiratory_sensitivity" VARCHAR,
      "rising_time" VARCHAR,
      "source_row_number" INTEGER NOT NULL,
      UNIQUE ("session_id", "assessment_phase"),
      FOREIGN KEY ("session_id") REFERENCES "study_sessions" ("session_id")
    )',
    'CREATE TABLE "sleep_assessments" (
      "sleep_assessment_id" VARCHAR PRIMARY KEY,
      "session_id" VARCHAR NOT NULL UNIQUE,
      "time_in_bed" VARCHAR,
      "total_sleep_time" VARCHAR,
      "sleep_efficiency" VARCHAR,
      "wake_after_sleep_onset" VARCHAR,
      "sleep_latency" VARCHAR,
      "stage_shifts" VARCHAR,
      "stage1_duration" VARCHAR,
      "stage1_percent_tst" VARCHAR,
      "stage2_duration" VARCHAR,
      "stage2_percent_tst" VARCHAR,
      "slow_wave_sleep_duration" VARCHAR,
      "slow_wave_sleep_percent_tst" VARCHAR,
      "rem_sleep_duration" VARCHAR,
      "rem_sleep_percent_tst" VARCHAR,
      "arousal_index" VARCHAR,
      "rcsq_score" VARCHAR,
      "source_row_number" INTEGER NOT NULL,
      FOREIGN KEY ("session_id") REFERENCES "study_sessions" ("session_id")
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
    session_clinical_status = session_tables$session_clinical_status,
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
