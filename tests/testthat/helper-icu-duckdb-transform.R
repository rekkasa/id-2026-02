source(
  file = fs::path_abs(
    path = fs::path("..", "..", "R", "icu_duckdb_transform.R")
  ),
  local = globalenv()
)

write_excel_row <- function(workbook, sheet_name, row_values, row_number) {
  openxlsx::writeData(
    wb = workbook,
    sheet = sheet_name,
    x = t(row_values),
    startCol = 1L,
    startRow = row_number,
    colNames = FALSE
  )
}

overwrite_excel_cell <- function(
    workbook_path,
    sheet_name,
    value,
    column_number,
    row_number) {
  workbook <- openxlsx::loadWorkbook(file = workbook_path)
  openxlsx::writeData(
    wb = workbook,
    sheet = sheet_name,
    x = value,
    startCol = column_number,
    startRow = row_number,
    colNames = FALSE
  )
  openxlsx::saveWorkbook(
    wb = workbook,
    file = workbook_path,
    overwrite = TRUE
  )
}

make_source_row <- function(column_count, patient_id) {
  values <- stringr::str_c("value_", seq_len(column_count))
  values[[1L]] <- patient_id
  values[[2L]] <- "Synthetic Name"

  values
}

write_synthetic_workbook <- function(
    workbook_path,
    patient_ids = "1",
    eama_references = "EAMA-001",
    physiotherapy_patient_ids = patient_ids,
    admission_dates = "01/01/2026",
    admission_times = "14:30",
    admission_types = "MEDICAL",
    mechanical_ventilation_start_dates = "02/01/2026",
    mechanical_ventilation_start_times = "15:45",
    icu_discharge_dates = "03/01/2026",
    connection_dates = "04/01/2026",
    include_control = TRUE,
    duplicate_physiotherapy = FALSE,
    invalid_physio_header = FALSE,
    missing_crf_patient_id = FALSE) {
  layout <- workbook_layout()
  workbook <- openxlsx::createWorkbook()

  purrr::walk(
    .x = layout,
    .f = function(sheet_layout) {
      openxlsx::addWorksheet(
        wb = workbook,
        sheetName = sheet_layout$sheet_name
      )
      if (!is.null(sheet_layout$group_headers)) {
        group_row <- rep(x = "", times = sheet_layout$column_count)
        group_positions <- as.integer(names(sheet_layout$group_headers))
        group_row[group_positions] <- unname(sheet_layout$group_headers)
        write_excel_row(
          workbook = workbook,
          sheet_name = sheet_layout$sheet_name,
          row_values = group_row,
          row_number = sheet_layout$header_row - 1L
        )
      }

      header_row <- sheet_layout$leaf_headers
      header_row[is.na(header_row)] <- ""
      if (invalid_physio_header &&
          identical(sheet_layout$sheet_name, layout$physiotherapy$sheet_name)) {
        header_row[[3L]] <- "Unexpected header"
      }
      write_excel_row(
        workbook = workbook,
        sheet_name = sheet_layout$sheet_name,
        row_values = header_row,
        row_number = sheet_layout$header_row
      )
    }
  )

  eama_references <- rep(
    x = eama_references,
    length.out = length(patient_ids)
  )
  admission_dates <- rep(x = admission_dates, length.out = length(patient_ids))
  admission_times <- rep(x = admission_times, length.out = length(patient_ids))
  admission_types <- rep(x = admission_types, length.out = length(patient_ids))
  mechanical_ventilation_start_dates <- rep(
    x = mechanical_ventilation_start_dates,
    length.out = length(patient_ids)
  )
  mechanical_ventilation_start_times <- rep(
    x = mechanical_ventilation_start_times,
    length.out = length(patient_ids)
  )
  icu_discharge_dates <- rep(
    x = icu_discharge_dates,
    length.out = length(patient_ids)
  )

  purrr::walk(
    .x = seq_along(patient_ids),
    .f = function(index) {
      crf_row <- make_source_row(
        column_count = layout$crf$column_count,
        patient_id = if (missing_crf_patient_id && index == 1L) {
          NA_character_
        } else {
          patient_ids[[index]]
        }
      )
      crf_row[[8L]] <- eama_references[[index]]
      crf_row[[9L]] <- admission_dates[[index]]
      crf_row[[10L]] <- admission_times[[index]]
      crf_row[[12L]] <- admission_types[[index]]
      crf_row[[22L]] <- mechanical_ventilation_start_dates[[index]]
      crf_row[[23L]] <- mechanical_ventilation_start_times[[index]]
      crf_row[[27L]] <- icu_discharge_dates[[index]]
      write_excel_row(
        workbook = workbook,
        sheet_name = layout$crf$sheet_name,
        row_values = crf_row,
        row_number = layout$crf$first_data_row + index - 1L
      )
    }
  )

  connection_dates <- rep(
    x = connection_dates,
    length.out = length(physiotherapy_patient_ids)
  )
  purrr::walk(
    .x = seq_along(physiotherapy_patient_ids),
    .f = function(index) {
      physiotherapy_row <- make_source_row(
        column_count = layout$physiotherapy$column_count,
        patient_id = physiotherapy_patient_ids[[index]]
      )
      physiotherapy_row[[3L]] <- connection_dates[[index]]
      physiotherapy_row[[9L]] <- "27.5 (R) 24 (L)"
      physiotherapy_row[[20L]] <- "70/120"
      physiotherapy_row[[22L]] <- "1.5"
      physiotherapy_row[[30L]] <- "0,40"
      physiotherapy_row[[34L]] <- "72/118"
      physiotherapy_row[[36L]] <- "2.5"
      write_excel_row(
        workbook = workbook,
        sheet_name = layout$physiotherapy$sheet_name,
        row_values = physiotherapy_row,
        row_number = layout$physiotherapy$first_data_row + index - 1L
      )
    }
  )
  if (duplicate_physiotherapy) {
    duplicate_row <- make_source_row(
      column_count = layout$physiotherapy$column_count,
      patient_id = physiotherapy_patient_ids[[1L]]
    )
    duplicate_row[[3L]] <- connection_dates[[1L]]
    duplicate_row[[9L]] <- "27.5 (R) 24 (L)"
    duplicate_row[[20L]] <- "70/120"
    duplicate_row[[22L]] <- "1.5"
    duplicate_row[[30L]] <- "0,40"
    duplicate_row[[34L]] <- "72/118"
    duplicate_row[[36L]] <- "2.5"
    write_excel_row(
      workbook = workbook,
      sheet_name = layout$physiotherapy$sheet_name,
      row_values = duplicate_row,
      row_number = layout$physiotherapy$first_data_row +
        length(physiotherapy_patient_ids)
    )
  }

  if (include_control) {
    purrr::walk(
      .x = seq_along(patient_ids),
      .f = function(index) {
        control_row <- make_source_row(
          column_count = layout$control$column_count,
          patient_id = patient_ids[[index]]
        )
        control_row[[15L]] <- "68/116"
        control_row[[17L]] <- "3.5"
        control_row[[32L]] <- "70/115"
        control_row[[34L]] <- "4.5"
        write_excel_row(
          workbook = workbook,
          sheet_name = layout$control$sheet_name,
          row_values = control_row,
          row_number = layout$control$first_data_row + index - 1L
        )
      }
    )
  }

  purrr::walk(
    .x = seq_along(patient_ids),
    .f = function(index) {
      sleep_row <- make_source_row(
        column_count = layout$sleep$column_count,
        patient_id = patient_ids[[index]]
      )
      sleep_row[c(
        3L, 4L, 5L, 6L, 7L, 9L, 10L, 11L, 12L, 13L, 17L, 19L, 20L, 21L,
        22L, 23L, 25L, 26L, 27L, 28L, 29L, 33L
      )] <- c(
        "8h 15m", "7h 30m", "88%", "15.5m", "10m", "30m", "5%",
        "240m", "50%", "90m", "18.5/h", "8h 45m", "7h 45m", "91%",
        "12m", "8m", "25m", "4%", "250m", "52%", "85m", "12"
      )
      write_excel_row(
        workbook = workbook,
        sheet_name = layout$sleep$sheet_name,
        row_values = sleep_row,
        row_number = layout$sleep$first_data_row + index - 1L
      )
    }
  )

  openxlsx::saveWorkbook(
    wb = workbook,
    file = workbook_path,
    overwrite = TRUE
  )
}
