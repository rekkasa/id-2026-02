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
    mechanical_ventilation_start_dates = "02/01/2026",
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
  mechanical_ventilation_start_dates <- rep(
    x = mechanical_ventilation_start_dates,
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
      crf_row[[21L]] <- mechanical_ventilation_start_dates[[index]]
      crf_row[[26L]] <- icu_discharge_dates[[index]]
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
      physiotherapy_row[[30L]] <- "0,40"
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
    duplicate_row[[30L]] <- "0,40"
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
