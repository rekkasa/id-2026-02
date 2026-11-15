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

make_source_row <- function(column_count, source_record_number, patient_id) {
  values <- stringr::str_c("value_", seq_len(column_count))
  values[[1L]] <- source_record_number
  values[[2L]] <- "Synthetic Name"

  if (!is.na(patient_id)) {
    values[[8L]] <- patient_id
  }

  values
}

write_synthetic_workbook <- function(
    workbook_path,
    include_control = TRUE,
    duplicate_physiotherapy = FALSE,
    invalid_physio_header = FALSE) {
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

  crf_row <- make_source_row(
    column_count = layout$crf$column_count,
    source_record_number = "1",
    patient_id = "EAMA-001"
  )
  write_excel_row(
    workbook = workbook,
    sheet_name = layout$crf$sheet_name,
    row_values = crf_row,
    row_number = layout$crf$first_data_row
  )

  physiotherapy_row <- make_source_row(
    column_count = layout$physiotherapy$column_count,
    source_record_number = "1",
    patient_id = NA_character_
  )
  physiotherapy_row[[3L]] <- "2026-01-01"
  physiotherapy_row[[30L]] <- "0,40"
  write_excel_row(
    workbook = workbook,
    sheet_name = layout$physiotherapy$sheet_name,
    row_values = physiotherapy_row,
    row_number = layout$physiotherapy$first_data_row
  )
  if (duplicate_physiotherapy) {
    write_excel_row(
      workbook = workbook,
      sheet_name = layout$physiotherapy$sheet_name,
      row_values = physiotherapy_row,
      row_number = layout$physiotherapy$first_data_row + 1L
    )
  }

  if (include_control) {
    control_row <- make_source_row(
      column_count = layout$control$column_count,
      source_record_number = "1",
      patient_id = NA_character_
    )
    control_row[[3L]] <- "2026-01-02"
    write_excel_row(
      workbook = workbook,
      sheet_name = layout$control$sheet_name,
      row_values = control_row,
      row_number = layout$control$first_data_row
    )
  }

  sleep_row <- make_source_row(
    column_count = layout$sleep$column_count,
    source_record_number = "1",
    patient_id = NA_character_
  )
  write_excel_row(
    workbook = workbook,
    sheet_name = layout$sleep$sheet_name,
    row_values = sleep_row,
    row_number = layout$sleep$first_data_row
  )

  openxlsx::saveWorkbook(
    wb = workbook,
    file = workbook_path,
    overwrite = TRUE
  )
}
