source(
  file = fs::path_abs(
    path = fs::path(
      "..",
      "..",
      "scripts",
      "generate_combined_sleep_pair_plot.R"
    )
  ),
  local = globalenv()
)

testthat::test_that(
  desc = "numeric parsing handles comma decimals and blank values",
  code = {
    parsed_values <- parse_numeric(
      values = c(" 1,5 ", "", "not numeric", "2")
    )

    testthat::expect_identical(
      object = parsed_values,
      expected = c(1.5, NA_real_, NA_real_, 2)
    )
  }
)

testthat::test_that(
  desc = "complete pairs are retained independently by sleep outcome",
  code = {
    sleep_assessments <- tibble::tibble(
      patient_id = c("1", "1", "2", "2", "3"),
      assessment = c(
        "without_physiotherapy",
        "with_physiotherapy",
        "without_physiotherapy",
        "with_physiotherapy",
        "without_physiotherapy"
      ),
      total_sleep_time = c("100", "120", "200", "220", "300"),
      sleep_latency = c("", "12,5", "20", "10", "15")
    )

    paired_measurements <- build_sleep_pair_measurements(
      sleep_assessments = sleep_assessments
    )
    total_sleep_data <- paired_measurements |>
      dplyr::filter(.data$measurement == "total_sleep_time")
    sleep_latency_data <- paired_measurements |>
      dplyr::filter(.data$measurement == "sleep_latency")

    testthat::expect_identical(
      object = unique(x = total_sleep_data$patient_id),
      expected = c("1", "2")
    )
    testthat::expect_identical(
      object = unique(x = sleep_latency_data$patient_id),
      expected = "2"
    )
    testthat::expect_identical(
      object = levels(x = paired_measurements$comparison),
      expected = c("Without physiotherapy", "With physiotherapy")
    )
  }
)

testthat::test_that(
  desc = "paired plots label panels without subtitles or legend titles",
  code = {
    paired_data <- tibble::tibble(
      patient_id = c("1", "1"),
      measurement = c("total_sleep_time", "total_sleep_time"),
      value = c(100, 120),
      comparison = factor(
        x = c("Without physiotherapy", "With physiotherapy"),
        levels = c("Without physiotherapy", "With physiotherapy")
      )
    )
    total_sleep_plot <- make_paired_measurement_plot(
      data = paired_data,
      title = "Total sleep time by patient",
      y_label = "Total sleep time (min)",
      panel_label = "A"
    )
    sleep_latency_plot <- make_paired_measurement_plot(
      data = paired_data,
      title = "Sleep latency by patient",
      y_label = "Sleep latency (min)",
      panel_label = "B"
    )

    testthat::expect_identical(
      object = total_sleep_plot$labels$title,
      expected = "A. Total sleep time by patient"
    )
    testthat::expect_identical(
      object = sleep_latency_plot$labels$title,
      expected = "B. Sleep latency by patient"
    )
    testthat::expect_null(
      object = total_sleep_plot$labels$subtitle
    )
    testthat::expect_null(
      object = sleep_latency_plot$labels$subtitle
    )
    testthat::expect_null(
      object = total_sleep_plot$scales$scales[[1L]]$name
    )
    testthat::expect_identical(
      object = total_sleep_plot$guides$guides[["colour"]]$params$nrow,
      expected = 1L
    )
    testthat::expect_identical(
      object = total_sleep_plot$theme$plot.title$size,
      expected = 5
    )
    testthat::expect_identical(
      object = total_sleep_plot$theme$axis.title$size,
      expected = 4
    )
    testthat::expect_identical(
      object = total_sleep_plot$theme$axis.text$size,
      expected = 3
    )
    testthat::expect_identical(
      object = total_sleep_plot$theme$legend.text$size,
      expected = 2.5
    )
    testthat::expect_s3_class(
      object = extract_legend_grob(plot = total_sleep_plot),
      class = "gtable"
    )
    empty_plot <- make_paired_measurement_plot(
      data = tibble::tibble(
        patient_id = character(),
        measurement = character(),
        value = numeric(),
        comparison = factor(character())
      ),
      title = "Total sleep time by patient",
      y_label = "Total sleep time (min)",
      panel_label = "A"
    )
    testthat::expect_null(
      object = extract_legend_grob(plot = empty_plot)
    )
  }
)

testthat::test_that(
  desc = "combined paired plots are written as a non-empty JPEG",
  code = {
    paired_data <- tibble::tibble(
      patient_id = c("1", "1"),
      measurement = c("total_sleep_time", "total_sleep_time"),
      value = c(100, 120),
      comparison = factor(
        x = c("Without physiotherapy", "With physiotherapy"),
        levels = c("Without physiotherapy", "With physiotherapy")
      )
    )
    output_path <- fs::file_temp(ext = ".jpg")
    on.exit(
      expr = {
        if (fs::file_exists(path = output_path)) {
          fs::file_delete(path = output_path)
        }
      },
      add = TRUE
    )
    total_sleep_plot <- make_paired_measurement_plot(
      data = paired_data,
      title = "Total sleep time by patient",
      y_label = "Total sleep time (min)",
      panel_label = "A"
    )
    sleep_latency_plot <- make_paired_measurement_plot(
      data = paired_data,
      title = "Sleep latency by patient",
      y_label = "Sleep latency (min)",
      panel_label = "B"
    )

    write_combined_plot(
      total_sleep_plot = total_sleep_plot,
      sleep_latency_plot = sleep_latency_plot,
      output_path = output_path
    )

    testthat::expect_true(object = fs::file_exists(path = output_path))
    testthat::expect_gt(
      object = fs::file_info(path = output_path)$size,
      expected = 0
    )
    image_bytes <- readBin(
      con = output_path,
      what = "raw",
      n = fs::file_info(path = output_path)$size
    )
    image_values <- as.integer(x = image_bytes)
    frame_positions <- which(
      image_values[-length(x = image_values)] == 255L &
        image_values[-1L] == 192L
    )
    frame_position <- frame_positions[[1L]]
    image_height <- image_values[[frame_position + 5L]] * 256L +
      image_values[[frame_position + 6L]]
    image_width <- image_values[[frame_position + 7L]] * 256L +
      image_values[[frame_position + 8L]]

    testthat::expect_identical(
      object = image_bytes[1L:2L],
      expected = as.raw(x = c(255, 216))
    )
    testthat::expect_identical(object = image_width, expected = 945L)
    testthat::expect_identical(object = image_height, expected = 591L)
  }
)

testthat::test_that(
  desc = "the command-line interface requires two positional arguments",
  code = {
    testthat::expect_error(
      object = main(args = character()),
      regexp = "Usage: Rscript scripts/generate_combined_sleep_pair_plot.R"
    )
  }
)
