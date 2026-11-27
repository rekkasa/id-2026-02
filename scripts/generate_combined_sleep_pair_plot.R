parse_numeric <- function(values) {
  normalized_values <- values |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_replace_all(
      pattern = ",",
      replacement = "."
    ) |>
    dplyr::na_if(y = "")

  suppressWarnings(
    as.numeric(x = normalized_values)
  )
}

read_sleep_assessments <- function(database_path) {
  if (!fs::file_exists(path = database_path)) {
    rlang::abort(
      message = stringr::str_c(
        "The DuckDB database file does not exist: ",
        database_path
      )
    )
  }

  connection <- DBI::dbConnect(
    drv = duckdb::duckdb(),
    dbdir = database_path,
    read_only = TRUE
  )
  on.exit(
    expr = DBI::dbDisconnect(
      conn = connection,
      shutdown = TRUE
    ),
    add = TRUE
  )

  DBI::dbGetQuery(
    conn = connection,
    statement = stringr::str_c(
      "SELECT patient_id, assessment, total_sleep_time, sleep_latency",
      "FROM sleep_assessments",
      "WHERE assessment IN ('with_physiotherapy',",
      "'without_physiotherapy')",
      "ORDER BY patient_id, assessment",
      sep = " "
    )
  )
}

build_sleep_pair_measurements <- function(sleep_assessments) {
  sleep_assessments |>
    dplyr::select(
      "patient_id",
      "assessment",
      "total_sleep_time",
      "sleep_latency"
    ) |>
    tidyr::pivot_longer(
      cols = c("total_sleep_time", "sleep_latency"),
      names_to = "measurement",
      values_to = "value",
      values_transform = list(value = as.character)
    ) |>
    dplyr::mutate(
      patient_id = as.character(x = .data$patient_id),
      value = parse_numeric(values = .data$value),
      comparison = dplyr::recode(
        .x = .data$assessment,
        with_physiotherapy = "With physiotherapy",
        without_physiotherapy = "Without physiotherapy"
      ),
      comparison = factor(
        x = .data$comparison,
        levels = c("Without physiotherapy", "With physiotherapy")
      )
    ) |>
    dplyr::filter(!is.na(.data$value), !is.na(.data$comparison)) |>
    dplyr::group_by(.data$patient_id, .data$measurement) |>
    dplyr::filter(dplyr::n() == 2L) |>
    dplyr::ungroup()
}

make_paired_measurement_plot <- function(
    data,
    title,
    y_label,
    panel_label) {
  plot_title <- stringr::str_c(panel_label, ". ", title)

  if (nrow(x = data) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = "No complete patient pairs available"
        ) +
        ggplot2::labs(
          title = plot_title,
          x = NULL,
          y = y_label
        ) +
        ggplot2::theme_minimal(base_size = 10)
    )
  }

  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$comparison,
      y = .data$value,
      group = .data$patient_id
    )
  ) +
    ggplot2::geom_line(
      color = "#A6A6A6",
      alpha = 0.65,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(color = .data$comparison),
      shape = 16,
      size = 2.4
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Without physiotherapy" = "#70AD47",
        "With physiotherapy" = "#4472C4"
      ),
      name = NULL
    ) +
    ggplot2::scale_x_discrete(
      limits = c("Without physiotherapy", "With physiotherapy")
    ) +
    ggplot2::labs(
      title = plot_title,
      x = NULL,
      y = y_label
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1L)
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 6),
      legend.position = "top"
    )
}

write_combined_plot <- function(
    total_sleep_plot,
    sleep_latency_plot,
    output_path) {
  output_directory <- fs::path_dir(path = output_path)
  fs::dir_create(
    path = output_directory,
    recurse = TRUE
  )

  # Fits standard A4/Letter content widths without Word downscaling.
  ragg::agg_png(
    filename = output_path,
    width = 6.25,
    height = 3.5,
    units = "in",
    res = 300
  )
  on.exit(
    expr = grDevices::dev.off(),
    add = TRUE
  )

  grid::grid.newpage()
  grid::pushViewport(
    viewport = grid::viewport(
      layout = grid::grid.layout(
        nrow = 1L,
        ncol = 2L
      )
    )
  )
  print(
    x = total_sleep_plot,
    vp = grid::viewport(
      layout.pos.row = 1L,
      layout.pos.col = 1L
    )
  )
  print(
    x = sleep_latency_plot,
    vp = grid::viewport(
      layout.pos.row = 1L,
      layout.pos.col = 2L
    )
  )
  grid::popViewport()

  invisible(NULL)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  usage <- stringr::str_c(
    "Usage: Rscript scripts/generate_combined_sleep_pair_plot.R",
    " <input.duckdb> <output.png>"
  )

  if (length(x = args) != 2L) {
    rlang::abort(message = usage)
  }

  database_path <- args[[1L]]
  output_path <- args[[2L]]

  if (!fs::file_exists(path = database_path)) {
    rlang::abort(
      message = stringr::str_c(
        "The DuckDB database file does not exist: ",
        database_path
      )
    )
  }

  if (stringr::str_to_lower(
    string = fs::path_ext(path = database_path)
  ) != "duckdb") {
    rlang::abort(message = "The input database path must end in .duckdb.")
  }

  if (stringr::str_to_lower(
    string = fs::path_ext(path = output_path)
  ) != "png") {
    rlang::abort(message = "The output path must end in .png.")
  }

  sleep_assessments <- read_sleep_assessments(
    database_path = database_path
  )
  sleep_pair_measurements <- build_sleep_pair_measurements(
    sleep_assessments = sleep_assessments
  )
  total_sleep_data <- sleep_pair_measurements |>
    dplyr::filter(.data$measurement == "total_sleep_time")
  sleep_latency_data <- sleep_pair_measurements |>
    dplyr::filter(.data$measurement == "sleep_latency")

  total_sleep_plot <- make_paired_measurement_plot(
    data = total_sleep_data,
    title = "Total sleep time by patient",
    y_label = "Total sleep time (min)",
    panel_label = "A"
  )
  sleep_latency_plot <- make_paired_measurement_plot(
    data = sleep_latency_data,
    title = "Sleep latency by patient",
    y_label = "Sleep latency (min)",
    panel_label = "B"
  )
  write_combined_plot(
    total_sleep_plot = total_sleep_plot,
    sleep_latency_plot = sleep_latency_plot,
    output_path = output_path
  )

  total_sleep_pair_count <- nrow(
    x = dplyr::distinct(
      .data = total_sleep_data,
      .data$patient_id
    )
  )
  sleep_latency_pair_count <- nrow(
    x = dplyr::distinct(
      .data = sleep_latency_data,
      .data$patient_id
    )
  )
  base::message(
    "Wrote ",
    output_path,
    "; complete pairs - total sleep time: ",
    total_sleep_pair_count,
    ", sleep latency: ",
    sleep_latency_pair_count
  )

  invisible(NULL)
}

if (sys.nframe() == 0L) {
  main()
}
