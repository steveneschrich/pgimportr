function(x) {
  dplyr::mutate(
    x,
    # Grant submission date
    submission_date=lubridate::ymd(date),

    # Create specific funding start/stop dates from the project_period
    funding_start_date =
      lubridate::mdy(split_date_range(`project_period`)[,1]),
    funding_end_date =
      lubridate::mdy(split_date_range(`project_period`)[,2])
  ) |>
    dplyr::select(-date, -project_period)
}
