function(x) {
  dplyr::mutate(
    x,

    # Presentation date has to be handled via special-case code
    presentation_date =  lubridate::parse_date_time(
      presentation_date,
      truncated=1,
      orders=c("ymd","m/d/y","y")
    ) |>
      as.Date.POSIXct(),

    presentation_year = as.character(lubridate::year(presentation_date)),

    submission_date = lubridate::parse_date_time(
      submission_date,
      truncated=1,
      orders=c("ymd","m/d/y","y")
    ) |>
      as.Date.POSIXct(),

    submission_year = as.character(lubridate::year(submission_date))
  )


}
