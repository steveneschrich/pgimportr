function(x) {
  dplyr::mutate(
    x,

    # Publication date has to be handled via special-case code
    publication_date =  lubridate::parse_date_time(
      publication_date,
      truncated=1,
      orders=c("ymd","m/d/y","y")
    ) |>
      as.Date.POSIXct(),

    publication_year = as.character(lubridate::year(publication_date))
  )


}
