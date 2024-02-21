#' Title
#'
#' @param pg
#'
#' @return
#' @export
#'
#' @examples
program_grant <- function(pg = getOption("pgimportr.program_grant")) {
  if (is.null(pg)) {
    cli::cli_alert_warning("Program grant configuration is not set, please refer to pgimportr::get_config().")
  }
  pg
}






#' Given a Partnership Grant funding year, return the start date of that year.
#'
#' @param yr The funding year - default is the initial grant year ("Y01")
#'
#' @return The start date (as a lubridate) of the funding year.
#' @export
#'
#'
pg_start_date<-function(year="Y01", pg = program_grant()) {

  dplyr::pull(
    pg_project_period(year = year, pg = pg),
    "start_date"
  )

}

#' Create a program grant object from a configuration list
#'
#' @param pg
#'
#' @return
#'
#' @examples
new_program_grant <- function(pg) {

  # Modify the grant years to a table with some additional
  # date math.
  gy <- purrr::map(pg$grant_years, tibble::as_tibble) |>
    purrr::list_rbind(names_to="name") |>
    dplyr::mutate(
      start_date = lubridate::ymd(start_date),
      end_date = lubridate::ymd(end_date),
      interval = lubridate::interval(start = start_date, end = end_date)
    )
  pg$grant_years <- gy

  pg
}





#' Convert a calendar date to a Program Grant year.
#'
#' Grant years are often not aligned to calendar years or to fiscal years.
#' They represent their own internal clock. Here, the Program Grant
#' is the reason for reporting on activity within the Program. Therefore,
#' it is important to track the Program Grant (pg) year.
#'
#' The logic implemented in this function is straight-forward. If the date is
#' less than the start date of a given pg year, then it is the previous
#' year. This works since each year is tested in increasing order.
#'
#' Note that dates starting earlier than Y01 start date are considered
#' Y00.
#'
#' @param d A lubridate-style date.
#' @param as_factor Convert the string to a factor, based on all available grant years.
#' @return A string representing the pg year, or NA if not defined.
#' @export
#'
#' @examples
#'
convert_date_to_pg_year<-function(d, as_factor = FALSE) {
  yrs <- purrr::map_chr(d, pg_project_year)
  if ( as_factor )
    yrs <- factor(yrs, levels=pg_project_years())

  yrs
}



#' Project year from program grant
#'
#' @param d
#' @param year
#' @param pg
#'
#' @return
#' @export
#'
#' @examples
pg_project_year <- function(d = NULL, year = NULL, pg = program_grant(), as_factor = FALSE) {
  yrs <- dplyr::pull(pg_project_period(d = d, year = year,  pg = pg), "name")

  if ( as_factor ) {
    yrs <- factor(yrs, levels = pg_project_years())
  }
  yrs
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
pg_project_years <- function() {
  pg_project_year()
}

#' Project period information from program grant
#'
#' @description Retrieve program grant information for the different
#' periods of activity as a table.
#'
#' @details The program project has activity across a number of years. This
#' function can provide the information about these years. Note that the time
#' periods are not necessarily aligned with calendar, fiscal or other boundaries.
#'
#' @param d An optional date to report program grant project period
#' @param year An optional named year for the program project
#' @param pg A program grant (optional)
#'
#' @return A table consisting of information about the requested project period.
#' @export
#'
#' @examples
pg_project_period <- function(d = NULL, year = NULL, pg = program_grant()) {

  pgy <- pg$grant_years
  if ( !is.null(d) ) {
    d <- lubridate::ymd(d)


    # Given dates (d), lookup the grant year name
    # First, identify which entries are within the target date (date-by-date).
    pgy_which <- purrr::map(d, \(.date) {
      which(lubridate::`%within%`(lubridate::ymd(.date), pgy$interval))
    })
    # Pick the first match (arbitrarily)
    pgy_which <- purrr::map_int(pgy_which, \(.w){.w[1]})
    # Then map to name
    pgy_name <- pgy$name[pgy_which]

    # Put together the dates and names, then join into grant table to get
    # annotated mapping
    pgy <- tibble::tibble(
      target_date = d,
      name = pgy_name
    ) |>
      dplyr::left_join(pg$grant_years, by="name")

  } else if ( !is.null(year) ) {
    # Other option, the year is provided for lookup (one or more)
    pgy <- dplyr::filter(
      pgy,
      name %in% year
    )
  }

  pgy
}

#' Return the last year of the Partnership Grant
#'
#' @return An string representing the last (max) grant year.
#' @export
#'
#' @examples
pg_final_year<-function(pg = program_grant()) {
  tail(pg_project_year(pg = pg),1)
}

#' Title
#'
#' @param pg
#'
#' @return
#' @export
#'
#' @examples
pg_final_period <- function(pg = program_grant()) {
  dplyr::slice_tail(pg_project_period(pg = pg), n=1)
}

#' Title
#'
#' @param pg
#'
#' @return
#' @export
#'
#' @examples
pg_cores <- function(pg = program_grant()) {
  pg$cores
}

#' Title
#'
#' @param pg
#'
#' @return
#' @export
#'
#' @examples
pg_other_support <- function(pg = program_grant()) {
  pg$other_support
}

#' Title
#'
#' @param pg
#'
#' @return
#' @export
#'
#' @examples
pg_support <- function(pg = program_grant()) {
  c(pg_cores(), pg_other_support())
}
