
#' Split dates that are in the form of start_date - end_date.
#'
#' @param s A character vector representing the combined date.
#'
#' @return Two character vectors representing the first and second date, respectively.
#' @export
#'
split_date_range<-function(s) {
  stringr::str_split_fixed(s, "-", 2)
}
