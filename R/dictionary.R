#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
create_dictionary <- function(x) {
  checkmate::assert_data_frame(x)
  checkmate::assert_choice("select_choices_or_calculations", colnames(x))

  dplyr::mutate(x, Levels = split_choices(select_choices_or_calculations))
}
