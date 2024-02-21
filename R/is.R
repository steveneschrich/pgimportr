

#' Determine if any authors are ESI's
#'
#' @description This function provides a way, in mutate, to test if any
#' of the authors are ESI's.
#'
#' @details It is a little tricky, since the authors are nested, to test
#' if any of the authors are ESI's. This code is private to the library
#' since the whole purpose of the function is to create an indicator
#' variable so that we don't need to do this regularly.
#'
#' @param .x
#'
#' @return
#'
#' @examples
is_esi_related <- function(.x) {
  purrr::map_lgl(.x, function(.y) {
    any(.y$partnership_role == "ESI")
  })
}
