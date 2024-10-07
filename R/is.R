

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
#is_esi_related <- function(.x) {
#  purrr::map_lgl(.x, function(.y) {
#    any(.y$partnership_role == "ESI")
#  })
#}



#' Determine if any people are Current ESI status
#' @description This function provides a way, in mutate, to test if any
#' of the authors are Current ESI status.
#'
#' @details It is a little tricky, since the authors are nested, to test
#' if any of the authors are ESI's. This code is private to the library
#' since the whole purpose of the function is to create an indicator
#' variable so that we don't need to do this regularly.
#'
#' @param people A data frame of investigators or authors
#'
#' @return A logical indicating if any of the people are Current ESI
#' @export
#'
#' @examples
#' \dontrun{calculate_is_current_esi_related(investigators[[1]])}
calculate_is_current_esi_related <- function(people) {
  any(people[["isPartnershipRole_Current ESI"]])
}
#'  Determine if any people are Former ESI status
#'
#' @param people A data frame of investigators or authors
#' @description This function provides a way, in mutate, to test if any
#' of the authors are Former ESI status.
#'
#' @details It is a little tricky, since the authors are nested, to test
#' if any of the authors are ESI's. This code is private to the library
#' since the whole purpose of the function is to create an indicator
#' variable so that we don't need to do this regularly.
#'
#' @return A logical indicating if any of the people are Former ESI
#' @export
#'
#' @examples
#' \dontrun{calculate_is_former_esi_related(investigators[[1]])}
calculate_is_former_esi_related <- function(people) {
  any(people[["isPartnershipRole_Former ESI"]])
}
#' Determine if any people are ESI status
#' @description This function provides a way, in mutate, to test if any
#' of the authors are ESI (Current or Former).
#'
#' @details It is a little tricky, since the authors are nested, to test
#' if any of the authors are ESI's. This code is private to the library
#' since the whole purpose of the function is to create an indicator
#' variable so that we don't need to do this regularly.

#' @param people
#'
#' @return A data frame of investigators or authors
#' @export
#'
#' @examples
#' \dontrun{calculate_is_esi_related(investigators[[1]])}
calculate_is_any_esi_related <- function(people) {
  calculate_is_current_esi_related(people) | calculate_is_former_esi_related(people)
}


