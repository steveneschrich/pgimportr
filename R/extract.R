#' Create a table specific for grant information.
#'
#' Grant information is the bulk of the raw data, with investigators
#' as extra rows (to be deleted here).
#'
#' The data is formatted for data entry, not reporting, so there are
#' many different columns representing the same type of information.
#' Therefore, a significant amount of this function involves coalescing this
#' data into single columns.
#'
#' @param data Raw source data
#'
#' @return A tibble corresponding to grants.
#'
#' @importFrom rlang .data
#' @examples
extract_grants <-function(redcap_data) {

  checkmate::assert_list(redcap_data)
  checkmate::assert_subset("label",names(redcap_data))

  # Use the labelled data. The redcap_repeat_instrument is either NA (a grant)
  # or "Investigators".
  x <- dplyr::filter(redcap_data$label, is.na(redcap_repeat_instrument))

  # Remove investigator-related columns
  x <- select_investigator_columns(x, negate = TRUE)

  # Remove control columns
  x <- select_redcap_control_columns(x, negate = TRUE)

  x
}

#' Create a table specific for grant investigator information.
#'
#' Grant investigator information is repeated rows from the raw data
#'
#' The data is formatted for data entry, not reporting, so there are
#' many different columns representing the same type of information.
#'
#' @param data Raw source data
#'
#' @return A tibble corresponding to investigators.
#'
#' @examples
extract_investigators <- function(redcap_data) {

  checkmate::assert_list(redcap_data)
  checkmate::assert_subset("label", names(redcap_data))

  # Use the labelled data. The redcap_repeat_instrument is either NA (a grant)
  # or "Investigators".
  x <- dplyr::filter(redcap_data$label, redcap_repeat_instrument == "Investigators")


  # Remove non-investigator columns.
  x <- select_investigator_columns(x, negate = FALSE)


  x
}


extract_authors <- function(redcap_data) {
  checkmate::assert_list(redcap_data)
  checkmate::assert_subset("label", names(redcap_data))

  # Use the labelled data. The redcap_repeat_instrument is either NA (a publication)
  # or "Author".
  x <- dplyr::filter(redcap_data$label, redcap_repeat_instrument == "Authors")


  # Remove non-author columns.
  x <- select_author_columns(x, negate = FALSE)


  x

}

extract_publications <- function(redcap_data) {
  checkmate::assert_list(redcap_data)
  checkmate::assert_subset("label",names(redcap_data))

  # Use the labelled data. The redcap_repeat_instrument is either NA (a publication)
  # or "Authors".
  x <- dplyr::filter(redcap_data$label, is.na(redcap_repeat_instrument))

  # Remove author-related columns
  x <- select_author_columns(x, negate = TRUE)

  # Remove control columns
  x <- select_redcap_control_columns(x, negate = TRUE)

  x
}

extract_presentations <- function(redcap_data) {
  checkmate::assert_list(redcap_data)
  checkmate::assert_subset("label",names(redcap_data))

  # Use the labelled data. The redcap_repeat_instrument is either NA (a publication)
  # or "Authors".
  x <- dplyr::filter(redcap_data$label, is.na(redcap_repeat_instrument))

  # Remove presenter-related columns
  x <- select_presenter_columns(x, negate = TRUE)

  # Remove control columns
  x <- select_redcap_control_columns(x, negate = TRUE)

  x
}
extract_presenters <- function(redcap_data) {
  checkmate::assert_list(redcap_data)
  checkmate::assert_subset("label", names(redcap_data))

  # Use the labelled data. The redcap_repeat_instrument is either NA (a publication)
  # or "Author".
  x <- dplyr::filter(redcap_data$label, redcap_repeat_instrument == "Presenters")


  # Remove non-author columns.
  x <- select_presenter_columns(x, negate = FALSE)


  x

}

