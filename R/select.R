#' Select Investigator-related columns from data frame
#'
#'
#' @param x A data frame imported from redcap form
#' @param negate (FALSE) Should investigator columns be extracted (FALSE) or removed (TRUE)
#' @param verbose Report the number of columns removed to console
#'
#' @return A data frame with the Investigator-related columns selected (or removed).
#' @export
#'
#' @examples
select_investigator_columns <- function(x, negate = FALSE, verbose = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_named(x)
  checkmate::assert_subset("record_id", colnames(x))

  # Figure out the investigator columns by examining colnames
  cn <- colnames(x)

  # This is rather custom code and will have to be tweaked as the
  # forms change.
  selected_cn <- which(
    stringr::str_starts(cn, "investigator_") |
      cn %in% c("institution_other","role_other","partnership_role_other",
                "redcap_repeat_instance", "redcap_repeat_instrument")
  )


  if ( negate )
    x <- dplyr::select(x, record_id, dplyr::everything(), -selected_cn)
  else
    x <- dplyr::select(x, record_id, selected_cn)

  if ( verbose ) cli::cli_alert_success("Removed {length(cn) - ncol(x)} columns from raw redcap data (investigator-related).")

  x
}



#' Select REDCap-related columns
#'
#' @description There are a few redcap control columns in the raw export
#' that may not be not needed (beyond initial processing).
#'
#' @param x
#' @param negate
#' @param verbose
#'
#' @return
#'
select_redcap_control_columns <- function(x, negate = FALSE, verbose = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_named(x)
  checkmate::assert_subset("record_id", colnames(x))

  cn <- colnames(x)
  control_columns <- which(
    # Don't remove these too soon as these indicate investigator vs. grant
    stringr::str_starts(cn, "redcap_repeat_") |
      # These can be removed anytime (redcap columns)
      stringr::str_ends(cn, "_complete")
  )
  if ( negate )
    x <- dplyr::select(x, record_id, dplyr::everything(), -control_columns)
  else
    x <- dplyr::select(x, record_id, control_columns)


  if ( verbose ) cli::cli_alert_success("Removed {length(cn) - ncol(x)} columns from raw redcap data (redcap-related).")


  x

}


#' Remove author fields from publication tibble
#'
#' @description The publication table includes rows (and columns)
#' representing authors (one row per author). This function keeps
#' the rows and columns belonging to authors specifically.
#'
#' @param d A tibble containing publications and authors.
#'
#' @return A modified tibble with publication entries removed.
#'
#' @importFrom rlang .data
#'
#' @examples
select_author_columns<-function(x, negate = FALSE, verbose = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_named(x)
  checkmate::assert_subset("record_id", colnames(x))

  # Figure out the author columns by examining colnames
  cn <- colnames(x)

  # This is rather custom code and will have to be tweaked as the
  # forms change.
  selected_cn <- which(
    stringr::str_starts(cn, "author_") |
      cn %in% c(
        "partnership_role_other",
        "redcap_repeat_instance"
      )
  )


  if ( negate )
    x <- dplyr::select(x, record_id, dplyr::everything(), -dplyr::all_of(selected_cn))
  else
    x <- dplyr::select(x, record_id, dplyr::all_of(selected_cn))

  if ( verbose ) cli::cli_alert_success("Removed {length(cn) - ncol(x)} columns from raw redcap data (author-related).")

  x

}
