#' Selecting variables for object
#' @rdname select
#' @name select
#' @description Tools for selecting column subsets from raw data
#' @details
#' There are a number of columns available for processing in the grants,
#' publications and presentations. Some are specific to the grant, some
#' are specific to the investigator (individual). These routines help
#' select the various subsets. Specifically, they are implemented by
#' 'creator': investigator, author or presenter. There is an option to
#' negate the selection, allowing for both selecting of the grant/pub or
#' the creator(s).
#'
#'
NULL



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
select_investigator_columns <- function(x, negate = FALSE, verbose = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_named(x)
  checkmate::assert_subset("record_id", colnames(x))

  # Figure out the investigator columns by examining colnames
  cn <- colnames(x)

  # This is rather custom code and will have to be tweaked as the
  # forms change.
  selected_cn <- cn[
    which(
      stringr::str_starts(cn, "investigator_") |
        cn %in% c("institution_other","role_other","partnership_role_other",
                  "redcap_repeat_instance", "redcap_repeat_instrument")
    )
  ]

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
#' @param x A data frame imported from redcap form
#' @param negate (FALSE) Should redcap columns be extracted (FALSE) or removed (TRUE)
#' @param verbose Report the number of columns removed to console
#'
#' @return A data frame with the REDCap-related columns selected (or removed).
#'
#' @export
select_redcap_control_columns <- function(x, negate = FALSE, verbose = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_named(x)
  checkmate::assert_subset("record_id", colnames(x))

  cn <- colnames(x)
  control_columns <- cn[
    which(
      # Don't remove these too soon as these indicate investigator vs. grant
      stringr::str_starts(cn, "redcap_repeat_") |
        # These can be removed anytime (redcap columns)
        stringr::str_ends(cn, "_complete")
    )
  ]
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
#' @param x A tibble containing publications and authors.
#' @param negate (FALSE) Should author columns be extracted (FALSE) or removed (TRUE)
#' @param verbose Report the number of columns removed to console
#' @return A data frame with the Author-related columns selected (or removed).
#'
#' @importFrom rlang .data
#'
#' @export
select_author_columns<-function(x, negate = FALSE, verbose = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_named(x)
  checkmate::assert_subset("record_id", colnames(x))

  # Figure out the author columns by examining colnames
  cn <- colnames(x)

  # This is rather custom code and will have to be tweaked as the
  # forms change.
  selected_cn <- cn[
    which(
      stringr::str_starts(cn, "author_") |
        cn %in% c(
          "partnership_role_other",
          "redcap_repeat_instance"
        )
    )
  ]

  if ( negate )
    x <- dplyr::select(x, record_id, dplyr::everything(), -dplyr::all_of(selected_cn))
  else
    x <- dplyr::select(x, record_id, dplyr::all_of(selected_cn))

  if ( verbose ) cli::cli_alert_success("Removed {length(cn) - ncol(x)} columns from raw redcap data (author-related).")

  x

}


#' Remove presenter fields from presentations tibble
#'
#' @description The presentation table includes rows (and columns)
#' representing presenters (one row per presenter). This function keeps
#' the rows and columns belonging to presenters specifically.
#'
#' @param x A tibble containing presentations and presenters
#' @param negate (FALSE) Should presenter columns be extracted (FALSE) or removed (TRUE)
#' @param verbose Report the number of columns removed to console
#'
#' @return A data frame with the Presenter-related columns selected (or removed).
#'
#' @importFrom rlang .data
#'
#' @export
select_presenter_columns<-function(x, negate = FALSE, verbose = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_named(x)
  checkmate::assert_subset("record_id", colnames(x))

  # Figure out the author columns by examining colnames
  cn <- colnames(x)

  # This is rather custom code and will have to be tweaked as the
  # forms change.
  selected_cn <- cn[
    which(
      stringr::str_starts(cn, "presenter_") |
        cn %in% c(
          "institution_other",
          "partnership_role_other",
          "rec_activity",
          "rec_activity_year",
          "redcap_repeat_instance"
        )
    )
  ]


  if ( negate )
    x <- dplyr::select(x, record_id, dplyr::everything(), -dplyr::all_of(selected_cn))
  else
    x <- dplyr::select(x, record_id, dplyr::all_of(selected_cn))

  if ( verbose ) cli::cli_alert_success("Removed {length(cn) - ncol(x)} columns from raw redcap data (presenter-related).")

  x

}


