#' Split multiple choices into discrete fields
#'
#' @description A selection is a series of choices that can be selected. This
#' function extracts them as a tibble (since each choice is itself coded).
#'
#' @details This code is vectorized, which can be a little confusing in the context
#' of embedded index/value lists. Briefly, a choice (here) is of the form:
#' ```
#' "1, Choice A | 2, Choice B | 3, Choice C"
#' ```
#'
#' This function will take the choice text and convert it to a list of choices:
#' ```
#' [1] "1, Choice A" "2, Choice B"
#' [3] "3, Choice C"
#' ```
#'
#' This choice can be split apart into a named list with [split_choice_levels()]. That is,
#' ```
#'
#' ```
#'
#' This function will take multiple character vectors with embedded choice text,
#' extract the individual choices and convert each character vector a named list.
#'
#' @note If the input is a single choice text blob, then a single named vector
#' is returned. Otherwise, it is a list of named vectors (corresponding to the
#' input length).
#'
#' @param s A string to split into choices (vectorized).
#'
#' @return A tibble of `Value` and `Label` fields for all choices.
#'
#' @export
#' @examples
split_choices <- function(s) {

  # Extract text elements (choices) separated by '|' and then call
  # split levels on the list of individual
  choices <- stringi::stri_split_fixed(s, "|") |>
    purrr::map(split_choice_levels)

  # Special case, if only a single element return the contents (not the list)
  if (length(choices) == 1)
    choices[[1]]
  else
    choices
}

#' Split choice levels in discrete columns
#'
#' @description REDCap choices are in the form of N, VALUE. This separates out
#' the two components, trims them and returns them as a tibble.
#'
#' @param s A string to split into Value and Label (vectorized).
#'
#' @return A tibble with `Value` and `Label` fields.
#'
#' @examples
split_choice_levels <- function(s) {

  if (all(is.na(s)))
    return(NA)
  if (!methods::is(s, "character"))
    s <- purrr::map_chr(s, 1)

  # Split on commas and clean up leading/trailing spaces. Then, list_transpose
  # will give us two vectors: names and values.
  choices <- stringi::stri_trim_both(s) |>
    stringi::stri_remove_empty() |>
    stringi::stri_split_fixed(pattern=",", omit_empty = TRUE,n=2) |>
    purrr::map(stringi::stri_trim_both) |>
    purrr::list_transpose()

  rlang::set_names(choices[[2L]], as.character(choices[[1L]]))

}

