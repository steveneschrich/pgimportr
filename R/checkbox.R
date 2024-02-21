
#' Convert checkbox fields to indicator variables
#'
#' A checkbox field consists of many different fields with values Checked
#' and Unchecked. This function converts this to more human-readable form.
#'
#' @details This function performs several related tasks to transform a
#' checkbox (set of columns) into several human-readable columns. It is
#' highly customized for the specific data input and not a general-purpose
#' function.
#'
#' @param .x Data frame to operate on
#' @param raw_prefix The checkbox variable prefix (excluding the ___) in the original table
#' @param indicator_prefix The prefix to use in for human-readable flags. It will be prepended
#' to the variable names, with a '_' separating it.
#'
#' @return Data frame with new columns representing human-readable indicators.
#'
#' @examples
convert_checkbox_to_flags <- function(.x, raw_prefix, indicator_prefix, dictionary) {

  stopifnot(length(dplyr::starts_with(raw_prefix, vars=colnames(.x)))>0)

  .x |>
    # Change core attribution columns to value, not checkbox status
    dplyr::mutate(dplyr::across(dplyr::starts_with(raw_prefix),
                                ~dplyr::recode(
                                  .x,
                                  "Checked" =map_checkbox_index_to_label(
                                    dplyr::cur_column(),
                                    dictionary
                                  ),
                                  .default = NA_character_
                                )
    )) |>
    # Rename checkbox field to include label, not index.
    dplyr::rename_with(
      .fn = \(cn) { rename_field_from_checkbox_index(cn, dictionary) },
      .cols = dplyr::starts_with(raw_prefix)
    ) |>

    dplyr::mutate(dplyr::across(
      dplyr::starts_with(raw_prefix),
      ~!is.na(.x),
      .names="{indicator_prefix}_{stringr::str_remove(.col,paste0(raw_prefix,'___'))}"
    ))

}


#' Dictionary routines
#'
#' There is a data dictionary (metadata) that can be extracted from REDCap,
#' documenting the instruments and variables. This is important for mapping
#' some fields that are either in coded, checkbox or other forms.
#'
#' The functions in `dictionary.R` provide ways to extract information from
#' the data dictionary and to translate survey data (using the dictionary)
#' into usable form.
#'
#' @name dictionary
NULL
#> NULL


#' Return the choices for a field from a grant/pub table
#'
#' @param x A grant/pub table
#' @param field The field name to return values for
#'
#' @return
#'
#' @examples
get_field_choices <- function(x, field) {


  # NB: What happens if not a checkbox? A little messy, NULL for a variable
  # not in the data, a NA for a non-checkbox. Document this.

  x |>
    dplyr::filter( field_name == field ) |>
    dplyr::pull("Levels") |>
    unlist()
}


#' Map a checkbox variable into levels
#'
#' @param .x
#' @param dictionary
#' @param prefix
#'
#' @return
#'
#' @examples
map_checkbox <- function(.x, dictionary, target, prefix = "manu_supported_cores") {

  choices <- get_field_choices(dictionary, prefix)
  .x |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(prefix),
        map_checkbox_choice,
        choices
      )) |>
    tidyr::unite(!!target,dplyr::starts_with(prefix), sep=", ", na.rm=TRUE)
}


#' Map checkbox choice to label
#'
#' @description A checkbox choice is a field that can be either Checked
#' or Unchecked. Transform this entry to the corresponding label text.
#'
#' @details In REDCap, a checkbox is represented as a series of fields
#' with the choice index appended. That is, it is of the form
#' ```
#' checkbox___1
#' ```
#' The data dictionary (metadata) provides a lookup of what 1 is labeled. The
#' content of the field is NA, Checked or Unchecked.
#'
#' This function will take a column of data from a tibble assumed to be in
#' checkbox form. It requires a tibble of choices, to map the Checked
#' value to a corresponding Label value.
#'
#' @param .x
#' @param .choices
#'
#' @return
#'
#' @examples
map_checkbox_choice<-function(.x, .choices) {

  # NA out the Unchecked option
  .x <- dplyr::na_if(.x, "Unchecked")
  # The value of the column selected embeds the index that can be mapped
  # to .choices.
  n <- .choices[stringr::str_extract(dplyr::cur_column(), "(?<=___)\\d+")]

  ifelse(.x == "Checked",n,"")

}


#' Title
#'
#' @param s
#' @param dictionary
#'
#' @return
#'
#' @examples
map_checkbox_index_to_label <- function(s, dictionary) {
  n <- extract_checkbox_index(s)

  label <- get_field_choices(dictionary, unique(stringr::str_remove(s,"___\\d+")))

  mapped_labels <- label[n]

  unname(mapped_labels)
}

#' Title
#'
#' @param s
#'
#' @return
#'
#' @examples
extract_checkbox_index <- function(s) {
  stringr::str_extract(s, "(?<=___)\\d+")
}

#' Title
#'
#' @param x
#' @param dictionary
#'
#' @return
#'
#' @examples
rename_field_from_checkbox_index <- function(x, dictionary) {
  n <- map_checkbox_index_to_label(x, dictionary)

  stringr::str_replace(x, "___\\d+",paste0("___",n))
}



