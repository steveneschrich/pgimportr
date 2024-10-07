#' Dispatch transformation of raw data
#' @description A series of ETL scripts applied to raw input data
#'
#' @details The derivation of grants, publications and presentations occurs
#'  via a series of transformation scripts. These functions perform this
#'  ETL.
#'
#'  The overall process is:
#'   - Download REDCap data and data dictionary
#'   - Convert this data (as tibbles) into an object (grants, publications, presentations).
#'      Note this is mostly to keep the data dictionary with the object, and to help in
#'      data verification via typing.
#'   - Apply a series of ETL scripts to object in order to transform it into usable
#'      reporting data.
#'
#'  These functions are the last step - transforming the raw input into usable fields.
#'  Steps include things like converting checkboxes into structured variables, lists,
#'  normalizing variable names and other pieces.
#'
#' Code is included with the package as separate R files for each step of the
#' transformation (numbered so that the process occurs in order). That is, there
#' is a package `R.d/publications` directory with a number of scripts of the
#' form `05-do_this_operation.R`. Each contains a function that accepts a data
#' frame as input and returns a data frame, presumably somewhat transformed by
#' operations within the function.
#'
#' The process is generic enough that additional `R.d/publications` entries
#' can be added/modified where needed without impacting these driver functions.
#'
#' @param x A raw REDCap data frame to transform
#'
#' @return A transformed object (grants, publications, presentations) after
#'   appropriate ETL.
#' @export
#'
#' @examples
transform <- function(obj) {
  which <- dplyr::case_when(
    methods::is(obj, "grant_table") ~ "grants",
    methods::is(obj, "publication_table") ~ "publications",
    methods::is(obj, "presentation_table") ~ "presentations",
    .default = "unrecognized"
  )

  stopifnot(
    "Error in transformation - object provided is not a grant, publication or presentation table" =
      which %in% c("grants","publications","presentations")
  )

  cli::cli_h1("Transforming {which}")
  transformers <- fs::dir_ls(
    path = system.file(
      glue::glue("R.d/{which}"),
      package="pgimportr"
    ),
    # Only NN-scriptname.R formatted files.
    regexp = "/\\d+-.*\\.R$"
  )
  names(transformers) <- basename(transformers) |>
    stringr::str_remove("\\.R$")
  flist <- purrr::map(transformers, function(f) {
    source(f, local=new.env())[["value"]]
  })

  obj <- fapply(obj, flist, verbose =TRUE)
  cli::cli_alert_success("{stringr::str_to_title(which)} transformed")

  obj
}

