#' Test if data frame is a grant table
#'
#' @description Indicate if the passed data frame is a
#' grant table.
#'
#' @details A grant table is a data frame (tibble) that has
#' certain columns available for processing. For convenience,
#' we attach an attribute to the table for indicating this
#' fact.
#'
#' @param x A data frame
#'
#' @return Logical indicating if data frame is a grant table
#' @export
#'
#' @examples
is_grant_table <- function(x) {
  is(x, "grant_table")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_publication_table <- function(x) {
  is(x, "publication_table")
}


#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
as_grant_table <- function(x, source = NULL) {
  checkmate::assert_data_frame(x)

  if ( is.null(source) ) {
    source <- list(raw=NULL, label=NULL, metadata=NULL)
  }
  x <- tibble::new_tibble(x, class = "grant_table")
  attr(x, "source") <- source
  attr(x, "dictionary") <- create_dictionary(source$metadata)

  x
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
dictionary.grant_table <- function(x) {
  attr(x, "dictionary")
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dictionary <- function (x, ...) {
  UseMethod("dictionary", x)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
dictionary.publication_table <- function(x) {
  attr(x,  "dictionary")
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_publication_table <- function(x, source = NULL) {
  checkmate::assert_data_frame(x)

  if ( is.null(source) ) {
    source <- list(raw=NULL, label=NULL, metadata=NULL)
  }
  x <- tibble::new_tibble(x, class = "publication_table")
  attr(x, "source") <- source
  attr(x, "dictionary") <- create_dictionary(source$metadata)

  x
}
