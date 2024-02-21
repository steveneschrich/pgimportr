#' Import pgreportr object
#'
#' @description General-purpose routines for importing data
#' from REDCap for pgreportr library
#'
#' @details The `pgreportr` library manages two different object
#' types: grants and publications. The overall process is to import
#' this data directly from REDCap (using the [REDCapR::redcap_read()])
#' function. And then perform a number of data manipulations on the
#' imported data to make it suitable for later reporting.
#'
#' Therefore each individual import: [import_grants()]
#' and [import_publications()] will call [read_redcap_data()]
#' and then do followup processing. See the help items for
#' [import_grants()] and [import_publications()] for specifics.
#'
#' Where possible, common functionality, code and terminology is used
#' between objects. For instance, the concept of a program grant
#' year, creators (whether grant investigators or publication authors).
#'
#'
import <- function(uri, token, which = c("grants","publications"),
                   use_cache = TRUE, verbose = TRUE) {

  which <- match.arg(which)

  if ( which == "grants") {
    x <- import_grants(uri, token, use_cache = use_cache, verbose = verbose)
  } else {
    x <- import_publications(uri, token, use_cache = use_cache, verbose = verbose)
  }

  x
}





#' Import REDCap export data into R for reporting.
#'
#' @description The REDCap data can be exported in R format. This consists of an
#' R script and corresponding CSV to read in. Labels are defined,
#' factors are created, etc.
#'
#' @details
#' There are a few caveats to using this type of data which this function
#' overcomes. The result is a similar data structure, but cleaned up for use
#' in this library.
#'
#' Note that if you provide the `use_cache` option, the data will be loaded
#' from local cache (if available).
#'
#' @param uri
#' @param token
#' @param use_cache (Default: TRUE) Should a cached copy of the REDCap data be used (if available)?
#' @param verbose (Default: FALSE) Should messages be logged to the console?
#'
#' @return A tibble with grant data available.
#' @export
#'
#' @examples
import_grants<-function(uri, token, use_cache = TRUE, verbose = TRUE) {

  redcap_data <- import_redcap_data(uri, token, use_cache = use_cache, verbose = verbose)

  #grant_table <- convert_redcap_to_grants(redcap_data)
  # Create the pubs and authors separately
  grants <- extract_grants(redcap_data)
  investigators <- extract_investigators(redcap_data)

  # Join by the grant id, then nest the investigator table inside the investigators variable
  grant_table<-dplyr::nest_join(grants, investigators, by="record_id",
                                name = "investigators")

  grant_table <- as_grant_table(grant_table, redcap_data)

  grant_table
}

etl_grants <- function(uri, token, use_cache = TRUE, verbose = TRUE, file = "grants.rds") {

  get_config()
  g <- import_grants(uri, token, use_cache , verbose ) |>
    transform_grants()

  saveRDS(g, file = file)

  g
}



import_publications<-function(uri, token, use_cache = TRUE, verbose = TRUE) {

  redcap_data <- import_redcap_data(uri, token, use_cache = use_cache, verbose = verbose)


  # Create the pubs and authors separately
  pubs <- extract_publications(redcap_data)
  authors <- extract_authors(redcap_data)

  # Join by the grant id, then nest the investigator table inside the investigators variable
  pub_table<-dplyr::nest_join(pubs, authors, by="record_id",
                                name = "authors")

  pub_table <- as_publication_table(pub_table, redcap_data)

  pub_table
}

etl_publications <- function(uri, token, use_cache = TRUE, verbose = TRUE, file = "publications.rds") {

  get_config()
  p <- import_publications(uri, token, use_cache , verbose ) |>
    transform_publications()

  saveRDS(p, file = file)

  p
}
