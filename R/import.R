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
#' @export
import <- function(uri, token, which = c("grants","publications","presentations"),
                   use_cache = TRUE, verbose = TRUE) {

  which <- match.arg(which)

  x <- switch(
    which,
    grants = import_grants(uri, token, use_cache = use_cache, verbose = verbose),
    publications = import_publications(uri, token, use_cache = use_cache, verbose = verbose),
    presentations = import_presentations(uri, token, use_cache=use_cache, verbose = verbose)
  )

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



import_presentations<-function(uri, token, use_cache = TRUE, verbose = TRUE) {

  redcap_data <- import_redcap_data(uri, token, use_cache = use_cache, verbose = verbose)


  # Create the presentations and presenters separately
  presentations <- extract_presentations(redcap_data)
  presenters <- extract_presenters(redcap_data)

  # Join by the grant id, then nest the investigator table inside the investigators variable
  presentation_table<-dplyr::nest_join(presentations, presenters, by="record_id",
                              name = "presenters")

  presentation_table <- as_presentation_table(presentation_table, redcap_data)

  presentation_table
}



#' Import raw data to corresponding object
#'
#' @description Import REDcap data into corresponding object and transform
#'  variables into usable reporting format.
#'
#' @details This function is likely the main driver of functionality within
#' the library. Specifically, this function will import data from redcap
#' (possibly using cached data if requested) then transform variables into
#' useful reporting variables suitable for the library `pgreportr`.
#'
#' There are three different input types (grants, publications and
#' presentations) that this function will handle.
#'
#' @param which The object type to load (grants, publications, presentations)
#' @param uri REDCap URI to import from
#' @param token REDCap URI Authentication token for importing
#' @param use_cache If previously run, the cached object can be used instead of reloading
#' @param verbose Should the process be verbose so the steps can be viewed
#' @param file Output file to store results (useful for caching).
#'
#' @return An object (grants,publications, presentations) which is mostly
#'  a data frame with an attached data dictionary.
#' @export
#'
#' @examples
etl <- function(
    which = c("grants","publications","presentations"),
    uri, token, use_cache = TRUE, verbose = TRUE, file = NULL
) {

  which <- match.arg(which)
  if ( is.null(file) )
    file <- paste0(which, ".rds")

  get_config()
  p <- import(
    uri = uri, token = token,
    which = which,
    use_cache=use_cache, verbose=verbose
  ) |>
    transform()

  saveRDS(p, file = file)

  p
}

