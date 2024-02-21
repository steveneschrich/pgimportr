#' Retrieve REDCap data from the server
#'
#' @description Using the REDCapR library, import the raw and labelled versions of
#' a project; and the data dictionary.
#'
#' @details The overall process is to import
#' data directly from REDCap (using the [REDCapR::redcap_read()]
#' function). The raw and labelled data are both read so that various mappings can
#' be created. Additionally, the metadata is imported as a data dictionary.
#'
#' The result of this function is a list:
#' - raw (unlabelled data)
#' - label (labelled data)
#' - dictionary (corresponding data dictionary)
#'
#' @note The input token defines the specific project forms to be imported. This can
#' be generated from within REDCap.
#'
#' @param uri URI of the redcap data source (general api path)
#' @param token An authentication token to use when retrieving the data. Note that
#'   the token indicates the project/forms to download.
#' @param verbose (default: TRUE) Should messages be printed to console during operation.
#' @return A list consisting of `raw`, `label` and `dictionary` tables for the given
#'  URI reference.
#'
#' @export
#' @examples
import_redcap_data_online <- function(uri, token, verbose = TRUE) {
  ds_raw <- REDCapR::redcap_read(redcap_uri = uri, token = token, raw_or_label = "raw", verbose = verbose)
  ds_label <- REDCapR::redcap_read(redcap_uri = uri, token = token, raw_or_label = "label", verbose = verbose)

  ds_metadata <- REDCapR::redcap_metadata_read(redcap_uri=uri, token=token, verbose = verbose)

  stopifnot(ds_raw$success & ds_label$success & ds_metadata$success)

  list(raw = ds_raw$data, label = ds_label$data, metadata = ds_metadata$data)
}


#' Return name of REDCap cache file
#'
#' @param uri A URI that was originally used to retrieve results.
#' @param token A token corresponding to the project in URI.
#'
#' @return A full pathname to a cache file for the URI
#'
#' @examples
get_cache_file <- function(uri, token) {
  file.path(
    rappdirs::user_cache_dir(appname = "pgimportr"),
    paste0(digest::digest(c(uri, token), algo="sha256"),".rds")
  )
}


#' Read REDCap data with caching
#'
#' @description Load REDCap data from server with built-in support
#' for caching of results
#'
#' @details
#' This function is the main entry-point for loading REDCap pgreportr data
#' into R. It will managing caching of the results, so that subsequent requests
#' do not have to hit the server.
#'
#' The [read_redcap_data_online()] function includes details on the process
#' and the return result.
#'
#'
#' @param uri URI of the REDCap server project
#' @param token Token for access authorization
#' @param use_cache Should a cached copy of the data be used instead of
#'  querying the URI?
#' @param verbose (default: TRUE) Should messages be printed to console during operation.
#'
#' @return A list of raw, labelled and data dictionary (see [read_redcap_data_online()])
#' @export
#'
#' @examples
import_redcap_data <- function(uri, token, use_cache = TRUE, verbose = TRUE) {
  uc <- get_cache_file(uri, token)

  if ( use_cache && ! file.exists(uc) ) {
    if (verbose)
      cli::cli_alert_warning("No cache available for requested {uri}, reverting to server retrieval.")
    use_cache <- FALSE
  }


  if ( use_cache ) {
    rd <- readRDS(uc)
    if (verbose)
      cli::cli_alert_success("Retrieved {uri} data from local cache.")
  } else {
    rd <- import_redcap_data_online(uri, token)
    if ( ! dir.exists(dirname(uc)))
      dir.create(dirname(uc))
    saveRDS(rd, file = uc)
    if ( verbose ) cli::cli_alert_success("Retrieved {uri} data from server.")
  }

  rd
}

#TODO: Someday: isTRUE(getOption("blabla.foo", FALSE)))


