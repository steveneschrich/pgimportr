#options
#


#' Title
#'
#' @return
#' @export
#'
#' @examples
get_config <- function() {
  # Load package-level defaults first.
  pkg_defaults <- config::get(
    file = system.file("config.yml",package="pgimportr"),
    use_parent = FALSE
  )

  # Load any local configuration (hopefully there is some).
  pg_local <- tryCatch(config::get(use_parent=FALSE), error = function(e) NA)

  # Merge the two (pg_local over the top of package defaults)
  pg_config <- config::merge(pkg_defaults, pg_local)

  # Separate function to set the configuration in options()
  pg <- set_config(pg_config)

  invisible(pg)
}

# copy default config.yml to directory, ala usethis
use_config <- function() {

}
# Set to a specific config, not sure this is needed. It is
# definitely not exportable since it needs the config
# file raw, not processed.
#' Internal function for processing config object
#'
#' @description The config information read from `config.yml` must
#'  be translated into usable library options. This function does
#'  that transformation.
#'
#' @details This is not likely a function useful outside of the
#'  library itself. Briefly, a configuration is
#'  read from a file (`config.yml`). This function will take that
#'  very nested list of information and separate it into a program_grant
#'  object and a set of library options (defaults).
#'
#'  The program_grant object is stored as `pgimportr.program_grant`
#'  and the options are stored as `pgimportr.option` in the [options()]
#'  variable.
#'
#' @param pg_config
#'
#' @return The program grant object is returned (invisibly).
#'
#' @examples
set_config <- function(pg_config) {
  # Do some processing on config
  pg <- new_program_grant(pg_config$program_grant)
  options("pgimportr.program_grant" = pg)

  # Store program-specific settings into options (prefixed with package name).
  if ( !is.null(pg_config$pgimportr) ) {
    lo <- pg_config$pgimportr
    names(lo) <- paste("pgimportr",names(lo),sep=".")
    options(lo)
  }

  invisible(pg)
}
