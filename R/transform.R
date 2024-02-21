transform <- function(x, which = c("grants","publications")) {
  which <- match.arg(which)

  if ( which == "grants")
    transform_grants(x)
  else
    transform_publications(x)
}

transform_grants <- function(x, debug = NULL) {
  checkmate::assert_class(x, "grant_table")

  cli::cli_h1("Transforming grants")
  transformers <- fs::dir_ls(path = system.file("R.d/grants",package="pgimportr"), regexp = "/\\d+-.*\\.R$")
  names(transformers) <- basename(transformers) |>
    stringr::str_remove("\\.R$")
  flist <- purrr::map(transformers, function(f) {
    source(f, local=new.env())[["value"]]
  })

  x <- fapply(x, flist, verbose =TRUE)
  cli::cli_alert_success("Grants transformed")

  x

}


transform_publications <- function(x, debug = NULL) {
  checkmate::assert_class(x, "publication_table")

  cli::cli_h1("Transforming publications")
  transformers <- fs::dir_ls(path = system.file("R.d/publications",package="pgimportr"), regexp = "/\\d+-.*\\.R$")
  names(transformers) <- basename(transformers) |>
    stringr::str_remove("\\.R$")
  flist <- purrr::map(transformers, function(f) {
    source(f, local=new.env())[["value"]]
  })

  x <- fapply(x, flist, verbose =TRUE)
  cli::cli_alert_success("Publications transformed")

  x

}




