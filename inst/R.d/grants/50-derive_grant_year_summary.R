#' Format the fiscal year as submitted (funded).
#'
#' @details
#' Partnership grant years are numbered like Y05, Y06. These are important when
#' tracking outcomes in the grant. However, it can be a little confusing that there
#' is the date a grant was submitted vs the date a grant was funded. These can often
#' be years apart. This function takes both the year submitted (everyone has this) and
#' the year funded (not everyone has this) and formats a combination of the two.
#'
#'
function(x) {
  dplyr::mutate(
    x,
    pg_year= paste0(
      pg_year_submitted,
      ifelse(
        is.na(pg_year_funded),
        "",
        paste(" (",pg_year_funded,")",sep=""))
    )
  )

}
