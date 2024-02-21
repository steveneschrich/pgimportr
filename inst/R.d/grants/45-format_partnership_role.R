
#' Recode partnership role check box into variable
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
function(x) {

  dplyr::mutate(
    x,
    investigators = purrr::map(
      investigators, \(investigator_table) {
        convert_checkbox_to_flags(
          investigator_table,
          raw_prefix = "investigator_partnership_role",
          indicator_prefix = "isPartnershipRole",
          dictionary = dictionary(x)
        ) |>
          dplyr::mutate(
            partnership_role = tidyr::unite(
              dplyr::pick(dplyr::starts_with("investigator_partnership_role___")),
              col = "X", sep=", ",remove = TRUE, na.rm=TRUE
            ) |>
              dplyr::pull("X"),
          ) |>
          dplyr::select(-dplyr::starts_with("investigator_partnership_role___"))
      })
  )
}
