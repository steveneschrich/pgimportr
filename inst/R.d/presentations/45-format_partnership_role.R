# 2023-11-06. Partnership role has been vastly expanded to be checkboxes, including
# an "other" which needs to be converted.
function(x) {

  dplyr::mutate(
    x,
    presenters = purrr::map(
      presenters, \(presenter_table) {
        convert_checkbox_to_flags(
          presenter_table,
          raw_prefix = "presenter_partnership_role",
          indicator_prefix = "isPartnershipRole",
          dictionary = dictionary(x)
        ) |>
          dplyr::mutate(
            presenter_partnership_role___Other = dplyr::na_if(presenter_partnership_role___Other, "Other"),

            partnership_role = tidyr::unite(
              dplyr::pick(c(dplyr::starts_with("presenter_partnership_role___"),dplyr::all_of("partnership_role_other"))),
              col = "X", sep=", ",remove = TRUE, na.rm=TRUE
            ) |>
              dplyr::pull("X") |>
              dplyr::na_if(""),
          ) |>
          dplyr::mutate(
            partnership_role = dplyr::na_if(partnership_role, "None")
          ) |>
          dplyr::select(-dplyr::starts_with("presenter_partnership_role___"),-dplyr::all_of("partnership_role_other"))
      })
  )
}

