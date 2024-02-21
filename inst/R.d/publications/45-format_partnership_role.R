# 2023-11-06. Partnership role has been vastly expanded to be checkboxes, including
# an "other" which needs to be converted.
function(x) {

  dplyr::mutate(
    x,
    authors = purrr::map(
      authors, \(author_table) {
        convert_checkbox_to_flags(
          author_table,
          raw_prefix = "author_partnership_role",
          indicator_prefix = "isPartnershipRole",
          dictionary = dictionary(x)
        ) |>
          dplyr::mutate(
            author_partnership_role___Other = dplyr::na_if(author_partnership_role___Other, "Other"),

            partnership_role = tidyr::unite(
              dplyr::pick(c(dplyr::starts_with("author_partnership_role___"),dplyr::all_of("partnership_role_other"))),
              col = "X", sep=", ",remove = TRUE, na.rm=TRUE
            ) |>
              dplyr::pull("X"),
          ) |>
          dplyr::mutate(
            partnership_role = dplyr::na_if(partnership_role, "None")
          ) |>
          dplyr::select(-dplyr::starts_with("author_partnership_role___"),-dplyr::all_of("partnership_role_other"))
      })
  )
}

