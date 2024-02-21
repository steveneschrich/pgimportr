function(x) {

  # Grant tags are also checkboxes
  convert_checkbox_to_flags(
    x,
    raw_prefix = "grant_tags",
    indicator_prefix = "isTag",
    dictionary = dictionary(x)
  ) |>
    dplyr::mutate(
      `Tags` = tidyr::unite(
        dplyr::pick(dplyr::starts_with("grant_tags___")),
        col = "X", sep=", ",remove = TRUE, na.rm=TRUE
      ) |>
        dplyr::pull("X"),

    )
}
