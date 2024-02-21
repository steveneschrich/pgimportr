function(x) {
  # Cores are a series of checkboxes that can be combined in various ways.
  x <- convert_checkbox_to_flags(
    x,
    raw_prefix = "cores_support",
    indicator_prefix = "isSupportedBy",
    dictionary = dictionary(x)
  )

  # Create consolidated fields (comma-separated)
  x |>
    dplyr::mutate(
      core_support = tidyr::unite(
        dplyr::pick(dplyr::all_of(paste0("cores_support___", pg_cores()))),
        col = "X", sep=", ",remove = TRUE, na.rm=TRUE
      ) |>
        dplyr::pull("X"),

      other_support = tidyr::unite(
        dplyr::pick(dplyr::all_of(paste0("cores_support___",pg_other_support()))),
        col = "X", sep=", ", remove = TRUE, na.rm = TRUE
      ) |>
        dplyr::pull("X"),
      support = tidyr::unite(
        dplyr::pick(dplyr::all_of(paste0("cores_support___", pg_support()))),
        col = "X", sep = ", ", remove = TRUE, na.rm = TRUE
      ) |>
        dplyr::pull("X")
    )
}
