function(x) {
  # Cores are a series of checkboxes that can be combined in various ways.
  x <- convert_checkbox_to_flags(
    x,
    raw_prefix = "supported_components",
    indicator_prefix = "isSupportedBy",
    dictionary = dictionary(x)
  )

  # Create consolidated fields (comma-separated)
  x |>
    dplyr::mutate(
      core_support = tidyr::unite(
        dplyr::pick(dplyr::all_of(paste0("supported_components___", pg_cores()))),
        col = "X", sep=", ",remove = TRUE, na.rm=TRUE
      ) |>
        dplyr::pull("X"),

      other_support = tidyr::unite(
        dplyr::pick(dplyr::all_of(paste0("supported_components___",pg_other_support()))),
        col = "X", sep=", ", remove = TRUE, na.rm = TRUE
      ) |>
        dplyr::pull("X"),
      support = tidyr::unite(
        dplyr::pick(dplyr::all_of(paste0("supported_components___", pg_support()))),
        col = "X", sep = ", ", remove = TRUE, na.rm = TRUE
      ) |>
        dplyr::pull("X")
    )
}
