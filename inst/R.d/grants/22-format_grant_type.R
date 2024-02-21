function(x) {
  # As is funding mechanism
  # The funding program is really spread out.
  cn <- colnames(x)
  fp <- cn[c(stringr::str_ends(cn, "_mech")|stringr::str_ends(cn, "nih_other_2"))]

  x <- dplyr::mutate(
    x,
    dplyr::across(
      dplyr::all_of(fp),
      \(.x){ replace(.x, .x=="Other", NA) }
    )) |>
    dplyrx::coalesce_columns(
      new_var = "grant_type",
      var_list = fp
    )
}
