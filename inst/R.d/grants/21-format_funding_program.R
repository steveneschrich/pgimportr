function(x) {

  # The funding program is really spread out. We need names because (for now),
  # the coalesce in dplyrx does not do tidyselect.
  cn <- colnames(x)
  fp <- cn[c(stringr::str_ends(cn, "_other")|stringr::str_ends(cn, "_prog"))]

  x <- dplyr::mutate(
    x,
    dplyr::across(
      dplyr::all_of(fp),
      \(.x){ replace(.x, .x=="Other", NA) }
    )) |>
    dplyrx::coalesce_columns(
      new_var = "funding_program",
      var_list = fp
    )
  x
}
