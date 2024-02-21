function(x) {

  dplyr::mutate(
    x,

    # The investigators are an embedded table in each row (grant). For
    # each of these entries, we have to apply the following transformations.
    investigators = purrr::map(
      investigators, \(investigator_table) {

        # Each of these fields has an "Other" option, so we coalesce and remove the
        # other column.
        dplyr::mutate(
          investigator_table,

          # First, NA values that currently hold "Other" because they have data in the
          # "Other" variable (to be found with coalesce).
          dplyr::across(
              dplyr::all_of(
                c(
                  "investigator_name",
                  "investigator_institution",
                  "investigator_role",
                  "partnership_role"
                  )
              ),
              \(.x) {dplyr::na_if(.x, "Other") }
          )
        ) |>


          # Now coalesce name, institution and role.
          dplyrx::coalesce_columns(
            new_var = "investigator_name",
            var_list = c("investigator_name","investigator_other")
          ) |>
          dplyrx::coalesce_columns(
            new_var = "investigator_role",
            var_list = c("investigator_role","role_other")
          ) |>
          dplyrx::coalesce_columns(
            new_var = "investigator_institution",
            var_list = c("investigator_institution","institution_other")
          ) |>

          # Then change a few more things here.
          dplyr::mutate(

            # The investigator partnership role is a bit more involved
            partnership_role = tidyr::unite(
              dplyr::pick(dplyr::all_of(c("partnership_role","partnership_role_other"))),
              col = "X", sep=", ",remove = TRUE, na.rm=TRUE
            ) |>
              dplyr::pull("X"),
            partnership_role_other = NULL
          )
      })
  )
}
