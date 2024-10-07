function(x) {

  dplyr::mutate(
    x,

    # The presenters are an embedded table in each row (presentation). For
    # each of these entries, we have to apply the following transformations.
    presenters = purrr::map(
      presenters, \(presenter_table) {

        # This is a weird edge case, if there are no rows then just quit. This
        # shouldn't really happen but has happened.
        if ( nrow(presenter_table)==0) return(presenter_table)
        # Each of these fields has an "Other" option, so we coalesce and remove the
        # other column.
        dplyr::mutate(
          presenter_table,

          # First, NA values that currently hold "Other" because they have data in the
          # "Other" variable (to be found with coalesce).
          dplyr::across(
              dplyr::all_of(
                c(
                  "presenter_name",
                  "presenter_institution",
                  "partnership_role"
                  )
              ),
              \(.x) {
                dplyr::na_if(.x, "Other")
              }
          )
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(
              c(
                "presenter_name", "presenter_institution",
                "partnership_role", "presenter_other",
                "institution_other"
              )
            ),
            as.character
          )
        ) |>


          # Now coalesce name, institution and role.
          dplyrx::coalesce_columns(
            new_var = "presenter_name",
            var_list = c("presenter_name","presenter_other")
          ) |>
          dplyrx::coalesce_columns(
            new_var = "presenter_institution",
            var_list = c("presenter_institution","institution_other")
          )
      })
  )
}
