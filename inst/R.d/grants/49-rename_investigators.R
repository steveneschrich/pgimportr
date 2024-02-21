function(x) {
  dplyr::mutate(
    x,
    investigators = purrr::map(investigators, \(investigator_table) {

      # Clean up remaining variables.
      dplyr::rename(
        investigator_table,
        investigator_id=redcap_repeat_instance
      ) |>
        # Select the order for the table
        dplyr::select(
          investigator_id,
          investigator_name,
          investigator_institution,
          investigator_role,
          partnership_role,
          investigator_summary,
          dplyr::everything())
    })
  )
}
