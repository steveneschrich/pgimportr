function(x) {

  dplyr::mutate(
    x,
    investigators = purrr::map(
      investigators, \(investigator_table) {

        dplyr::mutate(
          investigator_table,
          investigator_role = dplyr::recode(
            investigator_role,
            "Principal Investigator" = "PI",
            "Co-Investigator" = "co-I"
          )
        )
      })
  )

}
