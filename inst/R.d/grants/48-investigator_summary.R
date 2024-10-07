function(x) {
  dplyr::mutate(
    x,
    investigators = purrr::map(
      investigators,
      \(investigator_table) {
        dplyr::mutate(
          investigator_table,
          investigator_summary = format_creator_summary(
            name = investigator_name,
            pg_role = partnership_role,
            role = investigator_role,
            is_Current_ESI = `isPartnershipRole_Current ESI`
          )
        )
      }
    )
  )
}
