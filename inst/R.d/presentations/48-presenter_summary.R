function(x) {
  dplyr::mutate(
    x,
    presenters = purrr::map(
      presenters,
      \(presenter_table) {
        dplyr::mutate(
          presenter_table,
          presenter_summary = format_creator_summary(
            name = presenter_name,
            pg_role = partnership_role,
            is_Current_ESI = `isPartnershipRole_Current ESI`,
            use_degree = FALSE
          )
        )
      }
    )
  )
}



