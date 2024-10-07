function(x) {
  dplyr::mutate(
    x,
    authors = purrr::map(
      authors,
      \(author_table) {
        dplyr::mutate(
          author_table,
          author_summary = format_creator_summary(
            name = author_name,
            pg_role = partnership_role,
            is_Current_ESI = `isPartnershipRole_Current ESI`
          )
        )
      }
    )
  )
}



