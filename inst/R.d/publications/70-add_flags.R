function(x) {
  dplyr::mutate(
    x,

    is_support = !is.na(support),
    is_core_support = !is.na(core_support),
    is_other_support = !is.na(other_support),
    # ESI status is whether any investigator is an ESI
    is_esi_related = purrr::map_lgl(authors, function(.y) {
      any(.y$partnership_role == "ESI")
    })

  )
}
