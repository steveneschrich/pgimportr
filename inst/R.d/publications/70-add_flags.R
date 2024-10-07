function(x) {
  dplyr::mutate(
    x,

    is_support = !is.na(support),
    is_core_support = !is.na(core_support),
    is_other_support = !is.na(other_support),
    # ESI status is whether any author is an ESI
    is_any_esi_related = purrr::map_lgl(authors, calculate_is_any_esi_related),
    is_current_esi_related = purrr::map_lgl(authors, calculate_is_current_esi_related),
    is_former_esi_related = purrr::map_lgl(authors, calculate_is_former_esi_related)

  )
}
