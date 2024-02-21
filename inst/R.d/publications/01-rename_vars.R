function(x) {
  dplyr::rename(
    x,
    pub_id = record_id,
    publication_date = date_publication
  ) |>
  dplyr::mutate(
    authors = purrr::map(
      authors, \(.a) {
        dplyr::rename(.a, author_id = redcap_repeat_instance)
      }
    )
  )
}
