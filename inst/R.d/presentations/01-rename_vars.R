function(x) {
  dplyr::rename(
    x,
    abstract_id = record_id,
    presentation_date = date,
    submission_date = date_2
  ) |>
  dplyr::mutate(
    presenters = purrr::map(
      presenters, \(.a) {
        dplyr::rename(.a, presenter_id = redcap_repeat_instance)
      }
    )
  )
}
