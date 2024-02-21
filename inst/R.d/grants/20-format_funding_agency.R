function(x) {

  # The granting agency is spread out but only one unique.
  dplyr::mutate(x, agency = dplyr::na_if(agency, "Other")) |>
    dplyrx::coalesce_columns(
      new_var="grant_agency",
      var_list=c("agency","agency_other")
    )
}
