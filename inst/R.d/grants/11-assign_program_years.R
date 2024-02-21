# assign program grant years (pg_years).
# The Program Grant has fiscal years. Assign grant years.
assign_program_years = function(x) {
  dplyr::mutate(
    x,
    # The Fiscal Year Submitted is simple
    pg_year_submitted =
      pg_project_year(submission_date, as_factor=TRUE),
    # The Fiscal Year Funded requires a Funded status, or else should be empty.
    pg_year_funded =
      pg_project_year(
        dplyr::case_when(.data$grant_status=="Funded" ~ .data$funding_start_date),
        as_factor = TRUE
      ),

    # The Fiscal Year Not Funded is trickier. If the grant is not funded, then we assume that
    # the year not funded corresponds to 6 months after the Submission Date.
    pg_year_not_funded =
      pg_project_year(
        dplyr::case_when(grant_status=="Not Funded" ~
                           lubridate::add_with_rollback(submission_date, base::months(6))),
        as_factor = TRUE
      )

  )
}
