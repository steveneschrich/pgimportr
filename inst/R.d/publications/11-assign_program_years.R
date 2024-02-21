# assign program grant years (pg_years).
# The Program Grant has fiscal years. Assign grant years.
assign_program_years = function(x) {
  dplyr::mutate(
    x,

    pg_year_published = pg_project_year(publication_date, as_factor = TRUE)

  )
}
