function(x) {

  # Create a composite string for the funding source (agency/program)
  dplyr::mutate(x,
                grant_source =  sprintf("%s%s",
                                        `grant_agency`,
                                        ifelse(is.na(`funding_program`), "", paste0("/",`funding_program`))
                ),
                grant_summary = sprintf("%s%s",
                                        grant_source,
                                        ifelse(is.na(`grant_type`), "", paste0(" ",`grant_type`))
                )
  )

}
