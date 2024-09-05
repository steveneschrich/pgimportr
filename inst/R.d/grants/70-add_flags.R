function(x) {

  pg <- program_grant()
  rgrants <- pg$pgimportr$R_grants
  isup <- pg$pgimportr$ISUP
  cc <- pg$pgimportr$CC

  dplyr::mutate(
    x,

    # Joint grant is both ISUP and CC (for U54)
    is_grant_joint = purrr::map_lgl(investigators, \(.i) {
      all(c(isup, cc) %in% .i$investigator_institution)
    }),
    # NIH grants can be R-series grants, based on configuration.
    is_grant_rtype = ((grant_agency == "NIH") & (grant_type %in% rgrants)),
    # ESI status is whether any investigator is an ESI
    is_esi_related = purrr::map_lgl(investigators, calculate_is_esi_related),
    is_current_esi_related = purrr::map_lgl(investigators, calculate_is_current_esi_related),
    is_former_esi_related = purrr::map_lgl(investigators, calculate_is_former_esi_related),


    # The remaining are logical flags based on specific status.
    is_grant_funded = (grant_status == "Funded"),
    is_grant_pending_review = (grant_status =="Pending Review"),
    is_grant_not_funded = (grant_status =="Not Funded"),
    is_grant_in_preparation = (grant_status =="In Preparation"),
    is_grant_submitted = (!grant_status %in% c("In Preparation","Resubmission")),
    is_support = !is.na(support),
    is_core_support = !is.na(core_support),
    is_other_support = !is.na(other_support)
  )
}
