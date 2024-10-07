
#' Derive creator name and roles as single text
#'
#' A creator (investigator or author) can have a partnership role and a
#' specific role. This function formats the information for output.
#'
#' @details
#' A creator is a person associated with a work (grant or publication). There
#' can be one or
#' more creators, and they may have different roles on
#' each work. Additionally, the creator has a partnership role which
#' may change over time. Thus, each work has a tibble associated with the
#' investigators for that work and include descriptions of their name,
#' grant role and partnership role.
#'
#' This function produces a text summary of this information so that it could
#' be printed out succinctly. There are many different parameters that can be
#' tweaked (see `format_investigator_name` for many formatting options passed on
#' through this function).
#'
#' @seealso [format_investigator_name()] for flags to format the investigator name.
#'
#' @param investigators The investigators tibble for a grant.
#'
#' @return A formatted string (or vector of strings) representing investigators
#'
format_creator_summary<-function(name, pg_role, role=NA, is_Current_ESI=FALSE, ...) {

  sprintf("%s%s%s%s",format_name(name, ...),
          ifelse(!is.na(pg_role), "*", ""),
          ifelse(is.na(role), "", paste0(" (",role,")")),
          ifelse(is_Current_ESI, " [Current ESI]","")
  )


}
