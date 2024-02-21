#' Format investigator name
#'
#' For reporting, the investigator full name is often not needed because it takes up too much space.
#' To uniquely identify a person, we use the convention of `GivenNameInitials. LastName, PhD`
#' for instance.
#'
#' @details
#' The data for pgreportr uses full names for investigators. Shortening these names to first initials
#' is tricky since people may have multiple given names (i.e., middle names). This code takes all names
#' except the last name (tokenized by whitespace) and abbreviates them, followed by a period. The last
#' name is then added and returned.
#'
#' The degree (in the form of ,degree) is another variation. This code handles it's presence
#' or absence. You can use the use_degree flag to print it out (or not).
#'
#' So, for instance
#' in the case of
#' ```
#'      John James Smith
#' ```
#' his formatted name would be
#' ```
#'      JJ. Smith
#' ```
#'
#' Note that this function supports either a single name or a vector of names, and will transform
#' accordingly.
#'
#' Of note, while this code is reasonably competent it is by no means bullet-proof. You would need
#' to check the results to verify it's worked as desired.
#'
#' TODO: There is bug if someone's name is John (Joe) Smith. The parens screw up a regex or
#' something.
#'
#' @param n String representing the investigator name
#' @param use_degree Should the degree be printed in the name?
#' @param use_first_name_only Should only the first given name be used?
#' @param use_initials Should only initials of the given name(s) be used?
#' @param use_period_after_initials Should a period follow given name initials?
#' @param use_last_name_first Should it be last name, first?
#'
#' @return A shortened name. If a vector is provided, a vector of shortened names is returned.
#'
#' @export
#'
#'
format_name<-function(n, use_degree=TRUE, use_first_name_only=FALSE,
                      use_initials=TRUE, use_period_after_initials=TRUE,
                      use_last_name_first=FALSE, ...) {
  # Name should be of the form
  # First other last, Degree
  name_and_degree<-stringr::str_split_fixed(n, ",( )*", n=2)

  # Get the space-separated fields
  nsplit<-stringr::str_split(name_and_degree[,1], " ")

  # Last name is the last one of the list
  last_name<-purrr::map_chr(nsplit, function(y){y[length(y)]})

  # Given names are all except the last.
  given_names<-purrr::map(nsplit, function(y){
    if ( length(y) > 1)
      y[1:length(y)-1]
    else
      NA
  })


  # Given name initials are the first character of all non-last name elements.
  # Note we replace empty with NA (degenerate case of no first name).
  if ( use_initials ) {
    given_names<-purrr::map(given_names, function(y){
      dplyr::na_if(stringr::str_sub(y[1:length(y)], start=1,end=1), "")
    })
  }

  # Reduce to first name/initial if needed
  given_names <- if ( use_first_name_only ) {
    purrr::map(given_names,1)
  } else {
    purrr::map_chr(given_names, function(s){
      stringr::str_c(s, collapse=ifelse(use_initials,""," "))
    })
  }

  # Add in the trailing period after initials, if needed
  if ( use_initials && use_period_after_initials )
    given_names<-purrr::map_chr(given_names, function(s) stringr::str_c(s, ".",sep=""))

  # Sometimes we have last, first ...
  if ( use_last_name_first ) {
    tmp<-given_names
    given_names<-last_name
    last_name<-tmp
    rm(tmp)
  }

  # Now collapse given and last name, with punctuation/spacing
  # given_name possibly ., then space unless it's NA and then nothing
  name<-purrr::map2_chr(given_names, last_name, function(given,last) {

    if (any(is.na(c(given,last))))
      stats::na.omit(c(given,last))
    else
      stringr::str_c(given, last, sep=ifelse(use_last_name_first,", "," "))

  })


  if ( use_degree )
    name <- purrr::map2_chr(name, name_and_degree[,2], \(.x, .y) {
      stringr::str_flatten_comma(c(.x, .y), na.rm = TRUE)
    })

  name

}
