% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.R
\name{calculate_is_former_esi_related}
\alias{calculate_is_former_esi_related}
\title{Determine if any people are Former ESI status}
\usage{
calculate_is_former_esi_related(people)
}
\arguments{
\item{people}{A data frame of investigators or authors}
}
\value{
A logical indicating if any of the people are Former ESI
}
\description{
This function provides a way, in mutate, to test if any
of the authors are Former ESI status.
}
\details{
It is a little tricky, since the authors are nested, to test
if any of the authors are ESI's. This code is private to the library
since the whole purpose of the function is to create an indicator
variable so that we don't need to do this regularly.
}
\examples{
\dontrun{calculate_is_former_esi_related(investigators[[1]])}
}
