% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choice.R
\name{split_choices}
\alias{split_choices}
\title{Split multiple choices into discrete fields}
\usage{
split_choices(s)
}
\arguments{
\item{s}{A string to split into choices (vectorized).}
}
\value{
A tibble of \code{Value} and \code{Label} fields for all choices.
}
\description{
A selection is a series of choices that can be selected. This
function extracts them as a tibble (since each choice is itself coded).
}
\details{
This code is vectorized, which can be a little confusing in the context
of embedded index/value lists. Briefly, a choice (here) is of the form:

\if{html}{\out{<div class="sourceCode">}}\preformatted{"1, Choice A | 2, Choice B | 3, Choice C"
}\if{html}{\out{</div>}}

This function will take the choice text and convert it to a list of choices:

\if{html}{\out{<div class="sourceCode">}}\preformatted{[1] "1, Choice A" "2, Choice B"
[3] "3, Choice C"
}\if{html}{\out{</div>}}

This choice can be split apart into a named list with \code{\link[=split_choice_levels]{split_choice_levels()}}. That is,

\if{html}{\out{<div class="sourceCode">}}\preformatted{
}\if{html}{\out{</div>}}

This function will take multiple character vectors with embedded choice text,
extract the individual choices and convert each character vector a named list.
}
\note{
If the input is a single choice text blob, then a single named vector
is returned. Otherwise, it is a list of named vectors (corresponding to the
input length).
}
