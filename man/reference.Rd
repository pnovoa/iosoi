% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reference.R
\name{reference}
\alias{reference}
\title{Identify the reference solution}
\usage{
reference(interval_matrix, sel_fun = max_lower_bound, append_output = TRUE)
}
\arguments{
\item{interval_matrix}{an \eqn{n \times p} matrix (with \eqn{p \geq 2})
containing at least two columns with names \code{'LB'} and \code{'UB'} (if
\code{max_lower_bound} is set as selection function, or at least one column
(\code{'volume'}) if \code{max_volume} is set as the selection function.}

\item{sel_fun}{the selection criterion function. By default is
\code{max_lower_bound}, which select the solution with the maximum lower
bound (\code{LB}) as the reference solution.}

\item{append_output}{Whether or not the result is append to the input matrix
\code{interval_matrix}. It is \code{TRUE} by default.}
}
\value{
a matrix with at least one column (named as \code{'REF'}) with 1 for
the reference solution and 0 for the rest.
}
\description{
Identify the reference solution
}
\examples{
create_eval_matrix_example(10, 3) \%>\%
  score() \%>\%
  intervals() \%>\%
  reference()

}
