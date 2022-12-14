% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{create_eval_matrix_example}
\alias{create_eval_matrix_example}
\title{Create a random evaluation matrix}
\usage{
create_eval_matrix_example(nsol = 10, ncrit = 3)
}
\arguments{
\item{nsol}{number of solutions}

\item{ncrit}{number of criteria}
}
\value{
an nsol by ncrit matrix with entries ranging between 0 and 1
}
\description{
This function creates a random evaluation matrix of dimension "nsol"
by "ncrit" and entries ranging between 0 and 1. The entry values are
generated following a uniform random distribution.
}
\examples{
# An example matrix containing the evaluations for 10
# solutions at three criteria
example_matrix <- create_eval_matrix_example(10, 3)

}
