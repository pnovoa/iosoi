% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify.R
\name{poss_identify_sois}
\alias{poss_identify_sois}
\title{Possibility approach for the identification of the solution of interest}
\usage{
poss_identify_sois(
  eval_matrix,
  vert_matrix = NA,
  by = "neutral",
  threshold = 0
)
}
\arguments{
\item{eval_matrix}{the evaluation matrix. It is a \eqn{m \times n} matrix with
entries \eqn{e_{ij}}, corresponding to the evaluation of solution \eqn{i}
in the criteria \eqn{j}.}

\item{vert_matrix}{the coordinates of the vertices. It is a \eqn{n \times n} 
matrix with the coordinates of the vertices of the polyhedron of the feasible
weights located at the columns. If \code{NA} then it is assumed that the 
criteria follow a decreasing order of importance, that is, with criterion 
1 more important than criterion 2, and so on.}

\item{by}{the attitude adopted by the decision-maker in order to assess each
solution. It can take one of the following values: \code{'neutral'} (default),
\code{'optimistic'}, or \code{'pessimistic'}.}

\item{threshold}{threshold to identify the solutions of interest.
In particular, a filter is applied on the attitude value of each solution
so that only those with values greater than or equal to threshold will
appear in the returned matrix.}
}
\value{
A matrix is returned that includes, for each solution, the interval
of scores, whether it is the reference solution or not, and the degrees
of possibility corresponding to the decision maker's attitude.
}
\description{
Identify the solutions of interest of a multi-criteria decision problem
by applying a possibility approach by Torres et al. (2021).
}
\examples{
eval_mat <- create_eval_matrix_example()
eval_mat \%>\% poss_identify_sois()

}
