% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score.R
\name{score}
\alias{score}
\title{Scoring the solutions at the vertices}
\usage{
score(eval_matrix, vert_matrix = NA, append_output = TRUE)
}
\arguments{
\item{eval_matrix}{Evaluation matrix.}

\item{vert_matrix}{Vertices matrix. If \code{NA} then it will be assumed that
the vertices are arranged as it is induced by a decreasing order of the
weights. That is, \eqn{w_1 \geq w_2 \geq ... \geq w_n}.}

\item{append_output}{Whether or not the output will be append to
the evaluation matrix (input).}
}
\value{
An m-row matrix with at least n columns corresponding to the
scores achieved by the solutions at each vertex (extreme point) of the
polyhedron of feasible weights. The new columns will be named with the prefix
 \code{'VE_'} plus the name of the criterion (e.g. for criterion \code{'C1'} its
 corresponding column will be named as \code{'VE_C1'}).
}
\description{
Given a matrix of solution evaluations and (optionally) a matrix containing
the vertices of the polyhedron induced by the set of linear constraints over
the weights, this function computes the score for each solution at each of
the vertices (extreme points).
}
\examples{
create_eval_matrix_example(10, 3) \%>\% score()
}
