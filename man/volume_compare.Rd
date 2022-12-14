% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volume_based_assess.R
\name{volume_compare}
\alias{volume_compare}
\title{Volume approach to compare against the reference solution}
\usage{
volume_compare(sol_eval, ref_sol_eval, vert_matrix)
}
\arguments{
\item{sol_eval}{a vector with the reference solution evaluations at extreme
points (vertices of the polyhedron of feasible weights)}

\item{ref_sol_eval}{a vector with the reference solution evaluations at
extreme points (vertices of the polyhedron of feasible weights)}

\item{vert_matrix}{a m by m matrix containing the vertices of the
polyhedron of feasible weights. The coordinates of these vertices must be
in the columns of the matrix.}
}
\value{
A numerical value in the range of \eqn{[-1, 1]} corresponding to the
superiority of solution \code{sol_eval} over the reference solution
(\code{ref_sol_eval})
}
\description{
This function compute the superiority degree of a given solution against the
reference solution. This degree is compute based on the notion of the
integral of a linear function (scoring function of a given solution) over
a simplex (polyhedron of feasible weights). Specifically, it returns the
normalized difference of the integrals over the sum of both integrals. Thus,
the range of values is \eqn{[-1, 1]} with \eqn{-1} indicating that the
solution is definitely worse than the reference solution, and \eqn{1}
indicating the opposite. In particular, if the result is \eqn{0}, it means
that the solution in question is equal to the reference solution.
}
\examples{
vert_mat <- generate_polyhedron_vertices(3)
ref_sol_eval <- runif(3)
sol_eval <- runif(3)
volume_compare(sol_eval, ref_sol_eval, vert_mat)

}
