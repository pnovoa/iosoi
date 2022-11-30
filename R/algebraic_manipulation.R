
#' Scoring interval for each solution
#'
#' This function returns, for each solution, its range of possible scores
#' (intervals).
#'
#' @param vertex_scores_matrix An m by n matrix containing the scores of
#' each of the m solutions (rows) at each of the n vertices (columns).
#'
#' @return An m by 2 matrix containing the lower bound (LB) and
#' upper bound (UB) for each of the m solutions (rows).
#' @export
#'
#' @examples
#' # Scoring two solutions on three criteria
#' m <- matrix(c(1, 2, 3, 4, 3, 2), byrow = TRUE, nrow = 2)
#' compute_intervals(m)
compute_intervals <- function(vertex_scores_matrix) {
  intervals_bounds <- apply(vertex_scores_matrix, MARGIN = 1, FUN = range)
  intervals_bounds <- t(intervals_bounds)
  colnames(intervals_bounds) <- c("LB", "UB")
  return(intervals_bounds)
}






#' Vertices of the polyhedron of feasible weights
#'
#' Generates an n by n matrix of the vertices of the polyhedron
#' of the feasible region of weights. This region is induced by the following
#' set of constraints: wi >= 0 (for all i = 1...n), w1+...+w2 = 1 and
#' w1 <=...<= wn, where wj is the weight of the jth most important criterion
#' (according to the order of preference given by the decision maker).
#' In particular, w1 is the weight of the least important criterion,
#' while wn is the weight of the most important criterion.
#' The order of the rows of the resulting matrix is consistent with
#' this order of preference. For example,
#' the first row corresponds to the least important criterion
#' (e.g. the last in the order of preference and weight w(1)),
#' the second column to the second least important criterion, and so on.
#' This matrix is required to compute the range of possible scores for each
#' solution.
#'
#' @param ncrit the number of criteria (dimension of the search space)
#'
#' @return an n by n matrix with the coordinates of each vertex of the
#' polyhedron in the columns.
#' @export
#'
#' @examples
#' # Generating the coordinates of the three vertices in a 3-criteria problem.
#' generate_polyhedron_vertices(3)
#'
generate_polyhedron_vertices <- function(ncrit) {
  m <- matrix(
    rep(1 / 1:ncrit, ncrit),
    nrow = ncrit,
    byrow = T
  )

  m[lower.tri(m)] <- 0.

  return(m)
}



evaluate_at_vertices <- function(eval_matrix, vert_matrix) {
  eval_matrix %*% vert_matrix
}



get_reference_solution_index <- function(interval_matrix) {
  which.max(interval_matrix[, "LB"])
}
