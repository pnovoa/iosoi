
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
#' m <- matrix(c(1,2,3,4,3,2), byrow = TRUE, nrow = 2)
#' compute_intervals(m)
compute_intervals <- function(vertex_scores_matrix){
  intervals_bounds <- apply(vertex_scores_matrix, MARGIN=1, FUN=range)
  intervals_bounds <- t(intervals_bounds)
  colnames(intervals_bounds) <- c("LB", "UB")
  return(intervals_bounds)
}
