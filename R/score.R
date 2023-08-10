#' Scoring the solutions at the vertices
#'
#' Given a matrix of solution evaluations and (optionally) a matrix containing
#' the vertices of the polyhedron induced by the set of linear constraints over
#' the weights, this function computes the score for each solution at each of
#' the vertices (extreme points).
#'
#' @param eval_matrix Evaluation matrix.
#' @param vert_matrix Vertices matrix. If \code{NA} then it will be assumed that
#' the vertices are arranged as it is induced by a decreasing order of the
#' weights. That is, \eqn{w_1 \geq w_2 \geq ... \geq w_n}.
#'
#' @param append_output Whether or not the output will be append to
#' the evaluation matrix (input).
#'
#' @return An m-row matrix with at least n columns corresponding to the
#' scores achieved by the solutions at each vertex (extreme point) of the
#' polyhedron of feasible weights. The new columns will be named with the prefix
#'  \code{'VE_'} plus the name of the criterion (e.g. for criterion \code{'C1'} its
#'  corresponding column will be named as \code{'VE_C1'}).
#' @export
#'
#' @examples
#' create_eval_matrix_example(10, 3) %>% score()
score <- function(eval_matrix,
                  vert_matrix = NA,
                  append_output = TRUE) {
  ncrit <- ncol(x = eval_matrix)
  nsol <- nrow(x = eval_matrix)

  # Assume that criteria are represented in ascending order of preference in
  # the columns of eval_matrix

  eval_mat <- eval_matrix

  empty_vert_matrix <- is.na(x = vert_matrix)[1] || is.null(x = vert_matrix)


  if (empty_vert_matrix) { # A criteria preference is not provided
    v_mat <- generate_polyhedron_vertices(ncrit = ncrit)
  } else {
    v_mat <- vert_matrix
  }

  criteria_names <- row.names(vert_matrix)

  if (is.null(criteria_names)) {
    criteria_names <- generate_criteria_names(ncrit = ncrit)
  }

  # Computing the scores for each solution at each vertex
  vert_score_mat <- eval_matrix %*% v_mat

  rownames(vert_score_mat) <- rownames(eval_matrix)

  criteria_names <- paste0(get_eval_vertex_prefix(), criteria_names)

  colnames(vert_score_mat) <- criteria_names

  if (append_output) {
    return(
      cbind(
        eval_matrix,
        vert_score_mat
      )
    )
  }

  return(vert_score_mat)
}
