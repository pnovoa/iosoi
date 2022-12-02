

#' Read an evaluation matrix from a csv file
#'
#' This function reads an evaluation matrix from a csv file. The
#' contents of the file must include, in tabular form, the numerical
#' evaluations for each solution (rows) in each criterion (column).
#' If the first column includes an identifier for each solution,
#' it will be read as a string.
#' In case it is indicated, through the corresponding parameters, that the
#' file does not have these identifiers, then the function will assign names
#' following an ascending order (e.g. S1, S2,... for the solutions,
#' and C1, C2,... for the criteria).
#'
#' @param path the path to the csv file containing the evaluation matrix
#' @param solutions_ids a boolean value indicating whether the file includes
#' identifiers for the solutions in the first column. The default value is TRUE.
#' @param criteria_ids a boolean value indicating whether the file includes
#' identifiers for the criteria in the first row. The default value is TRUE.
#'
#' @return an matrix containing the evaluations
#' @export
#'
#' @examples
#' # eval_mat <- read_eval_matrix_csv("eval_mat.csv")
#'
read_eval_matrix_csv <- function(path, solutions_ids = TRUE, criteria_ids = TRUE) {
  dt_eval <- utils::read.csv(file = path, header = criteria_ids)

  if (solutions_ids) {
    eval_mat <- as.matrix(dt_eval[, -1])
    rownames(eval_mat) <- dt_eval[, 1]
  } else {
    eval_mat <- as.matrix(dt_eval)
    rownames(eval_mat) <- generate_solutions_names(nsol = nrow(eval_mat))
  }

  if (!criteria_ids) {
    colnames(eval_mat) <- generate_criteria_names(ncrit = ncol(eval_mat))
  }

  return(eval_mat)
}



#' Writes an output or evaluation matrix to a csv file
#'
#' Writes a solution type matrix (e.g. with solution identifiers as row names)
#' to a specified csv file.
#'
#' @param output_matrix the matrix to be saved
#' @param path the path where the file will be saved ("output.csv" is set by
#' default).
#'
#' @return NULL
#' @export
#'
#' @examples
#' # write_output_csv(out_matrix, path="output_matrix.csv")
write_output_csv <- function(output_matrix, path = "output.csv") {
  dt_out <- as.data.frame(output_matrix)

  dt_out$Solution <- rownames(output_matrix)

  dt_out <- dt_out[, c(ncol(dt_out), 1:(ncol(dt_out) - 1))]

  utils::write.csv(x = dt_out, file = path, row.names = FALSE)
}




#' Create a random evaluation matrix
#'
#' This function creates a random evaluation matrix of dimension "nsol"
#' by "ncrit" and entries ranging between 0 and 1. The entry values are
#' generated following a uniform random distribution.
#'
#' @param nsol number of solutions
#' @param ncrit number of criteria
#'
#' @return an nsol by ncrit matrix with entries ranging between 0 and 1
#' @export
#'
#' @examples
#' # An example matrix containing the evaluations for 10
#' # solutions at three criteria
#' example_matrix <- create_eval_matrix_example(10, 3)
#'
create_eval_matrix_example <- function(nsol = 10, ncrit = 3) {
  if (!assertthat::is.count(nsol)) {
    cli::cli_abort(
      c(
        "{.var nsol} must be a non-negative integer"
      )
    )
  }

  if (!assertthat::is.count(ncrit)) {
    cli::cli_abort(
      c(
        "{.var ncrit} must be a non-negative integer"
      )
    )
  }

  eval_mat <- matrix(stats::runif(n = nsol * ncrit), nrow = nsol)

  rownames(eval_mat) <- paste0("S", 1:nsol)

  colnames(eval_mat) <- paste0("C", 1:ncrit)

  return(eval_mat)
}





#' Vertices of the polyhedron of feasible weights
#'
#' Generates an n by n matrix of the vertices of the polyhedron
#' of the feasible region of weights. This region is induced by the following
#' set of constraints: wi >= 0 (for all i = 1...n), w1+...+w2 = 1 and
#' w1 <=...<= wn, where wj is the weight of the jth most important criterion
#' (according to the order of preference given by the decision maker).
#' In particular, w1 is the weight of the most important criterion,
#' while wn is the weight of the least important criterion.
#' The order of the rows of the resulting matrix is consistent with
#' this descendant order of preference. For example,
#' the first row corresponds to the most important criterion
#' (e.g. the first in the order of preference),
#' the second column to the second most important criterion, and so on.
#' The columns of the matrix correspond to the coordinates of the polyhedron.
#' This matrix is required for computing the range (intervals) of possible
#' scores for each solution.
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


vertify <- function(A_matrix, b_vector) {
  V <- rcdd::scdd(
    rcdd::makeH(
      a1 = A_matrix,
      b1 = b_vector,
      a2 = rep(1, ncol(A_matrix)),
      b2 = c(1)
    )
  )$output[, -(1:2)]

  return(
    t(apply(V, MARGIN = 2, rev))
  )
}
