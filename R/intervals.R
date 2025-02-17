
#' Scoring interval for each solution
#'
#' This function returns, for each solution, its range of possible scores
#' (intervals).
#'
#' @param eval_at_vert_matrix An m by n matrix containing the scores of
#' each of the m solutions (rows) at each of the n vertices (columns).
#' @param append_output A Boolean parameter indicating whether (or not) the
#' results will be append to the input matrix. It is \code{TRUE} by default.
#'
#' @return An m by 2 matrix containing the lower bound (LB) and
#' upper bound (UB) for each of the m solutions (rows).
#' @export
#'
#' @examples
#' # Scoring two solutions on three criteria
#' m <- matrix(c(1, 2, 3, 4, 3, 2), byrow = TRUE, nrow = 2)
#' intervals(m)
intervals <- function(eval_at_vert_matrix,
                      append_output = TRUE) {
  # Filter the score matrix (those column with VE_ prefix)

  col_names <- get_prefixed_column_names(colnames(eval_at_vert_matrix))

  true_eav_matrix <- eval_at_vert_matrix[, col_names]

  if (length(true_eav_matrix) == 0) {
    true_eav_matrix <- eval_at_vert_matrix
  }
  
  ncrit <- ncol(true_eav_matrix)
  weights_matrix <- generate_polyhedron_vertices(ncrit = ncrit)

  # Get the scoring interval for each solution
  interval_matrix <- apply(true_eav_matrix, MARGIN = 1, FUN = range)
  interval_matrix <- t(interval_matrix)
  colnames(interval_matrix) <- c("LB", "UB")
  
  weigth_indexes_matrix <- apply(interval_matrix, MARGIN = 1, FUN = function(x){
    xmin <- which.min(x) 
    xmax <- which.max(x)
    weight_bounds <- weights_matrix[, c(xmin, xmax)]
    return(weight_bounds)
  })
  
  weigth_indexes_matrix <- t(weigth_indexes_matrix)
  colnames(weigth_indexes_matrix) <- c(paste0(get_weights_prefix(), "_LB_", 1:ncrit),
                                       paste0(get_weights_prefix(), "_UB_", 1:ncrit)
                                       )

  if (append_output) {
    return(
      cbind(eval_at_vert_matrix, interval_matrix, weigth_indexes_matrix)
    )
  }

  return(interval_matrix)
}
