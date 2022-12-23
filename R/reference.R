
#' Identify the reference solution
#'
#' @param interval_matrix an \eqn{n \times p} matrix (with \eqn{p \geq 2})
#' containing at least two columns with names \code{'LB'} and \code{'UB'} (if
#' \code{max_lower_bound} is set as selection function, or at least one column 
#' (\code{'volume'}) if \code{max_volume} is set as the selection function.
#' @param sel_fun the selection criterion function. By default is
#' \code{max_lower_bound}, which select the solution with the maximum lower
#' bound (\code{LB}) as the reference solution.
#' @param append_output Whether or not the result is append to the input matrix
#' \code{interval_matrix}. It is \code{TRUE} by default.
#'
#' @return a matrix with at least one column (named as \code{'REF'}) with 1 for
#' the reference solution and 0 for the rest.
#'
#' @export
#'
#' @examples
#' create_eval_matrix_example(10, 3) %>%
#'   score() %>%
#'   intervals() %>%
#'   reference()
#'
reference <- function(interval_matrix,
                      sel_fun = max_lower_bound,
                      append_output = TRUE) {
  range_names <- get_range_names()

  # if (sum(range_names %in% colnames(interval_matrix))) {
  #   true_int_mat <- interval_matrix[, range_names]
  # } else {
  #   true_int_mat <- interval_matrix
  # }

  ref_sol_index <- sel_fun(interval_matrix)

  label_matrix <- matrix(rep(0, nrow(interval_matrix)), byrow = FALSE, ncol = 1)
  label_matrix[ref_sol_index, 1] <- 1
  colnames(label_matrix) <- c(get_reference_name())

  if (append_output) {
    return(cbind(
      interval_matrix,
      label_matrix
    ))
  }
  return(label_matrix)
}



max_lower_bound <- function(interval_matrix) {
  return(
    which.max(interval_matrix[, get_range_names()[1]])
  )
}


max_volume <- function(volume_matrix){
  return(
    which.max(volume_matrix[, "volume"])
  )
}

max_mean_score <- function(volumes_matrix){
  return(
    which.max(volume_matrix[, "volume"])
  )
}
