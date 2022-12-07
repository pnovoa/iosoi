
#' Rank a matrix with assessed solutions
#'
#' This is an assistant function for sorting the solutions according to a
#' given assessment indicator (ie. \code{'neutral'}, \code{'volume'}, etc.).
#'
#' @param result_matrix a \eqn{m \times p} matrix (\eqn{p \geq 1}) containing an
#' assessment for each of the \eqn{m} solutions.
#' @param by the name of the assessment indicator.
#' @param decreasing whether or not the solutions are sorted in decreasing
#' (default) order.
#'
#' @return A matrix with the same dimension as \code{result_matrix} and sorted
#' according to \code{by} and \code{decreasing}.
#' @export
#'
#' @examples
#' create_eval_matrix_example() %>%
#'   poss_identify_sois(threshold = -1) %>%
#'   rank_sois(by = "neutral")
rank_sois <- function(result_matrix, by, decreasing = TRUE) {
  result_matrix[order(result_matrix[, by], decreasing = decreasing), ]
}


#' Filtering the solution of interest
#'
#' This assistant function filter a result matrix of assessed solutions
#' according a given threshold. More specifically, it returns those solutions
#' (rows) with values for the assessment column greater or equal to the
#' provided threshold.
#'
#' @param result_matrix a \eqn{m \times p} matrix (\eqn{p \geq 1}) containing an
#' assessment for each of the \eqn{m} solutions.
#' @param by the name of the assessment indicator by which the solutions will
#' be filtered.
#' @param threshold a number corresponding to the threshold.
#'
#' @return a \eqn{k \times p} matrix (\eqn{k \leq m}) with those solutions
#' that meet the filter condition.
#' @export
#'
#' @examples
#' create_eval_matrix_example() %>%
#'   score() %>%
#'   intervals() %>%
#'   reference() %>%
#'   poss_assess() %>%
#'   filter_sois(by = "neutral", threshold = 0.0)
filter_sois <- function(result_matrix, by, threshold = 0.0) {
  sel_idx <- result_matrix[, by] > threshold

  result_matrix[sel_idx, ]
}


#' Selecting the top n solutions of interest
#'
#' This is an assistant function to get the top n solutions according to
#' a given assessment indicator.
#'
#' @param result_matrix a \eqn{m \times p} matrix (\eqn{p \geq 1}) containing an
#' assessment for each of the \eqn{m} solutions.
#' @param by the name of the assessment indicator by which the solution will
#' be selected.
#' @param n the number of solutions to be selected.
#'
#' @return a \eqn{n \times p} matrix (\eqn{n \leq m}) with those solutions
#' with the higher values for the assessment indicator defined in \code{by}.
#' @export
#'
#' @examples
#' create_eval_matrix_example() %>%
#'   score() %>%
#'   intervals() %>%
#'   reference() %>%
#'   poss_assess() %>%
#'   select_sois(by = "neutral", n = 3)
#'   
select_sois <- function(result_matrix, by, n) {
  res_mat <- rank_sois(result_matrix, by = by, decreasing = TRUE)
  row_names <- rownames(result_matrix) %in% rownames(res_mat[1:n, ])
  result_matrix[row_names, ]
}
