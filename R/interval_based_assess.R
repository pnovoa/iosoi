
#' Possibility assessment of solutions
#'
#' This function computes, for each solution, the possibility degree of being
#' superior than the reference solution.
#'
#' @param interval_matrix a \eqn{m \times p (p \geq 3)} matrix containing at
#' least three columns corresponding to the interval lower bound, interval
#' upper bound and the type of solution, respectively. Each row corresponds
#' to a solution.
#' @param interval_columns a 2-dimensional vector containing the names of the
#' matrix column corresponding to lower and upper bounds of the intervals. If
#' \code{NA}, then it will be assummed that the names are \code{'LB'} and
#' \code{'UB'}.
#' @param reference_column the name of the matrix column corresponding to the
#' type of solution. If \code{NA} then it will be assumed that the column is
#' named as \code{'REF'}.
#' @param reference_column the name of column corresponding to the reference
#' solution.
#' @param by a vector indicating the assessment indicators to compute. It can
#' be one or the combination of the following:
#' \describe{
#'   \item{\code{'neutral'}}{Neutral attitude.}
#'   \item{\code{'optimistic'}}{Optimistic attitude.}
#'   \item{\code{'pessimistic'}}{Pessimistic attitude.}
#'   \item{\code{'all'}}{For computing the three above indicators.}
#' }
#' @param append_output A Boolean value indicating whether the output is
#' append (default) or not to the input matrix.
#'
#' @return A \eqn{m \times p (p \geq 1)} matrix with the computed assessments
#' at the columns
#' @export
#'
#' @examples
#' create_eval_matrix_example(10, 3) %>%
#'   score() %>%
#'   intervals() %>%
#'   reference() %>%
#'   poss_assess()
#'
poss_assess <- function(interval_matrix,
                        interval_columns = NA,
                        reference_column = NA,
                        by = c("neutral"),
                        append_output = TRUE) {
  if (is.na(interval_columns)) {
    interval_names <- get_range_names()
  } else {
    interval_names <- interval_columns
  }

  if (is.na(reference_column)) {
    ref_name <- get_reference_name()
  } else {
    ref_name <- reference_column
  }


  int_matrix <- interval_matrix[, interval_names]
  ref_matrix <- interval_matrix[, ref_name]
  ref_sol_indx <- which.max(ref_matrix)
  ref_sol <- int_matrix[ref_sol_indx, ]

  if (by == "all") {
    by <- get_poss_approaches()
  }

  result_matrix <- sapply(by, function(att) {
    m <- apply(int_matrix, MARGIN = 1, FUN = function(solA) {
      return(compute_possibility_degree(solA, ref_sol, attitude = att))
    })
  })

  if (append_output) {
    return(
      cbind(interval_matrix, result_matrix)
    )
  }

  return(result_matrix)
}




#' Degree of possibility that solution A is superior to solution B.
#'
#' Given the ranges of possible scores (intervals) of two solutions,
#' this function returns the degree of possibility that solution A is
#' superior to solution B.
#'
#' @param A a vector of the score range (interval) for the solution A. The
#' first element is the lower bound, while the second is the upper bound.
#' @param B a vector of the score range (interval) for the solution B and with
#' the same structure as A.
#' @param attitude attitude of the decision-maker. The three possible options
#' are "neutral" (default), "optimistic", "pessimistic".
#'
#' @return a value between 0 and 1, where a value close to 0 means that
#' solution A is inferior to B, and a value close to 1, the opposite.
#' In case A = B, then the degree of possibility is 0.5.
#' @export
#'
#' @examples
#' A <- c(1, 3)
#' B <- c(2, 4)
#' # Neutral attitude
#' compute_possibility_degree(A, B)
#' # Pessimistic attitude
#' compute_possibility_degree(A, B, attitude = "pessimistic")
#' # Optimistic attitude
#' compute_possibility_degree(A, B, attitude = "optimistic")
compute_possibility_degree <- function(A, B, attitude = "neutral") {
  if (A[2] <= B[1]) {
    return(0)
  }

  if (A[1] >= B[2]) {
    return(1)
  }


  af <- neutral_attitude
  if (attitude == "pessimistic") {
    af <- pessimistic_attitude
  } else if (attitude == "optimistic") {
    af <- optimistic_attitude
  }

  return(
    (af(A[2]) - af(B[1])) / (af(A[2]) - af(A[1]) + af(B[2]) - af(B[1]))
  )
}


#' Neutral attitude
#'
#' This is an assistant function for computing the possibility degree as
#' proposed by Torres et al (2021).
#'
#' @param x a numeric value
#'
#' @return return x
#'
neutral_attitude <- function(x) {
  return(x)
}

#' Pessimistic attitude
#'
#' This is an assistant function for computing the possibility degree as
#' proposed by Torres et al (2021).
#'
#' @param x a numeric value
#'
#' @return return the \eqn{\log(x)}
#'
pessimistic_attitude <- function(x) {
  return(log(x))
}

#' Optimistic attitude
#'
#' This is an assistant function for computing the possibility degree as
#' proposed by Torres et al (2021).
#'
#' @param x a numeric value
#'
#' @return return \eqn{x\sqrt{x}}
#'
optimistic_attitude <- function(x) {
  return(x * sqrt(x))
}
