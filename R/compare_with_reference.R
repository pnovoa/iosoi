

compare_with_reference <- function(interval_matrix,
                                   range_columns = NA,
                                   reference_column = NA,
                                   by = c("neutral"),
                                   append_output = TRUE) {
  if (is.na(range_columns)) {
    range_names <- get_range_names()
  } else {
    range_names <- range_columns
  }

  if (is.na(reference_column)) {
    ref_name <- get_reference_name()
  } else {
    ref_name <- reference_column
  }


  int_matrix <- interval_matrix[, range_names]
  ref_matrix <- interval_matrix[, ref_name]
  ref_sol_indx <- which.max(ref_matrix)
  ref_sol <- int_matrix[ref_sol_indx, ]


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


neutral_attitude <- function(x) {
  return(x)
}

pessimistic_attitude <- function(x) {
  return(log(x))
}

optimistic_attitude <- function(x) {
  return(x * sqrt(x))
}
