
reference <- function(interval_matrix,
                      sel_fun = max_lower_bound,
                      append_output = TRUE) {
  range_names <- get_range_names()

  if (sum(range_names %in% colnames(interval_matrix))) {
    true_int_mat <- interval_matrix[, range_names]
  } else {
    true_int_mat <- interval_matrix
  }

  ref_sol_index <- sel_fun(true_int_mat)

  label_matrix <- matrix(rep(0, nrow(true_int_mat)), byrow = FALSE, ncol = 1)
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
    which.max(interval_matrix[, 1])
  )
}
