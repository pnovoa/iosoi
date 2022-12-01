
rank_solutions <- function(result_matrix, by, decreasing = TRUE) {
  result_matrix[order(result_matrix[, by], decreasing = decreasing), ]
}


filter_by_threshold <- function(result_matrix, by, threshold = 0.0) {
  sel_idx <- result_matrix[, by] > threshold

  result_matrix[sel_idx, ]
}


select_top_n_solutions <- function(result_matrix, by, n) {
  res_mat <- rank_solutions(result_matrix, by = by, decreasing = TRUE)

  row_names <- rownames(result_matrix) %in% rownames(res_mat[1:n, ])

  result_matrix[row_names, ]
}
