

make_intervals <- function(eval_at_vert_matrix,
                           append_output = TRUE) {
  
  # Filter the score matrix (those column with VE_ prefix)
  
  col_names <- get_eval_vertex_names(colnames(eval_at_vert_matrix))
  
  true_eav_matrix <- eval_at_vert_matrix[, col_names]
  
  if (length(true_eav_matrix) == 0){
    true_eav_matrix <- eval_at_vert_matrix
  }

  # Get the scoring interval for each solution
  interval_matrix <- apply(true_eav_matrix, MARGIN = 1, FUN = range)
  interval_matrix <- t(interval_matrix)
  colnames(interval_matrix) <- c("LB", "UB")
  
  if (append_output) {
    return(
      cbind(eval_at_vert_matrix, interval_matrix)
    )
  }
  
  return(interval_matrix)
}



