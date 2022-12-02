score <- function(eval_matrix,
                  vert_matrix = NA,
                  append_output = TRUE) {
  ncrit <- ncol(x = eval_matrix)
  nsol <- nrow(x = eval_matrix)

  # Assume that criteria are represented in ascending order of preference in
  # the columns of eval_matrix

  eval_mat <- eval_matrix

  empty_vert_matrix <- is.na(x = vert_matrix) | is.null(x = vert_matrix)


  if (empty_vert_matrix) { # A criteria preference is not provided

    v_mat <- generate_polyhedron_vertices(ncrit = ncrit)
    criteria_names <- colnames(eval_matrix)
  } else {
    v_mat <- vert_matrix
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
