



identify_sois <- function(eval_matrix,
                          crit_pref = NA,
                          include_interval = TRUE,
                          att_output=c("neutral")){


  # TODO: Check equal sizes between eval_matrix columns and crit_pref length.
  #       Validate the composition of input data



  ncrit <- ncol(x = eval_matrix)
  nsol <- nrow(x = eval_matrix)

  # Assume that criteria are represented in ascending order of preference in
  # the columns of eval_matrix

  eval_mat <- eval_matrix

  if(!is.na(x = crit_pref)){ # A criteria preference is provided

    eval_mat <- eval_mat[,crit_pref] # Reflecting the preference in eval_mat

  }

  # Getting the polyhedron vertices
  ver_mat <- generate_polyhedron_vertices(n = ncrit)

  # Computing the scores for each solution at each vertex
  vert_score_mat <- eval_mat %*% ver_mat

  # Get the scoring interval for each solution
  interval_matrix <- compute_intervals(vertex_scores_matrix = vert_score_mat)

  # Identifying the reference solution index (the one with the max lower bound)
  ref_sol_ind <- which.max(interval_matrix[,"LB"])

  # Getting the possibility degree for each solution against the reference one.

  poss_mat <- compute_all_possibility_degrees(
    interval_matrix = interval_matrix,
    ref_sol = interval_matrix[ref_sol_ind,],
    att_list = att_output
    )

  if(is.null(rownames(eval_mat))){
    rownames(interval_matrix) <- 1:nsol
  }

  result_matrix <- cbind(interval_matrix, poss_mat)

  return(result_matrix)

}





filter_sois_by_threshold <- function(sois_matrix,
                             filter_by = "neutral",
                             threshold = 0.0,
                             sort_output=TRUE){

  # TODO: Validate sois_matrix if contains filter_by and 0 <= threshold <= 1.



  filter_idxs <- sois_matrix[, filter_by] > threshold

  result_mat <- sois_matrix[filter_idxs,]

  if(sort_output){
    result_mat <- result_mat[order(result_mat[, filter_by], decreasing = TRUE),]
  }

  return(
    result_mat
  )


}


filter_top_n_sois <- function(sois_matrix, top_n, sort_by = "neutral"){

  # TODO: Validate sois_matrix if contains sort_by and its sizes vs. top_n




  sorted_mat <- sois_matrix[order(sois_matrix[, sort_by], decreasing = TRUE),]

  top_range <- max(1, top_n)
  top_range <- min(nrow(sorted_mat), top_range)

  return(
    sorted_mat[1:top_range,]
  )

}




