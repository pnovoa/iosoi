

compare_with_reference <- function(interval_matrix,
                                   range_columns = NA,
                                   reference_column = NA,
                                   compare_by = c("neutral"),
                                   append_output = TRUE){
  
  
  if (is.na(range_columns)){
    range_names <- get_range_names()
  } else{
    range_names <- range_columns
  }
  
  if (is.na(reference_column)) {
    ref_name <- get_reference_name()
  } else{
    ref_name <- reference_column
  }
  
  
  int_matrix <- interval_matrix[, range_names]
  ref_matrix <- interval_matrix[, ref_name]
  ref_sol_indx <- which.max(ref_matrix)
  ref_sol <- int_matrix[ref_sol_indx,]
  
  
  result_matrix <- compute_all_possibility_degrees(
    interval_matrix = int_matrix,
    ref_sol = ref_sol,
    att_list = compare_by
    )
  
  if (append_output) {
    return(
      cbind(interval_matrix, result_matrix)
    )
  }
  
  return(result_matrix)
  
}

