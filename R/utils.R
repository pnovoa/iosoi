

#' Read an evaluation matrix from a csv file
#'
#' This function helps to read an evaluation matrix from a csv file. The
#' contents of the file must include, in tabular form, the numerical
#' evaluations for each solution (rows) in each criterion (column).
#' If the first column includes an identifier for each solution,
#' it will be read as a string. The same is true for the criteria.
#' In case it is indicated, through the corresponding parameters, that the
#' file does not have these identifiers, then the function will assign names
#' following an ascending order (e.g. S1, S2,... for the solutions,
#' and C1, C2,... for the criteria).
#'
#' @param path the path to the csv file containing the evaluation matrix
#' @param solutions_ids a boolean value indicating whether the file includes
#' identifiers for the solutions in the first column. The default value is TRUE.
#' @param criteria_ids a boolean value indicating whether the file includes
#' identifiers for the criteria in the first row. The default value is TRUE.
#'
#' @return an matrix containing the evaluations
#' @export
#'
#' @examples
#' # eval_mat <- read_eval_matrix_csv("eval_mat.csv")
#'
read_eval_matrix_csv <- function(path, solutions_ids=TRUE, criteria_ids=TRUE){

  dt_eval <- utils::read.csv(file=path, header = criteria_ids)

  if(solutions_ids){
    eval_mat <- as.matrix(dt_eval[,-1])
    rownames(eval_mat) <- dt_eval[,1]
  } else{
    eval_mat <- as.matrix(dt_eval)
    rownames(eval_mat) <- paste0("S", 1:nrow(eval_mat))
  }

  if(!criteria_ids){
    colnames(eval_mat) <- paste0("C", 1:ncol(eval_mat))
  }

  return(eval_mat)
}



#' Writes an output or evaluation matrix to a csv file
#'
#' Writes a solution type matrix (e.g. with solution identifiers as row names)
#' to a specified csv file.
#'
#' @param output_matrix the matrix to be saved
#' @param path the path where the file will be saved ("output.csv" is set by
#' default).
#'
#' @return NULL
#' @export
#'
#' @examples
#' # write_output_csv(out_matrix, path="output_matrix.csv")
write_output_csv <- function(output_matrix, path = "output.csv"){

  dt_out <- as.data.frame(output_matrix)

  dt_out$Solution <- rownames(output_matrix)

  dt_out <- dt_out[, c(ncol(dt_out),1:(ncol(dt_out)-1))]

  utils::write.csv(x = dt_out, file = path, row.names = FALSE)

}




#' Create a random evaluation matrix
#'
#' This function creates a random evaluation matrix of dimension "nsol"
#' by "ncrit" and entries ranging between 0 and 1. The entry values are
#' generated following a uniform random distribution.
#'
#' @param nsol number of solutions
#' @param ncrit number of criteria
#'
#' @return an nsol by ncrit matrix with entries ranging between 0 and 1
#' @export
#'
#' @examples
#' # An example matrix containing the evaluations for 10
#' # solutions at three criteria
#' example_matrix <- create_eval_matrix_example(10,3)
#'
create_eval_matrix_example <- function(nsol=10, ncrit=3){

  eval_mat <- matrix(stats::runif(n = nsol*ncrit), nrow = nsol)

  rownames(eval_mat) <- paste0("S", 1:nsol)

  colnames(eval_mat) <- paste0("C", 1:ncrit)

  return(eval_mat)

}


