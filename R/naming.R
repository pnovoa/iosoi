

#' Automatic names for solutions
#'
#' It generates names for the solutions consecutively.
#'
#' @param nsol The number of solutions.
#'
#' @return A character vector of length \code{nsol}.
generate_solutions_names <- function(nsol) {
  paste0(get_solutions_prefix(), 1:nsol)
}


#' Name prefix for criteria columns
#'
#' @return Return the prefix for the automatic names of solutions.
get_solutions_prefix <- function() {
  return(
    "S"
  )
}

#' Automatic names for criteria
#'
#' It generates names for the criteria consecutively.
#'
#' @param ncrit The number of criteria
#'
#' @return A character vector of length \code{ncrit}.
generate_criteria_names <- function(ncrit) {
  paste0(get_criteria_prefix(), 1:ncrit)
}

#' Automatic names for weights
#'
#' It generates names for the weights consecutively.
#'
#' @param ncrit The number of weights
#'
#' @return A character vector of length \code{ncrit}.
generate_weights_names <- function(ncrit) {
  paste0(get_weights_prefix(), 1:ncrit)
}

#' Name prefix for criteria columns
#'
#' @return Return the prefix for the automatic names of criteria.
get_criteria_prefix <- function() {
  return("C")
}

#' Name prefix for weights columns
#'
#' @return Return the prefix for the automatic names of weights.
get_weights_prefix <- function() {
  return("W")
}

#' Name prefix for vertices score columns
#'
#' @return Return the prefix for the automatic names of vertices.
get_eval_vertex_prefix <- function() {
  return("VE_")
}


#' Generation of the column names for the vertex scores
#'
#' @param colum_names a character vector containing several column names
#' @param prefix the prefix used to denote the names of the columns
#' corresponding to vertex scores
#'
#' @return the column names of the vertices scores
get_prefixed_column_names <- function(colum_names, prefix = "VE_") {
  grepl(
    paste0("^", prefix),
    colum_names
  )
}

#' Name of the reference solution column
#'
#' @return a character value
get_reference_name <- function() {
  return(
    "REF"
  )
}

#' Names for the interval columns
#'
#' @return a 2-dimensional character vector
get_range_names <- function() {
  return(
    c("LB", "UB")
  )
}


#' Decision-maker attitudes
#'
#' @return a 3-dimensional character vector with the names of
#' the decision-maker attitudes. These attitudes are used for assessing the
#' solutions from the possibility approach of Torres et al (2021).
get_poss_approaches <- function() {
  return(
    c(
      "optimistic",
      "neutral",
      "pessimistic"
    )
  )
}

#' Geometric approaches
#'
#' @return a 3-dimensional character vector with the names of
#' the geometric approaches used to assess the solutions.
get_geom_approaches <- function() {
  return(
    c(
      "volume",
      "poss_volume",
      "vert_prop"
    )
  )
}


#' Automatic names for the rows and columns of the evaluation matrix
#' 
#' This function automatically assigns names to the solutions (rows) and
#' criteria (columns) of an evaluation matrix if they are not already set.
#'
#' @param eval_matrix the evaluation matrix
#'
#' @return the same matrix with names for its rows and columns.
#' @export
#'
#' @examples
#' E <- matrix(runif(15), byrow = TRUE, nrow = 5)
#' E %>% name_eval_matrix()
name_eval_matrix <- function(eval_matrix){
  
  ncrit <- ncol(eval_matrix)
  nsol <- nrow(eval_matrix)
  
  if(is.null(colnames(eval_matrix))){
    colnames(eval_matrix) <- generate_criteria_names(ncrit)
  }
  
  if(is.null(rownames(eval_matrix))){
    rownames(eval_matrix) <- generate_solutions_names(nsol)
  }
  
  return(eval_matrix)
}
