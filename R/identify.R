

#' Possibility approach for the identification of the solution of interest
#'
#' Identify the solutions of interest of a multi-criteria decision problem
#' by applying a possibility approach by Torres et al. (2021).
#'
#' @param eval_matrix the evaluation matrix. It is a \eqn{m \times n} matrix with
#' entries \eqn{e_{ij}}, corresponding to the evaluation of solution \eqn{i}
#' in the criteria \eqn{j}.
#' @param vert_matrix the coordinates of the vertices. It is a \eqn{n \times n} 
#' matrix with the coordinates of the vertices of the polyhedron of the feasible
#' weights located at the columns. If \code{NA} then it is assumed that the 
#' criteria follow a decreasing order of importance, that is, with criterion 
#' 1 more important than criterion 2, and so on.
#' @param by the attitude adopted by the decision-maker in order to assess each
#' solution. It can take one of the following values: \code{'neutral'} (default),
#' \code{'optimistic'}, or \code{'pessimistic'}.
#' @param threshold threshold to identify the solutions of interest.
#' In particular, a filter is applied on the attitude value of each solution
#' so that only those with values greater than or equal to threshold will
#' appear in the returned matrix.
#'
#' @return A matrix is returned that includes, for each solution, the interval
#' of scores, whether it is the reference solution or not, and the degrees
#' of possibility corresponding to the decision maker's attitude.
#' @export
#'
#' @examples
#' eval_mat <- create_eval_matrix_example()
#' eval_mat %>% poss_identify_sois()
#'
poss_identify_sois <- function(eval_matrix,
                               vert_matrix = NA,
                               by = "neutral",
                               threshold = 0.0) {
  eval_matrix %>%
    score(vert_matrix = vert_matrix) %>%
    intervals() %>%
    reference() %>%
    poss_assess(by = by) %>%
    filter_sois(
      by = by,
      threshold = threshold
    )
}



#' Geometric approach for the identification of the solution of interest
#'
#' Identify the solutions of interest of a multi-criteria decision problem
#' by applying a geometric assessment approach.
#'
#' @param eval_matrix the evaluation matrix. It is a \eqn{m \times n} matrix with
#' entries \eqn{e_{ij}}, corresponding to the evaluation of solution \eqn{i}
#' in the criteria \eqn{j}.
#' @param vert_matrix the coordinates of the vertices. It is a \eqn{n \times n} 
#' matrix with the coordinates of the vertices of the polyhedron of the feasible
#' weights located at the columns. If \code{NA} then it is assumed that the 
#' criteria follow a decreasing order of importance, that is, with criterion 
#' 1 more important than criterion 2, and so on.
#' @param by the geometric assessment approach.
#' It can take one of the following values: \code{'volume'}, 
#' \code{'poss_volume'}, or \code{'vert_prop'}.
#' @param threshold threshold to identify the solutions of interest.
#' In particular, a filter is applied on the assessment value of each solution
#' so that only those with values greater than or equal to threshold will
#' appear in the returned matrix.
#'
#' @return A matrix is returned that includes, for each solution, the interval
#' of scores, whether it is the reference solution or not, and the assessments
#' corresponding to the geometric approach.
#' @export
#'
#' @examples
#' eval_mat <- create_eval_matrix_example()
#' eval_mat %>% geom_identify_sois()
#'
geom_identify_sois <- function(eval_matrix,
                               vert_matrix = NA,
                               by = "volume",
                               threshold = 0.0) {
  eval_matrix %>%
    score(vert_matrix = vert_matrix) %>%
    intervals() %>%
    reference() %>%
    geom_assess(by = by) %>%
    filter_sois(
      by = by,
      threshold = threshold
    )
}



#' Performs all solution assessments
#'
#' This function evaluates solutions according to all existing approaches.
#' That is, both the possibility ones proposed in Torres et al (2021), as
#' well as the geometric ones.
#'
#' @param eval_matrix the evaluation matrix. It is a \eqn{m \times n} matrix with
#' entries \eqn{e_{ij}}, corresponding to the evaluation of solution \eqn{i}
#' in the criteria \eqn{j}.
#' @param vert_matrix the coordinates of the vertices. It is a \eqn{n \times n} 
#' matrix with the coordinates of the vertices of the polyhedron of the feasible
#' weights located at the columns. If \code{NA} then it is assumed that the 
#' criteria follow a decreasing order of importance, that is, with criterion 
#' 1 more important than criterion 2, and so on.
#'
#' @return A matrix is returned that includes, for each solution, the interval
#' of scores, whether it is the reference solution or not, and the degrees
#' of possibility corresponding to the decision maker's attitude.
#' @export
#'
#' @examples
#' eval_mat <- create_eval_matrix_example()
#' eval_mat %>% assess_all()
#'
assess_all <- function(eval_matrix,
                       vert_matrix = NA) {
  eval_matrix %>%
    score(vert_matrix = vert_matrix) %>%
    intervals() %>%
    reference() %>%
    poss_assess(by = "all") %>%
    geom_assess(by = "all")
}
