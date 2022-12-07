
#' Geometric assessment of solutions
#'
#' @param eval_matrix the evaluation matrix. It is a \eqn{m \times n} matrix with
#' entries \eqn{e_{ij}} where it represents the evaluation given to solution
#' \eqn{i} in the criterion \eqn{j}.
#' @param vert_matrix the vertices matrix. It is an \eqn{n \times n} matrix
#' with the coordinates of the vertices of the polyhedron of the feasible
#' weights in the columns. If \code{NA} then it is assumed that the criteria
#' follow a decreasing order of importance, that is, with criterion 1 more
#' important than criterion 2, and so on.
#' @param crit_columns the column names corresponding to the criteria. It is
#' a character vector. If \code{NA}, then it is assumed that the names are those
#' starting with the \code{'C'} prefix.
#' @param reference_column the name of column corresponding to the reference
#' solution.
#' @param by a vector indicating the assessment indicators to compute. It can
#' be one or the combination of the following values:
#' \describe{
#'   \item{\code{'volume'}}{Normalized volume difference}
#'   \item{\code{'poss_volume'}}{Normalized volume difference in the range
#'   \eqn{[0, 1]}.}
#'   \item{\code{'vert_prop'}}{Proportion of vertices where the solution
#'   is greater or equal than the reference solution}
#'   \item{\code{'all'}}{For computing the three above indicators.}
#' }
#'
#' @param append_output Whether the output is append (default) or not to the
#' input matrix.
#'
#' @return A m by p matrix with the computed assessments at the columns
#' @export
#'
#' @examples
#' create_eval_matrix_example(10, 3) %>%
#'   score() %>%
#'   intervals() %>%
#'   reference() %>%
#'   geom_assess()
#'
geom_assess <- function(eval_matrix,
                        vert_matrix = NA,
                        crit_columns = NA,
                        reference_column = NA,
                        by = c("volume"),
                        append_output = TRUE) {
  if (is.na(crit_columns)) {
    eval_col_names <- get_prefixed_column_names(colnames(eval_matrix),
      prefix = get_criteria_prefix()
    )
  } else {
    eval_col_names <- crit_columns
  }

  e_matrix <- eval_matrix[, eval_col_names]

  if (length(e_matrix) == 0) {
    cli::cli_abort(
      c("It was not possible to determine which
        columns of {.var eval_matrix} correspond
        to the evaluations of the solutions.")
    )
  }

  if (is.na(reference_column)) {
    ref_name <- get_reference_name()
  } else {
    ref_name <- reference_column
  }

  ref_col <- eval_matrix[, ref_name]

  if (length(ref_col) == 0) {
    cli::cli_abort(
      c(
        "Reference column no found.",
        paste0("Please provide an existing one
        in {.var reference_column}, or as a column in
        {. eval_matrix} with the name ", get_reference_name())
      )
    )
  }

  ref_sol_eval <- e_matrix[which.max(ref_col), ]

  if (is.na(vert_matrix)) {
    v_matrix <- generate_polyhedron_vertices(ncrit = sum(eval_col_names))
  } else {
    v_matrix <- vert_matrix
  }

  if (by == "all") {
    by <- get_geom_approaches()
  }


  result_matrix <- sapply(by, function(app) {
    m <- apply(e_matrix, MARGIN = 1, FUN = function(other_sol) {
      return(geom_compare(
        sol_eval = other_sol,
        ref_sol_eval = ref_sol_eval,
        vert_matrix = v_matrix,
        approach = app
      ))
    })
  })

  if (append_output) {
    return(
      cbind(
        eval_matrix,
        result_matrix
      )
    )
  }

  return(result_matrix)
}



#' Reference solution comparison
#'
#' Compute the degree to which a given solution outperforms the reference
#' solution. It acts as a function selector for the available geometric
#' approaches.
#'
#' @param sol_eval A vector containing the solution evaluation in the extreme
#' points (vertices of the polyhedron of feasible weights).
#' @param ref_sol_eval A vector containing the evaluations of the reference
#' solution in the extreme points (vertices of the polyhedron of feasible
#' weights).
#'
#' @param vert_matrix The matrix of vertices coordinates.
#' @param approach The geometric approach used to compare the solutions. It can
#' take one of the following character values:
#' \describe{
#'   \item{\code{'volume'}}{Normalized volume difference}
#'   \item{\code{'poss_volume'}}{Normalized volume difference in the range
#'   \eqn{[0, 1]}.}
#'   \item{\code{'vert_prop'}}{Proportion of vertices where the solution
#'   is greater or equal than the reference solution}
#' }
#'
#' @return A numeric value corresponding to the superiority degree of solution
#' 'sol_eval' over 'ref_sol_eval'.
#' @export
#'
#' @examples
#' vert_mat <- generate_polyhedron_vertices(3)
#' ref_sol_eval <- runif(3)
#' sol_eval <- runif(3)
#' geom_compare(sol_eval, ref_sol_eval, vert_mat)
#'
geom_compare <- function(sol_eval,
                         ref_sol_eval,
                         vert_matrix,
                         approach = "volume") {
  if (approach == "volume") {
    volume_compare(sol_eval, ref_sol_eval, vert_matrix)
  } else if (approach == "poss_volume") {
    poss_volume_compare(sol_eval, ref_sol_eval, vert_matrix)
  } else if (approach == "vert_prop") {
    sol_scores <- sol_eval %*% vert_matrix
    ref_sol_scores <- ref_sol_eval %*% vert_matrix
    best_vert_prop_compare(sol_scores, ref_sol_scores)
  }
}



#' Volume approach to compare against the reference solution
#'
#' This function compute the superiority degree of a given solution against the
#' reference solution. This degree is compute based on the notion of the
#' integral of a linear function (scoring function of a given solution) over
#' a simplex (polyhedron of feasible weights). Specifically, it returns the
#' normalized difference of the integrals over the sum of both integrals. Thus,
#' the range of values is \eqn{[-1, 1]} with \eqn{-1} indicating that the
#' solution is definitely worse than the reference solution, and \eqn{1}
#' indicating the opposite. In particular, if the result is \eqn{0}, it means
#' that the solution in question is equal to the reference solution.
#'
#' @param ref_sol_eval a vector with the reference solution evaluations at
#' extreme points (vertices of the polyhedron of feasible weights)
#' @param sol_eval a vector with the reference solution evaluations at extreme
#' points (vertices of the polyhedron of feasible weights)
#' @param vert_matrix a m by m matrix containing the vertices of the
#' polyhedron of feasible weights. The coordinates of these vertices must be
#' in the columns of the matrix.
#'
#' @return A numerical value in the range of \eqn{[-1, 1]} corresponding to the
#' superiority of solution \code{sol_eval} over the reference solution
#' (\code{ref_sol_eval})
#' @export
#'
#' @examples
#' vert_mat <- generate_polyhedron_vertices(3)
#' ref_sol_eval <- runif(3)
#' sol_eval <- runif(3)
#' volume_compare(sol_eval, ref_sol_eval, vert_mat)
#'
volume_compare <- function(sol_eval, ref_sol_eval, vert_matrix) {
  identity_matrix <- diag(length(ref_sol_eval))

  pol_ref_sol <- SimplicialCubature::definePoly(
    coef = ref_sol_eval,
    k = identity_matrix
  )

  pol_other_sol <- SimplicialCubature::definePoly(
    coef = sol_eval,
    k = identity_matrix
  )

  Vol_sol_ref <- SimplicialCubature::integrateSimplexPolynomial(
    p = pol_ref_sol,
    S = vert_matrix
  )$integral

  Vol_other_sol <- SimplicialCubature::integrateSimplexPolynomial(
    p = pol_other_sol,
    S = vert_matrix
  )$integral

  return((Vol_other_sol - Vol_sol_ref) / (Vol_other_sol + Vol_sol_ref))
}


poss_volume_compare <- function(sol_eval, ref_sol_eval, vert_matrix) {
  return(
    (volume_compare(sol_eval, ref_sol_eval, vert_matrix) + 1) / 2.
  )
}


#' Best vertices proportion
#'
#' Given a solution scores at the extreme points (vertices), this function
#' computes the proportion of vertices for which their corresponding scores
#' are equal or better than the ones from the reference solution. It is
#' another geometric approach to compare solutions.
#'
#' @param sol_scores an n-dimensional vector containing the solution scores at
#' the vertices.
#' @param ref_sol_scores an n-dimensional vector containing the reference
#' solution scores at the vertices.
#'
#' @return the proportion of the numerical scores that are better than the
#' reference solution. A value equal to 0 means that the solution has no scores
#' equal to or better than the reference solution, while a value equal to 1
#' indicates the opposite. The latter value is achieved by the reference
#' solution, i.e., if it has been identified as the one with the highest
#' lower score.
#' @export
#'
#' @examples
#' sol_scores <- runif(3)
#' ref_sol_scores <- runif(3)
#' best_vert_prop_compare(sol_scores, ref_sol_scores)
best_vert_prop_compare <- function(sol_scores, ref_sol_scores) {
  return(
    sum(sol_scores >= ref_sol_scores) / length(sol_scores)
  )
}
