
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

  if(by == "all"){
    by <- get_geom_approaches()
  }


  result_matrix <- sapply(by, function(app) {
    m <- apply(e_matrix, MARGIN = 1, FUN = function(other_sol) {
      return(compute_other_approach(
        other_sol_eval = other_sol,
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


compute_other_approach <- function(other_sol_eval,
                                   ref_sol_eval,
                                   vert_matrix,
                                   approach = "volume") {
  if (approach == "volume") {
    volume_compare(ref_sol_eval, other_sol_eval, vert_matrix)
  }
  else if (approach == "poss_volume") {
    poss_volume_compare(ref_sol_eval, other_sol_eval, vert_matrix)
  } else if (approach == "vert_prop"){
    vert_prop_compare(ref_sol_eval, other_sol_eval)
  }
}


volume_compare <- function(ref_sol_eval, other_sol_eval, vert_matrix) {

  identity_matrix <- diag(length(ref_sol_eval))

  pol_ref_sol <- SimplicialCubature::definePoly(
    coef = ref_sol_eval,
    k = identity_matrix
  )

  pol_other_sol <- SimplicialCubature::definePoly(
    coef = other_sol_eval,
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



poss_volume_compare <- function(ref_sol_eval, other_sol_eval, vert_matrix) {

  return(
    (volume_compare(ref_sol_eval, other_sol_eval, vert_matrix) + 1) / 2.
  )
}


vert_prop_compare <- function(ref_sol_eval, other_sol_eval, vert_matrix){

  return(
    sum(other_sol_eval >= ref_sol_eval) / length(other_sol_eval)
  )

}
