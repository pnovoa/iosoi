
compute_other_approach <- function(other_sol_eval,
                                   ref_sol_eval,
                                   vert_matrix,
                                   approach = "volume") {
  # If approach = "volume"
  af <- volume_approach
  if (approach == "poss_volume") {
    af <- possibility_ranged_volume_approach
    # } else {
    #  af <- optimistic_attitude
  }

  af(ref_sol_eval, other_sol_eval, vert_matrix)
}

compute_all_other_approaches <- function(eval_matrix,
                                         ref_sol_eval,
                                         vert_matrix,
                                         approaches = c("volume")) {
  result <- sapply(approaches, function(app) {
    m <- apply(eval_matrix, MARGIN = 1, FUN = function(other_sol) {
      return(compute_other_approach(
        other_sol_eval = other_sol,
        ref_sol_eval = ref_sol_eval,
        vert_matrix = vert_matrix,
        approach = app
      ))
    })
  })

  return(result)
}


volume_approach <- function(ref_sol_eval, other_sol_eval, vert_matrix) {
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


possibility_ranged_volume_approach <- function(ref_sol_eval, other_sol_eval, vert_matrix) {
  identity_matrix <- diag(length(ref_sol_eval))

  return(
    (volume_approach(ref_sol_eval, other_sol_eval, vert_matrix) + 1) / 2.
  )
}


# compute_volume_approach <- function(eval_matrix, vert_matrix, eval_ref_sol) {
#   volumes <- apply(
#     X = eval_matrix,
#     MARGIN = 1,
#     FUN = function(sol) {
#       normalized_volume_difference(
#         ref_sol = eval_ref_sol,
#         other_sol = sol,
#         vert_matrix = vert_matrix
#       )
#     }
#   )
# }
