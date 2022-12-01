

identify_sois <- function(eval_matrix,
                          crit_preference = NA,
                          by = "neutral",
                          threshold = 0.0) {
  c_pref <- 1:ncol(eval_matrix)

  if (!is.na(crit_preference)) {
    c_pref <- crit_preference
  }

  e_matrix <- eval_matrix[, c_pref]

  e_matrix %>%
    evaluate_at_vertices() %>%
    make_intervals() %>%
    identify_reference() %>%
    compare_with_reference(by = by) %>%
    filter_by_threshold(
      by = by,
      threshold = threshold
    )
}


identify_sois_geom <- function(eval_matrix,
                               crit_preference = NA,
                               by = "volume",
                               threshold = 0.0) {
  c_pref <- 1:ncol(eval_matrix)

  if (!is.na(crit_preference)) {
    c_pref <- crit_preference
  }

  e_matrix <- eval_matrix[, c_pref]

  e_matrix %>%
    evaluate_at_vertices() %>%
    make_intervals() %>%
    identify_reference() %>%
    geom_compare(by = by) %>%
    filter_by_threshold(
      by = by,
      threshold = threshold
    )
}



identify_top_sois <- function(eval_matrix,
                              crit_preference,
                              by = "neutral",
                              n) {




}
