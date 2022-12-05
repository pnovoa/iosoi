

#' Plots the score intervals of the solutions
#'
#' This function creates a graph of intervals representing the range of
#' possible scores that each solution can achieve, i.e., based on the
#' evaluations by criteria and the order of preference of these criteria.
#'
#' @param interval_matrix A matrix of m rows containing at least three columns:
#' LB, UB and REF, corresponding to the lower bound of the interval, the upper
#' bound, and the type of solution (reference solution = 1, other solution = 0),
#' respectively.
#' @param highlight_reference Whether to highlight the reference solution
#' (default) or not.
#'
#' @return A segment plot with scores on the x-axis and solutions on the y-axis.
#' @export
#'
#' @examples
#' create_eval_matrix_example() %>%
#' assess_all() %>%
#' plot_intervals()
#'
plot_intervals <- function(interval_matrix,
                           highlight_reference = TRUE) {
  sol_labels <- rownames(interval_matrix)

  if (is.null(sol_labels)) {
    sol_labels <- generate_solutions_names(nrow(interval_matrix))
  }

  ref_idx <- which.max(interval_matrix[, get_reference_name()])

  xs <- append(interval_matrix[, "LB"], interval_matrix[, "UB"])
  ys <- rep(1:nrow(interval_matrix), 2)

  x0 <- interval_matrix[, "LB"]
  x1 <- interval_matrix[, "UB"]

  y0 <- 1:nrow(interval_matrix)

  graphics::plot(
    x = xs,
    y = ys,
    xlab = "Score",
    type = "n",
    ylab = "Solution",
    yaxt = "n",
    main = "Intervals of solution scores"
  )
  graphics::axis(side = 2, at = 1:nrow(interval_matrix), labels = sol_labels)
  graphics::segments(x0 = x0, y0 = y0, x1 = x1)
  graphics::points(x = xs, y = ys, pch = 16)
  if (highlight_reference) {
    graphics::segments(
      x0 = x0[ref_idx],
      y0 = y0[ref_idx],
      x1 = x1[ref_idx],
      col = "red",
      lwd = "2"
    )
    graphics::points(
      x = c(x0[ref_idx], x1[ref_idx]),
      y = c(ref_idx, ref_idx),
      pch = 16,
      col = "red"
    )
  }
}



#' Plots the assessments of the solutions
#'
#' This function creates a bar plot corresponding to the solution assessments.
#'
#' @param result_matrix A matrix of m rows containing at least two columns:
#' the assessment indicator (e,g. neutral, volume, etc) and 'REF' (type of
#' solution, where is 1 for the reference solution and 0 for the rest).
#' @param by The assessment indicator (e.g. neutral,
#' pessimistic, optimistic, volume, poss_volume, vert_prop, etc.).
#'
#' @return A bar plot with assessment values on the y-axis
#' and solutions on the x-axis.
#' @export
#'
#' @examples
#' create_eval_matrix_example() %>%
#' assess_all() %>%
#' plot_assessment(by="neutral")
#'
plot_assessment <- function(result_matrix, by) {
  what_label <- paste0(
    toupper(substr(by, 1, 1)),
    substr(by, 2, nchar(by))
  )

  graphics::barplot(
    height = result_matrix[, by],
    names = rownames(result_matrix),
    col = result_matrix[, get_reference_name()],
    ylab = what_label,
    xlab = "Solution",
    main = what_label
  )
}
