

plot_intervals <- function(interval_matrix, highlight_reference = TRUE){

  sol_labels <- rownames(interval_matrix)

  if(is.null(sol_labels)){
    sol_labels <- generate_solutions_names(nrow(interval_matrix))
  }

  ref_idx <- which.max(interval_matrix[, get_reference_name()])

  xs <- append(interval_matrix[,"LB"], interval_matrix[,"UB"])
  ys <- rep(1:nrow(interval_matrix), 2)

  x0 <- interval_matrix[,"LB"]
  x1 <- interval_matrix[,"UB"]

  y0 <- 1:nrow(interval_matrix)

  graphics::plot(x=xs,
                 y=ys,
                 xlab = "Score",
                 type="n",
                 ylab="Solution",
                 yaxt = "n",
                main = "Intervals of solution scores")
  graphics::axis(side = 2, at = 1:nrow(interval_matrix), labels = sol_labels)
  graphics::segments(x0=x0, y0=y0, x1=x1)
  graphics::points(x=xs, y=ys, pch=16)
  if(highlight_reference){
    graphics::segments(x0=x0[ref_idx],
                       y0=y0[ref_idx],
                       x1=x1[ref_idx],
                       col = "red",
                       lwd = "2")
    graphics::points(x=c(x0[ref_idx],x1[ref_idx]),
                     y=c(ref_idx, ref_idx),
                     pch=16,
                     col="red")
  }
}
