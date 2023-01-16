


area_assess <- function(score_matrix, append_output = TRUE){

  score_cols <- get_prefixed_column_names(
    colnames(score_matrix),
    prefix = get_eval_vertex_prefix()
  )

  AUES <- apply(score_matrix[,score_cols], MARGIN = 1, function(sol_scores){
    points <- gen_poly_points(sol_scores)
    poly_area(points[, "x"], points[, "y"])
  })

  if(append_output){
    return(
      cbind(score_matrix, AUES)
    )
  }

   return(
     AUES
   )

}

gen_poly_points <- function(scores){
  l <- length(scores)
  x <- c(1, 1:l, l, 1)
  y <- c(0, scores, 0, 0)
  res <- matrix(c(x, y), byrow = TRUE, nrow = 2)
  res <- t(res)
  colnames(res) <- c("x", "y")
  return(
    res
  )
}

poly_area <- function(x0, y0){

  l <- length(x0)

  x1 <- x0[2:l]
  y1 <- y0[2:l]

  m <- matrix(c(x0[-l], y0[-l], x1, y1), ncol = 4)

  colnames(m) <- c("x0", "y0", "x1", "y1")

  0.5*abs(sum(m[,"x0"]*m[,"y1"] - m[,"x1"]*m[,"y0"]))

}
