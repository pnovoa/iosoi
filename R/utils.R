

read_eval_matrix_csv <- function(path){

  dt_eval <- read.csv(file=path)
  eval_mat <- as.matrix(dt_eval[,-1])

  rownames(eval_mat) <- dt_eval[,1]
  colnames(eval_mat) <- colnames(dt_eval)[-1]

  return(eval_mat)

}


write_output_csv <- function(output_matrix, path = "output.csv"){

  dt_out <- as.data.frame(output_matrix)

  dt_out$Solution <- rownames(output_matrix)

  dt_out <- dt_out[, c(ncol(dt_out),1:(ncol(dt_out)-1))]

  write.csv(x = dt_out, file = path, row.names = FALSE)

}


create_eval_matrix_example <- function(nsol=10, ncrit=3){

  eval_mat <- matrix(runif(n = nsol*ncrit), nrow = nsol)

  rownames(eval_mat) <- paste0("S", 1:nsol)

  colnames(eval_mat) <- paste0("C", 1:ncrit)

  return(eval_mat)

}


save_eval_matrix_csv_example <- function(eval_matrix){

  write.csv(x = eval_matrix, file = "eval_mat_example.csv")

}
