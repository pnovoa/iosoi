


find_similar_preferences <- function(eval_matrix, current_preference, current_rank, by, n_top){

  n_crit <- length(current_preference)

  V = generate_polyhedron_vertices(ncrit = n_crit)

  curr_rank_sols <- rownames(current_rank)[1:n_top]

  fitness <- function(x){

    if (is.null(dim(x))) {
      x <- matrix(x, nrow = 1)
    }

    mat_fitness <- apply(x, MARGIN = 1, function(xi){

      Vi <- V[xi,]

      rank_i <- eval_matrix %>%
        poss_identify_sois(vert_matrix = Vi, by = by, threshold = -1) %>%
        rank_sois(by = by)

      rank_i_sols <- rownames(rank_i)[1:n_top]

      commom_sols <- intersect(curr_rank_sols, rank_i_sols)

      match_rate <- length(commom_sols)/n_top

      rank_i_comm_sols <- rank_i_sols[rank_i_sols %in% commom_sols]
      curr_rank_comm_sols <- curr_rank_sols[curr_rank_sols %in% commom_sols]

      curr_rank_vals <- current_rank[curr_rank_comm_sols, by]
      rank_i_vals <- rank_i[rank_i_comm_sols, by]

      kendall_corr <- cor(x = curr_rank_vals, y=rank_i_vals, method = "kendall")

      result <- c(-match_rate, -kendall_corr)
      names(result) <- c("match_rate", "kendall_corr")

      return(
        result
      )

    })

  }


  res <- nsga2(type = "permutation",
               fitness = fitness,
               lower = rep(1, n_crit),
               upper = rep(n_crit, n_crit),
               popSize = 20,
               monitor = FALSE,
               maxiter = 500
               )

  return(res)

}


E <- create_eval_matrix_example(nsol = 10, ncrit = 5)
V <- generate_polyhedron_vertices(5)
CUR <- E %>% poss_identify_sois(vert_matrix = V, threshold = -1) %>% rank_sois(by = "neutral")
PREF <- c(1,2,3,4,5)
res <- find_similar_preferences(eval_matrix = E, current_preference = PREF, current_rank = CUR, by = "neutral", n_top = 7)


