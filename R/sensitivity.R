

#' WS Coefficient of Rankings Similarity
#'
#' This function computes the WS coefficient as defined by Sałabun 
#' and Urbaniak (2020).
#' From two ranges of equal size the function returns a scalar in the range 
#' \eqn{[0, 1]}, where a value close to 0 means that the ranges differ 
#' substantially, while one close to 1 means the opposite.
#' 
#' More details can be found in Sałabun, W., Urbaniak, K. (2020). 
#' A New Coefficient of Rankings Similarity in Decision-Making Problems. 
#' In: Krzhizhanovskaya, V., et al. Computational Science – ICCS 2020. ICCS 2020. 
#' Lecture Notes in Computer Science(), vol 12138. Springer, Cham. 
#' \url{https://doi.org/10.1007/978-3-030-50417-5_47}.
#' 
#' @param rank1 A permutation of size n of integers in the range [1, n]. 
#' @param rank2 A permutation of size n of integers in the range [1, n].
#'
#' @return WS Coefficient.
#' @export
#'
#' @examples
#' 
#' rank1 <- c(1, 2, 3, 4, 5)
#' rank2 <- c(3, 1, 2, 4, 5)
#' 
#' ws_similarity(rank1, rank2)
#' 
ws_similarity <- function(rank1, rank2){
  
  N <- length(rank1)
  
  match_rank1 <- match(rank1, rank1)
  match_rank2 <- match(rank1, rank2)
  
  match_matrix <- matrix(c(match_rank1, match_rank2), ncol = 2)

  result <- apply(match_matrix, MARGIN = 1, function(mrow){
    Rxi <- mrow[1]
    Ryi <- mrow[2]
    2^-(Rxi) * abs(Rxi - Ryi)/max(abs(1-Rxi), abs(N-Rxi))
  }
    )

  return(
    1 - sum(result)
  )
}


generate_ranks <- function(eval_matrix, crit_pref, rank_crit, strength=1){
  
  N <- length(crit_pref)
  
  crit_names <- names(sort(crit_pref))
  
  vert_mat <- generate_polyhedron_vertices(N)
  curr_vert_mat <- vert_mat[crit_pref,]
  
  curr_res <- eval_matrix %>% 
    poss_identify_sois(vert_matrix = curr_vert_mat, by = rank_crit, threshold = -1) %>%
    rank_sois(by = rank_crit, decreasing = TRUE)
  
  curr_rank <- rownames(curr_res)
  curr_rank_ids <- match(rownames(eval_matrix), curr_rank)
  
  N_swaps <- N - strength
  
  
  results <- sapply(1:N_swaps, function(i){
    
    new_crit_pref <- crit_pref
    new_crit_pref[i] <- crit_pref[i+strength]
    new_crit_pref[i+strength] <- crit_pref[i]
    
    new_vert_mat <- vert_mat[new_crit_pref,]
    
    new_res <- eval_matrix %>% 
      poss_identify_sois(vert_matrix = new_vert_mat, by = rank_crit, threshold = -1) %>%
      rank_sois(by = rank_crit, decreasing = TRUE)
    
    new_rank <- rownames(new_res)
    new_rank_ids <- match(new_rank, rownames(eval_matrix))
    
    similarity <- ws_similarity(curr_rank_ids, new_rank_ids)
  
    new_name <- crit_names[new_crit_pref]
    
    rank_name <- paste0(new_name, collapse = ",")
    
    similarity <- c(similarity)
    names(similarity) <- rank_name
    
    return(
      similarity
    )
    
  })
  
  return(
    results
  )
  
}

# ncrit <- 5
# E <- create_eval_matrix_example(nsol = 10, ncrit = ncrit) 
# crit_pref <- sample(ncrit, ncrit)
# names(crit_pref) <- paste0("C", 1:ncrit)
# 
# print(names(crit_pref))
# 
# res <- generate_ranks(eval_matrix = E, crit_pref = crit_pref, rank_crit = "neutral", strength = 2)
# print(res)