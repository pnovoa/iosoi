

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


#' One-to-one swapping sensitivity analysis
#' 
#' This function performs a sensitivity analysis based on a one-to-one swapping 
#' of the preference (order) of the criteria of an MCDM problem. Specifically, 
#' it generates all possible alternatives (permutations) of a criteria 
#' preference and evaluates them according to a ranking comparison measure 
#' (i.e. taking as a reference the ranking obtained by a given order of 
#' preference of the criteria). It requires as parameters the solution 
#' evaluation matrix used, the original criterion preference, the criterion 
#' under which the solutions will be ranked (i.e. neutral, pessimistic, 
#' optimistic) and the strength of the swapping 
#' between criteria (i.e. 1, 2,...). As a result, a matrix is returned where the 
#' first column corresponds to the preference ID and the second column to the 
#' ranking comparison measure. 
#'
#' @param eval_matrix the original problem evaluation matrix.
#' @param crit_pref a vector with the preference of criteria such that the 
#' first element of the vector is the most important criterion, the second, 
#' the second most important, and so on.
#' @param rank_crit the criteria used to rank the solutions (e.g. neutral, optimistic)
#' @param strength the distance (size) of the swap between the criteria positions.
#'
#' @return a data frame where each row is a criteria swap and the columns 
#' contain information about the swap, its associated solution ranking and its 
#' similarity to the reference ranking.
#' @export
#'
#' @examples
#' E <- create_eval_matrix_example(20, 5)
#' P <- c(2,1,5,3,4)
#' names(P) <- paste0("C", 1:5)[P]
#' print(P)
#' sens <- one_to_one_swap_sensitivity_analysis(E, P, "neutral", 1)
#' print(sens)
#' 
one_to_one_swap_sensitivity_analysis <- function(eval_matrix, crit_pref, rank_crit="neutral", strength=1){
  
  N <- length(crit_pref)
  
  crit_names <- names(sort(crit_pref))
  
  vert_mat <- generate_polyhedron_vertices(N)
  curr_vert_mat <- vert_mat
  curr_vert_mat[crit_pref,] <- vert_mat
  
  rownames(curr_vert_mat) <- names(crit_pref)
  colnames(curr_vert_mat) <- names(crit_pref)
  
  curr_res <- eval_matrix %>% 
    poss_identify_sois(vert_matrix = curr_vert_mat, by = rank_crit, threshold = -1) #%>%
    #rank_sois(by = rank_crit, decreasing = TRUE)
  
  curr_res[,rank_crit] = -curr_res[,rank_crit]
  
  curr_rank_ids <- rank(curr_res[,rank_crit])
  
  curr_rank_ids <- match(names(sort(curr_rank_ids)), rownames(eval_matrix))
  
  #curr_rank <- rownames(curr_res)
  #curr_rank_ids <- match(curr_rank, rownames(eval_matrix))
  
  N_swaps <- N - strength
  
  
  results <- sapply(1:N_swaps, function(i){
    
    new_crit_pref <- crit_pref
    new_crit_pref[i] <- crit_pref[i+strength]
    new_crit_pref[i+strength] <- crit_pref[i]
    
    new_vert_mat <- vert_mat[new_crit_pref,]
    
    new_res <- eval_matrix %>% 
      poss_identify_sois(vert_matrix = new_vert_mat, by = rank_crit, threshold = -1)
    
    new_res[,rank_crit] = -new_res[,rank_crit]
    
    new_rank_ids <- rank(new_res[,rank_crit])
    
    new_rank_ids <- match(names(sort(new_rank_ids)), rownames(eval_matrix))
    
    similarity <- ws_similarity(curr_rank_ids, new_rank_ids)
  
    new_name <- crit_names[new_crit_pref]
    
    rank_name <- paste0(new_name, collapse = ",")
    
    swap_name <- paste0(crit_names[c(new_crit_pref[i], new_crit_pref[i+strength])], collapse = ",")
    
    ranking <- paste0(new_rank_ids, collapse = ",")
    
    res <- c(new_crit_pref[i], new_crit_pref[i+strength], similarity, ranking)
    
    list_res <- list(res)
    
    names(list_res) <- swap_name
    
    return(
      list_res
    )
    
  })
  
  mm <- matrix(unlist(results), byrow = TRUE, nrow = N_swaps)
  rownames(mm) <- names(results)
  colnames(mm) <- c("si", "sj", "sim", "ranking")
  
  dtf <- data.frame(mm)
  
  dtf$swap <- rownames(dtf)
  
  rownames(dtf) <- NULL
  
  return(
    dtf[c(5,1,2,3,4)]
  )
  
}