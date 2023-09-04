

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
#' ws_coefficient(rank1, rank2)
#' 
ws_coefficient <- function(rank1, rank2){
  
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


#' Blest's measure of rank correlation
#'
#' This function computes the Blest's measure of rank correlation as defined by 
#' Blest (2020).
#' From two ranges of equal size the function returns a scalar in the range 
#' \eqn{[-1, 1]}, where a value close to -1 means that the ranges differ 
#' substantially, while one close to 1 means the opposite.
#' 
#' More details can be found in Sałabun, W., Urbaniak, K. (2020). 
#' A New Coefficient of Rankings Similarity in Decision-Making Problems. 
#' In: Blest, D.C.: Theory & methods: Rank correlation - an alternative 
#' measure. Aust. NZ. J. Stat. 42(1), 101–111 (2000). 
#' https://doi.org/10.1111/1467-842X.00110.
#' 
#' @param rank1 A permutation of size n of integers in the range [1, n]. 
#' @param rank2 A permutation of size n of integers in the range [1, n].
#'
#' @return \eqn{v} correlation coefficient.
#' @export
#'
#' @examples
#' 
#' rank1 <- c(1, 2, 3, 4, 5)
#' rank2 <- c(3, 1, 2, 4, 5)
#' 
#' blest_rank_correlation(rank1, rank2)
#' 
blest_rank_correlation <- function(rank1, rank2){
  
  N <- length(rank1)
  
  match_rank1 <- match(rank1, rank1)
  match_rank2 <- match(rank1, rank2)
  
  match_matrix <- matrix(c(match_rank1, match_rank2), ncol = 2)
  
  result <- apply(match_matrix, MARGIN = 1, function(mrow){
    Rxi <- mrow[1]
    Ryi <- mrow[2]
    Ryi*(N + 1 - Rxi)^2
  }
  )
  
  return(
    1 - (12*sum(result) - N*(N+1)^2*(N+2))/(N*(N+1)^2*(N-1))
  )
}


#' Weighted rank measure of correlation
#'
#' This function computes the weighted rank measure of correlation as defined in 
#' (da Costa & Soares, 2005).
#' From two ranges of equal size the function returns a scalar in the range 
#' \eqn{[-1, 1]}, where a value close to -1 means that the ranges differ 
#' substantially, while one close to 1 means the opposite.
#' 
#' More details can be found in Pinto da Costa, J., Soares, C.: 
#' A weighted rank measure of correlation. 
#' Aust. NZ. J. Stat. 47(4), 515–529 (2005). 
#' https://doi.org/10.1111/j.1467-842X.2005.00413.x
#' 
#' @param rank1 A permutation of size n of integers in the range [1, n]. 
#' @param rank2 A permutation of size n of integers in the range [1, n].
#'
#' @return \eqn{w_r} correlation coefficient.
#' @export
#'
#' @examples
#' 
#' rank1 <- c(1, 2, 3, 4, 5)
#' rank2 <- c(3, 1, 2, 4, 5)
#' 
#' weighted_rank_measure_of_correlation(rank1, rank2)
#' 
weighted_rank_measure_of_correlation <- function(rank1, rank2){
  N <- length(rank1)
  
  match_rank1 <- match(rank1, rank1)
  match_rank2 <- match(rank1, rank2)
  
  match_matrix <- matrix(c(match_rank1, match_rank2), ncol = 2)
  
  result <- apply(match_matrix, MARGIN = 1, function(mrow){
    Rxi <- mrow[1]
    Ryi <- mrow[2]
    
    (Rxi - Ryi)^2 * ((N - Rxi + 1) + (N - Ryi + 1))
  }
  )
  
  return(
    1 - (6 * sum(result))/(N^4 + N^3 - N^2 - N)
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
#' between criteria (i.e. 1, 2,...). 
#' 
#' As a result, a dataframe is returned 
#' containing for each swap five correlation coefficients (similarity) between 
#' the ranking due to the swap and the reference ranking. The correlation 
#' coefficients are: Spearman, Kendall, Blest, Weighted Correlation 
#' Coefficient and WS.
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
#' similarities to the reference ranking.
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
    
    ws_coeff <- ws_coefficient(curr_rank_ids, new_rank_ids)
    
    spearman_coeff <- stats::cor(curr_rank_ids, new_rank_ids, method = "spearman")
    
    kendall_coeff <- stats::cor(curr_rank_ids, new_rank_ids, method = "kendall")
    
    blest_coeff <- blest_rank_correlation(curr_rank_ids, new_rank_ids)
    
    weighted_coeff <- weighted_rank_measure_of_correlation(curr_rank_ids, new_rank_ids)
  
    new_name <- crit_names[new_crit_pref]
    
    rank_name <- paste0(new_name, collapse = ", ")
    
    swap_name <- paste0(crit_names[c(new_crit_pref[i], new_crit_pref[i+strength])], collapse = "<=>")
    
    ranking <- paste0(new_rank_ids, collapse = ", ")
    
    res <- c(new_crit_pref[i], new_crit_pref[i+strength], spearman_coeff, kendall_coeff, blest_coeff, weighted_coeff, ws_coeff,  ranking)
    
    list_res <- list(res)
    
    names(list_res) <- swap_name
    
    return(
      list_res
    )
    
  })
  
  mm <- matrix(unlist(results), byrow = TRUE, nrow = N_swaps)
  rownames(mm) <- names(results)
  colnames(mm) <- c("si", "sj", "spearman", "kendall", "blest", "weighted", "ws", "ranking")
  
  dtf <- data.frame(mm)
  
  dtf$swap <- rownames(dtf)
  
  rownames(dtf) <- NULL
  
  return(
    dtf[c(ncol(dtf),1:(ncol(dtf)-1))]
  )
  
}
