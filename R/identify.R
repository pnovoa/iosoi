

poss_identify_sois <- function(eval_matrix,
                          vert_matrix = NA,
                          by = "neutral",
                          threshold = 0.0) {

  eval_matrix %>%
    score(vert_matrix = vert_matrix) %>%
    intervals() %>%
    reference() %>%
    assess(by = by) %>%
    filter_soi(
      by = by,
      threshold = threshold
    )
}


geom_identify_sois <- function(eval_matrix,
                               vert_matrix = NA,
                               by = "volume",
                               threshold = 0.0) {


  eval_matrix %>%
    score(vert_matrix = vert_matrix) %>%
    intervals() %>%
    reference() %>%
    geom_assess(by = by) %>%
    filter_soi(
      by = by,
      threshold = threshold
    )
}

all_assessments <- function(eval_matrix,
                              vert_matrix = NA){

  eval_matrix %>%
    score(vert_matrix = vert_matrix) %>%
    intervals() %>%
    reference() %>%
    assess(by = "all") %>%
    geom_assess(by = "all")

}



identify_top_sois <- function(eval_matrix,
                              crit_preference,
                              by = "neutral",
                              n) {




}
