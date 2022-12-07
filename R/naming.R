
generate_solutions_names <- function(nsol) {
  paste0(get_solutions_prefix(), 1:nsol)
}

get_solutions_prefix <- function() {
  return(
    "S"
  )
}

generate_criteria_names <- function(ncrit) {
  paste0(get_criteria_prefix(), 1:ncrit)
}

get_criteria_prefix <- function() {
  return("C")
}

get_eval_vertex_prefix <- function() {
  return("VE_")
}

get_prefixed_column_names <- function(colum_names, prefix = "VE_") {
  grepl(
    paste0("^", prefix),
    colum_names
  )
}

get_reference_name <- function() {
  return(
    "REF"
  )
}

get_range_names <- function() {
  return(
    c("LB", "UB")
  )
}


get_poss_approaches <- function(){

  return(
    c("optimistic",
      "neutral",
      "pessimistic"
      )
  )
}


get_geom_approaches <- function(){

  return(
    c("volume",
      "poss_volume",
      "vert_prop"
    )
  )
}
