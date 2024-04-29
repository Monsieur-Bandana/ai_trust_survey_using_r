source("helper_functions.R")

multiple_choice_outcome <- function(value){
  string <- value
  calc_vec <- stringsplitter(string)
  # print(calc_vec)
  # number of semicolons says, how many tasks got selected
  vec_length <- length(calc_vec)
  endpoints <- 12-vec_length
  return(endpoints)
}