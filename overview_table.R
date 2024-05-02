add_new_score_line <- function(row_name, score_name, summary_name){
  aggree_h <- table(data_extended[[score_name]])
  mean_score <- mean(data_extended[[summary_name]])
  new_row <- c(row_name, aggree_h[1], aggree_h[2], mean_score)
  overview_data <- rbind(overview_data, new_row)
  return(overview_data)
}

get_t_test <- function(summary_name){
  data <- data_extended[[summary_name]]
  t_test_result <- t.test(data, mu = 50)
  print(t_test_result)
}

overview_data <- data.frame(matrix(ncol = 4, nrow = 0))

# Assign column names
names(overview_data) <- c("Hypothesis", "Disagree", "Agree", "Mean_Score")

overview_data <- add_new_score_line("H1", "score_h1", "summary_of_h1_percentile")
overview_data <- add_new_score_line("H2", "score_h2", "summary_of_h2_percentile")
overview_data <- add_new_score_line("H7", "score_h7", "summary_of_h7_percentile")
get_t_test("summary_of_h1_percentile")
get_t_test("summary_of_h2_percentile")
get_t_test("summary_of_h7_percentile")