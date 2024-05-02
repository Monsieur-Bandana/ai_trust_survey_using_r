add_new_score_line <- function(row_name, score_name, summary_name){
  aggree_h <- table(data_extended[[score_name]])
  mean_score <- mean(data_extended[[summary_name]])
  new_row <- c(row_name, aggree_h[1], aggree_h[2], mean_score)
  overview_data <- rbind(overview_data, new_row)
  return(overview_data)
}

get_t_test <- function(summary_name){
  data <- data_extended[[summary_name]]
  t_test_result <<- t.test(data, mu = 50)
  print(t_test_result)
  new_row <- c(t_test_result$statistic, t_test_result$parameter, t_test_result$p.value, t_test_result$conf.int[1], t_test_result$conf.int[2])
  df_t <- rbind(df_t, new_row)
  return(df_t)
}

overview_data <- data.frame(Hypothesis=c("h"), Disagree=c(0), Agree=c(0), Mean_Score=c(0))
overview_data <- add_new_score_line("H1", "score_h1", "summary_of_h1_percentile")
overview_data <- overview_data[-1, ]
overview_data <- add_new_score_line("H2", "score_h2", "summary_of_h2_percentile")
overview_data <- add_new_score_line("H3", "score_h3", "summary_of_h3_percentile")
overview_data <- add_new_score_line("H4", "score_h4", "summary_of_h4_percentile")
overview_data <- add_new_score_line("H5", "score_h5", "summary_of_h5_percentile")
overview_data <- add_new_score_line("H6", "score_h6", "summary_of_h6_percentile")
overview_data <- add_new_score_line("H7", "score_h7", "summary_of_h7_percentile")

df_t <- data.frame(statstic=c(0), df=c(0), p_value=c(0), confidence_interval_1=c(0), confidence_interval_2=c(0))
df_t <- get_t_test("summary_of_h1_percentile")
df_t <- df_t[-1, ]
df_t <- get_t_test("summary_of_h2_percentile")
df_t <- get_t_test("summary_of_h3_percentile")
df_t <- get_t_test("summary_of_h4_percentile")
df_t <- get_t_test("summary_of_h5_percentile")
df_t <- get_t_test("summary_of_h6_percentile")
df_t <- get_t_test("summary_of_h7_percentile")

overview_data <- cbind(overview_data, df_t)