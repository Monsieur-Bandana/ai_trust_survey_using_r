add_new_score_line <- function(row_name, score_name, summary_name){
  aggree_h <- table(data_extended[[score_name]])
  mean_score <- mean(data_extended[[summary_name]])
  var<- var(data_extended[[summary_name]])
  new_row <- c(row_name, aggree_h[1], aggree_h[2], mean_score, var)
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

get_variance_of_type <- function(type){
  
  filtered_df_ex_ea <- data_extended[data_extended$combined_openess == type, ]
  val_h1 <- var(filtered_df_ex_ea$summary_of_h1_percentile)
  val_h2 <- var(filtered_df_ex_ea$summary_of_h2_percentile)
  val_h3 <- var(filtered_df_ex_ea$summary_of_h3_percentile)
  val_h4 <- var(filtered_df_ex_ea$summary_of_h4_percentile)
  val_h5 <- var(filtered_df_ex_ea$summary_of_h5_percentile)
  val_h6 <- var(filtered_df_ex_ea$summary_of_h6_percentile)
  val_h7 <- var(filtered_df_ex_ea$summary_of_h7_percentile)
  
  variances <- c(val_h1, val_h2, val_h7)
  mean_var <- mean(variances)
  
  df_variance <- rbind(df_variance, c(type, val_h1, val_h2, val_h3, val_h4, val_h5, val_h6, val_h7, mean_var))

  
  return(df_variance)
}

execute_overview <- function(){
  
  overview_data <<- data.frame(Hypothesis=c("h"), Disagree=c(0), Agree=c(0), Mean_Score=c(0), Variance=c(0))
  overview_data <<- add_new_score_line("H1", "score_h1", "summary_of_h1_percentile")
  overview_data <<- overview_data[-1, ]
  overview_data <<- add_new_score_line("H2", "score_h2", "summary_of_h2_percentile")
  overview_data <<- add_new_score_line("H3", "score_h3", "summary_of_h3_percentile")
  overview_data <<- add_new_score_line("H4", "score_h4", "summary_of_h4_percentile")
  overview_data <<- add_new_score_line("H5", "score_h5", "summary_of_h5_percentile")
  overview_data <<- add_new_score_line("H6", "score_h6", "summary_of_h6_percentile")
  overview_data <<- add_new_score_line("H7", "score_h7", "summary_of_h7_percentile")
  
  
  df_t <<- data.frame(statstic=c(0), df=c(0), p_value=c(0), confidence_interval_1=c(0), confidence_interval_2=c(0))
  df_t <<- get_t_test("summary_of_h1_percentile")
  df_t <<- df_t[-1, ]
  df_t <<- get_t_test("summary_of_h2_percentile")
  df_t <<- get_t_test("summary_of_h3_percentile")
  df_t <<- get_t_test("summary_of_h4_percentile")
  df_t <<- get_t_test("summary_of_h5_percentile")
  df_t <<- get_t_test("summary_of_h6_percentile")
  df_t <<- get_t_test("summary_of_h7_percentile")
  
  overview_data <<- cbind(overview_data, df_t)
  
  df_variance <<- data.frame(
    Type=c("t"),
    Var_H1=c(0), 
    Var_H2=c(0), 
    Var_H3=c(0), 
    Var_H4=c(0), 
    Var_H5=c(0), 
    Var_H6=c(0), 
    Var_H7=c(0),
    Mean_var=c(0)
    )
  
  df_variance <<- get_variance_of_type("experienced-early adopter")
  df_variance <<- get_variance_of_type("experienced-majority")
  df_variance <<- get_variance_of_type("rather experienced-majority")
  df_variance <<- get_variance_of_type("unexperienced-laggard")
  df_variance <<- get_variance_of_type("others")
  df_variance <<- df_variance[-1, ]
  
}