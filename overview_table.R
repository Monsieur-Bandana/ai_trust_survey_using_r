### this file generates various dataframes for retrieving overviews and finding relations

## function to create overview of h1, h2, h7 outcomes
add_new_score_line <- function(row_name, score_name, summary_name){
  aggree_h <- table(data_extended[[score_name]])
  mean_score <- mean(data_extended[[summary_name]])
  var<- sd(data_extended[[summary_name]])/mean(data_extended[[summary_name]]) * 100
  new_row <- c(row_name, aggree_h[1], aggree_h[2], mean_score, var)
  overview_data_h1_h2_h7 <- rbind(overview_data_h1_h2_h7, new_row)
  return(overview_data_h1_h2_h7)
}

get_agree_stake <- function(aggree_h){
  if (length(aggree_h)>1){
    aggree <<-  aggree_h[2]
    disaggree <<- aggree_h[1]
  }else{
    aggree <<-  aggree_h[1]
  }
}

## function to create overview of h3 - h6 outcomes
add_new_score_line_h3_h6 <- function(grouptype, row_name, score_name, database){
  aggree_h <- table(database[[score_name]])
  aggree <<- 0
  disaggree <<- 0
  get_agree_stake(aggree_h)
  stake <- aggree/nrow(database) * 100
  new_row <- c(grouptype, row_name, disaggree, aggree, stake)
  overview_data_h3_h6 <- rbind(overview_data_h3_h6, new_row)
  return(overview_data_h3_h6)
}

## get right tailed t test for h1 2 7
get_t_test <- function(summary_name, mu_value = 50){
  data <- data_extended[[summary_name]]
  t_test_result <- t.test(data, mu = mu_value, alternative = "greater")
  deviation <- sd(data_extended[[summary_name]])
  variance <- var(data_extended[[summary_name]])
  print(t_test_result)
  new_row <- c(t_test_result$statistic, t_test_result$parameter, t_test_result$p.value, variance, deviation, t_test_result$conf.int[1], t_test_result$conf.int[2])
  df_t <- rbind(df_t, new_row)
  return(df_t)
}

## get probability t test for h3 4 5 6
get_prop_test <- function(score_name){
  binary_data <- data_extended[[score_name]]
  prop_test_result <- prop.test(sum(binary_data), length(binary_data), p = 0.5, alternative = "greater")
  print(prop_test_result)
  new_row <- c(prop_test_result$p.value, prop_test_result$conf.int[1], prop_test_result$conf.int[2] )
  df_prop <- rbind(df_prop, new_row)
  return(df_prop)
}

## calculate which group trusts how many tasks from the multiple choice section
get_multi_score <- function(group_name){
  filtered_df <- df_trust_multi[df_trust_multi$level_of_trust == group_name, ]
  mean_v <- mean(filtered_df$score)
  new_row <- c(group_name, mean_v)
  df_multi_by_group <<- rbind(df_multi_by_group, new_row)
  
}

## get cv value of group types in h1 2 and 7
get_cv_of_type <- function(type){
  
  filtered_df_ex_ea <- data_extended[data_extended$combined_openess == type, ]
  vec_h1 <- filtered_df_ex_ea$summary_of_h1_percentile
  vec_h2 <- filtered_df_ex_ea$summary_of_h2_percentile
  vec_h7 <- filtered_df_ex_ea$summary_of_h7_percentile
  val_h1 <- sd(vec_h1)/ mean(vec_h1) * 100
  val_h2 <- sd(vec_h2)/ mean(vec_h2) * 100
  val_h7 <- sd(vec_h7)/ mean(vec_h7) * 100
  
  mean_var <- mean(c(val_h1, val_h2, val_h7))
  mean_var_h1_h7 <- mean(c(val_h1, val_h7))
  
  df_variance <- rbind(df_variance, c(type, val_h1, val_h2, val_h7, mean_var, mean_var_h1_h7))

  
  return(df_variance)
}

## get mean of group types in h1 2 and 7
get_mean_of_type <- function(type){
  filtered_df_ex_ea <- data_extended[data_extended$combined_openess == type, ]
  vec_h1 <- filtered_df_ex_ea$summary_of_h1_percentile
  vec_h2 <- filtered_df_ex_ea$summary_of_h2_percentile
  vec_h7 <- filtered_df_ex_ea$summary_of_h7_percentile
  val_m_h1 <- mean(vec_h1) 
  val_m_h2 <- mean(vec_h2) 
  val_m_h7 <- mean(vec_h7) 
  print(vec_h1)
  
  mean_var <- mean(c(val_m_h1, val_m_h2, val_m_h7))
  mean_var_h1_h7 <- mean(c(val_m_h7, val_m_h7))
  
  df_means_by_group <- rbind(df_means_by_group, c(type, val_m_h1, val_m_h2, val_m_h7, mean_var, mean_var_h1_h7))
  
  
  return(df_means_by_group)
}

## creates various overview tables
execute_overview <- function(){
  
  # create general overview
  overview_data_h1_h2_h7 <<- data.frame(Hypothesis=c("h"), Disagree=c(0), Agree=c(0), Mean_Score=c(0), Coef_of_Variance=c(0))
  overview_data_h1_h2_h7 <<- add_new_score_line("H1", "score_h1", "summary_of_h1_percentile")
  overview_data_h1_h2_h7 <<- overview_data_h1_h2_h7[-1, ]
  overview_data_h1_h2_h7 <<- add_new_score_line("H2", "score_h2", "summary_of_h2_percentile")
  overview_data_h1_h2_h7 <<- add_new_score_line("H7", "score_h7", "summary_of_h7_percentile")
  
  # at t values
  df_t <<- data.frame(statstic=c(0), df=c(0), p_value=c(0), variance=(0), deviation=(0), confidence_interval_1=c(0), confidence_interval_2=c(0))
  df_t <<- get_t_test("summary_of_h1_percentile")
  df_t <<- df_t[-1, ]
  df_t <<- get_t_test("summary_of_h2_percentile")
  df_t <<- get_t_test("summary_of_h7_percentile")
  
  overview_data_h1_h2_h7 <<- cbind(overview_data_h1_h2_h7, df_t)
  
  ## get table of mean and cv values according to groups
  df_variance <<- data.frame(
    Type=c("t"),
    cv_H1=c(0), 
    cv_H2=c(0),
    cv_H7=c(0),
    Mean_cv=c(0),
    Mean_cv_h1_h7=c(0)
    )
  for(l in group_labels){
    df_variance <<- get_cv_of_type(l)
    
  }
  df_variance <<- df_variance[-1, ]
  
  df_means_by_group <<- data.frame(
    Type=c("x"),
    mean_H1=c(0), 
    mean_H2=c(0),
    mean_H7=c(0),
    gen_mean=c(0),
    gen_mean=c(0)
  )
  for(l in group_labels){
    df_means_by_group <<- get_mean_of_type(l)
    
  }
  df_means_by_group <<- df_means_by_group[-1, ]
  
  general_cv_performance <<- mean(as.numeric(df_variance[["Mean_cv"]]))
  
  df_variance <<- cbind(df_variance, df_means_by_group[, 2:4])
  
  ## get outcome values of h3 - h6
  overview_data_h3_h6 <<- data.frame(Grouptype=c("t"),Hypothesis=c("h"), Disagree=c(0), Agree=c(0), Stake=c(0))
  overview_data_h3_h6 <<- add_new_score_line_h3_h6("general","H3", "score_h3", data_extended)
  overview_data_h3_h6 <<- add_new_score_line_h3_h6("general","H4", "score_h4", data_extended)
  overview_data_h3_h6 <<- add_new_score_line_h3_h6("general","H5", "score_h5", data_extended)
  overview_data_h3_h6 <<- add_new_score_line_h3_h6("general","H6", "score_h6", data_extended)
  overview_data_h3_h6 <<- overview_data_h3_h6[-1, ]
  
  ## get group specific outcomes for h3 - h6
  for (ty in group_labels){
    current_df <- data_extended[data_extended$combined_openess == ty, ]
    overview_data_h3_h6 <<- add_new_score_line_h3_h6(ty,"H3", "score_h3", current_df)
    overview_data_h3_h6 <<- add_new_score_line_h3_h6(ty,"H4", "score_h4", current_df)
    overview_data_h3_h6 <<- add_new_score_line_h3_h6(ty,"H5", "score_h5", current_df)
    overview_data_h3_h6 <<- add_new_score_line_h3_h6(ty,"H6", "score_h6", current_df)
  }
  
  #get t values
  df_prop <<- data.frame(p_value=c(0), interval_1=c(0), interval_2=c(0))
  df_prop <<- get_prop_test("score_h3")
  df_prop <<- df_prop[-1, ]
  df_prop <<- get_prop_test("score_h4")
  df_prop <<- get_prop_test("score_h5")
  df_prop <<- get_prop_test("score_h6")
  
  overview_data_h3_h6 <<- cbind(overview_data_h3_h6, df_prop)
  
  # get which groups trust in how many tasks
  str_search <- "In.diesen.Aufgaben.vertraue.ich.virtuellen.Assistenten..Mehrfache.Auswahl.m.glich."
  df_trust_multi <<- data.frame(tasks=data[[str_search]], level_of_trust=data_extended$combined_openess, score=(6-data_extended[[str_search]]))
  df_multi_by_group <<- data.frame(group_name=c("string"), count=c(0))
  
  for(string_i in group_labels){
    get_multi_score(string_i)
    
  }
  new_row <- c("all", mean(df_trust_multi$score))
  df_multi_by_group <<- rbind(df_multi_by_group, new_row)
  df_multi_by_group <<- df_multi_by_group[-1, ]
  
  
}