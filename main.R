source("helper_functions.R")
source("add_group_column.R")
source("boxplot_creator.R")
source("h1.R")
source("h2.R")
source("h3_h4.R")
source("h5_h6.R")
source("h7.R")

data <- read.csv("outcome43.CSV", sep = ";", header = TRUE)
data_extended <- data
data_extended <- apply(data_extended, 2, replace_special)
data_extended <- as.data.frame(data_extended)
data_extended <- execute_add_group_column()

### execute Hypothesis 1
data_extended <- execute_h1()

### execute Hypothesis 2
data_extended <- execute_h2()

### execute Hypothesis 3
execute_question_h3_h4()
execute_h3()

### execute Hypothesis 4
execute_h4()

### execute Hypothesis 5
execute_question_h5_h6()
execute_h5()

### execute Hypothesis 6
execute_h6()

### execute Hypothesis 7
data_extended <- execute_h7()

frame_length <- length(data_extended[["summary_of_h1"]])
hypothesis_h1 <- rep("H1", frame_length)
hypothesis_h2 <- rep("H2", frame_length)
hypothesis_h7 <- rep("H7", frame_length)
data_extended[["summary_of_h1_percentile"]] <- calculate_percentile("summary_of_h1", highest_possible_score_h1)
data_extended[["summary_of_h2_percentile"]] <- calculate_percentile("summary_of_h2", highest_possible_score_h2)
data_extended[["summary_of_h3_percentile"]] <- calculate_percentile("score_h3", 1)
data_extended[["summary_of_h4_percentile"]] <- calculate_percentile("score_h4", 1)
data_extended[["summary_of_h5_percentile"]] <- calculate_percentile("score_h5", 1)
data_extended[["summary_of_h6_percentile"]] <- calculate_percentile("score_h6", 1)
data_extended[["summary_of_h7_percentile"]] <- calculate_percentile("summary_of_h7", highest_possible_score_h7)
data_frame_for_graph <- data.frame(
  values = c(data_extended[["summary_of_h1_percentile"]], data_extended[["summary_of_h2_percentile"]], data_extended[["summary_of_h7_percentile"]]),
  hypotheses = c(hypothesis_h1, hypothesis_h2, hypothesis_h7),
  combined_openess = rep(data_extended[["combined_openess"]], times = 3)
)


labels <- c("experienced-early adopter", "experienced-majority", "rather experienced-majority", "unexperienced-laggard", "others")
colors <- c("springgreen4", "springgreen2", "#FF3333", "#990000", "black")
color_palette <- transformer_subfunction(labels, colors, data_frame_for_graph$combined_openess, FALSE)
data_frame_for_graph["colors"] <- color_palette

create_mulitle_plots(data_frame_for_graph)

filtered_df <- data_frame_for_graph[data_frame_for_graph$combined_openess != "rather experienced-majority", ]
filtered_df <- filtered_df[filtered_df$combined_openess != "unexperienced-laggard", ]
filtered_df <- filtered_df[filtered_df$combined_openess != "others", ]

create_mulitle_plots(filtered_df)

filtered_df2 <- data_frame_for_graph[data_frame_for_graph$combined_openess != "experienced-early adopter", ]
filtered_df2 <- filtered_df2[filtered_df2$combined_openess != "experienced-majority", ]
filtered_df2 <- filtered_df2[filtered_df2$combined_openess != "others", ]

create_mulitle_plots(filtered_df2)

filtered_df_ex_ea <- data_frame_for_graph[data_frame_for_graph$combined_openess == "experienced-early adopter", ]

create_mulitle_plots(filtered_df_ex_ea)

filtered_df_ex_ma <- data_frame_for_graph[data_frame_for_graph$combined_openess == "experienced-majority", ]

create_mulitle_plots(filtered_df_ex_ma)

filtered_df_rex_ma <- data_frame_for_graph[data_frame_for_graph$combined_openess == "rather experienced-majority", ]

create_mulitle_plots(filtered_df_rex_ma)

filtered_df_uex_la <- data_frame_for_graph[data_frame_for_graph$combined_openess == "unexperienced-laggard", ]

create_mulitle_plots(filtered_df_uex_la)

write.csv(data_extended, "C:\\Users\\nicol\\Documents\\GitCode\\winkelmann_semi\\ai_trust_survey_using_r\\extended_data.csv", row.names=FALSE)
