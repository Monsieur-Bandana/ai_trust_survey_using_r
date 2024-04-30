source("helper_functions.R")
source("add_group_column.R")
source("h1.R")
source("h2.R")
source("h7.R")

data <- read.csv("outcome.CSV", sep = ";", header = TRUE)
data_extended <- data
data_extended <- apply(data_extended, 2, replace_special)
data_extended <- as.data.frame(data_extended)
data_extended <- execute_add_group_column()

### execute Hypothesis 1
data_extended[["summary_of_h1"]] <- execute_h1()
data_extended <- add_h_column("summary_of_h1", 70, "score_h1")
h1_counts <- table(data_extended[["score_h1"]])
create_boxplot("summary_of_h1", 70)

### execute Hypothesis 2
data_extended[["summary_of_h2"]] <- execute_h2()
data_extended <- add_h_column("summary_of_h2", 80, "score_h2")
h2_counts <- table(data_extended[["score_h2"]])
create_boxplot("summary_of_h2", 80)

### execute Hypothesis 7
values <- execute_h7()
highest_possible_score <- values[length(values)]
data_extended[["summary_of_h7"]] <- values[-length(values)]
data_extended <- add_h_column("summary_of_h7", highest_possible_score, "score_h7")
h7_counts <- table(data_extended[["score_h7"]])
create_boxplot("summary_of_h7", highest_possible_score)