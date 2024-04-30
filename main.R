source("helper_functions.R")
source("add_group_column.R")
source("h1.R")
data <- read.csv("outcome.CSV", sep = ";", header = TRUE)
data_extended <- data
data_extended <- apply(data_extended, 2, replace_special)
data_extended <- as.data.frame(data_extended)
data_extended <- execute_add_group_column()
data_extended <- execute_h1()