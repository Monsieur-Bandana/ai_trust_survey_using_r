source("helper_functions.R")
source("boxplot_creator.R")

### H5 Customers want to be informed explicitely & H6 want to be informed regulary about collection of data

# In.welcher.Regelm..igkeit.m.chten.sie..ber.das.Sammeln.von.Trainingsdaten.informiert.werden.
labels_short <- "In.welcher.Regelm..igkeit.m.chten.sie..ber.das.Sammeln.von.Trainingsdaten.informiert.werden."
label1 <- "Ich mchte vor jedem Gesprch darber informiert werden."
label2 <- "Bei meiner ersten Kontaktaufnahme und dann erst wieder bei der nchsten nderung der Datenschutzrichtlinien."
label3 <- "Gar nicht, reicht mir aus mir selbststndig auf der Unternehmens-Webseite die Datenschutzbestimmungen durchzulesen."
labels_vector <- c(label1, label2, label3)
numbers_vector <- c(10, 5, 0)
data_extended[[labels_short]] <- transformer(labels_vector, numbers_vector, labels_short)
op_closed_1 <- data_extended[[labels_short]]
print_mean(op_closed_1)
summarized_value <- sum(op_closed_1)
print(summarized_value)

### H3
data_extended <- data.frame(data_extended, "summary_of_h3" = op_closed_1)
# setting max value to 2 to filter only perticipants who voted 0
data_extended <- add_h_column("summary_of_h3", 2, "score_h3")
h3_counts <- table(data_extended[["score_h3"]])
print(h3_counts)

create_boxplot("summary_of_h3", 10)

### H4
data_extended <- data.frame(data_extended, "summary_of_h4" = op_closed_1)
data_extended <- add_h_column("summary_of_h4", 10, "score_h4")
h4_counts <- table(data_extended[["score_h4"]])
print(h4_counts)

create_boxplot("summary_of_h4", 10)