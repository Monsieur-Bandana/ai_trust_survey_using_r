source("helper_functions.R")
source("boxplot_creator.R")

### H5 Customers want to be informed explicitely & H6 want to be informed regulary about the Use of AI

# In.welcher.Regelm..igkeit.m.chten.sie..ber.den.Einsatz.von.Virtuellen.Assistenten.im.IT.Support.informiert.werden.
labels_short <- "In.welcher.Regelm..igkeit.m.chten.sie..ber.den.Einsatz.von.Virtuellen.Assistenten.im.IT.Support.informiert.werden."
label1 <- "Ich mchte vor jedem Gesprch darber informiert werden."
label2 <- "Bei meiner ersten Kontaktaufnahme und dann erst wieder bei der nchsten nderung der Datenschutzrichtlinien."
label3 <- "Garnicht, es reicht mir aus mir selbststndig auf der Unternehmens-Webseite die Datenschutzbestimmungen durchzulesen."
labels_vector <- c(label1, label2, label3)
numbers_vector <- c(10, 5, 0)
data_extended[[labels_short]] <- transformer(labels_vector, numbers_vector, labels_short)
op_closed_1 <- data_extended[[labels_short]]
print_mean(op_closed_1)
summarized_value <- sum(op_closed_1)
print(summarized_value)

### H5
data_extended <- data.frame(data_extended, "summary_of_h5" = op_closed_1)
# setting max value to 2 to filter only perticipants who voted 0
data_extended <- add_h_column("summary_of_h5", 2, "score_h5")
h5_counts <- table(data_extended[["score_h5"]])
print(h5_counts)

create_boxplot("summary_of_h5", 2)

### H6
data_extended <- data.frame(data_extended, "summary_of_h6" = op_closed_1)
data_extended <- add_h_column("summary_of_h6", 10, "score_h6")
h6_counts <- table(data_extended[["score_h6"]])
print(h6_counts)

create_boxplot("summary_of_h6", 10)