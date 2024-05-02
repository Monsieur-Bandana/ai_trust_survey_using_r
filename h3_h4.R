source("helper_functions.R")

### H3 Customers want to be informed explicitely & H3 want to be informed regulary about collection of data

execute_question_h3_h4 <- function(){
  # In.welcher.Regelm..igkeit.m.chten.sie..ber.das.Sammeln.von.Trainingsdaten.informiert.werden.
  labels_short_h3_h4 <<- "In.welcher.Regelm..igkeit.m.chten.sie..ber.das.Sammeln.von.Trainingsdaten.informiert.werden."
  label1 <- "Ich mchte vor jedem Gesprch darber informiert werden."
  label2 <- "Bei meiner ersten Kontaktaufnahme und dann erst wieder bei der nchsten nderung der Datenschutzrichtlinien."
  label3 <- "Gar nicht, reicht mir aus mir selbststndig auf der Unternehmens-Webseite die Datenschutzbestimmungen durchzulesen."
  labels_vector <- c(label1, label2, label3)
  numbers_vector <- c(10, 5, 0)
  transformed_vec <- transformer_subfunction(labels_vector, numbers_vector, data_extended[[labels_short_h3_h4]])
  data_extended[[labels_short_h3_h4]] <<- transformed_vec
}

### H3
execute_h3 <- function(){
  print(data_extended[[labels_short_h3_h4]])
  # setting max value to 2 to filter only perticipants who voted 0
  data_extended[["score_h3"]] <<- add_h_column(data_extended[[labels_short_h3_h4]], 2)
}



### H4
execute_h4 <- function(){
  data_extended[["score_h4"]] <<- add_h_column(data_extended[[labels_short_h3_h4]], 12)
}