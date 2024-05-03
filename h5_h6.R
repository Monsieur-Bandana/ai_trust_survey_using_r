source("helper_functions.R")

### H5 Customers want to be informed explicitely & H6 want to be informed regulary about the use of AI tools

execute_question_h5_h6 <- function(){
  # In.welcher.Regelm..igkeit.m.chten.sie..ber.das.Sammeln.von.Trainingsdaten.informiert.werden.
  labels_short_h5_h6 <<- "In.welcher.Regelm..igkeit.m.chten.sie..ber.den.Einsatz.von.Virtuellen.Assistenten.im.IT.Support.informiert.werden."
  label1 <- "Ich mchte vor jedem Gesprch darber informiert werden."
  label2 <- "Bei meiner ersten Kontaktaufnahme und dann erst wieder bei der nchsten nderung der Datenschutzrichtlinien."
  label3 <- "Garnicht, es reicht mir aus mir selbststndig auf der Unternehmens-Webseite die Datenschutzbestimmungen durchzulesen."
  labels_vector <- c(label1, label2, label3)
  numbers_vector <- c(10, 5, 0)
  transformed_vec <- transformer_subfunction(labels_vector, numbers_vector, data_extended[[labels_short_h5_h6]])
  data_extended[[labels_short_h5_h6]] <<- transformed_vec
}

### H5
execute_h5 <- function(){
  print(data_extended[[labels_short_h5_h6]])
  # setting max value to 2 to filter only perticipants who voted 0
  data_extended[["score_h5"]] <<- add_h_column(data_extended[[labels_short_h5_h6]], 2)
  
}



### H6
execute_h6 <- function(){
  data_extended[["score_h6"]] <<- add_h_column(data_extended[[labels_short_h5_h6]], 12)
}