## calculate ranking question no. 18
source("helper_functions.R")

get_ranked_outcome <- function(value){
  string <- value
  label1 <- "Das Unternehmen betreibt alles im eigenen Haus und hat eigens dafr abgestellte Experten."
  label2 <- "Das Unternehmen lagert die Daten im eigenen Haus, die Wartung wird aber durch Dritte durchgefhrt."
  label3 <- "Das Unternehmen verwendet Server von unbekannten Drittanbietern. Es ist davon auszugehen, dass diese sich an die bergeordneten Sicherheitsstandards ihres Klienten halten. Es ist auch davon auszugehen, dass andere Unternehmen ebenfalls die Server dieses Drittanbieters nutzen."
  label4 <- "Das Unternehmen verwendet Server eines groen Technologiekonzerns, etwa Google, Microsoft oder Amazon."
  
  labels_vector <- c(label1, label2, label3, label4)
  numbers_vector <- c(3, 2, 1, 0)
  
  calc_vec <- stringsplitter(string)
  calc_vec <- transformer_subfunction(labels_vector, numbers_vector, calc_vec)
  outcome <- 10 * calc_vec[1] + 7 * calc_vec[2] + 3 * calc_vec[3] + 0 * calc_vec[4]
  max_out <- 10 * 3 + 7 * 2 + 3 * 1 + 0
  endpoints <- round(outcome/max_out * 10)
  return(endpoints)
}