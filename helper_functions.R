## for single choice questions: replaces label-values by numeric weights

transformer_subfunction <- function(labels_vector, numbers_vector, loop_data){
  gen_counter <- 1
  for(x in loop_data){
    counter <- 1
    for(y in labels_vector){
      if(identical(x, y)){
        loop_data[gen_counter] <- numbers_vector[counter]
      }
      counter <- counter + 1
    }
    gen_counter <- gen_counter + 1
  }
  
  loop_data <- as.numeric(loop_data)
  return(loop_data)
}

stringsplitter <- function(string){
  substring_list <- unlist(strsplit(string, ";"))
  substring_vector <- as.vector(substring_list)
  return(substring_vector)
}

transformer <- function(labels_vector, numbers_vector, column_to_be_replaced){
  loop_data <- data_extended[[column_to_be_replaced]]
  loop_data <- transformer_subfunction(labels_vector, numbers_vector, loop_data)
  return(loop_data) 
}


change_to_bool_values <- function(column){
  data[[column]] <- ifelse(data[[column]] == "Trifft zu", TRUE, FALSE)
}

# remove special characters
replace_special <- function(x) {
  x <- gsub("[^[:print:]]", "", x, perl = TRUE)
  return(x)
}

print_mean <- function(dataset){
  mst <- mean(dataset)
  print(mst)
}

invert_values <- function(column_name){
  data_extended[[column_name]] <- 10 - as.numeric(data_extended[[column_name]])
  return(data_extended)}

categorize_h <- function(value, max_value){
  min_val <- max_value / 2
  if(value > min_val){
    return(1)
  }else{
    return(0)
  }
}

add_h_column <- function(column_name, max_value, name_of_new_column){
  categories_h <- sapply(data_extended[[column_name]], categorize_h, max_value=max_value)
  
  data_extended[[name_of_new_column]] <- categories_h
  return(data_extended)
}