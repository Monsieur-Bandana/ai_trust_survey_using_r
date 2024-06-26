## translate string labels into processable values and create a new list containing the translations
transformer_subfunction <- function(labels_vector, numbers_vector, loop_data, numeric = TRUE){
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
  if(numeric){
    
    loop_data <- as.numeric(loop_data)
  }
  return(loop_data)
}

## split chains of string labels and create a list
stringsplitter <- function(string){
  substring_list <- unlist(strsplit(string, ";"))
  substring_vector <- as.vector(substring_list)
  return(substring_vector)
}

## translate string labels into bool values
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

## inverts outcomes from statements, which were asked the other way as it would support the hypothesis
invert_values <- function(column_name){
  data_extended[[column_name]] <- 10 - as.numeric(data_extended[[column_name]])
  return(data_extended)}

## scale archieved outcomes on a 0 to 100 scale
calculate_percentile <- function(string, highest_possible_value){
  values_vec <- data_extended[[string]]/highest_possible_value*100
  return(values_vec)
}

## checks if participant passed the 50 boundary
categorize_h <- function(value, max_value){
  min_val <- max_value / 2
  if(value > min_val){
    return(1)
  }else{
    return(0)
  }
}

## create new column containing if particpants support or deny the thesis
add_h_column <- function(column_name, max_value){
  categories_h <- sapply(column_name, categorize_h, max_value=max_value)
  
  # data_extended[[name_of_new_column]] <- categories_h
  return(categories_h)
}