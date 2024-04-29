## for single choice questions: replaces label-values by numeric weights

transformer <- function(labels_vector, numbers_vector, column_to_be_replaced){
  loop_data <- data_extended[[column_to_be_replaced]]
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