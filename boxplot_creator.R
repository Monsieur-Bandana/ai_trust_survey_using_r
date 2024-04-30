source("helper_functions.R")

create_boxplot <- function(string, max_val){
  min_val <- max_val/2/max_val * 100
  # to do: change values into %
  
  labels <- c("experienced-early adopter", "experienced-majority", "rather experienced-majority", "unexperienced-laggard", "rather experienced-early adopter", "rather experienced-laggard")
  colors <- c("#0000FF", "#6666FF", "#FF3333", "#990000", "black", "black")
  
  color_palette <- transformer_subfunction(labels, colors, data_extended[["combined_openess"]], FALSE)
  values_vec <- data_extended[[string]]/max_val*100
  print(values_vec)
  boxplot(values_vec, pch = 19, main=string)
  
  # Points
  for (i in 1:length(values_vec)){
    
    stripchart(values_vec[i],              # Data
               method = "jitter", # Random noise
               pch = 19,
               col=color_palette[i],         # Color of the symbol
               vertical = TRUE,   # Vertical mode
               add = TRUE)        # Add it over
  }
  
  abline(h = min_val, lty = 2)
  
  legend("bottomright", legend = labels, col = colors, pch = 19, title = "Data", xpd = TRUE, inset = c(0, -0.25))
}

create_violin <- function(string){
  library(ggplot2)
  new_vector <- 1:41
  plot_data <- as.data.frame(new_vector)
  colnames(plot_data) <- c("name")
  plot_data["value"] <- data_extended[[string]]
  ggplot( plot_data,  aes(x=name, y=value), col=terrain.colors(4) ) + geom_violin()
  # myjitter <- jitter(data_extended[[string]])
  # points(jitter(myjitter, data_extended[[string]]), col = "black", pch = 20)
}