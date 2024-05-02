source("helper_functions.R")

create_boxplot <- function(string, max_val){
  min_val <- max_val/2/max_val * 100
  # to do: change values into %
  
  labels <- c("experienced-early adopter", "experienced-majority", "rather experienced-majority", "unexperienced-laggard", "others")
  colors <- c("#0000FF", "#6666FF", "#FF3333", "#990000", "black")
  
  color_palette <- transformer_subfunction(labels, colors, data_extended[["combined_openess"]], FALSE)
  values_vec <- data_extended[[string]]/max_val*100
  print(values_vec)
  
  boxplot(values_vec, pch = 19, main=string, ylab = "Support of Hypothesis in percent")
  
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

create_mulitle_plots <- function(){
  
  labels <- c("experienced-early adopter", "experienced-majority", "rather experienced-majority", "unexperienced-laggard", "others")
  colors <- c("springgreen4", "springgreen2", "#FF3333", "#990000", "black")
  # for loop 3x:
  color_palette <- transformer_subfunction(labels, colors, data_extended[["combined_openess"]], FALSE)
  
  par(mar = c(5, 5, 2, 17), xpd=TRUE)
  boxplot(values ~ hypotheses,data = data_frame_for_graph, pch = 19, ylab = "Support of hypotheses in percent")
  abline(h = 50, lty = 2)
  # Points
  # for(h in data_frame_for_graph)
  for (i in 1:length(color_palette)) {
    # Subset data for the current iteration
    subset_data1 <- data_frame_for_graph[i, ]
    h2 <- i+length(color_palette)
    print(h2)
    h7 <- i+2*length(color_palette)
    subset_data2 <- data_frame_for_graph[h2, ]
    subset_data3 <- data_frame_for_graph[h7, ]
    subset_data <- rbind(subset_data1, subset_data2, subset_data3)
 
    # Create the stripchart
    stripchart(values ~ hypotheses,
               data = subset_data,
               method = "jitter",   # Random noise
               pch = 19,
               jitter = 0.2,
               offset = 0.9,
               col = color_palette[i],  # Color of the symbol
               vertical = TRUE,         # Vertical mode
               add = TRUE)              # Add it over
  }
    
  # Assuming color_palette is a vector of colors for each group
  
                  # Add it over
  legend("right", inset=c(-0.7,0), legend = labels, col = colors, pch = 19)
  return()
  
}

