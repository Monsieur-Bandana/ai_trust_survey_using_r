source("helper_functions.R")

create_boxplot <- function(string){
  # to do: change values into %
  
  labels <- c("experienced-early adopter", "experienced-majority", "rather experienced-majority", "unexperienced-laggard", "others")
  colors <- c("#0000FF", "#6666FF", "#FF3333", "#990000", "black")
  
  color_palette <- transformer_subfunction(labels, colors, data_extended[["combined_openess"]], FALSE)
  values_vec <- data_extended[[string]]
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
  
  abline(h = 50, lty = 2)
  
  legend("bottomright", legend = labels, col = colors, pch = 19, title = "Data", xpd = TRUE, inset = c(0, -0.25))
}

create_mulitle_plots <- function(data_frame_in_question){

  
  par(mar = c(10, 5, 2, 2), xpd=TRUE)

  boxplot(values ~ hypotheses,data = data_frame_in_question, pch = 19, ylab = "Support of hypotheses in percent", outline = FALSE)
  abline(h = 50, lty = 2)
  # Points
  # for(h in data_frame_in_question)
  data_frame_length <- nrow(data_frame_in_question)/3
  for (i in 1:data_frame_length) {
    # Subset data for the current iteration
    subset_data1 <- data_frame_in_question[i, ]
    h2 <- i+data_frame_length
    print(h2)
    h7 <- i+2*data_frame_length
    subset_data2 <- data_frame_in_question[h2, ]
    subset_data3 <- data_frame_in_question[h7, ]
    subset_data <- rbind(subset_data1, subset_data2, subset_data3)
    
    print(subset_data)
 
    # Create the stripchart
    stripchart(values ~ hypotheses,
               data = subset_data,
               method = "jitter",   # Random noise
               pch = 19,
               jitter = 0.2,
               offset = 0.9,
               col = subset_data$colors,  # Color of the symbol
               vertical = TRUE,         # Vertical mode
               add = TRUE)              # Add it over
  }
    
  # Assuming color_palette is a vector of colors for each group
  
                  # Add it over
  legend("bottom", legend = labels, col = colors, pch = 19, ncol = 3, inset=c(0,-0.4))
  return()
  
}

