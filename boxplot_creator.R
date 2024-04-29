create_boxplot <- function(string){
  boxplot(data_extended[[string]] , col=terrain.colors(4) )
  points(jitter(data_extended[[string]]), col = "black", pch = 16)
}