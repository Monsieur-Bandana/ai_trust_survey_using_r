get_means <- function(dataset){
  plot_data <- dataset
  mst1 <- mean(plot_data)
  print(mst1)
  label_colors <- c("red", "blue", "green")
  labels_ai <- data_extended[[categories_ai]]
  labels_op <- data_extended[[categories]]
  barplot(plot_data, col = label_colors[as.factor(labels_ai)], main=names(dataset)[1])
  legend("bottom", legend = unique(labels_ai), fill = label_colors)
  mean_by_label_ai <- tapply(plot_data, labels_ai, mean)
  mean_by_label_op <- tapply(plot_data, labels_op, mean)
  print(mean_by_label_ai)
  print(mean_by_label_op)
}



# get vals st1
get_means(data_extended$`Ich.glaube.nicht..dass.bei.dem.Gespr.ch..ber.mein.Problem.mit.der.Internetgeschwindigkeit.private.Daten.von.mir.freigegeben.werden.` <- data_extended$`Ich.glaube.nicht..dass.bei.dem.Gespr.ch..ber.mein.Problem.mit.der.Internetgeschwindigkeit.private.Daten.von.mir.freigegeben.werden.`)

# get vals st2
get_means(data_extended$`Mir.ist.am.wichtigsten..dass.das.Problem.m.glichst.schnell.behoben.wird..wichtiger.als.der.Schutz.meiner.Privatsph.re.`)

# get vals st3
get_means(data_extended$`Eine.strenge.Anmeldekontrolle.beim.IT.Support.zum.Schutz.meiner.Privatsph.re.w.rde.mich.st.ren.`)
