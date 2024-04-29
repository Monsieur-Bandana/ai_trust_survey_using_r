label1 <- "Die Daten ber mich und mein Problem werden unter einem Pseudonym gespeichert, sodass sie nicht von Auenstehenden gelesen werden knnen. Dadurch kann die KI sich an Ihre persnlichen Bedrfnisse anpassen, kann aber auch ihre Leistung gegenber anderen Kunden mit hnlichen Problemen verbessern."
label2 <- "Der virtuelle Assistent  sammelt Ticket-Daten aber anonymisiert dabei alles, was sich auf den einzelnen Nutzer zurckfhren lsst. Diese Daten werden miteinbezogen, um den virtuellen Assistent zu trainieren und zu verbessern."
label3 <- "Der virtuelle Assistent sammelt gar keine Daten. Dadurch kann er sich aber leistungstechnisch nicht verbessern. Seine Antworten sind zeitlich auf dem Stand seines letzten Updates."
labels_vector <- c(label1, label2, label3)
numbers_vector <- c(10, 5, 0)

labels_short <- "Im.Folgenden.werden.Ihnen.verschiedene.virtuelle.Assistenten.vorgestellt..bitte.w.hen.Sie.welcher.Ihnen.am.meisten.zusagt."
# work arround wegen problem mit sonderzeichen
# data_extended[[labels_short]] <- substr(data_extended[[labels_short]], nchar(data_extended[[labels_short]]) - 15, nchar(data_extended[[labels_short]]))
loop_data <- data_extended[[labels_short]]
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
data_extended[[labels_short]] <- loop_data