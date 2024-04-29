source("replace_labels_by_numbers.R")

### Hypothesis 1

# Ich.glaube.nicht..dass.bei.dem.Gespr.ch..ber.mein.Problem.mit.der.Internetgeschwindigkeit.private.Daten.von.mir.freigegeben.werden.
statement_1 <- as.numeric(data_extended[["Ich.glaube.nicht..dass.bei.dem.Gespr.ch..ber.mein.Problem.mit.der.Internetgeschwindigkeit.private.Daten.von.mir.freigegeben.werden."]])

# Mir.ist.am.wichtigsten..dass.das.Problem.m.glichst.schnell.behoben.wird..wichtiger.als.der.Schutz.meiner.Privatsph.re.: activate to sort column ascending
statement_2 <- as.numeric(data_extended[["Mir.ist.am.wichtigsten..dass.das.Problem.m.glichst.schnell.behoben.wird..wichtiger.als.der.Schutz.meiner.Privatsph.re."]])

# Eine.strenge.Anmeldekontrolle.beim.IT.Support.zum.Schutz.meiner.Privatsph.re.w.rde.mich.st.ren.: activate to sort column ascending
statement_3 <- as.numeric(data_extended[["Eine.strenge.Anmeldekontrolle.beim.IT.Support.zum.Schutz.meiner.Privatsph.re.w.rde.mich.st.ren."]])

# Im.Folgenden.werden.Ihnen.verschiedene.virtuelle.Assistenten.vorgestellt..bitte.w.hen.Sie.welcher.Ihnen.am.meisten.zusagt.
labels_short <- "Im.Folgenden.werden.Ihnen.verschiedene.virtuelle.Assistenten.vorgestellt..bitte.w.hen.Sie.welcher.Ihnen.am.meisten.zusagt."
label1 <- "Die Daten ber mich und mein Problem werden unter einem Pseudonym gespeichert, sodass sie nicht von Auenstehenden gelesen werden knnen. Dadurch kann die KI sich an Ihre persnlichen Bedrfnisse anpassen, kann aber auch ihre Leistung gegenber anderen Kunden mit hnlichen Problemen verbessern."
label2 <- "Der virtuelle Assistent  sammelt Ticket-Daten aber anonymisiert dabei alles, was sich auf den einzelnen Nutzer zurckfhren lsst. Diese Daten werden miteinbezogen, um den virtuellen Assistent zu trainieren und zu verbessern."
label3 <- "Der virtuelle Assistent sammelt gar keine Daten. Dadurch kann er sich aber leistungstechnisch nicht verbessern. Seine Antworten sind zeitlich auf dem Stand seines letzten Updates."
labels_vector <- c(label1, label2, label3)
numbers_vector <- c(10, 5, 0)
data_extended[[labels_short]] <- transformer(labels_vector, numbers_vector, labels_short)
op_closed_1 <- data_extended[[labels_short]]

# Der.virtuelle.Assistent.zeichnet.Ihr.Gespr.ch.auf..um.Ihre.Aussagen.verarbeiten.und.beantworten.zu.k.nnen..Doch.zu.welchem.Zeitpunkt.soll.er.diese.Aufzeichnungen.wieder..vergessen..
labels_short <- "Im.Folgenden.werden.Ihnen.verschiedene.virtuelle.Assistenten.vorgestellt..bitte.w.hen.Sie.welcher.Ihnen.am.meisten.zusagt."
label1 <- "Die Daten ber mich und mein Problem werden unter einem Pseudonym gespeichert, sodass sie nicht von Auenstehenden gelesen werden knnen. Dadurch kann die KI sich an Ihre persnlichen Bedrfnisse anpassen, kann aber auch ihre Leistung gegenber anderen Kunden mit hnlichen Problemen verbessern."
label2 <- "Der virtuelle Assistent  sammelt Ticket-Daten aber anonymisiert dabei alles, was sich auf den einzelnen Nutzer zurckfhren lsst. Diese Daten werden miteinbezogen, um den virtuellen Assistent zu trainieren und zu verbessern."
label3 <- "Der virtuelle Assistent sammelt gar keine Daten. Dadurch kann er sich aber leistungstechnisch nicht verbessern. Seine Antworten sind zeitlich auf dem Stand seines letzten Updates."
label4 <- 
labels_vector <- c(label1, label2, label3, label4)
numbers_vector <- c(10, 5, 0)
data_extended[[labels_short]] <- transformer(labels_vector, numbers_vector, labels_short)
op_closed_2 <- data_extended[[labels_short]]

summary <- statement_1 + statement_2 + statement_3 + op_closed_1 + op_closed_2
print(summary)