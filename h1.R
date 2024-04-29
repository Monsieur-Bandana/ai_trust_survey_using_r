source("helper_functions.R")

### Hypothesis 1 (Performance over Security)

# Ich.glaube.nicht..dass.bei.dem.Gespr.ch..ber.mein.Problem.mit.der.Internetgeschwindigkeit.private.Daten.von.mir.freigegeben.werden.
statement_1 <- as.numeric(data_extended[["Ich.glaube.nicht..dass.bei.dem.Gespr.ch..ber.mein.Problem.mit.der.Internetgeschwindigkeit.private.Daten.von.mir.freigegeben.werden."]])
print_mean(statement_1)

# Mir.ist.am.wichtigsten..dass.das.Problem.m.glichst.schnell.behoben.wird..wichtiger.als.der.Schutz.meiner.Privatsph.re.: activate to sort column ascending
statement_2 <- as.numeric(data_extended[["Mir.ist.am.wichtigsten..dass.das.Problem.m.glichst.schnell.behoben.wird..wichtiger.als.der.Schutz.meiner.Privatsph.re."]])
print_mean(statement_2)

# Eine.strenge.Anmeldekontrolle.beim.IT.Support.zum.Schutz.meiner.Privatsph.re.w.rde.mich.st.ren.: activate to sort column ascending
statement_3 <- as.numeric(data_extended[["Eine.strenge.Anmeldekontrolle.beim.IT.Support.zum.Schutz.meiner.Privatsph.re.w.rde.mich.st.ren."]])
print_mean(statement_3)

# Im.Folgenden.werden.Ihnen.verschiedene.virtuelle.Assistenten.vorgestellt..bitte.w.hen.Sie.welcher.Ihnen.am.meisten.zusagt.
labels_short <- "Im.Folgenden.werden.Ihnen.verschiedene.virtuelle.Assistenten.vorgestellt..bitte.w.hen.Sie.welcher.Ihnen.am.meisten.zusagt."
label1 <- "Die Daten ber mich und mein Problem werden unter einem Pseudonym gespeichert, sodass sie nicht von Auenstehenden gelesen werden knnen. Dadurch kann die KI sich an Ihre persnlichen Bedrfnisse anpassen, kann aber auch ihre Leistung gegenber anderen Kunden mit hnlichen Problemen verbessern."
label2 <- "Der virtuelle Assistent  sammelt Ticket-Daten aber anonymisiert dabei alles, was sich auf den einzelnen Nutzer zurckfhren lsst. Diese Daten werden miteinbezogen, um den virtuellen Assistent zu trainieren und zu verbessern."
label3 <- "Der virtuelle Assistent sammelt gar keine Daten. Dadurch kann er sich aber leistungstechnisch nicht verbessern. Seine Antworten sind zeitlich auf dem Stand seines letzten Updates."
labels_vector <- c(label1, label2, label3)
numbers_vector <- c(10, 5, 0)
data_extended[[labels_short]] <- transformer(labels_vector, numbers_vector, labels_short)
op_closed_1 <- data_extended[[labels_short]]
print_mean(op_closed_1)
summarized_value <- sum(op_closed_1)
print(summarized_value)

# Der.virtuelle.Assistent.zeichnet.Ihr.Gespr.ch.auf..um.Ihre.Aussagen.verarbeiten.und.beantworten.zu.k.nnen..Doch.zu.welchem.Zeitpunkt.soll.er.diese.Aufzeichnungen.wieder..vergessen..
labels_short2 <- "Der.virtuelle.Assistent.zeichnet.Ihr.Gespr.ch.auf..um.Ihre.Aussagen.verarbeiten.und.beantworten.zu.k.nnen..Doch.zu.welchem.Zeitpunkt.soll.er.diese.Aufzeichnungen.wieder..vergessen.."
label1 <- "Der virtuelle Assistent zeichnet das gesamte Gesprch auf und hat sich auch Ihre vergangenen Gesprche gemerkt (pseudonymisiert und verschlsselt). Mglicherweise hngt Ihr aktuelles Problem mit einem vergangenen Problem zusammen."
label2 <- "Der virtuelle Assistent merkt sich das ganze Gesprch fr denselben Tag, fr den Fall, dass Sie Rckfragen haben, oder etwas nachschauen mssen, wodurch Sie das Gesprch unterbrechen mssen. Nach 24 Stunden wird das Gesprch gelscht."
label3 <- "Der virtuelle Assistent zeichnet das gesamte Gesprch auf und kann dadurch auch auf den Anfang des Gesprchs Bezug nehmen. Dadurch kann er auf komplexere Probleme eingehen, die mglicherweise ein hin-und-her erfordern. Der virtuelle Assistent lscht die Daten nach dem Gesprchsende direkt wieder."
label4 <- "Der virtuelle Assistent sollte gar kein Gedchtnis haben. Er bezieht sich in seinen Antworten lediglich auf das zuletzt von Ihnen Gesagte. Der vorhergehende Dialog hat keinen Einfluss auf seine Antwort"
labels_vector2 <- c(label1, label2, label3, label4)
numbers_vector2 <- c(10, 5, 2, 0)
data_extended[[labels_short2]] <- transformer(labels_vector2, numbers_vector2, labels_short2)
op_closed_2 <- data_extended[[labels_short2]]
print_mean(op_closed_2)
summarized_value <- sum(op_closed_2)
print(summarized_value)

summary <- statement_1 + statement_2 + statement_3 + op_closed_1 + op_closed_2
data_extended <- data.frame(data_extended, "summary_of_h1" = summary)
data_extended <- add_h_column("summary_of_h1", 50, "score_h1")
h1_counts <- table(data_extended[["score_h1"]])
print(h1_counts)

boxplot(data_extended[["summary_of_h1"]] , col=terrain.colors(4) )
points(jitter(data_extended[["summary_of_h1"]]), col = "black", pch = 16)
