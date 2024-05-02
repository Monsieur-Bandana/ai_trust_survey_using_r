source("helper_functions.R")
source("boxplot_creator.R")

### Hypothesis 2 (Performance over Security)
execute_h2 <-function()
  {# Ich.glaube.nicht..dass.bei.dem.Gespr.ch..ber.mein.Problem.mit.der.Internetgeschwindigkeit.private.Daten.von.mir.freigegeben.werden.
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
  data_extended[[labels_short]] <- transformer_subfunction(labels_vector, numbers_vector, data_extended[[labels_short]])
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
  data_extended[[labels_short2]] <- transformer_subfunction(labels_vector2, numbers_vector2, data_extended[[labels_short2]])
  op_closed_2 <- data_extended[[labels_short2]]
  print_mean(op_closed_2)
  summarized_value <- sum(op_closed_2)
  print(summarized_value)
  
  # Ich.m.chte.nicht..dass.die.Archivdaten..falls.meine.Daten.darin.vorhanden.sind..an.eine.KI.f.r.Trainingszwecke.weitergegeben.werden.
  str_st23 <- "Ich.m.chte.nicht..dass.die.Archivdaten..falls.meine.Daten.darin.vorhanden.sind..an.eine.KI.f.r.Trainingszwecke.weitergegeben.werden."
  data_extended[["agree_to_data_collection"]] <- 10 - as.numeric(data_extended[[str_st23]])
  statement_23 <- as.numeric(data_extended[["agree_to_data_collection"]])
  print_mean(statement_23)
  
  # Das.Telekommunikationsunternehmen.erschafft.den.virtuellen.Assistenten.basierend.auf.dem.eigenen.Datensatz.und.mit.eigenen.Entwicklerteams.
  str_st25 <- "Das.Telekommunikationsunternehmen.erschafft.den.virtuellen.Assistenten.basierend.auf.dem.eigenen.Datensatz.und.mit.eigenen.Entwicklerteams."
  data_extended[["prefer_in_house_inverted"]] <- 10 - as.numeric(data_extended[[str_st25]])
  statement_25 <- data_extended[["prefer_in_house_inverted"]]
  print_mean(statement_25)
  
  # Das.Telekommunikationsunternehmen.kauft.einen.etablierten.KI.Assistenten.von.einem.Spezialisten.Unternehmen..z.B..ChatGPT.von.OpenAI..ein..Es.ist.nicht.nachvollziehbar..wie.und.basierend.auf.welch...
  statement_26 <- as.numeric(data_extended[["Das.Telekommunikationsunternehmen.kauft.einen.etablierten.KI.Assistenten.von.einem.Spezialisten.Unternehmen..z.B..ChatGPT.von.OpenAI..ein..Es.ist.nicht.nachvollziehbar..wie.und.basierend.auf.welch..."]])
  print_mean(statement_26)
  
  highest_possible_score_h2 <<- 80
  mean(statement_1)
  df_h2 <<- data.frame(mean(statement_1), mean(statement_2), mean(statement_3), mean(op_closed_1), mean(op_closed_2), mean(statement_23), mean(statement_25), mean(statement_26))
  
  summary <- statement_1 + statement_2 + statement_3 + op_closed_1 + op_closed_2 + statement_23 + statement_25 + statement_26
  data_extended[["summary_of_h2"]] <- summary
  data_extended[["score_h2"]] <- add_h_column(summary, highest_possible_score_h2)
  return(data_extended)}

