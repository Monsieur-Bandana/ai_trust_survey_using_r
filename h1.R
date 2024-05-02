source("helper_functions.R")
source("rank_calculator.R")
source("boxplot_creator.R")

### Hypothesis 1 (Danger from AI over Danger from human)
execute_h1 <- function(){
  # Ich.sehe.meine.Privatsph.re.nur.dann.gef.hrdet..wenn.ein.Mensch.meine.aufgezeichneten.Daten.mitliest.
  str_1 <- "Ich.sehe.meine.Privatsph.re.nur.dann.gef.hrdet..wenn.ein.Mensch.meine.aufgezeichneten.Daten.mitliest."
  # = reading AI is seen as danger
  data_extended <- invert_values(str_1)
  statement_1 <- data_extended[[str_1]]
  print(str_1)
  print_mean(statement_1)
  
  # Das.ist.mir.bekannt.
  statement_0 <- as.numeric(data_extended[["Das.ist.mir.bekannt."]])
  print_mean(statement_0)
  
  # Ich.habe.wenig.Bedenken.dar.ber..wenn.die.Daten.in.einem.Archiv.lediglich.gelagert.werden.
  str_2 <- "Ich.habe.wenig.Bedenken.dar.ber..wenn.die.Daten.in.einem.Archiv.lediglich.gelagert.werden."
  statement_2 <- as.numeric(data_extended[[str_2]])
  print(str_2)
  print_mean(statement_2)
  
  # Ich.habe.wenig.Bedenken.dar.ber..dass.die.Daten.in.ein.Archiv.gelangen..das.von.einer.KI.ausgelesen.wird.
  str_3 <- "Ich.habe.wenig.Bedenken.dar.ber..dass.die.Daten.in.ein.Archiv.gelangen..das.von.einer.KI.ausgelesen.wird."
  data_extended <- invert_values(str_3)
  statement_3 <- data_extended[[str_3]]
  print_mean(statement_3)
  
  # Wir.nehmen.an..das.Unternehmen.speichert.ihre.Gespr.chsverl.ufe..Ordnen.Sie.im.Folgenden.wie.und.wo.die.Daten.gespeichert.werden.sollen.
  col_name <- "Wir.nehmen.an..das.Unternehmen.speichert.ihre.Gespr.chsverl.ufe..Ordnen.Sie.im.Folgenden.wie.und.wo.die.Daten.gespeichert.werden.sollen."
  data_extended[[col_name]] <- sapply(data_extended[[col_name]], get_ranked_outcome)
  rank <- data_extended[[col_name]]
  print(col_name)
  print_mean(rank)
  
  # Ich.m.chte.nicht..dass.die.Archivdaten..falls.meine.Daten.darin.vorhanden.sind..an.eine.KI.f.r.Trainingszwecke.weitergegeben.werden.
  str_st23 <- "Ich.m.chte.nicht..dass.die.Archivdaten..falls.meine.Daten.darin.vorhanden.sind..an.eine.KI.f.r.Trainingszwecke.weitergegeben.werden."
  statement_23 <- as.numeric(data_extended[[str_st23]])
  print(str_st23)
  print_mean(statement_23)
  
  # Das.Telekommunikationsunternehmen.lagert.die.vollst.ndige.Entwicklung.aus..Es.ist.davon.auszugehen..dass.der.Drittanbieter.auch.auf.bestehende.Modelle.zur.ckgreift..Das.Telekommunikationsunternehm...
  str_st27 <- "Das.Telekommunikationsunternehmen.lagert.die.vollst.ndige.Entwicklung.aus..Es.ist.davon.auszugehen..dass.der.Drittanbieter.auch.auf.bestehende.Modelle.zur.ckgreift..Das.Telekommunikationsunternehm..."
  data_extended[["agree_to_share_data"]] <- 10 - as.numeric(data_extended[[str_st27]])
  statement_27 <- data_extended[["agree_to_share_data"]]
  print(str_st27)
  print_mean(statement_27)
  
  # Generell.halte.ich.es.aber.f.r.sinnvoll.wenn.das.Unternehmen.ein.Archiv.mit.abgeschlossenen.F.llen.verwendet.
  str_st24 <- "Generell.halte.ich.es.aber.f.r.sinnvoll.wenn.das.Unternehmen.ein.Archiv.mit.abgeschlossenen.F.llen.verwendet."
  statement_24 <- as.numeric(data_extended[[str_st24]])
  print(str_st24)
  print_mean(statement_24)
  
  summary <- statement_1 + statement_2 + statement_3 + rank + statement_23 + statement_27 + statement_24
  
  df_h1 <<- data.frame(mean(statement_1), mean(statement_2), mean(statement_3), mean(rank), mean(statement_23), mean(statement_27), mean(statement_24))
  
  highest_possible_score_h1 <<- 70
  
  data_extended[["summary_of_h1"]] <- summary
  data_extended[["score_h1"]] <- add_h_column(summary, highest_possible_score_h1)
  # h1_counts <- table(data_extended[["score_h1"]])
  # create_boxplot("summary_of_h1", 70)s
  
  return(data_extended)
}