source("helper_functions.R")
source("rank_calculator.R")
source("boxplot_creator.R")

### Hypothesis 1 (Danger from AI over Danger from human)

# Ich.sehe.meine.Privatsph.re.nur.dann.gef.hrdet..wenn.ein.Mensch.meine.aufgezeichneten.Daten.mitliest.
statement_1 <- as.numeric(data_extended[["Ich.sehe.meine.Privatsph.re.nur.dann.gef.hrdet..wenn.ein.Mensch.meine.aufgezeichneten.Daten.mitliest."]])
print_mean(statement_1)

# Das.ist.mir.bekannt.
statement_0 <- as.numeric(data_extended[["Das.ist.mir.bekannt."]])
print_mean(statement_0)

# Ich.habe.wenig.Bedenken.dar.ber..wenn.die.Daten.in.einem.Archiv.lediglich.gelagert.werden.
statement_2 <- as.numeric(data_extended[["Ich.habe.wenig.Bedenken.dar.ber..wenn.die.Daten.in.einem.Archiv.lediglich.gelagert.werden."]])
print_mean(statement_2)

# Ich.habe.wenig.Bedenken.dar.ber..dass.die.Daten.in.ein.Archiv.gelangen..das.von.einer.KI.ausgelesen.wird.
data_extended <- invert_values("Ich.habe.wenig.Bedenken.dar.ber..dass.die.Daten.in.ein.Archiv.gelangen..das.von.einer.KI.ausgelesen.wird.")
statement_3 <- data_extended[["Ich.habe.wenig.Bedenken.dar.ber..dass.die.Daten.in.ein.Archiv.gelangen..das.von.einer.KI.ausgelesen.wird."]]
print_mean(statement_3)

# Wir.nehmen.an..das.Unternehmen.speichert.ihre.Gespr.chsverl.ufe..Ordnen.Sie.im.Folgenden.wie.und.wo.die.Daten.gespeichert.werden.sollen.
col_name <- "Wir.nehmen.an..das.Unternehmen.speichert.ihre.Gespr.chsverl.ufe..Ordnen.Sie.im.Folgenden.wie.und.wo.die.Daten.gespeichert.werden.sollen."
data_extended[[col_name]] <- sapply(data_extended[[col_name]], get_ranked_outcome)
rank <- data_extended[[col_name]]
print_mean(rank)

# Ich.m.chte.nicht..dass.die.Archivdaten..falls.meine.Daten.darin.vorhanden.sind..an.eine.KI.f.r.Trainingszwecke.weitergegeben.werden.
str_st23 <- "Ich.m.chte.nicht..dass.die.Archivdaten..falls.meine.Daten.darin.vorhanden.sind..an.eine.KI.f.r.Trainingszwecke.weitergegeben.werden."
statement_23 <- as.numeric(data_extended[[str_st23]])
print_mean(statement_23)

# Das.Telekommunikationsunternehmen.lagert.die.vollst.ndige.Entwicklung.aus..Es.ist.davon.auszugehen..dass.der.Drittanbieter.auch.auf.bestehende.Modelle.zur.ckgreift..Das.Telekommunikationsunternehm...
str_st27 <- "Das.Telekommunikationsunternehmen.lagert.die.vollst.ndige.Entwicklung.aus..Es.ist.davon.auszugehen..dass.der.Drittanbieter.auch.auf.bestehende.Modelle.zur.ckgreift..Das.Telekommunikationsunternehm..."
data_extended[["agree_to_share_data"]] <- 10 - as.numeric(data_extended[[str_st27]])
statement_27 <- data_extended[["agree_to_share_data"]]
print_mean(statement_27)

# Generell.halte.ich.es.aber.f.r.sinnvoll.wenn.das.Unternehmen.ein.Archiv.mit.abgeschlossenen.F.llen.verwendet.
str_st24 <- "Generell.halte.ich.es.aber.f.r.sinnvoll.wenn.das.Unternehmen.ein.Archiv.mit.abgeschlossenen.F.llen.verwendet."
statement_24 <- as.numeric(data_extended[[str_st24]])
print_mean(statement_24)

summary <- statement_1 + statement_2 + statement_3 + rank + statement_23 + statement_27 + statement_24
data_extended <- data.frame(data_extended, "summary_of_h1" = summary)
data_extended <- add_h_column("summary_of_h1", 70, "score_h1")
h1_counts <- table(data_extended[["score_h1"]])
print(h1_counts)

create_boxplot("summary_of_h1")