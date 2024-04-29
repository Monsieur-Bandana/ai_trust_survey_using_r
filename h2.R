source("helper_functions.R")
source("rank_calculator.R")

### Hypothesis 2 (Danger from AI over Danger from human)

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

summary <- statement_1 + statement_2 + statement_3 + rank
data_extended <- data.frame(data_extended, "summary_of_h2" = summary)
data_extended <- add_h_column("summary_of_h2", 40, "score_h2")
h2_counts <- table(data_extended[["score_h2"]])
print(h2_counts)

boxplot(data_extended[["summary_of_h2"]] , col=terrain.colors(4) )
points(jitter(data_extended[["summary_of_h2"]]), col = "black", pch = 16)