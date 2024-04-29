source("rank_calculator.R")
source("boxplot_creator.R")
source("multiple_choice_calculator.R")

### Hypothesis 7 (Customers dont trust VAs)
highest_possible_score <- 0
summary <- 0

# Das.Telekommunikationsunternehmen.kauft.einen.etablierten.KI.Assistenten.von.einem.Spezialisten.Unternehmen..z.B..ChatGPT.von.OpenAI..ein..Es.ist.nicht.nachvollziehbar..wie.und.basierend.auf.welch...
str_26 <- "Das.Telekommunikationsunternehmen.kauft.einen.etablierten.KI.Assistenten.von.einem.Spezialisten.Unternehmen..z.B..ChatGPT.von.OpenAI..ein..Es.ist.nicht.nachvollziehbar..wie.und.basierend.auf.welch..."
data_extended[["distrust_in_common_solution"]] <- 10 - as.numeric(data_extended[[str_26]])
statement_26 <- data_extended[["distrust_in_common_solution"]]
print_mean(statement_26)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_26

# Ich.m.chte.nicht..dass.ein.virtueller.Assistent.dar.ber.urteilt..an.welchen.Mitarbeiter.mein.Problem.weitergeleitet.wird.
str_28 <- "Ich.m.chte.nicht..dass.ein.virtueller.Assistent.dar.ber.urteilt..an.welchen.Mitarbeiter.mein.Problem.weitergeleitet.wird."
statement_28 <- as.numeric(data_extended[[str_28]])
print_mean(statement_28)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_28

# Ich.denke.f.r.meine.Zwecke.sind.die.Ergebnisse.des.virtuellen.Assistenten.v.llig.ausreichend.
str_29 <- "Ich.denke.f.r.meine.Zwecke.sind.die.Ergebnisse.des.virtuellen.Assistenten.v.llig.ausreichend."
data_extended[[str_29]] <- 10 - as.numeric(data_extended[[str_29]])
statement_29 <- data_extended[[str_29]]
print_mean(statement_29)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_29

# Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten.
str_30 <- "Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten."
data_extended[[str_30]] <- 10 - as.numeric(data_extended[[str_30]])
statement_30 <- data_extended[[str_30]]
print_mean(statement_30)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_30

# Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten.mehr.als.denen.eines.Menschen.
str_31 <- "Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten.mehr.als.denen.eines.Menschen."
data_extended[[str_31]] <- 10 - as.numeric(data_extended[[str_31]])
statement_31 <- data_extended[[str_31]]
print_mean(statement_31)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_31

# Wenn.der.virtuelle.Assistent.zu.seinem.L.sungsvorschlag.zus.tzlich.eine.Begr.ndung.oder.Argumentation.ausgibt..vertraue.ich.seinem.Ergebnis.eher.
str_32 <- "Wenn.der.virtuelle.Assistent.zu.seinem.L.sungsvorschlag.zus.tzlich.eine.Begr.ndung.oder.Argumentation.ausgibt..vertraue.ich.seinem.Ergebnis.eher."
data_extended[[str_32]] <- 10 - as.numeric(data_extended[[str_32]])
statement_32 <- data_extended[[str_32]]
print_mean(statement_32)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_32

# Wenn.die.Firma.transparent.darstellt..auf.welcher.Datengrundlage.der.virtuelle.Assistent.trainiert.wurde.vertraue.ich.dem.virtuellen.Assistenten.mehr.
str_33 <- "Wenn.die.Firma.transparent.darstellt..auf.welcher.Datengrundlage.der.virtuelle.Assistent.trainiert.wurde.vertraue.ich.dem.virtuellen.Assistenten.mehr."
data_extended[[str_33]] <- 10 - as.numeric(data_extended[[str_33]])
statement_33 <- data_extended[[str_33]]
print_mean(statement_33)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_33

# Einen.Techniker..der.sich.vor.Ort.um.mein.Problem.k.mmert..halte.ich.f.r.unprofessionell.wenn.ich.merke..dass.er.auf.seinem.Smartphone.einen.virtuellen.Assistenten.einsetzt.
str_34 <- "Einen.Techniker..der.sich.vor.Ort.um.mein.Problem.k.mmert..halte.ich.f.r.unprofessionell.wenn.ich.merke..dass.er.auf.seinem.Smartphone.einen.virtuellen.Assistenten.einsetzt."
data_extended[[str_34]] <- 10 - as.numeric(data_extended[[str_34]])
statement_34 <- data_extended[[str_34]]
print_mean(statement_34)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_34

## the following outcomes are weighted
# Ich.kommuniziere.ausschlie.lich.mit.einem.menschlichen.Mitarbeiter.
str_35 <- "Ich.kommuniziere.ausschlie.lich.mit.einem.menschlichen.Mitarbeiter."
statement_35 <- as.numeric(data_extended[[str_35]]) * 6
print_mean(statement_35)
highest_possible_score <- highest_possible_score + 60
summary <- summary + statement_35

# Ich.kommuniziere.ausschlie.lich.mit.einem.menschlichen.Mitarbeiter.
str_36 <- "Ich.telefoniere.mit.einem.menschlichen.Mitarbeiter..Dieser.nutzt.den.Assistenten.optional..indem.er.mit.diesem.per.Chat.kommuniziert."
statement_36 <- as.numeric(data_extended[[str_36]]) * 5
print_mean(statement_36)
highest_possible_score <- highest_possible_score + 50
summary <- summary + statement_36

# Ich.telefoniere.mit.einem.Menschen..der.virtuelle.Assistent.h.rt.das.Gespr.ch.mit.und.schl.gt.dem.Mitarbeiter.mehrere.Ergebnisse.vor..Der.Mitarbeiter.w.hlt.das.am.besten.Passende.nach.Gef.hl.Erfah...
str_37 <- "Ich.telefoniere.mit.einem.Menschen..der.virtuelle.Assistent.h.rt.das.Gespr.ch.mit.und.schl.gt.dem.Mitarbeiter.mehrere.Ergebnisse.vor..Der.Mitarbeiter.w.hlt.das.am.besten.Passende.nach.Gef.hl.Erfah..."
statement_37 <- as.numeric(data_extended[[str_37]]) * 4
print_mean(statement_37)
highest_possible_score <- highest_possible_score + 40
summary <- summary + statement_37

# Ich.telefoniere.mit.einem.virtuellen.Assistenten..dieser.erarbeitet.die.L.sung.weitgehend.selbstst.ndig..ein.Mensch.wird.lediglich.zur.abschlie.enden.Kontrolle.des.L.sungsvorschlags.eingesetzt.
str_38 <- "Ich.telefoniere.mit.einem.virtuellen.Assistenten..dieser.erarbeitet.die.L.sung.weitgehend.selbstst.ndig..ein.Mensch.wird.lediglich.zur.abschlie.enden.Kontrolle.des.L.sungsvorschlags.eingesetzt."
statement_38 <- as.numeric(data_extended[[str_38]]) * 3
print_mean(statement_38)
highest_possible_score <- highest_possible_score + 30
summary <- summary + statement_38

# Ich.chatte.direkt.mit.einem.virtuellen.Assistenten.und.dieser.gibt.mir.selbstst.ndig.L.sungsvorschl.ge.
str_40 <- "Ich.chatte.direkt.mit.einem.virtuellen.Assistenten.und.dieser.gibt.mir.selbstst.ndig.L.sungsvorschl.ge."
data_extended[[str_40]] <- 10 - as.numeric(data_extended[[str_40]])
statement_40 <- as.numeric(data_extended[[str_40]]) * 2
print_mean(statement_40)
highest_possible_score <- highest_possible_score + 20
summary <- summary + statement_40

# Ich.telefoniere.direkt.mit.einem.virtuellen.Assistenten.und.er.gibt.mir.selbstst.ndig.L.sungsvorschl.ge.
str_39 <- "Ich.telefoniere.direkt.mit.einem.virtuellen.Assistenten.und.er.gibt.mir.selbstst.ndig.L.sungsvorschl.ge."
data_extended[[str_39]] <- 10 - as.numeric(data_extended[[str_39]])
statement_39 <- as.numeric(data_extended[[str_39]])
print_mean(statement_39)
highest_possible_score <- highest_possible_score + 10
summary <- summary + statement_39

# In.diesen.Aufgaben.vertraue.ich.virtuellen.Assistenten..Mehrfache.Auswahl.m.glich.
col_name <- "In.diesen.Aufgaben.vertraue.ich.virtuellen.Assistenten..Mehrfache.Auswahl.m.glich."
data_extended[[col_name]] <- sapply(data_extended[[col_name]], multiple_choice_outcome)
mult <- data_extended[[col_name]]
print_mean(mult)
highest_possible_score <- highest_possible_score + 12
summary <- summary + mult

data_extended <- data.frame(data_extended, "summary_of_h7" = summary)
data_extended <- add_h_column("summary_of_h7", highest_possible_score, "score_h7")
h7_counts <- table(data_extended[["score_h7"]])
print(h7_counts)

create_boxplot("summary_of_h7")
