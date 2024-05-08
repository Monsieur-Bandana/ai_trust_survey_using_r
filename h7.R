### calculate outcome values for H7 and add them to the main data frame

source("rank_calculator.R")
source("boxplot_creator.R")
source("multiple_choice_calculator.R")

### Hypothesis 7 (Customers dont trust VAs)
execute_h7 <- function()
  {highest_possible_score <- 0
  summary <- 0
  
  # Das.Telekommunikationsunternehmen.kauft.einen.etablierten.KI.Assistenten.von.einem.Spezialisten.Unternehmen..z.B..ChatGPT.von.OpenAI..ein..Es.ist.nicht.nachvollziehbar..wie.und.basierend.auf.welch...
  str_26 <- "Das.Telekommunikationsunternehmen.kauft.einen.etablierten.KI.Assistenten.von.einem.Spezialisten.Unternehmen..z.B..ChatGPT.von.OpenAI..ein..Es.ist.nicht.nachvollziehbar..wie.und.basierend.auf.welch..."
  data_extended[["distrust_in_common_solution"]] <- 10 - as.numeric(data_extended[[str_26]])
  statement_26 <- data_extended[["distrust_in_common_solution"]]
  print_mean(statement_26)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + statement_26
  df_h7 <<- data.frame(mean(statement_26))
  
  # Ich.m.chte.nicht..dass.ein.virtueller.Assistent.dar.ber.urteilt..an.welchen.Mitarbeiter.mein.Problem.weitergeleitet.wird.
  str_28 <- "Ich.m.chte.nicht..dass.ein.virtueller.Assistent.dar.ber.urteilt..an.welchen.Mitarbeiter.mein.Problem.weitergeleitet.wird."
  statement_28 <- as.numeric(data_extended[[str_28]])
  print_mean(statement_28)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + statement_28
  df_h7[["statement_28"]] <<- mean(statement_28)
  
  # Ich.denke.f.r.meine.Zwecke.sind.die.Ergebnisse.des.virtuellen.Assistenten.v.llig.ausreichend.
  str_29 <- "Ich.denke.f.r.meine.Zwecke.sind.die.Ergebnisse.des.virtuellen.Assistenten.v.llig.ausreichend."
  data_extended[[str_29]] <- 10 - as.numeric(data_extended[[str_29]])
  statement_29 <- data_extended[[str_29]]
  print_mean(statement_29)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + statement_29
  df_h7[["statement_29"]] <<- mean(statement_29)
  
  # Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten.
  str_30 <- "Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten."
  data_extended[[str_30]] <- 10 - as.numeric(data_extended[[str_30]])
  statement_30 <- data_extended[[str_30]]
  print_mean(statement_30)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + statement_30
  df_h7[["statement_30"]] <<- mean(statement_30)
  
  # Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten.mehr.als.denen.eines.Menschen.
  str_31 <- "Ich.vertraue.den.Ergebnissen.eines.virtuellen.Assistenten.mehr.als.denen.eines.Menschen."
  data_extended[[str_31]] <- 10 - as.numeric(data_extended[[str_31]])
  statement_31 <- data_extended[[str_31]]
  print_mean(statement_31)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + statement_31
  df_h7[["statement_31"]] <<- mean(statement_31)
  
  # Wenn.der.virtuelle.Assistent.zu.seinem.L.sungsvorschlag.zus.tzlich.eine.Begr.ndung.oder.Argumentation.ausgibt..vertraue.ich.seinem.Ergebnis.eher.
  str_32 <- "Wenn.der.virtuelle.Assistent.zu.seinem.L.sungsvorschlag.zus.tzlich.eine.Begr.ndung.oder.Argumentation.ausgibt..vertraue.ich.seinem.Ergebnis.eher."
  data_extended[[str_32]] <- as.numeric(data_extended[[str_32]])
  statement_32 <- data_extended[[str_32]]
  print_mean(statement_32)
  # highest_possible_score <- highest_possible_score + 10
  # summary <- summary + statement_32
  df_h7[["statement_32"]] <<- mean(statement_32)
  
  # Wenn.die.Firma.transparent.darstellt..auf.welcher.Datengrundlage.der.virtuelle.Assistent.trainiert.wurde.vertraue.ich.dem.virtuellen.Assistenten.mehr.
  str_33 <- "Wenn.die.Firma.transparent.darstellt..auf.welcher.Datengrundlage.der.virtuelle.Assistent.trainiert.wurde.vertraue.ich.dem.virtuellen.Assistenten.mehr."
  data_extended[[str_33]] <- as.numeric(data_extended[[str_33]])
  statement_33 <- data_extended[[str_33]]
  print_mean(statement_33)
  # highest_possible_score <- highest_possible_score + 10
  # summary <- summary + statement_33
  df_h7[["statement_33"]] <<- mean(statement_33)
  
  # Einen.Techniker..der.sich.vor.Ort.um.mein.Problem.k.mmert..halte.ich.f.r.unprofessionell.wenn.ich.merke..dass.er.auf.seinem.Smartphone.einen.virtuellen.Assistenten.einsetzt.
  str_34 <- "Einen.Techniker..der.sich.vor.Ort.um.mein.Problem.k.mmert..halte.ich.f.r.unprofessionell.wenn.ich.merke..dass.er.auf.seinem.Smartphone.einen.virtuellen.Assistenten.einsetzt."
  data_extended[[str_34]] <- 10 - as.numeric(data_extended[[str_34]])
  statement_34 <- data_extended[[str_34]]
  print_mean(statement_34)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + statement_34
  df_h7[["statement_34"]] <<- mean(statement_34)
  
  ### the following outcomes are weighted
  
  ## sinc we want to proof, that human-driven suppor is preffered, statements with a higher human-autononmy get a higher weight
  # Ich.kommuniziere.ausschlie.lich.mit.einem.menschlichen.Mitarbeiter.
  str_35 <- "Ich.kommuniziere.ausschlie.lich.mit.einem.menschlichen.Mitarbeiter."
  statement_35 <- as.numeric(data_extended[[str_35]])
  print_mean(statement_35)
  highest_possible_score <- highest_possible_score
  summary <- summary + statement_35
  df_h7[["statement_35"]] <<- mean(statement_35)
  
  # Ich.telefoniere.mit.einem.menschlichen.Mitarbeiter..Dieser.nutzt.den.Assistenten.optional..indem.er.mit.diesem.per.Chat.kommuniziert..
  str_36 <- "Ich.telefoniere.mit.einem.menschlichen.Mitarbeiter..Dieser.nutzt.den.Assistenten.optional..indem.er.mit.diesem.per.Chat.kommuniziert."
  df_h7[["statement_36"]] <<- mean(as.numeric(data_extended[[str_36]]))
  statement_36 <- round(as.numeric(data_extended[[str_36]]) * 0.9)
  print_mean(statement_36)
  highest_possible_score <- highest_possible_score + 9
  summary <- summary + statement_36
  
  # Ich.telefoniere.mit.einem.Menschen..der.virtuelle.Assistent.h.rt.das.Gespr.ch.mit.und.schl.gt.dem.Mitarbeiter.mehrere.Ergebnisse.vor..Der.Mitarbeiter.w.hlt.das.am.besten.Passende.nach.Gef.hl.Erfah...
  str_37 <- "Ich.telefoniere.mit.einem.Menschen..der.virtuelle.Assistent.h.rt.das.Gespr.ch.mit.und.schl.gt.dem.Mitarbeiter.mehrere.Ergebnisse.vor..Der.Mitarbeiter.w.hlt.das.am.besten.Passende.nach.Gef.hl.Erfah..."
  df_h7[["statement_37"]] <<- mean(as.numeric(data_extended[[str_37]]))
  statement_37 <- round(as.numeric(data_extended[[str_37]]) * 0.7)
  print_mean(statement_37)
  highest_possible_score <- highest_possible_score + 7
  summary <- summary + statement_37
  
  # Ich.telefoniere.mit.einem.virtuellen.Assistenten..dieser.erarbeitet.die.L.sung.weitgehend.selbstst.ndig..ein.Mensch.wird.lediglich.zur.abschlie.enden.Kontrolle.des.L.sungsvorschlags.eingesetzt.
  str_38 <- "Ich.telefoniere.mit.einem.virtuellen.Assistenten..dieser.erarbeitet.die.L.sung.weitgehend.selbstst.ndig..ein.Mensch.wird.lediglich.zur.abschlie.enden.Kontrolle.des.L.sungsvorschlags.eingesetzt."
  df_h7[["statement_38"]] <<- mean(as.numeric(data_extended[[str_38]]))
  statement_38 <- round(as.numeric(data_extended[[str_38]]) * 0.5)
  print_mean(statement_38)
  highest_possible_score <- highest_possible_score + 5
  summary <- summary + statement_38
  
  # Ich.chatte.direkt.mit.einem.virtuellen.Assistenten.und.dieser.gibt.mir.selbstst.ndig.L.sungsvorschl.ge.
  # Since the better the score is in this column, the more AI-driven solutions are suppoerted, we have to invert the score
  str_40 <- "Ich.chatte.direkt.mit.einem.virtuellen.Assistenten.und.dieser.gibt.mir.selbstst.ndig.L.sungsvorschl.ge."
  data_extended[[str_40]] <- 10 - as.numeric(data_extended[[str_40]])
  statement_40 <- as.numeric(data_extended[[str_40]])
  print_mean(statement_40)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + statement_40
  df_h7[["statement_40"]] <<- mean(as.numeric(statement_40))
  
  # Ich.telefoniere.direkt.mit.einem.virtuellen.Assistenten.und.er.gibt.mir.selbstst.ndig.L.sungsvorschl.ge.
  # Since the better the score is in this column, the more AI-driven solutions are suppoerted, we have to invert the score, we also then have to nerve the score, since it proves that people even trust the auditive capacities very much
  str_39 <- "Ich.telefoniere.direkt.mit.einem.virtuellen.Assistenten.und.er.gibt.mir.selbstst.ndig.L.sungsvorschl.ge."
  data_extended[[str_39]] <- 10 - as.numeric(data_extended[[str_39]])
  df_h7[["statement_39"]] <<- mean(as.numeric(data_extended[[str_39]]))
  statement_39 <- round(as.numeric(data_extended[[str_39]]) * 0.8)
  print_mean(statement_39)
  highest_possible_score <- highest_possible_score + 8
  summary <- summary + statement_39
  
  # In.diesen.Aufgaben.vertraue.ich.virtuellen.Assistenten..Mehrfache.Auswahl.m.glich.
  col_name <- "In.diesen.Aufgaben.vertraue.ich.virtuellen.Assistenten..Mehrfache.Auswahl.m.glich."
  data_extended[[col_name]] <- sapply(data_extended[[col_name]], multiple_choice_outcome) * 10/6
  mult <- data_extended[[col_name]]
  print_mean(mult)
  highest_possible_score <- highest_possible_score + 10
  summary <- summary + mult
  df_h7[["mult"]] <<- mean(mult)
  
  # In.welchen.Situationen.wollen.Sie.vor.dem.Gespr.ch.dar.ber.informiert.werden..dass.ein.virtueller.Assistent.bei.der.Beratung..per.Telefon.oder.Chat..eingesetzt.wird...Mehrfachauswahl.m.glich.
  col_name2 <- "In.welchen.Situationen.wollen.Sie.vor.dem.Gespr.ch.dar.ber.informiert.werden..dass.ein.virtueller.Assistent.bei.der.Beratung..per.Telefon.oder.Chat..eingesetzt.wird...Mehrfachauswahl.m.glich."
  data_extended[[col_name2]] <- 6 - sapply(data_extended[[col_name2]], multiple_choice_outcome)
  mult2 <- table(data_extended[[col_name2]])
  df_19 <<- data.frame(mult2)
  
  # Ich.habe.au.erdem.folgende.Bedenken.bei.Virtuellen.Assistenten.im.IT.Support
  
  df_opt_answ <<- data.frame(answers=c(data_extended[["Ich.habe.au.erdem.folgende.Bedenken.bei.Virtuellen.Assistenten.im.IT.Support"]]))
  
  df_human_machine <<- data.frame(human_approach=c(df_h7[["statement_35"]]),
                                  human_machine=c(mean(df_h7[["statement_36"]], df_h7[["statement_37"]], df_h7[["statement_38"]])),
                                  machine=c(mean(df_h7[["statement_39"]], mean(df_h7[["statement_40"]])))
                                  )

  
  highest_possible_score_h7 <<- highest_possible_score
  # summary[length(summary) + 1] <- highest_possible_score
  data_extended[["summary_of_h7"]] <- summary
  data_extended[["score_h7"]] <- add_h_column(summary, highest_possible_score)
  return(data_extended)}

