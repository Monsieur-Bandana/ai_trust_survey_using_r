data <- read.csv("outcome.CSV", sep = ";", header = TRUE)

# get distribution of digital openness among the participants
dig_op_1 <- data$`Ich.nutze.innerhalb.meines.Bekanntenkreis.oft.als.erste.Person.neue.technische.oder.digitale.Produkte.`
dig_op_2 <- data$`Ich.informiere.mich.regelm..ig..ber.neueste.Entwicklungen.im.Software..und.Hardwarebereich.`
dig_op_avg_data <- (dig_op_1 + dig_op_2) / 2
dig_op_groups <- cut(dig_op_avg_data, breaks = c(-Inf, 4, 8, Inf), labels = c("laggards", "majority", "early adopters"), right = FALSE)
dig_op_group_counts <- table(dig_op_groups)
dig_op_group_table <- as.data.frame(dig_op_group_counts)


# get distribution of AI openness among the participants
ai_1 <- data$`Ich.habe.ein.generelles.Wissen..ber.die.Funktionsweise.von.KI.Modellen.`
ai_2 <- data$`Ich.kenne.mich.insbesondere.mit.der.Funktionsweise.von.Generative.AI.aus.`
ai_avg_data <- (ai_1 + ai_2) / 2
ai_groups <- cut(ai_avg_data, breaks = c(-Inf, 3, 6, Inf), labels = c("unexperienced", "rather", "experienced"), right = FALSE)
ai_group_counts <- table(ai_groups)
ai_group_table <- as.data.frame(ai_group_counts)


