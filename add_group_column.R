source("helper_functions.R")
# this file prepares the data, by adding new columns and further removing unreadable characters

data <- read.csv("outcome.CSV", sep = ";", header = TRUE)

# get distribution of digital openness among the participants
dig_op_prefilter <- "Ich.bin.in.einem.IT.Berufsfeld.t.tig."
dig_op_1 <- data$`Ich.nutze.innerhalb.meines.Bekanntenkreis.oft.als.erste.Person.neue.technische.oder.digitale.Produkte.`
dig_op_2 <- data$`Ich.informiere.mich.regelm..ig..ber.neueste.Entwicklungen.im.Software..und.Hardwarebereich.`
dig_op_avg_data <- (dig_op_1 + dig_op_2) / 2

data[[dig_op_prefilter]] <- change_to_bool_values(dig_op_prefilter)

categorize <- function(value){
  if(value < 4){
    return("laggard")
  }else if(value >= 8){
    return("early adopter")
  }else{
    return("majority")
  }
}

categories <- sapply(dig_op_avg_data, categorize)

data_extended <- data.frame(data, "openess.towards.new.technologies" = categories)
categories = "openess.towards.new.technologies"

# selected_columns <- data_extended[, c(dig_op_prefilter, "categories")]
data_extended[[categories]][data_extended[[dig_op_prefilter]]] <- "early adopter"

string_counts <- table(data_extended[[categories]])
string_counts2 <- table(data_extended[[dig_op_prefilter]])

print(string_counts)
print(string_counts2)

# get distribution of ai openness among the participants
ai_prefilter <- "Ich.habe.bereits.Large.Language.Models.eingesetzt..wie.z.B..Chat.GPT."
ai_1 <- data$`Ich.habe.ein.generelles.Wissen..ber.die.Funktionsweise.von.KI.Modellen.`
ai_2 <- data$`Ich.kenne.mich.insbesondere.mit.der.Funktionsweise.von.Generative.AI.aus.`
ai_avg_data <- (ai_1 + ai_2) / 2

data_extended[[ai_prefilter]] <- change_to_bool_values(ai_prefilter)

categorize_ai <- function(value){
  if(value < 3){
    return("unexperienced")
  }else if(value >= 6){
    return("experienced")
  }else{
    return("rather experienced")
  }
}

# extend datatable by the group the participant is belonging to
categories_ai <- sapply(dig_op_avg_data, categorize_ai)

data_extended <- data.frame(data_extended, "preknowledge.about.ai" = categories_ai)
categories_ai = "preknowledge.about.ai"

selected_columns <- data_extended[, c(ai_prefilter, categories_ai)]
string_counts_before_filter <- table(data_extended[[categories_ai]])
print(string_counts_before_filter)
selected_columns[[categories_ai]] <- ifelse(selected_columns[[ai_prefilter]], selected_columns[[categories_ai]], "unexperienced")

# get the count of the group members
string_counts <- table(data_extended[[categories_ai]])
string_counts2 <- table(data_extended[[ai_prefilter]])

print(string_counts)
print(string_counts2)


