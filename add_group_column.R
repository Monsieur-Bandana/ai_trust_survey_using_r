data <- read.csv("outcome.CSV", sep = ";", header = TRUE)

# get distribution of digital openness among the participants
dig_op_1 <- data$`Ich.nutze.innerhalb.meines.Bekanntenkreis.oft.als.erste.Person.neue.technische.oder.digitale.Produkte.`
dig_op_2 <- data$`Ich.informiere.mich.regelm..ig..ber.neueste.Entwicklungen.im.Software..und.Hardwarebereich.`
dig_op_avg_data <- (dig_op_1 + dig_op_2) / 2

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

data_extended <- data.frame(data, categories)
