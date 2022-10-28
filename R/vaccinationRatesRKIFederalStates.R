#Impfungen Bundesländer
library(tidyverse)
library(lubridate)

rkiVaccinationRateFedStateFunc <- function(saveDataSet, savePlots){

  #Creating some warning messages for incorrect input 
  yesNo <- c("yes", "no")
  if(!is.element(saveDataSet, yesNo)) stop("Please enter either \"yes\" or \"no\" to decide whether or not you would like to save the created data set as a csv file.")
  if(!is.element(savePlots, yesNo)) stop("Please enter either \"yes\" or \"no\" to decide whether or not you would like to save the created plot.")
    
  #Reading in vaccination data from RKI
  rohdatenRKI <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv")
  
  #Setting up a dataframe containing the abbreviations of the federal states, their id as well as their residents
  #resident numbers come from https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-nichtdeutsch-laender.html
  dictStateId <- data.frame(matrix(nrow=0, ncol=3))
  colnames(dictStateId) <- c("Bundesland", "ID", "Einwohner:innen")
  dictStateId[nrow(dictStateId)+1,] <- c("SH", "01", 2922005)
  dictStateId[nrow(dictStateId)+1,] <- c("HH", "02", 1853935)
  dictStateId[nrow(dictStateId)+1,] <- c("NI", "03", 8027031)
  dictStateId[nrow(dictStateId)+1,] <- c("HB", "04", 676463)
  dictStateId[nrow(dictStateId)+1,] <- c("NW", "05", 17924591)
  dictStateId[nrow(dictStateId)+1,] <- c("HE", "06", 6295017)
  dictStateId[nrow(dictStateId)+1,] <- c("RP", "07", 4106485)
  dictStateId[nrow(dictStateId)+1,] <- c("BW", "08", 11124642)
  dictStateId[nrow(dictStateId)+1,] <- c("BY", "09", 13176989)
  dictStateId[nrow(dictStateId)+1,] <- c("SL", "10", 982348)
  dictStateId[nrow(dictStateId)+1,] <- c("BE", "11", 3677472)
  dictStateId[nrow(dictStateId)+1,] <- c("BB", "12", 2537868)
  dictStateId[nrow(dictStateId)+1,] <- c("MV", "13", 1611160)
  dictStateId[nrow(dictStateId)+1,] <- c("SN", "14", 4043002)
  dictStateId[nrow(dictStateId)+1,] <- c("ST", "15", 2169253)
  dictStateId[nrow(dictStateId)+1,] <- c("TH", "16", 2108863)
  dictStateId$`Einwohner:innen` <- as.double(dictStateId$`Einwohner:innen`)
  
  vaccinationRatesFederalStates <- data.frame(matrix(nrow=0, ncol=6))
  
  #We're iterating over the federal states to compute the different vaccination rates
  for(id in unique(dictStateId$ID)){
    filtered <- filter(rohdatenRKI, BundeslandId_Impfort == id) 
    
    Erstimpfungen <- filter(filtered, Impfserie == 1)
    Booster <- filter(filtered, Impfserie ==3)
    
    vaccinationRatesFederalStates <- rbind(vaccinationRatesFederalStates, c(id, dictStateId[which(dictStateId$ID == id),1], sum(Erstimpfungen$Anzahl), sum(Erstimpfungen$Anzahl)/dictStateId[which(dictStateId$ID == id),3], sum(Booster$Anzahl), sum(Booster$Anzahl)/dictStateId[which(dictStateId$ID == id),3]))
  }
  
  colnames(vaccinationRatesFederalStates) <- c("ID", "Bundesland", "Quantity(Erstimpfungen)", "Rate(Erstimpfungen)", "Quantity(Booster)", "Rate(Booster)")
  
  #R turns also columns into character-columns, so we're converging them to doubles and integers
  vaccinationRatesFederalStates$`Rate(Erstimpfungen)` <- as.double(vaccinationRatesFederalStates$`Rate(Erstimpfungen)`)
  vaccinationRatesFederalStates$`Quantity(Erstimpfungen)` <- as.integer(vaccinationRatesFederalStates$`Quantity(Erstimpfungen)`)
  vaccinationRatesFederalStates$`Rate(Booster)` <- as.double(vaccinationRatesFederalStates$`Rate(Booster)`)
  vaccinationRatesFederalStates$`Quantity(Booster)` <- as.integer(vaccinationRatesFederalStates$`Quantity(Booster)`)
  vaccinationRatesFederalStates <<- vaccinationRatesFederalStates
  
  #Plot of Erstimpfungen
  firstVaccinationPlot <- ggplot(data=vaccinationRatesFederalStates)+
    geom_bar(mapping=aes(x=Bundesland,y=`Rate(Erstimpfungen)`), fill="#CC79A7", stat="identity")+
    ggtitle(paste("Impfquote (Erstimpfungen) nach Bundesländern (Stand:", as.character(max(rohdatenRKI$Impfdatum)), ")"))+
    scale_y_continuous(labels = scales::comma)+
    xlab("Bundeslaender")+
    ylab("Quote")+
    theme_minimal()
  
  if(savePlots == "yes"){
    ggsave("firstVaccinationRatesFedStates.png", width = 12.5, height = 6, dpi = 300)
  }
  
  #Plot of Booster
  boosterPlot <- ggplot(data=vaccinationRatesFederalStates)+
    geom_bar(mapping=aes(x=Bundesland,y=`Rate(Booster)`), fill="#E69F00", stat="identity")+
    ggtitle(paste("Impfquote (Booster) nach Bundesländern (Stand:", as.character(max(rohdatenRKI$Impfdatum)), ")"))+
    scale_y_continuous(labels = scales::comma)+
    xlab("Bundeslaender")+
    ylab("Quote")+
    theme_minimal()
  
  if(savePlots == "yes"){
    ggsave("boosterRatesFedStates.png", width = 12.5, height = 6, dpi = 300)
  }
  
  return(list(firstVaccinationPlot,boosterPlot))
}
