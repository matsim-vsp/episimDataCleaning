#' Import RKI incidence data on federal state level
#' 
#' @param federalState "A string vector specifying the federal state you are interested in. Use "all" if you want to obtain data for all 16 federal states. 
#' @param saveDataSet "TRUE/FAlSE"
#' @param savePlot "TRUE/FALSE"
#' 
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import tibble
#' @import stringr
#' @import forcats
#' @import lubridate
#' @import xlsx
#' @import httr
#' @import readxl
#' 
#' @return A list: 1 data frame containing the reported incidence from May 2020 onwards, 2 If federalState == "all", you may find a facet plot of the incidences over time here, else : empty; 3 Incidence plot over time
#' @export 
#' 
#' @examples 
#' Berlin <- rkiIncidenceFederalState("Berlin", FALSE, FALSE)

rkiIncidenceFederalState <- function(federalState, saveDataSet, savePlot){

#Creating some error messages
federalStateOptions <- c("all", paste("Baden-W","\\u00fc","rttemberg"), "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", paste("Th","\\u00fc","ringen"))
yesNo <- c(TRUE, FALSE)
if(!is.element(federalState, federalStateOptions)) stop("Please enter a valid federal state.")
if(!is.element(saveDataSet, yesNo)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created data set as a csv file.")
if(!is.element(savePlot, yesNo)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created plot.")
 
#Reading incidence data (until 09/10/2021) in
url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_Archiv.xlsx?__blob=publicationFile'
httr::GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
IncidenceDataArchiv <- readxl::read_excel(tf, 3)
colnames(IncidenceDataArchiv) <- IncidenceDataArchiv[4,]
colnames(IncidenceDataArchiv)[1] <- "MeldeLandkreisBundesland"
IncidenceDataArchiv <- IncidenceDataArchiv[-(1:4),]
IncidenceDataArchiv <- pivot_longer(IncidenceDataArchiv, names_to="Date", values_to="Incidence", cols=colnames(IncidenceDataArchiv)[2:ncol(IncidenceDataArchiv)])
IncidenceDataArchiv$Date <- as.integer(IncidenceDataArchiv$Date)
IncidenceDataArchiv$Date <- as.Date(IncidenceDataArchiv$Date,origin="1899-12-30")

#Reading incidence data (from 09/11/2021 onwards) in
url1<-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
httr::GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
IncidenceData <- readxl::read_excel(tf, 4)
colnames(IncidenceData) <- IncidenceData[4,]
IncidenceData <- IncidenceData[-(1:4),]
IncidenceData <- pivot_longer(IncidenceData, names_to="Date", values_to="Incidence", cols=colnames(IncidenceData)[2:ncol(IncidenceData)])
IncidenceData$Incidence <- as.double(IncidenceData$Incidence)
IncidenceData$Date <- paste(substr(IncidenceData$Date, start = 7, stop = 10), substr(IncidenceData$Date, start = 4, stop = 5), as.character(substr(IncidenceData$Date, start = 1, stop = 2)), sep ="-" )
IncidenceData$Date <- as.Date(IncidenceData$Date)

IncidenceData <- rbind(IncidenceDataArchiv,IncidenceData)

if(saveDataSet == TRUE){
  readr::write_csv(IncidenceData, paste("rkiIncidenceData", federalState, ".csv"))
}

if(federalState == "all"){
  facetedPlot <- ggplot2::ggplot(data=IncidenceData, mapping = aes (x=Date, y =Incidence)) +
    ggplot2::geom_line() + 
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~ MeldeLandkreisBundesland) + 
    ggplot2::scale_x_date(date_labels = "%m/%y",date_breaks="9 months")+
    ggplot2::theme(axis.text=element_text(size=6))
  if(savePlot == "yes"){
    ggplot2::ggsave("rkiIncidenceFederalStatesFaceted.png", width = 12.5, height = 6, dpi = 300)
  }

  timeSeriesPlot <- ggplot2::ggplot(data=IncidenceData, mapping = aes (x=Date, y =Incidence, col = MeldeLandkreisBundesland)) +
    ggplot2::geom_line() + 
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", text=element_text(size=15))+
    ggplot2::theme(legend.title=element_blank())+
    ggplot2::scale_x_date(date_labels = "%m/%y",date_breaks="4 months")+
    ggplot2::theme(legend.text=element_text(size=7))
  if(savePlot == TRUE){
   ggplot2:: ggsave("rkiIncidenceFederalStates.png", width = 12.5, height = 6, dpi = 300)
  }
  return(list(incidence_data = IncidenceData,faceted_plot = facetedPlot,time_series_plot = timeSeriesPlot))
} else {
  IncidenceData <- dplyr::filter(IncidenceData, MeldeLandkreisBundesland == federalState)
  timeSeriesPlot <- ggplot2::ggplot(data=IncidenceData, mapping = aes (x=Date, y =Incidence)) +
    ggplot2::geom_line() + 
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", text=element_text(size=15))+
    ggplot2::theme(legend.title=element_blank())+
    ggplot2::scale_x_date(date_labels = "%m/%y",date_breaks="4 months")+
    ggplot2::theme(legend.text=element_text(size=7))
  if(savePlot == TRUE){
    ggplot2::ggsave(paste("rkiIncidence", federalState, ".png"), width = 12.5, height = 6, dpi = 300)
  }
  print("")
  return(list(incidence_data = IncidenceData,faceted_plot = NULL, time_series_plot = timeSeriesPlot))
}
}


