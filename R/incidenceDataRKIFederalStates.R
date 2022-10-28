#' Import RKI incidence data on federal state level
#'
#' @param federal_state "A string vector specifying the federal state you are interested in. Use "all" if you want to obtain data for all 16 federal states. 
#' @param save_data_set "TRUE/FAlSE"
#' @param save_plot "TRUE/FALSE"
#'
#' @import ggplot2
#' @import dplyr
#' @import readr
#' @import httr
#' @import readxl
#' @import tidyr
#'
#' @return A list: 1 data frame containing the reported incidence from May 2020 onwards, 2 If federal_state == "all", you may find a facet plot of the incidences over time here, else : empty; 3 Incidence plot over time # nolint # nolint
#' @export
#'
#' @examples
#' Berlin <- rki_incidence_federal_state("Berlin", FALSE, FALSE)

rki_incidence_federal_state <- function(federal_state, save_data_set, save_plot){

#Creating some error messages
federal_stateOptions <- c("all", paste("Baden-W","\\u00fc","rttemberg"), "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", paste("Th","\\u00fc","ringen"))
yesNo <- c(TRUE, FALSE)
if(!is.element(federal_state, federal_stateOptions)) stop("Please enter a valid federal state.")
if(!is.element(save_data_set, yesNo)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created data set as a csv file.")
if(!is.element(save_plot, yesNo)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created plot.")

#Reading incidence data (until 09/10/2021) in
url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_Archiv.xlsx?__blob=publicationFile'
httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
incidence_data_archiv <- readxl::read_excel(tf, 3)
colnames(incidence_data_archiv) <- incidence_data_archiv[4, ]
colnames(incidence_data_archiv)[1] <- "MeldeLandkreisBundesland"
incidence_data_archiv <- incidence_data_archiv[-(1:4), ]
incidence_data_archiv <- tidyr::pivot_longer(incidence_data_archiv, names_to="Date", values_to="Incidence", cols=colnames(incidence_data_archiv)[2:ncol(incidence_data_archiv)])
incidence_data_archiv$Date <- as.integer(incidence_data_archiv$Date)
incidence_data_archiv$Date <- as.Date(incidence_data_archiv$Date,origin="1899-12-30")

#Reading incidence data (from 09/11/2021 onwards) in
url1 <- 'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
incidence_data <- readxl::read_excel(tf, 4)
colnames(incidence_data) <- incidence_data[4, ]
incidence_data <- incidence_data[-(1:4), ]
incidence_data <- tidyr::pivot_longer(incidence_data, names_to="Date", values_to="Incidence", cols=colnames(incidence_data)[2:ncol(incidence_data)])
incidence_data$Incidence <- as.double(incidence_data$Incidence)
incidence_data$Date <- paste(substr(incidence_data$Date, start = 7, stop = 10), substr(incidence_data$Date, start = 4, stop = 5), as.character(substr(incidence_data$Date, start = 1, stop = 2)), sep ="-" )
incidence_data$Date <- as.Date(incidence_data$Date)

incidence_data <- rbind(incidence_data_archiv, incidence_data)

if (save_data_set == TRUE) {
  readr::write_csv(incidence_data, paste("rkiincidence_data", federal_state, ".csv"))
}

if (federal_state == "all") {
  facetedPlot <- ggplot2::ggplot(data=incidence_data, mapping = ggplot2::aes (x="Date", y ="Incidence")) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~ MeldeLandkreisBundesland) +
    ggplot2::scale_x_date(date_labels = "%m/%y", date_breaks = "9 months") +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 6))
  if (save_plot == "yes") {
    ggplot2::ggsave("rkiIncidencefederal_statesFaceted.png", width = 12.5, height = 6, dpi = 300)
  }

  time_series_plot <- ggplot2::ggplot(data=incidence_data, mapping = ggplot2::aes (x="Date", y ="Incidence", col = "MeldeLandkreisBundesland")) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", text = ggplot2::element_text(size = 15)) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_date(date_labels = "%m/%y", date_breaks = "4 months") +
    ggplot2::theme(legend.text = element_text(size = 7))
  if (save_plot == TRUE) {
   ggplot2:: ggsave("rkiIncidencefederal_states.png", width = 12.5, height = 6, dpi = 300)
  }
  return(list(incidence_data = incidence_data,faceted_plot = facetedPlot,time_series_plot = time_series_plot))
} else {
  incidence_data <- dplyr::filter(incidence_data, "MeldeLandkreisBundesland" == federal_state)
  time_series_plot <- ggplot2::ggplot(data=incidence_data, mapping = ggplot2::aes (x="Date", y ="Incidence")) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", text = ggplot2::element_text(size = 15)) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_date(date_labels = "%m/%y", date_breaks = "4 months") +
    ggplot2::theme(legend.text = element_text(size = 7))
  if (save_plot == TRUE) {
    ggplot2::ggsave(paste("rkiIncidence", federal_state, ".png"), width = 12.5, height = 6, dpi = 300)
  }
  print("")
  return(list(incidence_data = incidence_data,faceted_plot = NULL, time_series_plot = time_series_plot))
}
}
