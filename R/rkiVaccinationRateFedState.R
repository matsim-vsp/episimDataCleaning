#' Vaccination rates on federal state level
#'
#' @param save_data_set "TRUE/FAlSE"
#' @param save_plots "TRUE/FALSE"
#'
#' @import ggplot2
#' @import dplyr
#' @import readr
#' @import scales
#'
#' @return A list: 1 data frame containing the reported incidence from May 2020 onwards, 2 If federalState == "all", you may find a facet plot of the incidences over time here, else : empty; 3 Incidence plot over time # nolint
#' @export
#'
#' @examples
#' vaccinationRate <- rki_vaccination_rate_fed_state(FALSE, FALSE)


rki_vaccination_rate_fed_state <- function(save_data_set, save_plots) {

  #Creating some warning messages for incorrect input
  true_false <- c(TRUE, FALSE)
  if(!is.element(save_data_set, true_false)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created data set as a csv file.")
  if(!is.element(save_plots, true_false)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created plot.")

  #Reading in vaccination data from RKI
  rohdatenRKI <- readr::read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv")

  #Setting up a dataframe containing the abbreviations of the federal states, their id as well as their residents
  #resident numbers come from https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-nichtdeutsch-laender.html
  dict_state_id <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(dict_state_id) <- c("Bundesland", "ID", "Einwohner:innen")
  dict_state_id[nrow(dict_state_id)+1, ] <- c("SH", "01", 2922005)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("HH", "02", 1853935)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("NI", "03", 8027031)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("HB", "04", 676463)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("NW", "05", 17924591)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("HE", "06", 6295017)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("RP", "07", 4106485)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("BW", "08", 11124642)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("BY", "09", 13176989)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("SL", "10", 982348)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("BE", "11", 3677472)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("BB", "12", 2537868)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("MV", "13", 1611160)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("SN", "14", 4043002)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("ST", "15", 2169253)
  dict_state_id[nrow(dict_state_id)+1, ] <- c("TH", "16", 2108863)
  dict_state_id$`Einwohner:innen` <- as.double(dict_state_id$`Einwohner:innen`)

  vaccinationRatesFederalStates <- data.frame(matrix(nrow = 0, ncol = 6))

  #We're iterating over the federal states to compute the different vaccination rates
  for (id in unique(dict_state_id$ID)){
    filtered <- dplyr::filter(rohdatenRKI, "BundeslandId_Impfort" == id)

    Erstimpfungen <- dplyr::filter(filtered, "Impfserie" == 1)
    Booster <- dplyr::filter(filtered, "Impfserie" == 3)

    vaccinationRatesFederalStates <- rbind(vaccinationRatesFederalStates, c(id, dict_state_id[which(dict_state_id$ID == id),1], sum(Erstimpfungen$Anzahl), sum(Erstimpfungen$Anzahl)/dict_state_id[which(dict_state_id$ID == id),3], sum(Booster$Anzahl), sum(Booster$Anzahl)/dict_state_id[which(dict_state_id$ID == id),3]))
  }

  colnames(vaccinationRatesFederalStates) <- c("ID", "Bundesland", "Quantity(Erstimpfungen)", "Rate(Erstimpfungen)", "Quantity(Booster)", "Rate(Booster)")

  #R turns also columns into character-columns, so we're converging them to doubles and integers
  vaccinationRatesFederalStates$`Rate(Erstimpfungen)` <- as.double(vaccinationRatesFederalStates$`Rate(Erstimpfungen)`)
  vaccinationRatesFederalStates$`Quantity(Erstimpfungen)` <- as.integer(vaccinationRatesFederalStates$`Quantity(Erstimpfungen)`)
  vaccinationRatesFederalStates$`Rate(Booster)` <- as.double(vaccinationRatesFederalStates$`Rate(Booster)`)
  vaccinationRatesFederalStates$`Quantity(Booster)` <- as.integer(vaccinationRatesFederalStates$`Quantity(Booster)`)

  #Plot of Erstimpfungen
  first_vaccination_plot<- ggplot2::ggplot(data = vaccinationRatesFederalStates)+
    ggplot2::geom_bar(mapping = ggplot2::aes(x = "Bundesland", y= "Rate(Erstimpfungen)"), fill="#CC79A7", stat="identity")+
    ggplot2::ggtitle(paste("Impfquote (Erstimpfungen) nach Bundeslaendern (Stand:", as.character(max(rohdatenRKI$Impfdatum)), ")")) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::xlab("Bundeslaender") +
    ggplot2::ylab("Quote") +
    ggplot2::theme_minimal()

  if (save_plots == TRUE) {
    ggplot2::ggsave("firstVaccinationRatesFedStates.png", width = 12.5, height = 6, dpi = 300)
  }

  #Plot of Booster
  booster_plot <- ggplot2::ggplot(data = vaccinationRatesFederalStates) +
    ggplot2::geom_bar(mapping = ggplot2::aes(x = "Bundesland", y = "Rate(Booster)"), fill="#E69F00", stat = "identity")+
    ggplot2::ggtitle(paste("Impfquote (Booster) nach Bundeslaendern (Stand:", as.character(max(rohdatenRKI$Impfdatum)), ")"))+
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::xlab("Bundeslaender") +
    ggplot2::ylab("Quote") +
    ggplot2::theme_minimal()

  if (save_plots == TRUE) {
    ggplot2::ggsave("boosterRatesFedStates.png", width = 12.5, height = 6, dpi = 300)
  }
  return(list(vaccination_rates = vaccinationRatesFederalStates,first_vaccination_plot = first_vaccination_plot, booster_plot = booster_plot))
}
