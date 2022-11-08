#' Import DIVI ICU admission data on federal state level
#'
#' @param federal_state "A string vector specifying the federal state you are interested in. Use "all" if you want to obtain data for all 16 federal states. 
#' @param save_data_set "TRUE/FAlSE"
#' @param save_plot "TRUE/FALSE"
#'
#' @import readr
#' @import dplyr
#' @import lubridate
#' @import rlang
#' @import ggplot2
#'
#' @return A list: 1 data frame containing the reported ICU admissions from end of July 2021 onwards, 2 admissions plot over time. 
#' @export
#'
#' @examples
#' Berlin <- divi_admissions_federal_state("Berlin", FALSE, FALSE)

divi_admissions_federal_state <- function(federal_state, save_dataset, save_plot){
    federal_stateOptions <- c("all", paste("Baden-W","\\u00fc","rttemberg"), "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", paste("Th","\\u00fc","ringen"))
    yes_no <- c(TRUE, FALSE)
    if(!is.element(federal_state, federal_stateOptions)) stop("Please enter a valid federal state or \"all\" if you want to see a comparison plot of the 16 federal states.")
    if(!is.element(save_dataset, yes_no)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created data set as a csv file.")
    if(!is.element(save_plot, yes_no)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created plot.")

    #From July 2021 DIVI is providing ICU admission data on federal state level
    #The authors want to thank DIVI for providing the upcoming data, which is publicly available on https://www.intensivregister.de/#/aktuelle-lage/downloads
    divi_data_federal_state <- readr::read_csv("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-bundeslaender.csv", na="null")
    # Following lines : Data cleaning and data preparation
    colnames(divi_data_federal_state)[1] <- "Date"
    divi_data_federal_state$Date <- as.Date(divi_data_federal_state$Date)
    divi_data_federal_state <- divi_data_federal_state %>% dplyr::mutate(Bundesland = dplyr::case_when(Bundesland == "SCHLESWIG_HOLSTEIN" ~ "Schleswig-Holstein",
                                                                            Bundesland == "HAMBURG" ~ "Hamburg",
                                                                            Bundesland == "NIEDERSACHSEN" ~ "Niedersachsen",
                                                                            Bundesland == "BREMEN" ~ "Bremen", 
                                                                            Bundesland == "NORDRHEIN_WESTFALEN" ~ "Nordrhein-Westfalen",
                                                                            Bundesland == "HESSEN" ~ "Hessen",
                                                                            Bundesland == "RHEINLAND_PFALZ" ~ "Rheinland-Pfalz",
                                                                            Bundesland == "BADEN_WUERTTEMBERG" ~ "Baden-Württemberg",
                                                                            Bundesland == "BAYERN" ~ "Bayern",
                                                                            Bundesland == "SAARLAND" ~ "Saarland",
                                                                            Bundesland == "BERLIN" ~ "Berlin",
                                                                            Bundesland == "BRANDENBURG" ~ "Brandenburg",
                                                                            Bundesland == "MECKLENBURG_VORPOMMERN" ~ "Mecklenburg-Vorpommern",
                                                                            Bundesland == "SACHSEN" ~ "Sachsen",
                                                                            Bundesland == "SACHSEN_ANHALT" ~ "Sachsen-Anhalt",
                                                                            Bundesland == "THUERINGEN" ~ "Thüringen"))
    divi_data_federal_state <- divi_data_federal_state %>%
    dplyr::select("Date", "Bundesland", "faelle_covid_erstaufnahmen") %>%
    dplyr::mutate(week = lubridate::isoweek("Date")) %>%
    dplyr::mutate(year = lubridate::year("Date")) %>%
    filter("Date" > "2021-07-28")
    divi_data_federal_state$faelle_covid_erstaufnahmen <- as.double(divi_data_federal_state$faelle_covid_erstaufnahmen)

    # We are computing weekly averages to make the reported admissions more stable
    divi_data_federal_stateWeeklyAvg <- divi_data_federal_state %>% dplyr::group_by("Bundesland", "week", "year") %>%
    dplyr::summarise(mean_erstaufnahmen = mean("faelle_covid_erstaufnahmen", na.rm = TRUE), Date = max("Date"))

    # Visualization of the created time series
    if (federal_state == "all") {
        plot <- ggplot2::ggplot(data = divi_data_federal_stateWeeklyAvg) +
        ggplot2::geom_line(ggplot2::aes(x = "Date", y = "mean_erstaufnahmen", col = "Bundesland"), size = 1.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom") +
        ggplot2::xlab("Datum") +
        ggplot2::facet_wrap(~ Bundesland) +
        ggplot2::scale_x_date(date_labels = "%m/%y", date_breaks = "6 months") +
        ggplot2::ylab("Anzahl gemeldeter ITS-Erstaufnahmen von COVID-19-Fällen / Woche")
    } else {
        divi_data_federal_stateWeeklyAvg <- divi_data_federal_stateWeeklyAvg %>%
        filter(Bundesland == federal_state)
        plot <- ggplot2::ggplot(data = divi_data_federal_stateWeeklyAvg) +
        ggplot2::geom_line(ggplot2::aes(x = "Date", y = "mean_erstaufnahmen"), size = 1.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom") +
        ggplot2::xlab("Datum") +
        ggplot2::scale_x_date(date_labels = "%m/%y", date_breaks = "6 months") +
        ggplot2::ylab("Anzahl gemeldeter ITS-Erstaufnahmen von COVID-19-Fällen / Woche")
    }
    if (save_dataset == TRUE) {
        readr::write_csv(divi_data_federal_stateWeeklyAvg, paste("divi_admission_", federal_state, ".csv"))
    }

    if (save_plot == TRUE) {
        ggplot2::ggsave(paste("divi_admission_", federal_state, ".png"), width = 12.5, height = 6, dpi = 300)
    }

    return(list(divi_data_federal_stateWeeklyAvg = divi_data_federal_stateWeeklyAvg, plot = plot))
}