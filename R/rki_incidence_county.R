#' Incidence over time on county level
#'
#' @param county "county or "all""
#' @param save_data_set "TRUE/FALSE"
#' @param save_plot "TRUE/FALSE, TRUE only possible if county != all"
#'
#' @import httr
#' @import readxl
#' @import httr
#' @import stringr
#' @import tidyr
#' @import dplyr
#' @import readr

#'
#' @return A list: 1) data frame containing the incidences for the chosen county, 2) If county != "all", line plot of this incidence over time over time
#' @export
#'
#' @examples
#' Leipzig <- rki_incidence_county("Leipzig", FALSE, FALSE)

rki_incidence_county <- function(county, save_data_set, save_plot) {
    LK <- Date <- Incidence <- NULL
    yesNo <- c(TRUE, FALSE)
    if(!is.element(save_data_set, yesNo)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created data set as a csv file.")
    if(!is.element(save_plot, yesNo)) stop("Please enter either TRUE or FALSE. If you entered \"all\" as the first argument, then no plot is created.")

    #Reading incidence data in
    url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_Archiv.xlsx?__blob=publicationFile'
    httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    first_archive_incidence_data <- readxl::read_xlsx(tf, 5)
    colnames(first_archive_incidence_data) <- first_archive_incidence_data[4, ]
    if (county != "all") {
    first_archive_incidence_data <- first_archive_incidence_data %>%
    filter(stringr::str_detect(LK, county))
    } else {
    first_archive_incidence_data <- first_archive_incidence_data[-(1:4), ]
    }
    first_archive_incidence_data <- tidyr::pivot_longer(first_archive_incidence_data, names_to="Date", values_to="Incidence", cols=colnames(first_archive_incidence_data)[4:152])
    first_archive_incidence_data <- dplyr::select(first_archive_incidence_data, c("LK", "LKNR", "Date", "Incidence"))
    first_archive_incidence_data$Date <- as.Date(first_archive_incidence_data$Date, "%d.%m.%Y")


    url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_Archiv.xlsx?__blob=publicationFile'
    httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    second_archive_incidence_data <- readxl::read_xlsx(tf, 5)
    colnames(second_archive_incidence_data) <- second_archive_incidence_data[4, ]
    if (county != "all") {
    second_archive_incidence_data <- second_archive_incidence_data %>% filter(stringr::str_detect(LK, county))
    } else {
    second_archive_incidence_data <- second_archive_incidence_data[-(1:4), ]
    }
    second_archive_incidence_data <- tidyr::pivot_longer(second_archive_incidence_data, names_to="Date", values_to="Incidence", cols=colnames(second_archive_incidence_data)[153:ncol(second_archive_incidence_data)])
    second_archive_incidence_data <- dplyr::select(second_archive_incidence_data, c("LK", "LKNR", "Date", "Incidence"))
    second_archive_incidence_data$Date <- as.Date(as.integer(second_archive_incidence_data$Date), origin="1899-12-30")


    url1<-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
    httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    incidence_data <- readxl::read_xlsx(tf, 6)
    colnames(incidence_data) <- incidence_data[1, ]
    if (county != "all") {
    incidence_data <- incidence_data %>%
    filter(stringr::str_detect(LK, county))
    } else {
    incidence_data <- incidence_data[-1, ] #Removing the 1st line, as this solely contains the column names
    }
    # incidence_data <- mutate(incidence_data, BundeslandId=c("08", "09", "11", "12", "04", "02", "06", "13", "03", "05", "07", "10", "14", "15", "01", "16", "00"))
    incidence_data <- tidyr::pivot_longer(incidence_data, names_to="Date", values_to="Incidence", cols=colnames(incidence_data)[3:ncol(incidence_data)])
    incidence_data$Date <- as.Date(incidence_data$Date, "%d.%m.%Y")

    incidence_dataArchiv <- rbind(first_archive_incidence_data, second_archive_incidence_data) 
    incidence_data <- rbind(incidence_dataArchiv, incidence_data)

    incidence_data$Incidence <- as.double(incidence_data$Incidence)

    #Slightly adapting the names of Landkreise
    incidence_data$LKNR <- as.integer(incidence_data$LKNR)
    incidence_data <- dplyr::mutate(incidence_data, LK = dplyr::case_when(LK == "SK Eisenach*" ~ "Eisenach",
                                                            LK == "StadtRegion Aachen" ~ "Aachen",
                                                            LK == "Berlin" ~ "Berlin",
                                                            substr(LK, start=1, stop=2)=="LK" ~ substr(LK, start=4,  stop=nchar(LK)),
                                                            substr(LK, start=1, stop=2)=="SK" ~ substr(LK, start=4,  stop=nchar(LK))))

    if (save_data_set == TRUE) {
    readr::write_csv(incidence_data, paste("rki_incidence_data_", county, ".csv"))
    }
    if (county != "all") {
    time_series_plot <- ggplot2::ggplot(incidence_data, ggplot2::aes(x=Date, y =Incidence)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Reported 7 Day Incidence/100.000")
    if (save_plot == TRUE) {
        ggplot2::ggsave(paste("rki_incidence_", county, ".png"), plot = time_series_plot, w = 12.5, h = 9, dpi =300)
    }
    return(list(incidence_data = incidence_data, time_series_plot = time_series_plot))
    }
}