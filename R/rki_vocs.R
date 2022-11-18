#' VOC shares on national level
#'
#' @param min_date "Date"
#' @param max_date "Date"
#' @param how_many "# of VOCs"
#'
#' @import readr
#' @import readxl
#' @import MMWRweek
#' @import httr
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'
#' @return A list: 1) data frame containing the shares of the how_many most common VOCs, 2) line plot of these VOCs over time
#' @export
#'
#' @examples
#' rkiVOCs <- rki_vocs("2022-01-01", today(), 10)

rki_vocs <- function(min_date, max_date, how_many) {
    voc <- share <- year <- KW <- NULL
    url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile'
    httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    rki_voc_data <- readxl::read_excel(tf, 2) %>%
    tidyr::pivot_longer(-KW, names_to = "voc", values_to = "share") %>%
    dplyr::filter(grepl("Anteil", voc)) %>%
    subset(stringr::str_length(KW) <= 9) %>%
    tidyr::separate(KW, into = c("year", "KW"), sep = "-") %>%
    dplyr::mutate(KW = stringr::str_replace(KW, "KW", "")) %>%
    mutate(voc = sapply(strsplit(voc, "_"), `[`, 1))

    #Converting the KW and year to doubles, as they're read in as strings
    rki_voc_data$KW <- as.double(rki_voc_data$KW)
    rki_voc_data$year <- as.double(rki_voc_data$year)

    rki_voc_data <- rki_voc_data %>% dplyr::mutate(date = MMWRweek::MMWRweek2Date(MMWRyear = year, 
                                                                MMWRweek = KW,
                                                                MMWRday = 7))

    #Filtering the data set according to our input parameters
    rki_voc_data_lastweek <- rki_voc_data %>%
    dplyr::filter(date == max(date))
    rki_voc_data_lastweek <- rki_voc_data_lastweek[order(rki_voc_data_lastweek$share, decreasing = TRUE), ] 
    most_common_vocs <- rki_voc_data_lastweek$voc[2:(how_many + 1)]

    #Creating a line plot depicting the shares of how_many VOCs
    plot <- rki_voc_data %>% filter(voc %in% most_common_vocs) %>%
    filter(date > min_date) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = share, col = voc)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank()) +
    ggplot2::ylab("Share in %") +
    ggplot2::xlab("Date")

    return(list(rki_voc_data = rki_voc_data, plot = plot))
}