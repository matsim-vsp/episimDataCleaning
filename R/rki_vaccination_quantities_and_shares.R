#' Vaccination counts and manufacturer shares on federal state level
#'
#' @param federal_state "A string vector specifying the federal state you are interested in."
#' @param save_data_set "TRUE/FAlSE"
#' @param save_plots "TRUE/FALSE"
#'
#' @import ggplot2
#' @import dplyr
#' @import readr
#' @import rlang
#' @import scales
#'
#' @return A list: 1) data frame containing the weekly vaccination counts for the selected state, 2) counts containing the shares of the different manufacturers, 3) Plot of counts over time, 4) Plots of shares
#' @export
#'
#' @examples
#' Bayern <- rki_vaccination_quantities_and_shares("Bayern", FALSE, FALSE)


rki_vaccination_quantities_and_shares <- function(federal_state, save_data_sets, save_plots) {

  #Creating some warning messages for the user
  federal_stateOptions <- c("Bundesresorts", paste("Baden-W","\\u00fc","rttemberg"), "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", paste("Th","\\u00fc","ringen"))
  yes_no <- c(TRUE, FALSE)
  if(!is.element(federal_state, federal_stateOptions)) stop("Please enter a valid federal state.")
  if(!is.element(save_data_sets, yes_no)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created data set as a csv file.")
  if(!is.element(save_plots, yes_no)) stop("Please enter either TRUE or FALSE to decide whether or not you would like to save the created plot.")

  #Reading in vaccination data from RKI
  Rohdaten <- readr::read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv")

  #Data frame containing the names of the federal states as well as their corresponding IDs
  dict_state_id <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(dict_state_id) <- c("MeldeLandkreisBundesland", "ID")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c(paste("Baden-W","\\u00fc","rttemberg"), "08")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bayern", "09")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Berlin", "11")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Brandenburg", "12")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bremen", "04")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hamburg", "02")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hessen", "06")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Mecklenburg-Vorpommern", "13")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Niedersachsen", "03")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Nordrhein-Westfalen", "05")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Rheinland-Pfalz", "07")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Saarland", "10")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen", "14")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen-Anhalt", "15")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Schleswig-Holstein", "01")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c(paste("Th","\\u00fc","ringen"), "16")
  dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bundesresorts", "17")


  bundesland <- Rohdaten %>% dplyr::group_by(rlang::.data$BundeslandId_Impfort, rlang::.data$Impfstoff) %>%
  filter(rlang::.data$Impfserie == "1") %>%
    dplyr::summarise(date = max(rlang::.data$Impfdatum), first_jabs = sum(rlang::.data$Anzahl))


  weekly_sum <- Rohdaten %>% filter(rlang::.data$Impfserie == "1") %>%
  dplyr::group_by(BundeslandId_Impfort, date = cut("Impfdatum", "week"), "Impfstoff", "Impfserie") %>%
  dplyr::summarise(value = sum("Anzahl"))
  weekly_sum$date <- as.Date(weekly_sum$date)
  weekly_sum <- dplyr::mutate(weekly_sum, bundesland = dict_state_id[which(dict_state_id$ID == "BundeslandId_Impfort"), 1])

  shares <- bundesland %>% dplyr::group_by("BundeslandId_Impfort") %>%
  dplyr::summarise(Impfstoff = rlang::.data$Impfstoff, share = rlang::.data$first_jabs / sum(rlang::.data$first_jabs))
  shares$share <- 100 * round(shares$share, digits = 2)
  shares <- dplyr::arrange(shares, dplyr::desc(rlang::.data$Impfstoff))
  shares <- shares %>% dplyr::group_by(rlang::.data$BundeslandId_Impfort) %>%
  dplyr::summarise(share = "share", Impfstoff = "Impfstoff", neededForPieChart = cumsum("share") - "share" / 2, bundesland = dict_state_id[which(dict_state_id$ID == BundeslandId_Impfort), 1])

  #From here: Plot quantities and shares for one(!) federal state.
  #OkabeItoPalette
  okabeIto = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999")

  plot_counts <- weekly_sum %>% dplyr::filter(bundesland == federal_state) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = rlang::.data$value, col = rlang::.data$Impfstoff), size = 1.1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", text = ggplot2::element_text(size = 20)) +
    ggplot2::ggtitle(paste("Weekly quantities for", federal_state, "(Erstimpfung)")) +
    ggplot2::scale_color_manual(values = okabeIto[2:9]) +
    ggplot2::ylab("Anzahl") +
    ggplot2::xlab("Datum") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    theme(legend.title = ggplot2::element_blank()) +
    theme(plot.title = element_text(hjust = 0.5))
  if (save_plots == TRUE) {
    ggplot2::ggsave(paste("rkiVaccinationCounts", federal_state, ".png"), width = 12.5, height = 6, dpi = 300)
  }

  plot_shares <- shares %>% dplyr::filter(bundesland == federal_state) %>%
  ggplot2::arrange(ggplot2::desc(rlang::.data$neededForPieChart)) %>%
  ggplot2::ggplot(ggplot2::aes(x = "", y = rlang::.data$share, fill = "Impfstoff")) +
    ggplot2::geom_bar(stat = "identity", color = 1) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::ggtitle(paste("Manufacturer shares for, federal_state ,(Erstimpfung)")) +
    ggplot2::geom_label_repel(aes(y = rlang::.data$neededForPieChart, label = paste0(rlang::.data$share, "%")), force_pull = 100, nudge_x = 1, show.legend=FALSE) +
    ggplot2::scale_fill_manual(values = okabeIto[2:9]) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom", text = ggplot2::element_text(size = 20)) +
    theme(legend.title = ggplot2::element_blank()) +
    theme(plot.title = element_text(hjust = 0.5))

  if (save_plots == TRUE) {
    ggplot2::ggsave(paste("rkiVaccinationShares", federal_state, ".png"), width = 12.5, height = 6, dpi = 300)
  }

  weekly_sum <- filter(weekly_sum, bundesland == federal_state)
  shares <- filter(shares, bundesland == federal_state)

  if (save_data_sets == TRUE) {
    readr::write_csv(weekly_sum, paste("weeklyVaccinationCounts", federal_state, ".csv"))
    readr::write_csv(shares, paste("vaccinationShares", federal_state, ".csv"))
  }

  return(list(vaccination_counts = weekly_sum, shares = shares, plot_counts = plot_counts, plot_shares = plot_shares)) 
}