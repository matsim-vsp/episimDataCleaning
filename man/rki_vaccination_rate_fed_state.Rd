% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rkiVaccinationRateFedState.R
\name{rki_vaccination_rate_fed_state}
\alias{rki_vaccination_rate_fed_state}
\title{Vaccination rates on federal state level}
\usage{
rki_vaccination_rate_fed_state(save_data_set, save_plots)
}
\arguments{
\item{save_data_set}{"TRUE/FAlSE"}

\item{save_plots}{"TRUE/FALSE"}
}
\value{
A list: 1 data frame containing the reported incidence from May 2020 onwards, 2 If federalState == "all", you may find a facet plot of the incidences over time here, else : empty; 3 Incidence plot over time # nolint
}
\description{
Vaccination rates on federal state level
}
\examples{
vaccinationRate <- rki_vaccination_rate_fed_state(FALSE, FALSE)
}
