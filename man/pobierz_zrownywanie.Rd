% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/pobierz_zrownywanie.R
\name{pobierz_zrownywanie}
\alias{pobierz_zrownywanie}
\title{Pobiera wyniki testów zrównujących ze wskazanego roku wraz z danymi kontekstowymi}
\usage{
pobierz_zrownywanie(rodzajEgzaminu, rok, punktuj = TRUE,
  idSkali = NA_integer_, skroc = TRUE)
}
\arguments{
\item{rodzajEgzaminu}{rodzaj egzaminu, ktorego wyniki maja zostac pobrane}

\item{rok}{rok, z ktorego dane maja zostac pobrane}

\item{punktuj}{wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow}

\item{idSkali}{identyfikator skali, ktora ma zostac zastosowana do danych}

\item{skroc}{czy do danych zastosowac skrocenia skal opisane w skali}
}
\description{
Jeśli nie zostanie podany parametr \code{idSkali}, wtedy zastąpiopny zostanie
wartością domyślną. W wypadku niepowodzenia pobrania wartości
domyślnejwyświetlony zostanie stosowny komunikat.
}

