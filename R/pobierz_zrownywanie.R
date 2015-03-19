#' @title Pobiera wyniki testów zrównujących ze wskazanego roku wraz z danymi kontekstowymi
#' @description 
#' Jeśli nie zostanie podany parametr \code{idSkali}, wtedy zastąpiopny zostanie
#' wartością domyślną. W wypadku niepowodzenia pobrania wartości
#' domyślnejwyświetlony zostanie stosowny komunikat.
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param rok rok, z ktorego dane maja zostac pobrane
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import ZPD
#' @export
pobierz_zrownywanie = function(
  rodzajEgzaminu, 
  rok, 
  punktuj = TRUE,
  idSkali = NA_integer_,
  skroc   = TRUE
){
  stopifnot(
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1, !is.na(rodzajEgzaminu),
    is.vector(rok), is.numeric(rok), length(rok) == 1, !is.na(rok),
    is.vector(punktuj), is.logical(punktuj), length(punktuj) == 1, !is.na(punktuj),
    is.vector(idSkali), is.numeric(idSkali), length(idSkali) == 1,
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, !is.na(skroc)
  )
  
  src = polacz()
  on.exit({
    DBI::dbDisconnect(src$con)
  })
  
  rokTmp = rok
  testy = pobierz_testy(src) %>%
    filter_(~rodzaj_egzaminu == rodzajEgzaminu, ~czy_egzamin == FALSE, ~rok == rokTmp) %>%
    collect() %>%
    filter_(~grepl('^zrównywanie;', opis_testu))

  if(is.na(idSkali)){
    skale = znajdz_skale_dla_testow(testy)
    if(nrow(skale) > 1){
      print(skale)
      stop('W bazie istnieje wiecej niz jedna skala pasujaca do wskazanych wynikow egzaminu')
    }
    if(nrow(skale) == 0){
      idSkali = NULL
      warning('Nie udalo sie dopasowac skali do wskazanych wynikow egzaminu - pobieranie danych bez zastosowania skali')
    }else{
      idSkali = skale$id_skali[1]
      message('Stosuje skale ', idSkali)
    }
  }
    
  wyniki = pobierz_wyniki_zrownywania(src, rodzajEgzaminu, rok, punktuj, idSkali, skroc)
  
  wyniki = pobierz_dane_kontekstowe(testy) %>%
    collect() %>%
    inner_join(wyniki %>% collect())
  
  return(wyniki)
}