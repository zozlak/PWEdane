#' @title Pobiera wyniki egzaminu z dołączonymi danymi kontekstowymi
#' @description 
#' Jeśli nie zostaną podane parametry \code{czyEwd} i/lub \code{idSkali},
#' wtedy zastąpiopne zostaną wartościami domyślnymi.
#' 
#' W wypadku niepowodzenia pobrania wartości domyślnej (możliwe dla 
#' \code{idSkali}), wyświetlony zostanie stosowny komunikat.
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param czescEgzaminu czesc egzaminu, ktorego wyniki maja zostac pobrane
#' @param rokEgzaminu rok egzaminu, ktorego wyniki maja zostac pobrane
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param czyEwd wybor, czy maja byc pobrane wyniki gromadzone przez EWD, czy ZAOU
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import ZPD
#' @export
pobierz_wyniki = function(
  rodzajEgzaminu,
  czescEgzaminu,
  rokEgzaminu,
  punktuj = TRUE,
  czyEwd = NA,
  idSkali = NA_integer_,
  skroc   = TRUE
){
  stopifnot(
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1, !is.na(rodzajEgzaminu),
    is.vector(czescEgzaminu), is.character(czescEgzaminu), length(czescEgzaminu) == 1, !is.na(czescEgzaminu),
    is.vector(rokEgzaminu), is.numeric(rokEgzaminu), length(rokEgzaminu) == 1, !is.na(rokEgzaminu),
    is.vector(punktuj), is.logical(punktuj), length(punktuj) == 1, !is.na(punktuj),
    is.vector(czyEwd), is.logical(czyEwd), length(czyEwd) == 1,
    is.vector(idSkali), is.numeric(idSkali), length(idSkali) == 1,
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, !is.na(skroc)
  )
  
  src  = polacz()
  on.exit({
    DBI::dbDisconnect(src$con)
  })
  
  if(is.na(czyEwd)){
    if(rodzajEgzaminu == 'egzamin gimnazjalny'){
      czyEwd = rokEgzaminu >= 2008
    }else if(rodzajEgzaminu == 'sprawdzian'){
      czyEwd = rokEgzaminu >= 2014
    }else{
      czyEwd = TRUE
    }
  }

  # podstawowa informacja filtrujaca
  testy = pobierz_testy(src) %>%
    filter_(~rodzaj_egzaminu == rodzajEgzaminu, ~czesc_egzaminu == czescEgzaminu, ~rok == rokEgzaminu, ~dane_ewd == czyEwd) %>%
    select_('id_testu', 'arkusz')
  lTestow = nrow(testy %>% collect())
  if(lTestow == 0){
    stop('W bazie danych brak wskazanych wynikow egzaminu')
  }
  
  # automatyczne dopasowywanie skali
  if(punktuj == TRUE){
    if(is.na(idSkali)){
      skale = znajdz_skale_dla_testow(testy)
      if(nrow(skale) > 1){
        print(skale)
        stop('W bazie istnieje wiecej niz jedna skala pasujaca do wskazanych wynikow egzaminu')
      }
      if(nrow(skale) == 0){
        idSkali = NULL
        message('Nie udalo sie dopasowac skali do wskazanych wynikow egzaminu - pobieranie danych bez zastosowania skali')
      }else{
        idSkali = skale$id_skali[1]
        message('Stosuje skale ', idSkali)
      }
    }
  }else{
    idSkali = NULL
  }

  wyniki = pobierz_wyniki_egzaminu(src, rodzajEgzaminu, czescEgzaminu, rokEgzaminu, czyEwd, punktuj, 'idSkali' = idSkali, 'skroc' = skroc)

  wyniki = suppressMessages(
    pobierz_dane_kontekstowe(testy) %>%
    collect() %>%
    inner_join(wyniki %>% collect())
  )
  
  return(wyniki)
}