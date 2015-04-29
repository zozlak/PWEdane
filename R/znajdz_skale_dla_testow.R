#' @title Wyszukuje skale dla wskazanych testów
#' @description
#' Wyszukuje skale zadanego rodzaju (parametr \code{rodzajSkali}) dla wskazanych
#' testów.
#' 
#' Wykorzystywana np. do pobierania domyślnych skal przez funkcje
#' \code{pobierz_wyniki()} i \code{pobierz_zrownywanie()}.
#' @param idTestow identyfikatory testów, dla których szukamy skali (jako wektor
#'   lub ramka danych z kolumną \code{id_testu})
#' @param rodzajSkali rodzaj wyszukiwanej skali ('zrównywanie'/'ktt'/'ewd')
#' @import ZPD
#' @import dplyr
#' @export
znajdz_skale_dla_testow = function(
  idTestow,
  rodzajSkali = 'zr\u00F3wnywanie'
){
  if(is.tbl(idTestow) | is.data.frame(idTestow)){
    stopifnot(
      any(colnames(idTestow) %in% 'id_testu')
    )
    if(any(class(idTestow) %in% 'tbl_sql')){
      idTestow = idTestow %>%
        collect()
    }
    idTestow = idTestow$id_testu
  }
  idTestow = unique(na.exclude(idTestow))
  stopifnot(
    is.numeric(idTestow), length(idTestow) > 0,
    is.vector(rodzajSkali), is.character(rodzajSkali), length(rodzajSkali) == 1, !is.na(rodzajSkali)
  )
  testy = data.frame('id_testu' = idTestow)
  lTestow = length(idTestow)

  src = polacz()
  on.exit({
    DBI::dbDisconnect(src$con)
  })
  skale = suppressMessages(
    pobierz_skale(src) %>%
    filter_(~rodzaj_skali == rodzajSkali) %>% 
    collect() %>% # z uwagi na błąd dplyr-a, który nie radzi sobie, jesli po stronie serwera SQL zostanie 0 wierszy
    semi_join(testy, copy = TRUE) %>%
    group_by_('id_skali', 'opis_skali') %>% 
    summarize_('n' = ~n()) %>%
    filter_(~n == lTestow)
  )

  return(skale)
}