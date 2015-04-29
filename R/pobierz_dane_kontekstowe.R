#' @title Pobiera dane kontekstowe o zadanym zakresie
#' @description
#' Pobiera dane kontekstowe używane przy skalowaniach zrównujących oraz
#' generowaniu oszacowań umiejętności uczniów.
#' 
#' Zakres pobieramych obserwacji jest ograniczany na podstawie parametru
#' \code{daneFiltr}. Może on być dowolną ramką danych, która posiada co najmniej
#' jedną kolumnę wspólną z kolumnami zwracanymi przez funkcję
#' \code{ZPD::pobierz_dane_uczniowie_testy()}. Odfiltrowanie polega na wykonaniu
#' \code{semi_join(pobierz_dane_uczniowie_testy(), daneFiltr)}.
#' @param daneFiltr ramka danych filtrujących - patrz opis
#' @import ZPD
#' @import dplyr
#' @export
pobierz_dane_kontekstowe = function(
  daneFiltr
){
  if(any(class(daneFiltr) %in% 'tbl_sql')){
    src = daneFiltr$src
  }else{
    src = polacz()
  }
  
  uczniowieTesty = pobierz_dane_uczniowie_testy(src)
  uczniowie = pobierz_uczniow(src)
  szkoly = pobierz_szkoly(src)
  testy = pobierz_testy(src)

  stopifnot(
    is.tbl(daneFiltr) | is.data.frame(daneFiltr),
    length(intersect(colnames(daneFiltr), colnames(uczniowieTesty))) > 0
  )
  
  daneKontekstowe = suppressMessages(
    uczniowieTesty %>%
    semi_join(daneFiltr, copy = TRUE) %>%
    inner_join(uczniowie) %>%
    inner_join(testy) %>%
    left_join(szkoly) %>%
    mutate_('populacja' = 
      ~(
        (typ_szkoly %in% c('SP', 'gimn.') & (id_szkoly < 0 | (dla_doroslych == FALSE & (specjalna == FALSE | is.na(specjalna)) & (przyszpitalna == FALSE | is.na(przyszpitalna))))) |
        (typ_szkoly %in% c('T', 'LO', 'LP') & id_szkoly > 0 & dla_doroslych == FALSE & (specjalna == FALSE | is.na(specjalna)) & (przyszpitalna == FALSE | is.na(przyszpitalna)))
      ) & is.na(pop_podejscie)
    ) %>%
    select_('id_obserwacji', 'plec', 'dysleksja', 'laureat', 'pop_podejscie', 'id_szkoly', 'teryt_szkoly', 'id_testu', 'opis_testu', 'populacja')
  )

  return(daneKontekstowe)
}