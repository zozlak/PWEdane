# PWEdane

[![Travis-CI Build Status](https://travis-ci.org/zozlak/ZPD.png?branch=master)](https://travis-ci.org/zozlak/PWEdane)
[![Coverage Status](https://coveralls.io/repos/zozlak/ZPD/badge.svg)](https://coveralls.io/r/zozlak/PWEdane)

Pakiet dla wygody Bartka, umożliwiający pobieranie wyników egzaminów i testów zrównujących z dołączonymi wszystkimi potrzebnymi do skalowań zrównujących zmiennymi.

## Instalacja

Pakiet nie jest wypchnięty na CRAN-a, więc instalować trzeba ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalacja możliwa jest w dwóch wariantach:

### Z użyciem pakietu devtools:
```r
install.packages('devtools') # potrzbne tylko gdy nie jest jeszcze zainstalowany
devtools::install_github('zozlak/ZPD')
devtools::install_github('zozlak/PWEdane')
```

**Jeśli podczas instalacji napotkasz na błąd, a używasz linuksa** sprawdź, czy nie dotyczy Cię [ten problem](https://github.com/hadley/devtools/issues/650) lub przeprowadź "uczciwą instalację ze źródeł" (patrz niżej).

### "Uczciwa instalacja ze źródeł":

* Pobrać z sieci i zainstalować [narzędzia GIT-a dla linii komend](http://git-scm.com/downloads) 
* Zainstalować [pakiet ZPD](https://github.com/zozlak/ZPD) wpisując w konsoli:

  ```r
  git clone https://github.com/zozlak/ZPD.git
  R CMD INSTALL ZPD
  ```
* W konsoli wywołać:
  ```r
  git clone https://github.com/zozlak/PWEdane.git
  R CMD INSTALL PWEdane
  ```

