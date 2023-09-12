![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

# MLASZdaneAdm3

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój.

Pakiet `MLASZdaneAdm3` zawiera zbiór funkcji służących do obliczania wskaźników dla 3. edycji Monitoringu Losów Absolwentów z użyciem danych administracyjnych (resjestry: CIE, ZUS, POLON i CKE). Przy pomocy tego pakietu oraz drugiego, będącego silnikiem agregacji - [`MLASZdane`](https://github.com/losyabsolwentow/MLASZdane), można tworzyć zbiory wskaźników na zadanym poziomie agregacji.

# Instalacja / aktualizacja

Pakiet nie jest dostępny na CRAN, więc trzeba instalować go ze źródeł.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('losyabsolwentow/MLASZdaneAdm3', build_opts = c("--no-resave-data"))
```

Pakiet `MLASZdaneAdm3` jest zależny od pakietu `MLASZdane`, ale nie ma potrzeby go dodatkowo instalować, ponieważ dzieje się to podczas instalacji pakietu `MLASZdaneAdm3`.

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.
