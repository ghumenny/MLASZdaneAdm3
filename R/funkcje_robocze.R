#' @title Funkcje robocze dla obliczania wskaźników dla 2. edycji monitoringu -
#' dane administracyjne
#' @description Funkcja przeliczająca liczbę miesięcy, które minęły od roku 0 na
#' miesiąc i rok.
#' @param x wartość liczbowa, która ma być przeliczona na miesiąc i rok
#' (zazwyczaj jest to zmienna \code{OKRES} w tabeli pośredniej)
#' @return list
#' @export
okres_na_date = function(x) {
  stopifnot(is.numeric(x))
  
  mies_rok = 
    list(mies = x - 12L*(floor((x - 1) / 12)),
         rok = floor((x - 1) / 12))
  
  return(mies_rok)
}
#' @title Funkcje robocze dla obliczania wskaźników dla 2. edycji monitoringu -
#' dane administracyjne
#' @description Funkcja przeliczająca liczbę miesięcy, które minęły od roku 0 na
#' miesiąc.
#' @param x data wyrażona jako liczbą miesięcy, które minęły od roku 0, czyli od
#' początku naszej ery (zazwyczaj jest to zmienna \code{OKRES} w tabeli
#' pośredniej), która ma być przeliczona na wartość liczbową będącą miesiącem
#' @return numeric
#' @export
okres_na_mies = function(x) {
  stopifnot(is.numeric(x))
  return(x - 12L*(floor((x - 1) / 12)))
}
#' @title Funkcje robocze dla obliczania wskaźników dla 2. edycji monitoringu -
#' dane administracyjne
#' @description Funkcja przeliczająca liczbę miesięcy, które minęły od roku 0 na
#' rok.
#' @param x data wyrażona jako liczbą miesięcy, które minęły od roku 0, czyli od
#' początku naszej ery (zazwyczaj jest to zmienna \code{OKRES} w tabeli
#' pośredniej), która ma być przeliczona na wartość liczbową będącą rokiem
#' @return numeric
#' @export
okres_na_rok = function(x) {
  stopifnot(is.numeric(x))
  return(floor((x - 1) / 12))
}
#' @title Funkcje robocze dla obliczania wskaźników dla 2. edycji monitoringu -
#' dane administracyjne
#' @description Funkcja przeliczająca miesiąc i rok na okres, czyli liczbę
#' miesięcy od roku 0
#' @param mies miesiąc wyrażony jako liczba z zakresu od 1 do 12, gdzie styczeń
#' to wartość 1, luty to wartość 2 itd.
#' @param rok rok kalendarzowy wyrażony jako liczba
#' @return numeric
#' @export
data_na_okres = function(mies, rok) {
  stopifnot(is.numeric(mies),
            mies %in% 1:12,
            is.numeric(rok))
  
  return((12 * rok) + mies)
}
