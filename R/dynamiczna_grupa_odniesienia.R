#' @title Obliczanie wskaznikow na poziomie zagregowanym - definiowanie grupowania
#' @description Funkcja pozwala przygotować ramkę danych opisującą podział na
#' grupy, dla których mają następnie zostać obliczone wskaźniki zagregowane
#' przy pomocy funkcji \code{\link{agreguj_wskazniki}}, oraz odpowiadające im
#' grupy odniesienia w sytuacji, kiedy te drugie mają zostać utworzone w oparciu
#' o kryterium geograficzne. Jako geograficzna jednostka odniesienia wybierany
#' jest powiat, podregion statystyczny, województwo lub cała Polska w zależności
#' od tego, która z nich spełnia zadane argumentami \code{minN} i \code{minM}
#' kryteria minimalnej liczby odpowiednio absolwentów i grup. Jeśli kryteria te
#' nie są spełnione nawet dla województwa, jako kod specyfikujący grupę
#' odniesienia zwracany jest brak danych.
#' @param x ramka danych z danymi indywidualnymi (zwykle \emph{tabela pośrednia}
#' P4)
#' @param zmGrupujace nazwa lub wektor nazw zmiennych (\emph{\<tidy-select\>}),
#' kombinacja wartości których definiuje podział na grupy
#' @param zmPominGrupaOdniesienia nazwa lub wektor nazw zmiennych
#' (\emph{\<tidy-select\>}), które należy \strong{pominąć} względem zbioru
#' podanego w argumencie \code{zmGrupujace}, przy definiowaniu przypisania do
#' \emph{grupy odniesienia} (pomijając kryterium geograficzne, które zostanie
#' obsłużone oddzielnie)
#' @param ... opcjonalnie specyfikacja dodatkowych kolumn z ramki danych
#' podanej argumentem \code{x}, które mają zostać dołączone do zwracanej ramki
#' danych (\emph{\<tidy-select\>})
#' @param idAbs nazwa lub wektor nazw zmiennych (\emph{\<tidy-select\>}),
#' kombinacja wartości których stanowi identyfikator absolwenta
#' @param zmTerytPow nazwa zmiennej zawierającej kod powiatu (w którym znajduje
#' się grupa, do której należy dany absolwent) (\emph{\<tidy-select\>})
#' @param minN minimalna liczba absolwentów, jaką musi obejmować grupa
#' odniesienia (jeśli \code{wykluczGrupeZGrupyOdniesienia=FALSE}, po odjęciu
#' liczby absolwentów w danej grupie)
#' @param minM minimalna liczba innych grup, jaką musi obejmować grupa
#' odniesienia (jeśli \code{wykluczGrupeZGrupyOdniesienia=FALSE}, powiększona
#' o 1 względem podanej)
#' @param wykluczGrupeZGrupyOdniesienia wartość logiczna - czy absolwenci
#' z analizowanej grupy powinni zostać wykluczeni z grupy odniesienia
#' @return ramka danych, która może zostać użyta jako argument \code{grupy}
#' w wywołaniu funkcji \code{\link{agreguj_wskazniki}}.
#' @examples
#' \dontrun{
#' # wywołanie definiujące grupy na poziomie szkoło-zawodo-lat
#' szkoloZawody <- utworz_grupowanie_odn_teryt(
#'   p4,
#'   c(id_szk, kod_zaw, rok_abs),
#'   nazwa_zaw, branza)
#' # wywołanie definiujące grupy na poziomie szkoło-lat
#' szkoly <- utworz_grupowanie_odn_teryt(
#'   p4,
#'   c(id_szk, rok_abs))
#' }
#' @export
#' @importFrom dplyr %>% .data across all_of case_when coalesce count cur_data
#' distinct group_by left_join mutate rename select summarise
utworz_grupowanie_odn_teryt = function(x, zmGrupujace, ...,
                                       zmPominGrupaOdniesienia = any_of("id_szk"),
                                       idAbs = all_of(c("id_abs", "rok_abs")),
                                       zmTerytPow = all_of("teryt_pow"),
                                       minN = 10, minM = 3,
                                       wykluczGrupeZGrupyOdniesienia = TRUE) {
  stopifnot(is.data.frame(x),
            is.numeric(minN), length(minN) == 1,
            is.numeric(minM), length(minM) == 1,
            is.logical(wykluczGrupeZGrupyOdniesienia),
            length(wykluczGrupeZGrupyOdniesienia) == 1)
  stopifnot(!is.na(minN), minN > 0,
            !is.na(minM), minM > 0,
            wykluczGrupeZGrupyOdniesienia %in% c(TRUE, FALSE))
  zmGrupujace <- names(select(x, {{zmGrupujace}}))
  if (!("rok_abs" %in% zmGrupujace)) {
    message("Wśród podanych zmiennych grupujących nie ma `rok_abs` (roku zostania absolwentem), co jest nietypowe.")
  }
  zmPominGrupaOdniesienia <- names(select(x, {{zmPominGrupaOdniesienia}}))
  stopifnot(all(zmPominGrupaOdniesienia %in% zmGrupujace))
  idAbs <- names(select(x, {{idAbs}}))
  zmTerytPow <- names(select(x, {{zmTerytPow}}))
  
  if ("podregion" %in% names(x)) {
    message("Przekazana ramka danych zawiera kolumnę 'podregion' - zostanie ona potraktowana jako opisująca podregion statystyczny klasyfikacji NUTS.")
  } else {
    stopifnot(mode(x[[zmTerytPow]]) == mode(podregiony$teryt_pow))
    x <- x %>%
      left_join(podregiony %>%
                  rename({{zmTerytPow}} := .data$teryt_pow),
                by = zmTerytPow)
  }
  stopifnot(all(!is.na(x$podregion)))
  x <- x %>%
    select(all_of(c(idAbs, zmGrupujace, zmTerytPow, "podregion")), ...) %>%
    mutate(teryt_woj = floor(.data[[zmTerytPow]] / 10000))
  nazwyZastrzezone <- c("grupa", "odniesienie",
                        "nOdnPow", "mOdnPow", "nOdnPodreg", "mOdnPodreg",
                        "nOdnWoj", "mOdnWoj")
  if (any(nazwyZastrzezone %in% names(x))) {
    warning("Kolumny o nazwach '",
            paste(nazwyZastrzezone, collapse = "', '"),
            "' nie mogą zostać dołączone do zwracanych danych, ponieważ funkcja sama tworzy kolumny o takich nazwach.")
    x <- x %>%
      select(-any_of(nazwyZastrzezone))
  }
  doDolaczenia <- x %>%
    select(all_of(c(zmGrupujace, zmTerytPow)), ...) %>%
    distinct()
  powtorzenia <- doDolaczenia %>%
    count(across(all_of(zmGrupujace)), name = "___powt___") %>%
    filter(.data$`___powt___` > 1)
  if (nrow(powtorzenia) > 0) {
    blad = paste("W ramach niektórych grup występują różne wartości zmiennej opisującej kod TERYT powiatu lub zmiennych, które mają zostać dołączone do zwracanej ramki danych. Grupy, w których wystąpił problem:\n\n",
                 MLASZdane:::pokaz_ramke_danych(powtorzenia), "\n\n")
    stop(blad)
  }
  
  x <- x %>%
    select(all_of(c(idAbs, zmGrupujace, zmTerytPow, "podregion", "teryt_woj")))
  
  grupy <- x %>%
    count(across(all_of(c(zmGrupujace, zmTerytPow, "podregion", "teryt_woj"))),
          name = "nGr") %>%
    mutate(grupa = cur_data() %>%
             select(all_of(zmGrupujace)) %>%
             utworz_warunki(),
           odniesieniePow = cur_data() %>%
             select(all_of(c(zmGrupujace, zmTerytPow)),
                    -all_of(zmPominGrupaOdniesienia)) %>%
             utworz_warunki(),
           odniesieniePodreg = cur_data() %>%
             select(all_of(c(zmGrupujace, "podregion")),
                    -all_of(zmPominGrupaOdniesienia)) %>%
             utworz_warunki(),
           odniesienieWoj = cur_data() %>%
             select(all_of(c(zmGrupujace, "teryt_woj")),
                    -all_of(zmPominGrupaOdniesienia)) %>%
             utworz_warunki(),
           odniesienieOgpol = cur_data() %>%
             select(all_of(zmGrupujace),
                    -all_of(zmPominGrupaOdniesienia)) %>%
             utworz_warunki())
  x <- x %>%
    left_join(grupy, by = c(zmGrupujace, zmTerytPow, "podregion", "teryt_woj"))
  grupy %>%
    left_join(x %>%
                group_by(.data$odniesieniePow) %>%
                summarise(nOdnPow = n_distinct(across(all_of(idAbs))),
                          mOdnPow = n_distinct(across(all_of(zmGrupujace))),
                          .groups = "drop"),
              by = "odniesieniePow") %>%
    left_join(x %>%
                group_by(.data$odniesieniePodreg) %>%
                summarise(nOdnPodreg = n_distinct(across(all_of(idAbs))),
                          mOdnPodreg = n_distinct(across(all_of(zmGrupujace))),
                          .groups = "drop"),
              by = "odniesieniePodreg") %>%
    left_join(x %>%
                group_by(.data$odniesienieWoj) %>%
                summarise(nOdnWoj = n_distinct(across(all_of(idAbs))),
                          mOdnWoj = n_distinct(across(all_of(zmGrupujace))),
                          .groups = "drop"),
              by = "odniesienieWoj") %>%
    left_join(x %>%
                group_by(.data$odniesienieOgpol) %>%
                summarise(nOdnOgpol = n_distinct(across(all_of(idAbs))),
                          mOdnOgpol = n_distinct(across(all_of(zmGrupujace))),
                          .groups = "drop"),
              by = "odniesienieOgpol") %>%
    mutate(across(all_of(c("nOdnPow", "nOdnPodreg", "nOdnWoj", "nOdnOgpol")),
                  ~. - nGr),
           across(all_of(c("mOdnPow", "mOdnPodreg", "mOdnWoj", "mOdnOgpol")),
                  ~. - 1)) %>%
    mutate(odniesieniePow =
             ifelse(.data$nOdnPow >= minN & .data$mOdnPow > minM,
                    .data$odniesieniePow, NA_character_),
           odniesieniePodreg =
             ifelse(.data$nOdnPodreg >= minN & .data$mOdnPodreg > minM,
                    .data$odniesieniePodreg, NA_character_),
           odniesienieWoj =
             ifelse(.data$nOdnWoj >= minN & .data$mOdnWoj > minM,
                    .data$odniesienieWoj, NA_character_),
           odniesienieOgpol =
             ifelse(.data$nOdnOgpol >= minN & .data$mOdnOgpol > minM,
                    .data$odniesienieOgpol, NA_character_)) %>%
    mutate(odniesienie = coalesce(.data$odniesieniePow,
                                  .data$odniesieniePodreg,
                                  .data$odniesienieWoj,
                                  .data$odniesienieOgpol),
           odniesieniePoziom =
             case_when(!is.na(odniesieniePow) ~ "powiat",
                       !is.na(odniesieniePodreg) ~ "podregion",
                       !is.na(odniesienieWoj) ~ "wojewodztwo",
                       !is.na(odniesienieOgpol) ~ "kraj")) %>%
    mutate(odniesienie =
             ifelse(wykluczGrupeZGrupyOdniesienia & !is.na(.data$odniesienie),
                    paste0(.data$odniesienie,
                           " & !(", .data$grupa, ")"),
                    .data$odniesienie)) %>%
    select(-all_of(c("odniesieniePow", "odniesieniePodreg", "odniesienieWoj",
                     "odniesienieOgpol"))) %>%
    left_join(doDolaczenia,
              by = c(zmGrupujace, zmTerytPow)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym - funkcje pomocnicze
#' @description Nieeksportowana funkcja używana w ramach
#' \code{\link{utworz_grupowanie_odn_teryt}} do sklejania ciągów znaków
#' zawierających wyrażenia opisujące kryteria przynależności do grup lub grup
#' odniesienia
#' @param x ramka danych zawierająca zmienne, kombinacje wartości których
#' definiują przypisanie do grupy (lub grupy odniesienia)
#' @return wektor tekstowy
#' @seealso \code{\link{utworz_grupowanie_odn_teryt}}
utworz_warunki <- function(x) {
  x %>%
    mutate(across(where(is.character), ~paste0("'", ., "'")),
           across(where(is.factor), ~paste("'", levels(.)[.], "'"))) %>%
    apply(1,
          function(x, zm) {
            return(paste(zm, x, sep = " %in% ", collapse = " & "))
          },
          zm = colnames(x)) %>%
    return()
}
