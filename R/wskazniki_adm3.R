#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja przechowująca nazwę i adres szkoły, której dotyczy
#' raport.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>%
#' @export
dane_szkoly = function(x) {
  stopifnot(is.data.frame(x))

  list(
    nazwa = unique(x$nazwa_szk),
    adres = unique(x$adres_szk)
  ) %>%
    return()
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę absolwentów na danym
#' poziomie agregacji.
#' @param x ramka danych pośrednich P4
#' @return liczba
#' @importFrom dplyr %>% select .data distinct n_distinct
#' @export
l_abs = function(x) {
  x %>%
    select(.data$id_abs, .data$rok_abs) %>% 
    distinct() %>%
    n_distinct() %>%
    return()
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę kobiet wśród
#' absolwentów na danym poziomie agregacji, którzy zostali objęci monitoringiem.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>% select filter .data n_distinct
#' @export
l_kobiet = function(x) {
  x %>%
    select(.data$id_abs, .data$rok_abs, .data$plec) %>% 
    filter(.data$plec %in% "K") %>%
    n_distinct() %>% 
    return()
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę absolwentów na danym
#' poziomie agregacji, o których pozyskano informacje z poszczególnych
#' rejestrów.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>% summarise .data
#' @export
l_abs_zrodla = function(x) {
  x %>%
    summarise(
      n_cie = sum(.data$abs_w_sio, na.rm = TRUE),
      n_opi = sum(.data$abs_w_polon, na.rm = TRUE),
      n_oke = sum(.data$abs_w_cke, na.rm = TRUE),
      n_zus = sum(.data$abs_w_zus, na.rm = TRUE)) %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów o danym statusie
#' edukacyjno-zawodowym (wskaźnik S3) w danym miesiącu \code{mies} dla raportu
#' dla danego roku \code{rok}. Wyróżniamy następujące statusy (bez KUZ i KKZ):
#' \itemize{
#'  \item{ucz_prac - nauka i praca}
#'  \item{tylko_ucz - tylko nauka}
#'  \item{tylko_prac - tylko praca}
#'  \item{neet - brak nauki i brak pracy lub brak informacji o tychże}
#' }
#' @param x ramka danych pośrednich P3
#' @param rok_od rok początku okresu, dla którego ma być policzony wskaźnik
#' @param mies_od miesiąc początku okresu, dla którego ma być policzony wskaźnik
#' @param rok_do rok końca okresu, dla którego ma być policzony wskaźnik
#' @param mies_do miesiąc końća okresu, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% filter .data summarise n_distinct
#' @export
S3_mies = function(x, rok_od, mies_od, rok_do, mies_do) {
  stopifnot(is.data.frame(x),
            rok_od %in% c(2021, 2022),
            mies_od %in% c(1:12),
            rok_do %in% c(2021, 2022),
            mies_do %in% c(1:12))
  
  l_od = data_na_okres(rok = rok_od, mies = mies_od)
  l_do = data_na_okres(rok = rok_do, mies = mies_do)
  
  x %>%
    filter(.data$okres %in% seq(l_od, l_do, by = 1)) %>%
    summarise(
      n = n_distinct(.data$id_abs),
      ucz_prac = sum((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca > 0, na.rm = TRUE) / n,
      tylko_ucz = sum((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & (.data$praca %in% 0 | .data$status_nieustalony %in% 1), na.rm = TRUE) / n,
      tylko_prac = sum((.data$nauka2 %in% 0 & .data$nauka_szk_abs %in% 0) & (.data$praca > 0), na.rm = TRUE) / n,
      neet = sum(.data$nauka2 %in% 0 & .data$nauka_szk_abs %in% 0 & (.data$praca %in% 0 | is.na(.data$praca) | .data$status_nieustalony %in% 1), na.rm = TRUE) / n
    ) %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów o danym statusie
#' edukacyjno-zawodowym wśród grup absolwentów wydzielonych ze względu na zawód,
#' w którym się kształcili w szkole. Zwracana lista służy jako wsad do tabeli w
#' raporcie automatycznym. Jeśli lista będąca wsadem tabeli generowałaby pustą
#' tabelę, to zwracana jest pusta lista.
#' @details Funkcja wykorzystuje inną funkcję z pakietu, czyli
#' \code{\link{S3_mies}}
#' @param x ramka danych pośrednich P3
#' @param rok_od rok początku okresu, dla którego ma być policzony wskaźnik
#' @param mies_od miesiąc początku okresu, dla którego ma być policzony wskaźnik
#' @param rok_do rok końca okresu, dla którego ma być policzony wskaźnik
#' @param mies_do miesiąc końća okresu, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% group_by .data as_tibble ungroup filter mutate select
#' @export
zawody_S3 = function(x, rok_od, mies_od, rok_do, mies_do) {
  stopifnot(is.data.frame(x),
            "nazwa_zaw" %in% names(x),
            rok_od %in% c(2021, 2022),
            mies_od %in% c(1:12),
            rok_do %in% c(2021, 2022),
            mies_do %in% c(1:12))
  
  if (any(unique(x$typ_szk) %in% c("Branżowa szkoła I stopnia", "Technikum", "Szkoła policealna", "Branżowa szkoła II stopnia"))) {
    tab = x %>%
      group_by(.data$nazwa_zaw) %>%
      S3_mies(rok_od, mies_od, rok_do, mies_do) %>%
      as_tibble() %>%
      ungroup() %>% 
      arrange(desc(n))
    
    tot = x %>%
      filter(.data$nazwa_zaw %in% unique(tab$nazwa_zaw)) %>%
      S3_mies(rok_od, mies_od, rok_do, mies_do) %>%
      as_tibble() %>%
      mutate(nazwa_zaw = "Ogółem") %>%
      select(nazwa_zaw, n:neet)
    
    if (nrow(tab) %in% 0) {
      return(list(n = 0))
    } else {
      rbind(tab, tot) %>%
        as.list() %>%
        return()
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca wskaźnik E2 - Sposoby kontynuowania edukacji.
#' @param x ramka danych pośrednich P3
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% filter .data summarise n_distinct
#' @export
E2_nauka_kontyn = function(x, rok, mies = 12) {
  stopifnot(is.data.frame(x),
            rok %in% c(2021, 2022),
            mies %in% c(1:12))
  
  x = x %>%
    filter(.data$okres %in% data_na_okres(mies, rok))
  
  if (nrow(x) %in% 0) {
    return(list(n = 0))
  } else {
    nka = n_distinct(x$id_abs)
    
    x %>%
      summarise(
        n = nka,
        bs2 = sum(.data$nauka_bs2st %in% 1, na.rm = TRUE) / nka,
        lodd = sum(.data$nauka_lodd %in% 1, na.rm = TRUE) / nka,
        spolic = sum(.data$nauka_spolic %in% 1, na.rm = TRUE) / nka,
        studia = sum(.data$nauka_studia %in% 1, na.rm = TRUE) / nka,
        kkz = sum(.data$nauka_kkz %in% 1, na.rm = TRUE) / nka,
        kuz = sum(.data$nauka_kuz %in% 1, na.rm = TRUE) / nka,
        brak = sum(.data$nauka_bs2st %in% 0 &
                     .data$nauka_lodd %in% 0 &
                     .data$nauka_spolic %in% 0 &
                     .data$nauka_studia %in% 0 &
                     .data$nauka_kkz %in% 0 &
                     .data$nauka_kuz %in% 0, na.rm = TRUE) / nka) %>%
      as.list() %>%
      return()
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca na potrzeby szablonu raportu odsetek pracujących
#' w danym miesiącu w podziale na pobierających i nie pobierających nauki.
#' Domyślnie jest to okres między wrześniem a grudniem roku przekazanego w
#' argumentach funkcji, ale te wartości można zmieniać.
#' @param x ramka danych pośrednich P3
#' @param rok_od rok początku okresu, dla którego ma być policzony wskaźnik
#' @param mies_od miesiąc początku okresu, dla którego ma być policzony wskaźnik
#' @param rok_do rok końca okresu, dla którego ma być policzony wskaźnik
#' @param mies_do miesiąc końća okresu, dla którego ma być policzony wskaźnik
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% .data filter count full_join mutate n_distinct between
#' @export
Z4_ods_prac_mies = function(x, rok_od, mies_od = 9, rok_do, mies_do = 12, nauka) {
  stopifnot(is.data.frame(x),
            rok_od %in% c(2021, 2022),
            rok_do %in% c(2021, 2022),
            mies_od %in% c(1:12),
            mies_do %in% c(1:12),
            is.logical(nauka))
  
  l_od = data_na_okres(rok = rok_od, mies = mies_od)
  l_do = data_na_okres(rok = rok_do, mies = mies_do)
  
  x = x %>%
    filter(.data$okres %in% seq(l_od, l_do, by = 1))
  
  if (nauka) {
    ucz = x %>%
      filter(.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) %>%
      count(.data$okres, .data$id_abs) %>% 
      count(id_abs, name = "l_mies_ucz")
    
    ucz_prac = x %>%
      filter((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca %in% c(1, 2, 4:7)) %>%
      count(.data$id_abs, name = "l_mies_ucz_prac")
    
    nka = n_distinct(ucz$id_abs)
    
    if (nrow(ucz) > 0) {
      ucz %>%
        full_join(ucz_prac, by = "id_abs") %>%
        mutate(ods_ucz_prac = ifelse(is.na(.data$l_mies_ucz_prac), 0, .data$l_mies_ucz_prac / .data$l_mies_ucz)) %>%
        summarise(
          n = nka,
          srednia = mean(.data$ods_ucz_prac, na.rm = TRUE),
          med = median(.data$ods_ucz_prac, na.rm = TRUE),
          p0 = sum(.data$ods_ucz_prac %in% 0) / nka,
          czesc = sum(.data$ods_ucz_prac > 0 & .data$ods_ucz_prac < 1) / nka,
          p100 = sum(.data$ods_ucz_prac %in% 1) / nka) %>%
        as.list() %>%
        return()
    } else {
      return(list(n = 0))
    }
  } else {
    nucz = x %>%
      filter(.data$nauka2 %in% 0) %>%
      count(.data$okres, .data$id_abs) %>% 
      count(.data$id_abs, name = "l_mies_nucz")
    
    nucz_prac = x %>%
      filter(.data$nauka2 %in% 0 & .data$praca != 0) %>%
      count(.data$id_abs, name = "l_mies_nucz_prac")
    
    nka = n_distinct(nucz$id_abs)
    
    if (nrow(nucz) > 0) {
      nucz %>%
        full_join(nucz_prac, by = "id_abs") %>%
        mutate(ods_nucz_prac = ifelse(is.na(.data$l_mies_nucz_prac), 0, round(.data$l_mies_nucz_prac / .data$l_mies_nucz, 2))) %>%
        summarise(
          n = n_distinct(.data$id_abs),
          srednia = mean(.data$ods_nucz_prac, na.rm = TRUE),
          med = median(.data$ods_nucz_prac, na.rm = TRUE),
          p0 = sum(.data$ods_nucz_prac %in% 0) / nka,
          czesc = sum(.data$ods_nucz_prac > 0 & .data$ods_nucz_prac < 1) / nka,
          p100 = sum(.data$ods_nucz_prac %in% 1) / nka) %>%
        as.list() %>%
        return()
    } else {
      return(list(n = 0))
    }
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów wykonujących dane formy
#' pracy w danym miesiącu (domyślnie jest to grudzień). Wskaźnik może być
#' liczony albo dla absolwentów pracujących i kontunuujących naukę (\code{nauka
#' = TRUE}) lub dla absolwentów pracujących i nie kontunuujących nauki
#' (\code{nauka = FALSE}). Wyróżnione formy pracy:
#' \itemize{
#'  \item{\code{ucz_uop} - Zatrudnienie na podstawie UOP oraz brak innej formy},
#'  \item{\code{ucz_samoz} - Prowadzący działalność gosp. (samozatrudnienie)
#'  oraz brak innej formy},
#'  \item{\code{ucz_inna} - Tylko forma inna niż samozatrudnienie i niż umowa o
#'  pracę (np. umowa-zlecenie)},
#'  \item{\code{ucz_wiecej} - Więcej niż jedna forma, czyli: Zatrudnienie na
#'  podstawie UOP oraz prowadzący działalność gosp., Zatrudnienie na podstawie
#'  UOP oraz zatrudnienie w innej formie, Prowadzący działalność gosp.
#'  (samozatrudnienie) oraz zatrudnienie w innej formie, Zatrudnienie na
#'  podstawie UOP oraz działalność gosp. oraz zatrudnienie w innej formie}
#' }
#' @param x ramka danych pośrednich P3
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% filter .data summarise pull n_distinct
#' @export
Z8_formy_prac_mies = function(x, rok, mies = 12, nauka) {
  stopifnot(is.data.frame(x),
            rok %in% c(2021, 2022),
            mies %in% c(1:12),
            is.logical(nauka))
  
  x = x %>%
    filter(.data$okres %in% data_na_okres(mies, rok))
  
  if (nrow(x) %in% 0) {
    return(list(n = 0))
  } else {
    if (nauka) {
      nka = x %>%
        filter((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca %in% c(1:7)) %>% 
        pull(.data$id_abs) %>% 
        n_distinct()
      if (nka %in% 0) {
        return(list(n = 0, ucz_uop = 0, ucz_samoz = 0, ucz_inna = 0, ucz_wiecej = 0))
      } else {
        x %>%
          summarise(
            n = nka,
            ucz_uop = sum((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca %in% 1, na.rm = TRUE) / nka,
            ucz_samoz = sum((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca %in% 2, na.rm = TRUE) / nka,
            ucz_inna = sum((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca %in% 3, na.rm = TRUE) / nka,
            ucz_wiecej = sum((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca %in% c(4:7), na.rm = TRUE) / nka) %>% 
          as.list() %>%
          return()
      }
    } else {
      nka = x %>% 
        filter(.data$nauka2 %in% 0 & .data$praca %in% c(1:7)) %>% 
        pull(.data$id_abs) %>% 
        n_distinct()
      if (nka %in% 0) {
        return(list(n = 0, nieucz_uop = 0, nieucz_samoz = 0, nieucz_inna = 0, nieucz_wiecej = 0))
      } else {
        x %>%
          summarise(
            n = nka,
            nieucz_uop = sum(.data$nauka2 %in% 0 & .data$praca %in% 1, na.rm = TRUE) / nka,
            nieucz_samoz = sum(.data$nauka2 %in% 0 & .data$praca %in% 2, na.rm = TRUE) / nka,
            nieucz_inna = sum(.data$nauka2 %in% 0 & .data$praca %in% 3, na.rm = TRUE) / nka,
            nieucz_wiecej = sum(.data$nauka2 %in% 0 & .data$praca %in% c(4:7), na.rm = TRUE) / nka) %>% 
          as.list() %>%
          return()
      }
    }
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów, którzy w danym miesiącu
#' (domyślnie jest to wrzesień) kontynuowali zatrudnienie u pracodawcy, który
#' wcześniej (tj. kiedy byli jeszcze uczniami) zatrduniał ich jako pracowników
#' młodocianych. Wskaźnik liczony jest tylko dla absolwentów branżowych szkół 1.
#' stopnia. Wyróżnianych jest 5 form kontynuowania pracy u pracodawcy
#' zatrudniającego poprzednio jako młodocianego:
#' \itemize{
#'  \item{\code{nieucz_niekontuop} - Nieuczący się, nie kontynuujący pracy u
#'  danego pracodawcy}
#'  \item{\code{nieucz_kont_uop} - Nieuczący się, kontynuujący pracę u danego
#'  pracodawcy na podstawie umowy o pracę}
#'  \item{\code{nieucz_kont_inne} - Nieuczący się, kontynuujący pracę u danego
#'  pracodawcy w formie innej niż umowa  o pracę}
#'  \item{\code{ucz_niekontuop} - Uczący się, nie kontynuujący pracy u danego
#'  pracodawcy na podstawie umowy o pracę}
#'  \item{\code{ucz_kontuop} - Uczący się, kontynuujący pracę u danego
#'  pracodawcy na podstawie umowy o pracę}
#' }
#' @param x ramka danych pośrednich P3
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% filter summarise n_distinct
#' @export
Z9_kont_mlod = function(x, rok, mies = 9, nauka) {
  stopifnot(is.data.frame(x),
            rok %in% c(2021, 2022),
            mies %in% c(1:12),
            is.logical(nauka))
  
  if (any(unique(x$typ_szk) %in% "Branżowa szkoła I stopnia")) {
    x = x %>%
      filter(.data$okres %in% data_na_okres(mies, rok),
             !is.na(.data$kont_mlodoc_prac))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      if (nauka) {
        x = x %>% 
          filter((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & .data$praca %in% c(1:7))
        if (nrow(x) %in% 0) {
          return(list(n = 0))
        } else {
          nka = x %>%
            pull(.data$id_abs) %>% 
            n_distinct()
          
          x %>%
            summarise(
              n = nka,
              niekontuop = sum(.data$kont_mlodoc_prac %in% 4) / nka,
              kontuop = sum(.data$kont_mlodoc_prac %in% 5) / nka) %>%
            as.list() %>%
            return()
        }
      } else {
        x = x %>% 
          filter(.data$nauka2 %in% 0 & .data$praca %in% c(1:7))
        if (nrow(x) %in% 0) {
          return(list(n = 0))
        } else {
          nka = x %>% 
            pull(.data$id_abs) %>% 
            n_distinct()
          
          x %>%
            summarise(
              n = nka,
              niekontuop = sum(.data$kont_mlodoc_prac %in% 1) / nka,
              kont_uop = sum(.data$kont_mlodoc_prac %in% 2) / nka,
              kont_inne = sum(.data$kont_mlodoc_prac %in% 3) / nka) %>%
            as.list() %>%
            return()
        }
      }
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca średni względny miesięczny przychód z pracy
#' \emph{etatowej} w danym okresie w odniesieniu do zarobków w zamieszkiwanym
#' powiecie. Domyślnie jest to okres między wrześniem a grudniem roku
#' przekazanego w argumentach funkcji, ale te wartości można zmieniać. Średnia
#' liczona jest oddzielnie dla uczących i nie uczących się absolwentów.
#' @param x ramka danych pośrednich P3
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param od początek okresu wyliczania wskaźnika wyrażony miesiącem
#' @param do koniec okresu wyliczania wskaźnika wyrażony miesiącem
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% filter group_by summarise ungroup
#' n_distinct
#' @export
W3_sr_doch_uop = function(x, rok, od = 9, do = 12, nauka) {
  stopifnot(is.data.frame(x),
            rok %in% c(2021, 2022),
            od %in% c(1:12),
            do %in% c(1:12),
            is.logical(nauka))
  
  l_od = data_na_okres(rok = min(rok), mies = od)
  l_do = data_na_okres(rok = max(rok), mies = od)
  
  x = x %>%
    filter(.data$okres %in% seq(l_od, l_do, by = 1))
  
  if (nrow(x) > 0) {
    if (nauka) {
      x = x %>%
        filter((.data$nauka2 %in% 1 | .data$nauka_szk_abs %in% 1) & !is.na(.data$wynagrodzenie_uop) & !is.na(.data$powiat_sr_wynagrodzenie),
               .data$wynagrodzenie_uop > 0,
               .data$powiat_sr_wynagrodzenie > 0)
      
      dol = quantile(x$wynagrodzenie_uop, 0.01)
      gora = quantile(x$wynagrodzenie_uop, 0.99)
      
      x %>%
        filter(.data$wynagrodzenie_uop > dol & .data$wynagrodzenie_uop < gora) %>% 
        group_by(.data$id_abs, .data$okres) %>%
        summarise(
          rel_sred_ind_mies = .data$wynagrodzenie_uop / .data$powiat_sr_wynagrodzenie
        ) %>%
        ungroup() %>%
        group_by(.data$id_abs) %>%
        summarise(
          rel_sred_ind = mean(.data$rel_sred_ind_mies, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        summarise(
          n = n_distinct(.data$id_abs),
          sred = round(mean(.data$rel_sred_ind), 2),
          q5 = unname(round(quantile(.data$rel_sred_ind, 0.05), 2)),
          q25 = unname(round(quantile(.data$rel_sred_ind, 0.25), 2)),
          med = unname(round(quantile(.data$rel_sred_ind, 0.5), 2)),
          q75 = unname(round(quantile(.data$rel_sred_ind, 0.75), 2)),
          q95 = unname(round(quantile(.data$rel_sred_ind, 0.95), 2))) %>%
        as.list() %>%
        return()
    } else {
      x = x %>%
        filter(.data$nauka2 %in% 0 & !is.na(.data$wynagrodzenie_uop) & !is.na(.data$powiat_sr_wynagrodzenie),
               .data$wynagrodzenie_uop > 0,
               .data$powiat_sr_wynagrodzenie > 0) %>%
        group_by(.data$id_abs, .data$okres)
      
      dol = quantile(x$wynagrodzenie_uop, 0.01)
      gora = quantile(x$wynagrodzenie_uop, 0.99)
      
      x %>%
        filter(.data$wynagrodzenie_uop > dol & .data$wynagrodzenie_uop < gora) %>% 
        summarise(
          rel_sred_ind_mies = .data$wynagrodzenie_uop / .data$powiat_sr_wynagrodzenie
        ) %>%
        ungroup() %>%
        group_by(.data$id_abs) %>%
        summarise(
          rel_sred_ind = mean(.data$rel_sred_ind_mies, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        summarise(
          n = n_distinct(.data$id_abs),
          sred = round(mean(.data$rel_sred_ind), 2),
          q5 = unname(round(quantile(.data$rel_sred_ind, 0.05), 2)),
          q25 = unname(round(quantile(.data$rel_sred_ind, 0.25), 2)),
          med = unname(round(quantile(.data$rel_sred_ind, 0.5), 2)),
          q75 = unname(round(quantile(.data$rel_sred_ind, 0.75), 2)),
          q95 = unname(round(quantile(.data$rel_sred_ind, 0.95), 2))) %>%
        as.list() %>%
        return()
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca na potrzeby szablonu raportu rozkład liczby
#' miesięcy bezrobocia rejestrowanego wśród absolwentów w danym okresie.
#' Domyślnie jest to okres między wrześniem a grudniem roku przekazanego w
#' argumentach funkcji, ale te wartości można zmieniać.
#' @param x ramka danych pośrednich P3
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param od początek okresu wyliczania wskaźnika wyrażony miesiącem
#' @param do koniec okresu wyliczania wskaźnika wyrażony miesiącem
#' @return lista
#' @importFrom dplyr %>% filter .data group_by summarise ungroup count mutate
#' across
#' @export
B2_ods_bezrob = function(x, rok, od = 9, do = 12) {
  stopifnot(is.data.frame(x),
            rok %in% c(2021, 2022),
            od %in% c(1:12),
            do %in% c(1:12))
  
  l_od = data_na_okres(rok = min(rok), mies = od)
  l_do = data_na_okres(rok = max(rok), mies = od)
  
  x = x %>%
    filter(.data$okres %in% seq(l_od, l_do, by = 1)) %>%
    group_by(.data$id_abs, .data$okres) %>%
    summarise(
      l_mies_bezrob = sum(.data$bezrobocie %in% 1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(across(.data$l_mies_bezrob,
                  ~ifelse(. > 1, 1, .))) %>%
    group_by(.data$id_abs) %>%
    summarise(
      l_mies_bezrob = sum(.data$l_mies_bezrob %in% 1, na.rm = TRUE)
    ) %>%
    ungroup()
  
  ods = x %>%
    count(l_mies_bezrob) %>%
    mutate(value = n / sum(n)) %>%
    filter(l_mies_bezrob %in% 0:4)
  
  if (nrow(ods) != 5) {
    tab_uzup_szk = structure(tibble(
      l_mies_bezrob = setdiff(0:4, ods$l_mies_bezrob),
      n = as.integer(rep(0, 5 - nrow(ods))),
      value = rep(0, 5 - nrow(ods))
    ))
  }
  
  if (exists("tab_uzup_szk")) {
    ods = rbind(ods, tab_uzup_szk) %>%
      arrange(l_mies_bezrob) %>%
      as.list()
  } else {
    ods = as.list(ods)
  }
  
  descr = x %>%
    summarise(
      srednia = mean(.data$l_mies_bezrob),
      mediana = median(.data$l_mies_bezrob)
    ) %>%
    as.list()
  
  c(ods, descr) %>%
    return()
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności wyuczonych zawodów wśród
#' absolwentów w podziale na branże. Dodatkowo, funkcja liczy liczebności
#' absolwentów w branżach, a informacja ta służy jako podstawa do definiowania
#' warunków w szablonie raportu dla szkół branżowych 1. stopnia.
#' @details UWAGA: Wynikiem działania funkcji jest rozkład liczebności, w którym
#' nie odfiltrowuje się obserwacji poniżej 10 absolwentów, więc należy to zrobić
#' na poziomie szablonu raportu.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate slice_max
#' @export
liczebnosc_branze_ucz = function(x) {
  stopifnot(is.data.frame(x))
  
  if (any(unique(x$typ_szk) %in% "Branżowa szkoła I stopnia")) {
    x = x %>%
      filter(!(is.na(.data$branza)))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      n_dist = n_distinct(x$id_abs)
      
      tab = x %>%
        count(.data$branza) %>%
        mutate(odsetek = .data$n / n_dist) %>%
        slice_max(n = 10, order_by = .data$n, with_ties = FALSE)
      if (nrow(tab) %in% 0) {
        return(list(n = 0))
      } else {
        tab %>%
          as.list() %>%
          return()
      }
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę w szkołach branżowych 2. stopnia w podziale na branże. Dodatkowo,
#' funkcja liczy liczebności absolwentów w branżach, a informacja ta służy jako
#' podstawa do definiowania warunków w szablonie raportu dla szkół branżowych 1.
#' stopnia. Funkcja ma sens tylko dla absolwentów szkół branżowych 1. stopnia,
#' ponieważ tylko oni mogą kontynuować naukę w szkołach branżowych 2. stopnia.
#' @details UWAGA: Wynikiem działania funkcji jest rozkład liczebności, w którym
#' nie odfiltrowuje się obserwacji poniżej 10 absolwentów, więc należy to zrobić
#' na poziomie szablonu raportu.
#' @param x ramka danych pośrednich P3
#' @param branza_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej branży (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie
#' grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max
#' @export
liczebnosc_branze_kont = function(x, branza_kont_df, rok, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(branza_kont_df),
            rok %in% c(2021, 2022),
            mies %in% c(1:12))
  
  if (any(unique(x$typ_szk) %in% "Branżowa szkoła I stopnia")) {
    x = x %>%
      filter(.data$okres %in% data_na_okres(mies, rok)) %>% 
      left_join(branza_kont_df %>% 
                  select(id_abs, rok_abs, branza_kont)) %>%
      filter(!(is.na(.data$branza_kont)))
    
    n_dist = n_distinct(x$id_abs)
    
    tab = x %>%
      count(.data$branza_kont) %>%
      mutate(odsetek = .data$n / n_dist) %>%
      slice_max(n = 10, order_by = .data$n, with_ties = FALSE)
    if (nrow(tab) %in% 0) {
      return(list(n = 0))
    } else {
      return(as.list(tab))
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę na studiach w podziale na dziedziny. Funkcja liczy wskaźnik tylko dla
#' absolwentów techników i liceów ogólnokształcących.
#' @param x ramka danych pośrednich P3
#' @param dziedzina_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dziedzinie (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie jest
#' to grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max
#' @export
liczebnosc_dziedziny = function(x, dziedzina_kont_df, rok, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(dziedzina_kont_df),
            rok %in% c(2021, 2022),
            mies %in% c(1:12))
  
  if (any(unique(x$typ_szk) %in% c("Technikum", "Liceum ogólnokształcące", "Branżowa szkoła II stopnia"))) {
    
    dziedzina_kont_df = dziedzina_kont_df %>%
      select(id_abs, rok_abs, dziedzina_kont)
    
    x = x %>%
      filter(.data$okres %in% data_na_okres(mies, rok)) %>%
      left_join(dziedzina_kont_df,
                by = c("id_abs", "rok_abs")) %>%
      filter(.data$nauka_studia %in% 1) %>%
      filter(!(is.na(.data$dziedzina_kont)))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      n_dist = n_distinct(x$id_abs)
      
      tab = x %>%
        count(.data$dziedzina_kont) %>%
        mutate(odsetek = .data$n / n_dist)
      if (nrow(tab) %in% 0) {
        return(list(n = 0))
      } else {
        tab %>%
          as.list() %>%
          return()
      }
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę na studiach w podziale na dyscypliny. Funkcja liczy wskaźnik tylko dla
#' absolwentów techników i liceów ogólnokształcących.
#' @param x ramka danych pośrednich P3
#' @param dyscyplina_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dyscyplinie (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie jest
#' to grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max
#' @export
liczebnosc_dyscypliny = function(x, dyscyplina_kont_df, rok, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(dyscyplina_kont_df),
            rok %in% c(2021, 2022),
            mies %in% c(1:12))
  
  if (any(unique(x$typ_szk) %in% c("Technikum", "Liceum ogólnokształcące", "Branżowa szkoła II stopnia"))) {
    
    dyscyplina_kont_df = dyscyplina_kont_df %>%
      select(id_abs, rok_abs, dyscyplina_wiodaca_kont)
    
    x = x %>%
      filter(.data$okres %in% data_na_okres(mies, rok)) %>%
      left_join(dyscyplina_kont_df,
                by = c("id_abs", "rok_abs")) %>%
      filter(.data$nauka_studia %in% 1) %>%
      filter(!(is.na(.data$dyscyplina_wiodaca_kont)))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      n_dist = n_distinct(x$id_abs)
      
      tab = x %>%
        count(.data$dyscyplina_wiodaca_kont) %>%
        mutate(odsetek = .data$n / n_dist)
      if (nrow(tab) %in% 0) {
        return(list(n = 0))
      } else {
        tab %>%
          as.list() %>%
          return()
      }
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę na studiach w podziale na dyscypliny i zawody - wynik działania funkcji
#' jest wsadem do tabeli krzyżowej dyscypliny przez zawody w raporcie. Funkcja
#' liczy wskaźnik tylko dla absolwentów techników i liceów ogólnokształcących.
#' Wskaźnik liczony jest tylko dla zawodów, w których uczyło się więcej niż 10
#' absolwentów (n>=10).
#' @param x ramka danych pośrednich P3
#' @param dyscyplina_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dyscyplinie (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie jest
#' to grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data select left_join count mutate group_by
#' ungroup rowwise across cur_column
#' @importFrom tidyr pivot_wider
#' @export
dyscypliny_zawody = function(x, dyscyplina_kont_df, rok, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(dyscyplina_kont_df),
            rok %in% c(2021, 2022),
            mies %in% c(1:12))
  
  if (any(unique(x$typ_szk) %in% c("Technikum", "Szkoła policealna", "Branżowa szkoła II stopnia"))) {
    
    dyscyplina_kont_df = dyscyplina_kont_df %>%
      select(id_abs, dyscyplina_wiodaca_kont)
    
    x = x %>%
      filter(.data$okres %in% data_na_okres(mies, rok)) %>%
      left_join(dyscyplina_kont_df,
                by = c("id_abs")) %>%
      filter(.data$nauka_studia %in% 1) %>%
      filter(!(is.na(.data$dyscyplina_wiodaca_kont)))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      nki = x %>% 
        count(.data$nazwa_zaw) %>% 
        filter(n >= 10) %>% 
        as.list()
      
      tab = x %>%
        filter(.data$nazwa_zaw %in% unique(nki$nazwa_zaw)) %>% 
        group_by(.data$dyscyplina_wiodaca_kont) %>%
        count(.data$nazwa_zaw) %>% 
        ungroup()
      if (nrow(tab) %in% 0) {
        return(list(n = 0))
      } else {
        tab %>%
          pivot_wider(names_from = nazwa_zaw, values_from = n, values_fill = 0) %>% 
          rowwise() %>% 
          mutate(across(2:ncol(.),
                        ~sum(.) / nki$n[nki$nazwa_zaw %in% cur_column()])) %>% 
          ungroup() %>% 
          as.list() %>%
          return()
      }
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę na studiach w podziale na dyscypliny i zawody - wynik działania funkcji
#' jest wsadem do tabeli krzyżowej dyscypliny przez zawody w raporcie. Funkcja
#' liczy wskaźnik tylko dla absolwentów techników i branżowych szkół I stopnia.
#' Wskaźnik liczony jest dla wszystkich zawodów, niezależnie od liczebności.
#' @param x ramka danych pośrednich P3
#' @param branza_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dyscyplinie (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie jest
#' to grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data select left_join count mutate group_by
#' ungroup rowwise across cur_column
#' @importFrom tidyr pivot_wider
#' @export
branze_zawody = function(x, branza_kont_df, rok, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(branza_kont_df),
            rok %in% c(2021, 2022),
            mies %in% c(1:12))
  
  if (any(unique(x$typ_szk) %in% c("Technikum", "Branżowa szkoła I stopnia", "Branżowa szkoła II stopnia"))) {
    
    branza_kont_df = branza_kont_df %>%
      select(id_abs, branza_kont)
    
    x = x %>%
      filter(.data$okres %in% data_na_okres(mies, rok)) %>%
      left_join(branza_kont_df,
                by = c("id_abs")) %>%
      filter(!(is.na(.data$branza_kont)))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      nki = x %>% 
        count(.data$nazwa_zaw) %>% 
        as.list()
      
      tab = x %>%
        filter(.data$nazwa_zaw %in% unique(nki$nazwa_zaw)) %>% 
        group_by(.data$branza_kont) %>%
        count(.data$nazwa_zaw) %>% 
        ungroup()
      if (nrow(tab) %in% 0) {
        return(list(n = 0))
      } else {
        tab %>%
          pivot_wider(names_from = nazwa_zaw, values_from = n, values_fill = 0) %>% 
          rowwise() %>% 
          mutate(across(2:ncol(.),
                        ~sum(.) / nki$n[nki$nazwa_zaw %in% cur_column()])) %>% 
          ungroup() %>% 
          as.list() %>%
          return()
      }
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności w zawodach na potrzeby
#' raportu wojewódzko-branżowego.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>% filter .data n_distinct count mutate
#' @export
licz_zawody = function(x) {
  stopifnot(is.data.frame(x))
  
  if (!any(unique(x$typ_szk) %in% "Liceum ogólnokształcące")) {
    
    x = x %>%
      filter(!(is.na(.data$nazwa_zaw)))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      n_dist = n_distinct(x$id_abs)
      
      tab = x %>%
        count(.data$nazwa_zaw) %>%
        mutate(odsetek = .data$n / n_dist)
      
      tab %>%
        as.list() %>%
        return()
    }
  } else {
    return(list(n = 0))
  }
}
#' @title Obliczanie wskaźników dla 3. edycji monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów danej płci
#' kontynuujących naukę na studiach w podziale na dyscypliny. Wskaźnik liczony
#' jest tylko dla absolwentów techników i liceów ogólnokształcących.
#' @param x ramka danych pośrednich P3
#' @param dyscyplina_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dyscyplinie (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie jest
#' to grudzień
#' @param plec płeć absolwenta przekazana jako wartość tekstowa ("K" lub "M")
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max
#' @export
liczebnosc_dyscypliny_plec = function(x, dyscyplina_kont_df, rok, mies = 12, plec) {
  stopifnot(is.data.frame(x),
            is.data.frame(dyscyplina_kont_df),
            "plec" %in% names(dyscyplina_kont_df),
            is.character(plec),
            plec %in% c("K", "M"),
            rok %in% c(2021, 2022),
            mies %in% c(1:12))
  
  if (any(unique(x$typ_szk) %in% c("Technikum", "Liceum ogólnokształcące", "Branżowa szkoła II stopnia"))) {
    
    dyscyplina_kont_df = dyscyplina_kont_df %>%
      select(id_abs, rok_abs, dyscyplina_wiodaca_kont, plec)
    
    x = x %>%
      filter(.data$okres %in% data_na_okres(mies, rok)) %>%
      left_join(dyscyplina_kont_df,
                by = c("id_abs", "rok_abs")) %>%
      filter(.data$plec %in% plec) %>% 
      filter(.data$nauka_studia %in% 1) %>%
      filter(!(is.na(.data$dyscyplina_wiodaca_kont)))
    
    if (nrow(x) %in% 0) {
      return(list(n = 0))
    } else {
      n_dist = n_distinct(x$id_abs)
      
      tab = x %>%
        count(.data$dyscyplina_wiodaca_kont) %>%
        mutate(odsetek = .data$n / n_dist)
      if (nrow(tab) %in% 0) {
        return(list(n = 0))
      } else {
        tab %>%
          as.list() %>%
          return()
      }
    }
  } else {
    return(list(n = 0))
  }
}
