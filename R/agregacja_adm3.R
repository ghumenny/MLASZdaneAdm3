#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 3. edycji
#' monitoringu na danych administracyjnych
#' @description Funkcja obliczająca wskaźniki na poziomie zagregowanym na
#' potrzeby raportów "1rokpo" (po roku od ukończenia szkoły).
#' @param wsk2 ramka danych z tabeli pośredniej nr 2 (P2) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param podzial_grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param rok_abso rok, w którym grupa absolwentów uzyskała status absolwenta.
#' Może to być więcej niż 1 wartość.
#' @return list
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{dane_szkoly}},}
#'  \item{\code{\link{l_abs}},}
#'  \item{\code{\link{l_kobiet}},}
#'  \item{\code{\link{l_abs_zrodla}},}
#'  \item{\code{\link{S3_mies}},}
#'  \item{\code{\link{zawody_S3}},}
#'  \item{\code{\link{E2_nauka_kontyn}},
#'  \item{\code{\link{Z4_ods_prac_mies}},}
#'  \item{\code{\link{Z8_formy_prac_mies}},}
#'  \item{\code{\link{Z9_kont_mlod}},}
#'  \item{\code{\link{W3_sr_doch_uop}},}
#'  \item{\code{\link{B2_ods_bezrob}},}
#'  \item{\code{\link{liczebnosc_branze_ucz}},}
#'  \item{\code{\link{liczebnosc_branze_kont}},}
#'  \item{\code{\link{liczebnosc_dyscypliny}},}
#'  \item{\code{\link{liczebnosc_dziedziny}},}
#'  \item{\code{\link{dyscypliny_zawody}},}
#'  \item{\code{\link{branze_zawody}},}
#'  \item{\code{\link{licz_zawody}},}
#'  \item{\code{\link{liczebnosc_dyscypliny_plec}}}
#' }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_1rokpo_adm3 = function(wsk2, wsk3, wsk4, podzial_grupy, rok_abso) {
  stopifnot(is.data.frame(wsk2),
            is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(podzial_grupy),
            rok_abso %in% c(2021, 2022) & length(rok_abso) %in% 1,
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk2),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk3),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk4))
  
  log_file = paste0("agreguj_1rokpo_logfile_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
  sink(log_file)
  cat("\nStart: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  
  wsk4 = wsk4 %>%
    filter(.data$rok_abs %in% (rok_abso))
  wsk3 = wsk3 %>%
    filter(.data$rok_abs %in% (rok_abso))
  wsk2 = wsk2 %>%
    filter(.data$rok_abs %in% (rok_abso))
  
  cat("\nWskaźniki wykorzystujące P4: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4 = agreguj_wskazniki(
    wsk4, podzial_grupy,
    dane_szkoly = dane_szkoly(.data),
    l_abs = l_abs(.data),
    l_kobiet = l_kobiet(.data),
    l_abs_zrodla = l_abs_zrodla(.data),
    liczebnosc_branze_ucz = liczebnosc_branze_ucz(.data))
  
  cat("\nWskaźniki wykorzystujące P3: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_3 = agreguj_wskazniki(
    wskazniki = wsk3, grupy = podzial_grupy,
    przekazArgumenty = list("rok_abso" = rok_abso, "wsk2" = wsk2),
    S3_01 = S3_mies(.data, min(rok_abso), 1, max(rok_abso), 1),
    S3_02 = S3_mies(.data, min(rok_abso), 2, max(rok_abso), 2),
    S3_03 = S3_mies(.data, min(rok_abso), 3, max(rok_abso), 3),
    S3_04 = S3_mies(.data, min(rok_abso), 4, max(rok_abso), 4),
    S3_05 = S3_mies(.data, min(rok_abso), 5, max(rok_abso), 5),
    S3_06 = S3_mies(.data, min(rok_abso), 6, max(rok_abso), 6),
    S3_07 = S3_mies(.data, min(rok_abso), 7, max(rok_abso), 7),
    S3_08 = S3_mies(.data, min(rok_abso), 8, max(rok_abso), 8),
    S3_09 = S3_mies(.data, min(rok_abso), 9, max(rok_abso), 9),
    S3_10 = S3_mies(.data, min(rok_abso), 10, max(rok_abso), 10),
    S3_11 = S3_mies(.data, min(rok_abso), 11, max(rok_abso), 11),
    S3_12 = S3_mies(.data, min(rok_abso), 12, max(rok_abso), 12),
    tab_s3_zaw = zawody_S3(.data, min(rok_abso), 12, max(rok_abso), 12),
    E2_nauka_kontyn = E2_nauka_kontyn(.data, rok_abso, 12),
    Z4_ucz = Z4_ods_prac_mies(.data, min(rok_abso), 9, max(rok_abso), 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies(.data, min(rok_abso), 9, max(rok_abso), 12, FALSE),
    Z8_formy_ucz = Z8_formy_prac_mies(.data, rok_abso, 12, TRUE),
    Z8_formy_nie_ucz = Z8_formy_prac_mies(.data, rok_abso, 12, FALSE),
    Z9_mlod_ucz = Z9_kont_mlod(.data, rok_abso, 9, TRUE),
    Z9_mlod_nie_ucz = Z9_kont_mlod(.data, rok_abso, 9, FALSE),
    W3_ucz = W3_sr_doch_uop(.data, rok_abso, 9, 12, TRUE),
    W3_nie_ucz = W3_sr_doch_uop(.data, rok_abso, 9, 12, FALSE),
    B2_bezrob = B2_ods_bezrob(.data, rok_abso, 9, 12),
    liczebnosc_branze_kont = liczebnosc_branze_kont(.data, wsk2, rok_abso, 12),
    liczebnosc_dziedziny = liczebnosc_dziedziny(.data, wsk2, rok_abso, 12),
    liczebnosc_dyscypliny = liczebnosc_dyscypliny(.data, wsk2, rok_abso, 12),
    dyscypliny_zawody = dyscypliny_zawody(.data, wsk2, rok_abso, 12),
    branze_zawody = branze_zawody(.data, wsk2, rok_abso, 12),
    dyscypliny_kob = liczebnosc_dyscypliny_plec(.data, wsk2, rok_abso, 12, "K"),
    dyscypliny_mez = liczebnosc_dyscypliny_plec(.data, wsk2, rok_abso, 12, "M")
  )
  
  cat("\nŁączenie wskaźników: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4$grupy = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(podzial_grupy))
  wskazniki_4$grupyOdniesienia = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(podzial_grupy))
  
  wskazniki = list(grupy = wskazniki_4$grupy, grupyOdniesienia = wskazniki_4$grupyOdniesienia)
  cat("\nKoniec: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  sink()
  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 3. edycji
#' monitoringu na danych administracyjnych
#' @description Funkcja obliczająca wskaźniki na poziomie zagregowanym na
#' potrzeby Aneksu w raportach "1rokpo" - liczy mniej wskaźników niż
#' \code{\link{agreguj_1rokpo_adm_2}}
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param podzial_grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param rok_abso rok, w którym grupa absolwentów uzyskała status absolwenta.
#' Może to być więcej niż 1 wartość.
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{l_abs}},}
#'  \item{\code{\link{l_kobiet}},}
#'  \item{\code{\link{S3_mies}},}
#'  \item{\code{\link{zawody_S3}},}
#'  \item{\code{\link{E2_nauka_kontyn}},}
#'  \item{\code{\link{Z4_ods_prac_mies}},}
#'  \item{\code{\link{Z8_formy_prac_mies}},}
#'  \item{\code{\link{B2_ods_bezrob}},}
#'  \item{\code{\link{licz_zawody}}}
#' }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_aneks_1rokpo_adm3 = function(wsk3, wsk4, podzial_grupy, rok_abso) {
  stopifnot(is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(podzial_grupy),
            rok_abso %in% c(2021, 2022) & length(rok_abso) %in% 1,
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj") %in% names(wsk3),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj") %in% names(wsk4))
  
  log_file = paste0("agreguj_aneks_1rokpo_logfile_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
  sink(log_file)
  cat("\nStart: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  
  wsk4 = wsk4 %>%
    filter(.data$rok_abs %in% (rok_abso))
  wsk3 = wsk3 %>%
    filter(.data$rok_abs %in% (rok_abso))

  cat("\nWskaźniki wykorzystujące P4: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4 = agreguj_wskazniki(
    wsk4, podzial_grupy,
    l_abs = l_abs(.data),
    l_kobiet = l_kobiet(.data))
  
  cat("\nWskaźniki wykorzystujące P3: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_3 = agreguj_wskazniki(
    wskazniki = wsk3, grupy = podzial_grupy,
    przekazArgumenty = list("rok_abso" = rok_abso),
    S3_12 = S3_mies(.data, min(rok_abso), 12, max(rok_abso), 12),
    tab_s3_zaw = zawody_S3(.data, min(rok_abso), 12, max(rok_abso), 12),
    E2_nauka_kontyn = E2_nauka_kontyn(.data, rok_abso, 12),
    Z4_ucz = Z4_ods_prac_mies(.data, min(rok_abso), 9, max(rok_abso), 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies(.data, min(rok_abso), 9, max(rok_abso), 12, FALSE),
    Z8_formy_ucz = Z8_formy_prac_mies(.data, rok_abso, 12, TRUE),
    Z8_formy_nie_ucz = Z8_formy_prac_mies(.data, rok_abso, 12, FALSE),
    B2_bezrob = B2_ods_bezrob(.data, rok_abso, 9, 12)
  )
  
  cat("\nŁączenie wskaźników: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4$grupy = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(podzial_grupy))
  wskazniki_4$grupyOdniesienia = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(podzial_grupy))
  
  wskazniki = list(grupy = wskazniki_4$grupy, grupyOdniesienia = wskazniki_4$grupyOdniesienia)
  cat("\nKoniec: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  sink()
  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 3. edycji
#' monitoringu na danych administracyjnych
#' @description Funkcja obliczająca wskaźniki na poziomie zagregowanym na
#' potrzeby tabeli prezentującej wskaźnik S3 w podziale na zawody w raportach
#' "1rokpo" - liczy mniej wskaźników niż \code{\link{agreguj_1rokpo_adm_2}}
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param podzial_grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param rok_abso rok, w którym grupa absolwentów uzyskała status absolwenta.
#' Może to być więcej niż 1 wartość.
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{l_abs}},}
#'  \item{\code{\link{S3_mies}},}
#'  \item{\code{\link{zawody_S3}},}
#'  \item{\code{\link{licz_zawody}}}
#' }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_szkozaw_1rokpo_adm3 = function(wsk3, wsk4, podzial_grupy, rok_abso) {
  stopifnot(is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(podzial_grupy),
            rok_abso %in% c(2021, 2022) & length(rok_abso) %in% 1,
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj") %in% names(wsk3),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj") %in% names(wsk4))
  
  log_file = paste0("agreguj_szkozaw_1rokpo_logfile_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
  sink(log_file)
  cat("\nStart: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  
  wsk4 = wsk4 %>%
    filter(.data$rok_abs %in% (rok_abso))
  wsk3 = wsk3 %>%
    filter(.data$rok_abs %in% (rok_abso))

  cat("\nWskaźniki wykorzystujące P4: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4 = agreguj_wskazniki(
    wsk4, podzial_grupy,
    l_abs = l_abs(.data),
    licz_zawody = licz_zawody(.data))
  
  cat("\nWskaźniki wykorzystujące P3: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_3 = agreguj_wskazniki(
    wskazniki = wsk3, grupy = podzial_grupy,
    przekazArgumenty = list("rok_abso" = rok_abso),
    S3_12 = S3_mies(.data, min(rok_abso), 12, max(rok_abso), 12),
    tab_s3_zaw = zawody_S3(.data, min(rok_abso), 12, max(rok_abso), 12)
  )
  
  cat("\nŁączenie wskaźników: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4$grupy = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(podzial_grupy))
  wskazniki_4$grupyOdniesienia = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(podzial_grupy))
  
  wskazniki = list(grupy = wskazniki_4$grupy, grupyOdniesienia = wskazniki_4$grupyOdniesienia)
  cat("\nKoniec: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  sink()
  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 3. edycji
#' monitoringu na danych administracyjnych
#' @description Funkcja obliczająca wskaźniki na poziomie zagregowanym na
#' potrzeby raportów branżowo-wojewódzkich
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param podzial_grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param rok_abso rok, w którym grupa absolwentów uzyskała status absolwenta.
#' Może to być więcej niż 1 wartość.
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{dane_szkoly}},}
#'  \item{\code{\link{l_abs}},}
#'  \item{\code{\link{l_kobiet}},}
#'  \item{\code{\link{l_abs_zrodla}},}
#'  \item{\code{\link{liczebnosc_zawody}},}
#'  \item{\code{\link{S3_mies}},}
#'  \item{\code{\link{zawody_S3}},}
#'  \item{\code{\link{E2_nauka_kontyn}},}
#'  \item{\code{\link{liczebnosc_branze_ucz}},}
#'  \item{\code{\link{liczebnosc_branze_kont}},}
#'  \item{\code{\link{Z4_ods_prac_mies}},}
#'  \item{\code{\link{Z8_formy_prac_mies}},}
#'  \item{\code{\link{Z9_kont_mlod}},}
#'  \item{\code{\link{W3_sr_doch_uop}},}
#'  \item{\code{\link{B2_ods_bezrob}},}
#'  \item{\code{\link{licz_zawody}},}
#'  \item{\code{\link{liczebnosc_dyscypliny}},}
#'  \item{\code{\link{liczebnosc_dziedziny}},}
#'  \item{\code{\link{dyscypliny_zawody}},}
#'  \item{\code{\link{branze_zawody}}}
#' }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_woj_branz_adm3 = function(wsk3, wsk4, podzial_grupy, rok_abso) {
  stopifnot(is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(podzial_grupy),
            rok_abso %in% c(2021, 2022) & length(rok_abso) %in% 1,
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk3),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk4))
  
  log_file = paste0("agreguj_wojbranz_1rokpo_logfile_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
  sink(log_file)
  cat("\nStart: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  
  wsk4 = wsk4 %>%
    filter(.data$rok_abs %in% (rok_abso))
  wsk3 = wsk3 %>%
    filter(.data$rok_abs %in% (rok_abso))

  cat("\nWskaźniki wykorzystujące P4: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4 = agreguj_wskazniki(
    wsk4, podzial_grupy,
    dane_szkoly = dane_szkoly(.data),
    l_abs = l_abs(.data),
    l_kobiet = l_kobiet(.data),
    l_abs_zrodla = l_abs_zrodla(.data),
    liczebnosc_branze_ucz = liczebnosc_branze_ucz(.data),
    liczebnosc_zawody = liczebnosc_zawody(.data))
  
  cat("\nWskaźniki wykorzystujące P3: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_3 = agreguj_wskazniki(
    wsk3, podzial_grupy, list("rok_abso" = rok_abso),
    S3_01 = S3_mies(.data, min(rok_abso), 1, max(rok_abso), 1),
    S3_02 = S3_mies(.data, min(rok_abso), 2, max(rok_abso), 2),
    S3_03 = S3_mies(.data, min(rok_abso), 3, max(rok_abso), 3),
    S3_04 = S3_mies(.data, min(rok_abso), 4, max(rok_abso), 4),
    S3_05 = S3_mies(.data, min(rok_abso), 5, max(rok_abso), 5),
    S3_06 = S3_mies(.data, min(rok_abso), 6, max(rok_abso), 6),
    S3_07 = S3_mies(.data, min(rok_abso), 7, max(rok_abso), 7),
    S3_08 = S3_mies(.data, min(rok_abso), 8, max(rok_abso), 8),
    S3_09 = S3_mies(.data, min(rok_abso), 9, max(rok_abso), 9),
    S3_10 = S3_mies(.data, min(rok_abso), 10, max(rok_abso), 10),
    S3_11 = S3_mies(.data, min(rok_abso), 11, max(rok_abso), 11),
    S3_12 = S3_mies(.data, min(rok_abso), 12, max(rok_abso), 12),
    tab_s3_zaw = zawody_S3(.data, min(rok_abso), 12, max(rok_abso), 12),
    Z4_ucz = Z4_ods_prac_mies(.data, min(rok_abso), 9, max(rok_abso), 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies(.data, min(rok_abso), 9, max(rok_abso), 12, FALSE),
    W3_ucz = W3_sr_doch_uop(.data, rok_abso, 9, 12, TRUE),
    W3_nie_ucz = W3_sr_doch_uop(.data, rok_abso, 9, 12, FALSE),
    B2_bezrob = B2_ods_bezrob(.data, rok_abso, 9, 12)
  )
  
  cat("\nŁączenie wskaźników: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  wskazniki_4$grupy = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(podzial_grupy))
  wskazniki_4$grupyOdniesienia = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(podzial_grupy))
  
  wskazniki = list(grupy = wskazniki_4$grupy, grupyOdniesienia = wskazniki_4$grupyOdniesienia)
  cat("\nKoniec: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
  sink()
  return(wskazniki)
}
