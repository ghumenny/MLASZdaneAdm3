# # tu będą funkcje robocze - od SFTP do danych do zbioru pod agregację
# 
# czyszczenie_
# 
# 
# 
# library(bit64)
# p3$wynagrodzenie = as.numeric(p3$wynagrodzenie)
# p3$wynagrodzenie_uop = as.numeric(p3$wynagrodzenie_uop)
# p5$wynagrodzenie = as.numeric(p5$wynagrodzenie)
# p5$wynagrodzenie_uop = as.numeric(p5$wynagrodzenie_uop)
# 
# 
# p4 = p4 %>% 
#   mutate(across(teryt_woj_szk,
#                 ~floor(as.numeric(.) / 10000))) %>% 
#   mutate(woj_nazwa = case_when(
#     teryt_woj_szk %in% 2 ~ "dolnośląskie",
#     teryt_woj_szk %in% 4 ~ "kujawsko-pomorskie",
#     teryt_woj_szk %in% 6 ~ "lubelskie",
#     teryt_woj_szk %in% 8 ~ "lubuskie",
#     teryt_woj_szk %in% 10 ~ "łódzkie",
#     teryt_woj_szk %in% 12 ~ "małopolskie",
#     teryt_woj_szk %in% 14 ~ "mazowieckie",
#     teryt_woj_szk %in% 16 ~ "opolskie",
#     teryt_woj_szk %in% 18 ~ "podkarpackie",
#     teryt_woj_szk %in% 20 ~ "podlaskie",
#     teryt_woj_szk %in% 22 ~ "pomorskie",
#     teryt_woj_szk %in% 24 ~ "śląskie",
#     teryt_woj_szk %in% 26 ~ "świętokrzyskie",
#     teryt_woj_szk %in% 28 ~ "warmińsko-mazurskie",
#     teryt_woj_szk %in% 30 ~ "wielkopolskie",
#     teryt_woj_szk %in% 32 ~ "zachodniopomorskie")) %>% 
#   left_join(podregiony,
#             by = c("teryt_pow_szk" = "teryt_pow"))
# 
# # rozbicie na lo i lodd
# 
# lo_lodd = p4 %>% 
#   select(id_szk, typ_szk) %>% 
#   distinct() %>% 
#   filter(typ_szk %in% "Liceum ogólnokształcące") %>% 
#   left_join(rspo %>% select(id_rspo, kategoria_uczniow),
#             by = c("id_szk" = "id_rspo")) %>% 
#   mutate(typ_szk_new = ifelse(kategoria_uczniow %in% "Dorośli",
#                               "Liceum dla dorosłych",
#                               typ_szk)) %>% 
#   select(id_szk, typ_szk_new)
# 
# p4 = p4 %>% 
#   left_join(lo_lodd) %>% 
#   mutate(across(typ_szk,
#                 ~ifelse(. %in% "Liceum ogólnokształcące",
#                         typ_szk_new,
#                         .))) %>% 
#   select(-typ_szk_new)
# 
# p4$typ_szk[p4$typ_szk %in% "Bednarska Szkoła Realna"] = "Liceum ogólnokształcące"
# 
# table(p4$typ_szk, useNA = "always")
# 
# 
# # Czyszczę zmienne z nazwami branż i dodaję do każdej z tabel zmienne potrzebne do tworzenia grup.
# 
# p4$branza = gsub("ceramiczna-szklarska", "ceramiczno-szklarska", p4$branza)
# p4$branza = gsub("^branża ", "", p4$branza)
# 
# p1 = p1 %>% 
#   left_join(p4 %>% select(id_abs, rok_abs, id_szk, typ_szk, teryt_woj_szk, teryt_pow_szk, podregion), # tu już mam branżę
#             by = c("id_abs", "rok_abs"))
# p1$branza = gsub("ceramiczna-szklarska", "ceramiczno-szklarska", p1$branza)
# p1$branza = gsub("^branża ", "", p1$branza)
# 
# p2 = p2 %>% 
#   left_join(p4 %>% select(id_abs, rok_abs, id_szk, typ_szk, teryt_woj_szk, teryt_pow_szk, podregion), # tu już mam branżę
#             by = c("id_abs", "rok_abs"))
# 
# p2$branza = gsub("ceramiczna-szklarska", "ceramiczno-szklarska", p2$branza)
# p2$branza = gsub("^branża ", "", p2$branza)
# p2$dziedzina_kont = gsub("^Dziedzina ", "", p2$dziedzina_kont)
# p2$branza_kont = gsub("^branża ", "", p2$branza_kont)
# p2$branza_kont = gsub("ceramiczna-szklarska", "ceramiczno-szklarska", p2$branza_kont)
# 
# p3 = p3 %>% 
#   left_join(p4 %>% select(id_abs, rok_abs, id_szk, typ_szk, teryt_woj_szk, teryt_pow_szk, branza, nazwa_zaw, podregion),
#             by = c("id_abs", "rok_abs")) %>% 
#   mutate(rok = floor((okres - 1) / 12))
# 
# p5 = p5 %>% 
#   left_join(p4 %>% select(id_abs, rok_abs, id_szk, typ_szk, teryt_woj_szk, teryt_pow_szk, branza, nazwa_zaw, podregion),
#             by = c("id_abs", "rok_abs")) %>% 
#   mutate(
#     okres_rok = ceiling(okres / 12),
#     okres_mies = okres %% 12
#   )
# 
# # rspo
# 
# rspo$Nazwa = gsub("\\\u2028", " ", rspo$Nazwa)
# 
# adresy = rspo %>% 
#   select(id_rspo, adres_szk = szk_adres, nazwa_szk = Nazwa)
# 
# p4 = p4 %>% 
#   left_join(adresy, by = c("id_szk" = "id_rspo")) %>% 
#   mutate(across(c("nazwa_szk", "adres_szk"),
#                 ~ifelse(is.na(.), "", .)))
# 
# sum(is.na(p4$nazwa_szk))
# sum(is.na(p4$adres_szk))
# 
# 
# 
# # p2 = p2 %>% rename(teryt_woj = teryt_woj_szk)
# # p3 = p3 %>% rename(teryt_woj = teryt_woj_szk)
# # p4 = p4 %>% rename(teryt_woj = teryt_woj_szk)
