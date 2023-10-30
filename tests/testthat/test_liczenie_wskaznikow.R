################################################################################
### setup ######################################################################
################################################################################

library(MLASZdaneAdm3) 

################################################################################
### Testowanie funkcji używających tabel P4 ####################################
################################################################################

# p4 = 
lista = list()

test_that("dane_szkoly() zwraca obiekt klasy list o długości 2", {
  expect_identical(class(lista), class(dane_szkoly(p4)))
  expect_error(expect_identical(length(dane_szkoly(p4)), 2))
})









# df = data.frame()
# 
# rspo_raw1 = rspo:::get_df_api(page_number = sample(100, 1))
# rspo_raw2 = rspo:::get_df_api(page_number = sample(100, 1))
# rspo_raw3 = rspo:::get_df_api(page_number = sample(100, 1))
# 
# test_that("get_df_api() zwraca obiekt klasy data.frame", {
#   expect_identical(class(df), class(rspo_raw1))
#   expect_identical(class(df), class(rspo_raw2))
#   expect_identical(class(df), class(rspo_raw3))
#   
#   expect_error(expect_identical(class(rspo_raw1), "list"))
#   expect_error(expect_identical(class(rspo_raw2), "list"))
#   expect_error(expect_identical(class(rspo_raw3), "list"))
# })
# 
# # zakładam, że w pierwszych 100 stronach będę miał maksymalną liczbę obserwacji
# test_that("get_df_api() zwraca data.frame o liczbie wierszy 100", {
#   expect_equal(nrow(rspo_raw1), expected = 100)
#   expect_equal(nrow(rspo_raw2), expected = 100)
#   expect_equal(nrow(rspo_raw3), expected = 100)
# })
# 
# rm(list = c("df", "rspo_raw1", "rspo_raw2", "rspo_raw3"))

################################################################################
### Testowanie funkcji używających tabel P3 ####################################
################################################################################
