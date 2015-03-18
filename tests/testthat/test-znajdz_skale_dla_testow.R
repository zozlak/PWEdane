context('znajdz_skale_dla_testow')

test_that('znajdz_skale_dla_testow działa', {
  dane = znajdz_skale_dla_testow(data.frame('id_testu' = 1289, 'smieci' = 1))
  expect_is(dane, 'data.frame')
  expect_equal(ncol(dane), 4)
  expect_more_than(nrow(dane), 0)
  
  src = polacz()
  dane = znajdz_skale_dla_testow(pobierz_testy(src) %>% filter_(~id_testu == 1289))
  expect_is(dane, 'data.frame')
  expect_equal(ncol(dane), 4)
  expect_more_than(nrow(dane), 0)

  dane = znajdz_skale_dla_testow(c(871, 872, 1289))
  expect_is(dane, 'data.frame')
  expect_equal(ncol(dane), 4)
  expect_more_than(nrow(dane), 0)

  dane = znajdz_skale_dla_testow(1174) # pseudoarkusz
  expect_is(dane, 'data.frame')
  expect_equal(ncol(dane), 4)
  expect_equal(nrow(dane), 0)
})
