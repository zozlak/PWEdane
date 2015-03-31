context('pobierz_zrownywanie')

test_that('pobierz_zrownywanie działa', {
  dane = pobierz_zrownywanie('sprawdzian', 2014)
  expect_is(dane, 'data.frame')
  expect_more_than(nrow(dane), 1000)
  expect_more_than(ncol(dane), 10)
  expect_more_than(sum(grepl('^k_', colnames(dane))), 0)
  expect_more_than(sum(grepl('^p_', colnames(dane))), 0)
  expect_equal(all(dane$populacja == TRUE), TRUE)
})


test_that('pobierz_zrownywanie niewypunktowane brak skali', {
  dane = pobierz_zrownywanie('sprawdzian', 2014, FALSE)
  expect_is(dane, 'data.frame')
  expect_more_than(sum(grepl('^k_', colnames(dane))), 0)
  expect_equal(sum(grepl('^p_', colnames(dane))), 0)

  dane = pobierz_zrownywanie('sprawdzian', 2014, FALSE, 41)
  expect_is(dane, 'data.frame')
  expect_more_than(sum(grepl('^k_', colnames(dane))), 0)
  expect_equal(sum(grepl('^p_', colnames(dane))), 0)
})