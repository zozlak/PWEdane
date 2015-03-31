context('pobierz_wyniki')

test_that('pobierz_wyniki działa', {
  dane = pobierz_wyniki('sprawdzian', '', 2002, TRUE, TRUE)
  expect_is(dane, 'data.frame')
  expect_more_than(nrow(dane), 1000)
  expect_more_than(ncol(dane), 10)
  expect_more_than(sum(grepl('^k_', colnames(dane))), 0)
})

test_that('pobierz_wyniki zgłasza błędy', {
  expect_error(pobierz_wyniki('sprawdzian', '', 2001), 'W bazie danych brak wskazanych wynikow egzaminu')
  expect_error(pobierz_wyniki('egzamin gimnazjalny', '', 2001), 'W bazie danych brak wskazanych wynikow egzaminu')
  expect_error(pobierz_wyniki('matura', '', 2009), 'W bazie danych brak wskazanych wynikow egzaminu')
})

  