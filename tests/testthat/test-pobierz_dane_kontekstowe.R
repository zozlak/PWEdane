context('pobierz_dane_kontekstowe')

test_that('pobierz_dane_kontekstwe działa', {
	dane = pobierz_dane_kontekstowe(data.frame('id_testu' = 1287, 'smieci' = 1))
	expect_is(dane, 'tbl_sql')
	expect_equal(ncol(dane), 10)
	expect_more_than(nrow(dane %>% collect()), 0)
	
	src = polacz()
	dane = pobierz_dane_kontekstowe(pobierz_testy(src) %>% filter_(~id_testu == 1287))
	expect_is(dane, 'tbl_sql')
	expect_equal(ncol(dane), 10)
	expect_more_than(nrow(dane %>% collect()), 0)
})
