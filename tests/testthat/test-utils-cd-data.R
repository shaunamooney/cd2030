test_that('attr_or_abort throws when attribute is missing', {
  x <- tibble()
  expect_error(attr_or_abort(x, 'iso3'))
})

test_that('get_vaccine_tracers retrieves tracer attribute', {
  x <- tibble()
  attr(x, 'tracers') <- c('penta1', 'penta3')
  expect_equal(get_vaccine_tracers(x), c('penta1', 'penta3'))
})

test_that('get_country_name retrieves country', {
  x <- tibble()
  attr(x, 'country') <- 'Liberia'
  expect_equal(get_country_name(x), 'Liberia')
})

test_that('get_country_iso3 strips attributes and returns string', {
  x <- tibble()
  attr(x, 'iso3') <- 'LBR'
  expect_equal(get_country_iso3(x), 'LBR')
  expect_type(get_country_iso3(x), 'character')
})

test_that('get_indicator_groups returns correct structure', {
  x <- tibble()
  attr(x, 'indicator_groups') <- list(vacc = c('penta1', 'penta3'), idelv = 'idelv')
  expect_equal(get_indicator_group_names(x), c('vacc', 'idelv'))
  expect_equal(get_all_indicators(x), c('penta1', 'penta3', 'idelv'))
  expect_named(get_named_indicators(x), c('vacc', 'vacc', 'idelv'))
})
