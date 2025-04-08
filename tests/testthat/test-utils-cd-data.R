test_that("attr_or_abort retrieves existing attribute", {
  x <- tibble()
  attr(x, "iso3") <- "LBR"
  expect_equal(attr_or_abort(x, "iso3"), "LBR")
})

test_that("attr_or_abort throws error if attribute is missing", {
  x <- tibble()
  expect_error(attr_or_abort(x, "iso3"), "Missing attribute: iso3")
})

test_that("get_country_name retrieves valid country attribute", {
  x <- tibble()
  attr(x, "country") <- "Liberia"
  class(x) <- c("cd_data", class(x))  # ensures check_cd_data() passes
  expect_equal(get_country_name(x), "Liberia")
})

test_that("get_country_iso3 returns character and checks type", {
  x <- tibble()
  attr(x, "iso3") <- "LBR"
  class(x) <- c("cd_data", class(x))
  expect_equal(get_country_iso3(x), "LBR")
  expect_type(get_country_iso3(x), "character")
})

test_that("get_country_name fails if class is wrong", {
  x <- tibble()
  attr(x, "country") <- "Liberia"
  expect_error(get_country_name(x), "x The data object must be of class <'cd_data'>.")
})

test_that("list_tracer_vaccines returns expected static tracer list", {
  expected <- c("bcg", "measles1", "opv1", "opv2", "opv3", "penta1", "penta2", "penta3")
  expect_equal(list_tracer_vaccines(), expected)
})

test_that("list_vaccines returns all standard vaccines", {
  result <- list_vaccines()
  expect_true(all(c("bcg", "penta1", "measles2", "rota2") %in% result))
  expect_length(result, 16)
  expect_type(result, "character")
})

test_that("get_indicator_groups returns expected group names", {
  result <- get_indicator_groups()
  expect_named(result, c("anc", "idelv", "vacc"))
  expect_true("penta1" %in% result$vacc)
})

test_that("get_indicator_group_names returns group names", {
  expect_equal(get_indicator_group_names(), c("anc", "idelv", "vacc"))
})

test_that("get_all_indicators flattens all indicators", {
  all <- get_all_indicators()
  expect_true("penta1" %in% all)
  expect_true("anc1" %in% all)
  expect_type(all, "character")
})

test_that("get_named_indicators returns named vector of correct length", {
  named <- get_named_indicators()
  expect_named(named)
  expect_equal(length(named), length(get_all_indicators()))
  expect_true(all(names(named) %in% get_indicator_group_names()))
})
