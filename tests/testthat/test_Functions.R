


testthat::test_that(desc = "testing 'dtFetchDataNbpApiSingleCurrency' function", code = {

  # 1. well-behaved parameters
  testthat::expect_equal(
    object = class(dtFetchDataNbpApiSingleCurrency(
      cTicker = "USD", dateStartDate = as.Date("2020-01-03"),
      dateEndDate = as.Date("2020-04-05"))),
    expected = c("data.table", "data.frame"))

  # 2. too old time range
  testthat::expect_equal(
    object = dtFetchDataNbpApiSingleCurrency(
      cTicker = "USD", dateStartDate = as.Date("1990-01-03"),
      dateEndDate = as.Date("1992-04-05")),
    expected = list(NULL))

  # 3. too old start date, end date OK
  testthat::expect_equal(
    object = class(dtFetchDataNbpApiSingleCurrency(
      cTicker = "USD", dateStartDate = as.Date("2001-01-01"),
      dateEndDate = as.Date("2002-12-31"))),
    expected = c("data.table", "data.frame"))

  # 4. check if error thrown when should be thrown
  testthat::expect_equal(
    object = class(dtFetchDataNbpApiSingleCurrency(
      cTicker = "USD", dateStartDate = as.Date("2001-01-01"),
      dateEndDate = as.Date("2002-12-31"))),
    expected = c("data.table", "data.frame"))


})



testthat::test_that(desc = "testing 'lFetchDailyHistoricalFxRates' function", code = {

  # 1. check if error thrown when expected
  testthat::expect_error(
    object = lFetchDailyHistoricalFxRates(
      cTickers = c("asdf", "qwerty"), dateStartDate = as.Date(x = "2009-01-01"),
      dateEndDate = as.Date(x = "2011-03-15")),
    regexp = "no correct tickers have been passed to the function")
  testthat::expect_error(
    object = lFetchDailyHistoricalFxRates(
      cTickers = c("USD", "EUR"), dateStartDate = as.Date(x = "2009-01-01"),
      dateEndDate = as.Date(x = "2008-03-15")),
    regexp = "the dateEndDate is not a scalar Date class object")

  # 2. well-behaved parameters
  testthat::expect_type(
    object = lFetchDailyHistoricalFxRates(
      cTickers = c("USD", "EUR"), dateStartDate = as.Date(x = "2009-01-01"),
      dateEndDate = as.Date(x = "2010-03-15")),
    type = "list")

  # 3. too old time range
  testthat::expect_equal(
    object = suppressWarnings(lFetchDailyHistoricalFxRates(
      cTickers = c("USD", "EUR"), dateStartDate = as.Date(x = "1990-01-01"),
      dateEndDate = as.Date(x = "1992-04-05"))),
    expected = list(
      "USDPLN" = list(NULL), "EURPLN" = list(NULL)))

  # 4. too old start date, end date OK
  testthat::expect_equal(
    object = class(suppressWarnings(lFetchDailyHistoricalFxRates(
      cTickers = c("USD", "EUR"), dateStartDate = as.Date(x = "2001-01-01"),
      dateEndDate = as.Date(x = "2002-12-31")))),
    expected = "list")

})
