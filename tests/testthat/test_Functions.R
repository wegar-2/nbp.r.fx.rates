
dtFetchDataNbpApiSingleCurrency(cTicker = "USD",
                                dateStartDate = as.Date("2020-01-03"),
                                dateEndDate = as.Date("2020-04-05"))


testthat::test_that(desc = "testing 'dtFetchDataNbpApiSingleCurrency' function", code = {

  # 1. well-behaved parameters
  testthat::expect_equal(
    object = class(dtFetchDataNbpApiSingleCurrency(
      cTicker = "USD", dateStartDate = as.Date("2020-01-03"),
      dateEndDate = as.Date("2020-04-05"))),
    expected = c("data.table", "data.frame"))

  # 2. too old time range
  dtFetchDataNbpApiSingleCurrency(
    cTicker = "USD", dateStartDate = as.Date("1990-01-03"),
    dateEndDate = as.Date("1992-04-05"))

  testthat::expect_equal(
    object = class(),
    expected = c("data.table", "data.frame"))


})



testthat::test_that(desc = "", code = {

})
