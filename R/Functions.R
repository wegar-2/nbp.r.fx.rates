
#' Fetch NBP rates against indicated foreign currencies
#'
#' Fetch FX rates against given foreign currencies from NBP API
#'
#' This function fetches the FX rates against Polish zloty for the currencies
#' indicated in the input character vector cTickers. The data is fetched for the
#' time scope defined by the parameters dateStartDate and dateEndDate
#' @param cTickers - character vector of IDs of the
#' @param dateStartDate - Date class scalar, indicates the date of the beginning
#' of the interval for which the data is to be fetched. It is included in the
#' final interval.
#' @param dateEndDate - Date class scalar, indicates the date of the end
#' of the interval for which the data is to be fetched. It is included in the final interval.
#' @return list of data.tables; names of the data.tables are standard ISO FX
#' pairs names with PLN featuring as the quote currency and the currencies indicated
#' in cTickers as base currencies; e.g. the results for query of USD rates
#' will be available under the output list's key "USDPLN".
#' @export
lFetchDailyHistoricalFxRates <- function(cTickers, dateStartDate, dateEndDate) {

  # 1. input validation --------------------------------------------------------
  # 1.1. cTickers
  if (!is.character(cTickers)) {
    stop("Error inside lFetchDailyHistoricalFxRates: the vector ",
         "cTickers is not of character type! ")
  }
  cTickers <- toupper(x = cTickers)
  if (!all(cTickers %in% cNbpHandledTickers)) {
    cCorrectTickers <- cTickers[cTickers %in% cNbpHandledTickers]
    if (length(x = cCorrectTickers) == 0L) {
      stop("ERROR: inside lFetchDailyHistoricalFxRates: no correct tickers ",
           "have been passed to the function ")
    } else {
      warning("WARNING: inside lFetchDailyHistoricalFxRates: some of the tickers ",
              "passed in the cTickers input vector are invalid! ",
              "discarding the following tickers: ",
              paste0(setdiff(x = cTickers, y = cCorrectTickers), collapse = ", "),
              "!!! ", immediate. = TRUE)
      cTickers <- cCorrectTickers
    }
  }
  # 1.2. dateStartDate
  if (!lubridate::is.Date(x = dateStartDate) | length(x = dateStartDate)!= 1) {
    stop("Error inside lFetchDailyHistoricalFxRates: the dateStartDate ",
         "is not a scalar Date class object! ")
  }
  # 1.3. dateEndDate
  if (!lubridate::is.Date(x = dateEndDate) | length(x = dateEndDate) != 1) {
    stop("Error inside lFetchDailyHistoricalFxRates: the dateEndDate ",
         "is not a scalar Date class object! ")
  }
  # 1.4. compare dateStartDate and dateEndDate
  if (dateStartDate > dateEndDate) {
    stop("Error inside lFetchDailyHistoricalFxRates: the dateEndDate ",
         "is not a scalar Date class object! ")
  }

  # 2. iterate over the tickers and fetch data for each one --------------------
  lDataOut <- vector(mode = "list", length = length(x = cTickers))
  names(lDataOut) <- paste0(cTickers, "PLN")
  for (cIterTicker in cTickers) {
    # 2.1. fetch the data
    dtIterData <- dtFetchDataNbpApiSingleCurrency(cTicker = cIterTicker,
                                                  dateStartDate = dateStartDate,
                                                  dateEndDate = dateEndDate)
    # 2.2. process the fetched data and store
    lDataOut[[paste0(cIterTicker, "PLN")]] <- dtIterData
  }

  return(lDataOut)
}



dtFetchDataNbpApiSingleCurrency <- function(cTicker, dateStartDate, dateEndDate) {

  # 1. split the time range into 90D periods -----------------------------------
  iSingleQueryWindowSize <- 90
  dateDaysSeq <- seq(dateStartDate, dateEndDate, 1L)
  cDaysSeq <- format(x = dateDaysSeq, format = "%Y-%m-%d")
  iDaysPointers <- seq(0, length(x = cDaysSeq), iSingleQueryWindowSize) + 1

  # 2. prepare list to store the data in ---------------------------------------
  lData <- vector(mode = "list", length = length(iDaysPointers))

  # 3. iterate over the sub-intervals, fetching the data -----------------------
  cCoreOfUrl <- "http://api.nbp.pl/api/exchangerates/rates/a"
  for (k in 1:length(x = iDaysPointers)) {
    iIterPointer <- iDaysPointers[[k]]

    # 3.1. prepare first and last date
    cIterStartDate <- cDaysSeq[[iIterPointer]]
    if (k != length(iDaysPointers)) {
      cIterEndDate <- cDaysSeq[[iIterPointer + (iSingleQueryWindowSize - 1)]]
    } else {
      cIterEndDate <- cDaysSeq[[length(x = cDaysSeq)]]
    }
    # 3.2. concatenate core url with ticker and time range
    cIterUrl <- paste0(cCoreOfUrl, "/", tolower(x = cTicker),
                       "/", cIterStartDate, "/", cIterEndDate, "/?format=json")
    # 3.3. run the query
    message("Querying URL: ", cIterUrl)
    cIterRes <- try(expr = { httr::GET(url = cIterUrl) }, silent = TRUE)
    # 3.4. check if returned error, or perhaps no data issue
    if (methods::is(object = cIterRes, class2 = "try-error")) {
      stop("Failed to query: ", cIterUrl)
    }
    # 3.5. output is error has not occurred
    cIterTextContent <- httr::content(x = cIterRes, "text")
    # 3.6. check for known errors
    if (cIterTextContent == "404 NotFound - Bark danych / No data available") {
      warning("The query: ", cIterUrl, " returned error: ",
              "'404 NotFound - Brak danych / No data available'",
              "; saving NULL...", immediate. = TRUE)
      lData[k] <- list(NULL)
      next
    }
    # 3.7. if output OK, unpack
    cIterParsedJson <- rjson::fromJSON(cIterTextContent)
    lJsonRates <- cIterParsedJson[["rates"]]
    # 3.8. extract FX rates
    lExtractedRates <- lapply(X = lJsonRates, FUN = function(x) {
      return(data.table::data.table("quote_date" = x[["effectiveDate"]],
                                    "FX_rate" = x[["mid"]])) })
    # 3.9. concatenate the rows and save the produced data
    dtIterData <- dplyr::bind_rows(lExtractedRates) %>% data.table::as.data.table()
    lData[[k]] <- dtIterData
  }

  # 4. concatenate the data.tables ---------------------------------------------
  # 4.1. extract NULL members of lData
  bNullsInData <- sapply(X = lData, FUN = function(x) { is.null(x) })
  # 4.2. if all nulls, return list(NULL)
  if (all(bNullsInData)) {
    warning("WARNING: data fetch for the currency ", cTicker, " for the time interval ",
            dateStartDate, " to ", dateEndDate, " has yileded no data; ",
            "returning list(NULL)!",
            immediate. = TRUE)
    return(list(NULL))
  }
  # 4.3. if not all NULLs, return the availble data
  if (!all(bNullsInData)) {
    # 4.3.1. keep non-NULLs
    lData <- lData[!bNullsInData]
    # 4.3.2. concatenate the table that have any data
    dtDataOut <- dplyr::bind_rows(lData) %>% data.table::as.data.table()
    # 4.3.3. format a bit and return
    data.table::setkey(x = dtDataOut, "quote_date")
    dtDataOut[["quote_date"]] <- as.Date(x = dtDataOut[["quote_date"]], format = "%Y-%m-%d")
    data.table::setnames(x = dtDataOut, old = "FX_rate", new = paste0("NBP_", cTicker, "PLN"))
    return(dtDataOut)
  }

}
