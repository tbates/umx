#' Resolve Option Implied Volatility from Ticker
#'
#' @description
#' `fin_resolveVolatility` looks up the implied volatility of a ticker from its option chain
#' closest to the target expiry and strike. If the retrieved IV is stale/zero (< 1%),
#' it falls back to calculating the 90-day Historical Volatility (HV) of the stock.
#'
#' @param impliedVol Either a numeric value (returned directly) or a character stock symbol.
#' @param strikePrice The strike price of the option.
#' @param daysToExpiry The number of days to expiration.
#' @return A numeric volatility value.
#' @export

fin_resolveVolatility <- function(impliedVol, strikePrice, daysToExpiry) {
  if (!is.character(impliedVol)) {
    return(impliedVol)
  }

  tickerSymbol = impliedVol
  volatilityValue = NA

  optionChain = tryCatch({
    quantmod::getOptionChain(tickerSymbol, src = "yahoo", NULL)
  }, error = function(e) {
    NULL
  })

  if (!is.null(optionChain) && length(optionChain) > 0) {
    targetDate = Sys.Date() + daysToExpiry
    expiryDates = as.Date(names(optionChain), format = "%b.%d.%Y")
    validIndices = which(!is.na(expiryDates))
    
    if (length(validIndices) > 0) {
      expiryDates = expiryDates[validIndices]
      closestExpiryIndex = validIndices[which.min(abs(expiryDates - targetDate))]
      closestExpiryName = names(optionChain)[closestExpiryIndex]
      callOptions = optionChain[[closestExpiryName]]$calls
      
      if (!is.null(callOptions) && nrow(callOptions) > 0) {
        closestStrikeIndex = which.min(abs(callOptions$Strike - strikePrice))
        retrievedIV = callOptions$IV[closestStrikeIndex]
        if (!is.na(retrievedIV) && retrievedIV >= 0.01) {
          volatilityValue = retrievedIV
          message("Retrieved IV of ", round(volatilityValue * 100, 2), "% from option chain (Expiry: ", closestExpiryName, ", Strike: ", callOptions$Strike[closestStrikeIndex], ")")
        }
      }
    }
  }

  if (is.na(volatilityValue)) {
    stockPrices = tryCatch({
      quantmod::getSymbols(tickerSymbol, auto.assign = FALSE, from = Sys.Date() - 90)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(stockPrices) && nrow(stockPrices) > 0) {
      closePrices = quantmod::Cl(stockPrices)
      logReturns = diff(log(as.numeric(closePrices)))
      volatilityValue = sd(logReturns, na.rm = TRUE) * sqrt(252)
      if (!is.na(volatilityValue)) {
        message("Option chain IV was unavailable, stale, or zero. Calculated 90-day historical volatility of ", round(volatilityValue * 100, 2), "% as fallback.")
      }
    }
  }

  if (is.na(volatilityValue) || is.nan(volatilityValue)) {
    warning("Volatility lookup failed for ticker '", tickerSymbol, "'. Defaulting to impliedVol = 0.20.")
    volatilityValue = 0.20
  }

  return(volatilityValue)
}
