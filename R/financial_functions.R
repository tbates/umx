# =======================
# = Financial utilities =
# =======================



#' Black-Scholes Call Option Price and Greeks Calculator
#'
#' @description
#' `fin_options_Greeks` calculates the theoretical European Call option price and its corresponding Greeks (Delta, Gamma, Theta, Vega, and Rho) using the Black-Scholes model.
#'
#' @param spotPrice The current price of the underlying asset.
#' @param strikePrice The strike price of the option.
#' @param daysToExpiry The number of days remaining until option expiration.
#' @param riskFreeRate The annual risk-free interest rate (expressed as a decimal, default = 0.04 for 4%).
#' @param impliedVol The annualized implied volatility (expressed as a decimal, default = 0.20 for 20%).
#' @return A data frame containing:
#' \itemize{
#'   \item \code{price}: Theoretical Call option price
#'   \item \code{delta}: Sensitivity of option price to underlying price (Delta)
#'   \item \code{gamma}: Sensitivity of Delta to underlying price (Gamma)
#'   \item \code{theta}: Daily time decay of option price (Theta)
#'   \item \code{vega}: Sensitivity of option price to a 1% change in implied volatility
#'   \item \code{rho}: Sensitivity of option price to a 1% change in risk-free interest rate
#' }
#' @export
#' @family financial functions
#' @examples
#' fin_options_Greeks(spotPrice = 100, strikePrice = 95, daysToExpiry = 30, 
#'	riskFreeRate = 0.04, impliedVol = 0.20)
#'
fin_options_Greeks<- function(spotPrice, strikePrice, daysToExpiry, riskFreeRate = 0.04, impliedVol = 0.20) {
  supplied = names(as.list(match.call())[-1])
  allArgs = c("spotPrice", "strikePrice", "daysToExpiry", "riskFreeRate", "impliedVol")
  missingArgs = setdiff(allArgs, supplied)

  if (length(missingArgs) > 0) {
    cat("=== Option Parameter Definitions (Teaching Mode) ===\n")
    if ("spotPrice" %in% missingArgs) {
      cat("  * spotPrice    : The current price of the underlying asset.\n")
    }
    if ("strikePrice" %in% missingArgs) {
      cat("  * strikePrice  : The strike price of the option (fixed exercise price).\n")
    }
    if ("daysToExpiry" %in% missingArgs) {
      cat("  * daysToExpiry : Days remaining until option expiration.\n")
    }
    if ("riskFreeRate" %in% missingArgs) {
      cat("  * riskFreeRate : Annual risk-free interest rate as a decimal (default = 0.04 for 4%).\n")
    }
    if ("impliedVol" %in% missingArgs) {
      cat("  * impliedVol   : Annualized implied volatility as a decimal (default = 0.20 for 20%).\n")
    }
    cat("===================================================\n\n")

    if ("spotPrice" %in% missingArgs) {
      cat("No spotPrice supplied. Defaulting to spotPrice = 100.\n")
      spotPrice = 100
    }
    if ("strikePrice" %in% missingArgs) {
      cat("No strikePrice supplied. Defaulting to strikePrice = 100.\n")
      strikePrice = 100
    }
    if ("daysToExpiry" %in% missingArgs) {
      cat("No daysToExpiry supplied. Defaulting to daysToExpiry = 30.\n")
      daysToExpiry = 30
    }
    cat("\n")
  }

  if (is.character(daysToExpiry) || inherits(daysToExpiry, "Date") || inherits(daysToExpiry, "POSIXt")) {
    expiryDate = as.Date(daysToExpiry)
    daysToExpiry = as.numeric(difftime(expiryDate, Sys.Date(), units = "days"))
  }

  impliedVol = fin_resolveVolatility(impliedVol, strikePrice, daysToExpiry)

  t = daysToExpiry / 365
  r = riskFreeRate
  sigma = impliedVol

  if (t <= 0) {
    callPrice = max(0, spotPrice - strikePrice)
    delta = if (spotPrice > strikePrice) 1.0 else if (spotPrice < strikePrice) 0.0 else 0.5
    gamma = 0.0
    theta = 0.0
    vega = 0.0
    rho = 0.0
  } else {
    d1 = (log(spotPrice / strikePrice) + (r + (sigma^2) / 2) * t) / (sigma * sqrt(t))
    d2 = d1 - sigma * sqrt(t)

    callPrice = spotPrice * pnorm(d1) - strikePrice * exp(-r * t) * pnorm(d2)
    delta = pnorm(d1)
    gamma = dnorm(d1) / (spotPrice * sigma * sqrt(t))
    theta = (-(spotPrice * dnorm(d1) * sigma) / (2 * sqrt(t)) - r * strikePrice * exp(-r * t) * pnorm(d2)) / 365
    vega = (spotPrice * sqrt(t) * dnorm(d1)) / 100
    rho = (strikePrice * t * exp(-r * t) * pnorm(d2)) / 100
  }

  res = data.frame(
    price = callPrice,
    delta = delta,
    gamma = gamma,
    theta = theta,
    vega = vega,
    rho = rho
  )
  return(res)
}

#' Plot Option Delta and Gamma Curves
#'
#' @description
#' `fin_options_plotGreeks` plots Call Option Delta and Gamma curves across a range of spot prices
#' (from -30% to +30% of the strike price) to show how Delta accelerates and Gamma peaks.
#'
#' @param strikePrice The strike price of the option (default = 100 if omitted).
#' @param daysToExpiry The number of days remaining until option expiration (default = 30).
#' @param riskFreeRate The annual risk-free interest rate (default = 0.04).
#' @param impliedVol The annualized implied volatility of the underlying asset (default = 0.20).
#' @return A ggplot object visualizing Delta and Gamma.
#' @export
#' @family financial functions
#' @examples
#' \dontrun{
#' fin_options_plotGreeks(strikePrice = 100)
#' # Run with missing arguments to print definitions
#' fin_options_plotGreeks()
#' }
#'
fin_options_plotGreeks <- function(strikePrice, daysToExpiry = 30, riskFreeRate = 0.04, impliedVol = 0.20) {
  supplied = names(as.list(match.call())[-1])
  allArgs = c("strikePrice", "daysToExpiry", "riskFreeRate", "impliedVol")
  missingArgs = setdiff(allArgs, supplied)

  if (length(missingArgs) > 0) {
    cat("=== Option Parameter Definitions (Teaching Mode) ===\n")
    if ("strikePrice" %in% missingArgs) {
      cat("  * strikePrice  : The strike price of the option (fixed exercise price).\n")
    }
    if ("daysToExpiry" %in% missingArgs) {
      cat("  * daysToExpiry : Days remaining until option expiration (default = 30).\n")
    }
    if ("riskFreeRate" %in% missingArgs) {
      cat("  * riskFreeRate : Annual risk-free interest rate as a decimal (default = 0.04 for 4%).\n")
    }
    if ("impliedVol" %in% missingArgs) {
      cat("  * impliedVol   : Annualized implied volatility as a decimal (default = 0.20 for 20%).\n")
    }
    cat("===================================================\n\n")

    if ("strikePrice" %in% missingArgs) {
      cat("No strikePrice supplied. Defaulting to strikePrice = 100 for visualization.\n\n")
      strikePrice = 100
    }
  }

  if (is.character(daysToExpiry) || inherits(daysToExpiry, "Date") || inherits(daysToExpiry, "POSIXt")) {
    expiryDate = as.Date(daysToExpiry)
    daysToExpiry = as.numeric(difftime(expiryDate, Sys.Date(), units = "days"))
  }

  impliedVol = fin_resolveVolatility(impliedVol, strikePrice, daysToExpiry)

  spotRange = seq(0.7 * strikePrice, 1.3 * strikePrice, length.out = 150)
  
  greeksList = lapply(spotRange, function(s) {
    fin_options_Greeks(spotPrice = s, strikePrice = strikePrice, daysToExpiry = daysToExpiry, riskFreeRate = riskFreeRate, impliedVol = impliedVol)
  })
  
  df = do.call(rbind, greeksList)
  df$spot = spotRange

  maxGamma       = max(df$gamma)
  scaleFactor    = if (maxGamma > 0) 1 / maxGamma else 1
  df$gammaScaled = df$gamma * scaleFactor

  peakIndex = which.max(df$gamma)
  peakSpot  = df$spot[peakIndex]
  peakDelta = df$delta[peakIndex]
  peakGamma = df$gamma[peakIndex]

  p = ggplot(df, aes(x = spot))
  p = p + geom_line(aes(y = delta, color = "Delta"), linewidth = 1.2)
  p = p + geom_line(aes(y = gammaScaled, color = "Gamma"), linewidth = 1.2)
  p = p + geom_vline(xintercept = peakSpot, linetype = "dashed", color = "gray40", alpha = 0.7)
  p = p + geom_point(data = data.frame(spot = peakSpot, gammaScaled = peakGamma * scaleFactor), aes(x = spot, y = gammaScaled), color = "#D55E00", size = 3)
  p = p + geom_point(data = data.frame(spot = peakSpot, delta = peakDelta), aes(x = spot, y = delta), color = "#0072B2", size = 3)
  labelText = sprintf("Peak Gamma: %.4f at Spot = $%.2f\nDelta: %.2f (Acceleration Point)", peakGamma, peakSpot, peakDelta)
  p = p + annotate("label", x = peakSpot, y = 0.5, label = labelText, fill = "white", color = "black", fontface = "bold", size = 3.5, alpha = 0.85, label.padding = unit(0.5, "lines"))
  p = p + scale_y_continuous(name = "Delta (Probability Proxy / Position Size)", limits = c(0, 1), sec.axis = ggplot2::sec_axis(~ . / scaleFactor, name = "Gamma (Rate of Change of Delta)"))
  p = p + scale_x_continuous(name = "Underlying Spot Price ($)")
  p = p + ggplot2::scale_color_manual(name = "Greeks", values = c("Delta" = "#0072B2", "Gamma" = "#D55E00"))
  titleText = sprintf("Delta & Gamma Sensitivity Curve (Strike = $%.2f, Expiry = %d Days)", strikePrice, daysToExpiry)
  p = p + labs(title = titleText, subtitle = "Delta represents position sensitivity; Gamma peaks where Delta changes fastest (At-The-Money)", caption = "Model: Black-Scholes European Option Calculator")
  p = p + theme_minimal(base_size = 11)
  p = p + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 12), axis.title.y.right = element_text(color = "#D55E00"), axis.title.y.left = element_text(color = "#0072B2"))
  return(p)
}

#' Simulate and Compare LEAP Extrinsic Premium Decay
#'
#' @description
#' `fin_options_LeapSimulate` compares two European Call options over a 931-day horizon:
#' one starting at a 0.80 Delta and another at a 0.95 Delta. It simulates how their extrinsic
#' value (rent/time-decay premium) bleeds to 0 as time runs out, plotting the results side-by-side.
#'
#' @param spotPrice The constant price of the underlying asset (default = 100).
#' @param impliedVol The constant implied volatility of the underlying asset (default = 0.20).
#' @param riskFreeRate The annual risk-free interest rate (default = 0.04).
#' @return A ggplot object comparing the extrinsic premium decay side-by-side.
#' @export
#' @family financial functions

#' @examples
#' \dontrun{
#' fin_options_LeapSimulate(spotPrice = 100, impliedVol = 0.20, riskFreeRate = 0.04)
#' }
#'
fin_options_LeapSimulate <- function(spotPrice = 100, impliedVol = 0.20, riskFreeRate = 0.04) {
  impliedVol = fin_resolveVolatility(impliedVol, spotPrice, 931)
  tInit = 931 / 365
  r = riskFreeRate
  sigma = impliedVol

  d1_80 = qnorm(0.80)
  strike80 = spotPrice * exp((r + (sigma^2) / 2) * tInit - d1_80 * sigma * sqrt(tInit))

  d1_95 = qnorm(0.95)
  strike95 = spotPrice * exp((r + (sigma^2) / 2) * tInit - d1_95 * sigma * sqrt(tInit))

  daysSeq = seq(931, 0, by = -1)

  simData = lapply(daysSeq, function(d) {
    g80 = fin_options_Greeks(spotPrice = spotPrice, strikePrice = strike80, daysToExpiry = d, riskFreeRate = r, impliedVol = sigma)
    intrinsic80 = max(0, spotPrice - strike80)
    extrinsic80 = g80$price - intrinsic80

    g95 = fin_options_Greeks(spotPrice = spotPrice, strikePrice = strike95, daysToExpiry = d, riskFreeRate = r, impliedVol = sigma)
    intrinsic95 = max(0, spotPrice - strike95)
    extrinsic95 = g95$price - intrinsic95

    data.frame(
      daysToExpiry = d,
      extrinsic80 = extrinsic80,
      extrinsic95 = extrinsic95,
      theta80 = g80$theta,
      theta95 = g95$theta
    )
  })

  df = do.call(rbind, simData)

  df80 = data.frame(
    daysToExpiry = df$daysToExpiry,
    position = "Delta 0.80 Option",
    strike = strike80,
    extrinsicValue = df$extrinsic80,
    dailyTheta = df$theta80
  )
  df95 = data.frame(
    daysToExpiry = df$daysToExpiry,
    position = "Delta 0.95 Option",
    strike = strike95,
    extrinsicValue = df$extrinsic95,
    dailyTheta = df$theta95
  )

  dfLong = rbind(df80, df95)

  p = ggplot(dfLong, aes(x = daysToExpiry, y = extrinsicValue, color = position))
  p = p + geom_line(linewidth = 1.2)
  p = p + ggplot2::facet_wrap(~ position, scales = "fixed")
  p = p + ggplot2::scale_x_reverse(name = "Days to Expiration (Time Running Out)")
  p = p + scale_y_continuous(name = "Extrinsic Premium Value / Rent Remaining ($)")
  p = p + ggplot2::scale_color_manual(values = c("Delta 0.80 Option" = "#E69F00", "Delta 0.95 Option" = "#56B4E9"))
  
  label80 = sprintf("Initial Strike: $%.2f\nMax Extrinsic: $%.2f", strike80, df$extrinsic80[1])
  label95 = sprintf("Initial Strike: $%.2f\nMax Extrinsic: $%.2f", strike95, df$extrinsic95[1])
  
  annData = data.frame(
    daysToExpiry = c(450, 450),
    extrinsicValue = c(df$extrinsic80[1] * 0.5, df$extrinsic80[1] * 0.5),
    position = c("Delta 0.80 Option", "Delta 0.95 Option"),
    labelText = c(label80, label95)
  )
  
  p = p + ggplot2::geom_label(data = annData, aes(label = labelText), color = "black", fill = "white", size = 3.5, fontface = "bold", label.padding = unit(0.5, "lines"), alpha = 0.9)
  
  p = p + labs(
    title = "LEAP Extrinsic Premium Decay: Delta 0.80 vs Delta 0.95",
    subtitle = "Deep ITM options (0.95 Delta) pay significantly less extrinsic rent, reducing time-decay risk.",
    caption = "Constant Spot Price and Implied Volatility. Standard European Option Model."
  )
  
  p = p + theme_minimal(base_size = 11)
  
  p = p + theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 13)
  )

  cat("=== LEAP Option Simulation Summary ===\n")
  cat(sprintf("Spot Price: $%.2f | Implied Vol: %.0f%% | Risk-Free Rate: %.1f%%\n", spotPrice, impliedVol * 100, riskFreeRate * 100))
  cat(sprintf("Option 1 (Delta 0.80): Strike = $%.2f | Start Extrinsic = $%.2f\n", strike80, df$extrinsic80[1]))
  cat(sprintf("Option 2 (Delta 0.95): Strike = $%.2f | Start Extrinsic = $%.2f\n", strike95, df$extrinsic95[1]))
  cat(sprintf("Rent Savings: 0.95 Delta Option saves $%.2f (%.1f%%) in extrinsic value compared to 0.80 Delta.\n",
              df$extrinsic80[1] - df$extrinsic95[1],
              100 * (df$extrinsic80[1] - df$extrinsic95[1]) / df$extrinsic80[1]))

  return(p)
}

#' Teaching function for options
#'
#' @description
#' `fin_options_teach` is a teaching function for understanding options (intrinsic/extrinsic value, annualized rent, leverage, and time decay).
#'
#' @param premium Cost to buy the option contract per share.
#' @param strike The strike price of the option.
#' @param stock The current stock price.
#' @param delta The delta of the option (default = 0.85). Used to calculate effective leverage (Omega).
#' @param years How far in time the option ends (e.g. 1.8 years for a LEAP).
#' @param type Whether it is a "call" or "put" option.
#' @return A list containing intrinsic value, extrinsic value, break-even price, annualized rent percent, effective leverage (Omega), and daily theta.
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()], [fin_tax_NI()], [fin_value_percent()]
#' @examples
#' # Call Option (In-The-Money LEAP)
#' fin_options_teach(premium = 134, strike = 200, stock = 304, delta = 0.85, years = 1.8)
#'
#' # Put Option (Out-Of-The-Money)
#' fin_options_teach(premium = 10, strike = 280, stock = 304, delta = -0.30, years = 0.5, type = "put")
#'
fin_options_teach <- function(premium = 134, strike = 200, stock = 304, delta = 0.85, years = 1.8, type = c("call", "put")) {
  type = match.arg(type)
  # Adjust default/positive delta if user specified a Put option
  if (type == "put" && delta > 0) {
    delta = -delta
  }

  # 1. Moneyness & Intrinsic/Break-even Calculations
  if (stock > strike) {
    moneyness = if (type == "call") "In-the-Money (ITM)" else "Out-of-the-Money (OTM)"
    moneynessDiff = stock - strike
  } else if (stock < strike) {
    moneyness = if (type == "call") "Out-of-the-Money (OTM)" else "In-the-Money (ITM)"
    moneynessDiff = strike - stock
  } else {
    moneyness = "At-the-Money (ATM)"
    moneynessDiff = 0
  }

  if (type == "call") {
    intrinsic = max(0, stock - strike)
    breakEven = strike + premium
    pctToBreakEven = 100 * (breakEven - stock) / stock
    maxProfit = Inf
    maxLoss = premium * 100
  } else {
    intrinsic = max(0, strike - stock)
    breakEven = strike - premium
    pctToBreakEven = 100 * (stock - breakEven) / stock
    maxProfit = (strike - premium) * 100
    maxLoss = premium * 100
  }

  extrinsic = premium - intrinsic
  pctIntrinsic = 100 * intrinsic / premium
  pctExtrinsic = 100 * extrinsic / premium

  # 2. Annualized cost of the 'insurance/rent'
  rentAnnualPct = 100 * (extrinsic / years) / stock

  # 3. Omega (Effective Leverage)
  leverage = (abs(delta) * stock) / premium

  # 4. Daily Theta (Linear approximation)
  thetaDaily = -extrinsic / (years * 365.25)

  # 5. Output Results to Console
  cat(sprintf("--- Option Analysis (%s Option) ---\n", toupper(type)))
  cat(sprintf("Current Stock: $%.2f | Strike: $%.2f | Premium: $%.2f\n", stock, strike, premium))
  cat(sprintf("Moneyness: %s by $%.2f (%.1f%% of stock price)\n", moneyness, moneynessDiff, 100 * moneynessDiff / stock))
  cat(sprintf("Premium Breakdown: Intrinsic (Equity): %.1f%% | Extrinsic (Time Value/Rent): %.1f%%\n", pctIntrinsic, pctExtrinsic))
  if (type == "call") {
    cat(sprintf("Break-even Hurdle: %+.2f%% (Stock must reach $%.2f by year %.1f)\n", pctToBreakEven, breakEven, years))
  } else {
    cat(sprintf("Break-even Hurdle: %+.2f%% (Stock must drop to $%.2f by year %.1f)\n", -pctToBreakEven, breakEven, years))
  }
  cat(sprintf("Annualized Rent: %.2f%% of stock value per year (Compare vs S&P 500 Benchmark)\n", rentAnnualPct))
  cat(sprintf("Effective Leverage (Omega): %.2fx (Delta: %.2f)\n", leverage, delta))
  cat(sprintf("Linear Theta: -$%.4f per day (Note: actual Theta accelerates closer to expiry)\n", abs(thetaDaily)))
  
  # Contract pricing details
  cat(sprintf("Contract Pricing: One standard contract (100 shares) costs $%.2f\n", premium * 100))
  if (type == "call") {
    cat(sprintf("  * Max Loss: $%.2f (100%% of premium)\n", maxLoss))
    cat("  * Max Profit: Unlimited\n")
  } else {
    cat(sprintf("  * Max Loss: $%.2f (100%% of premium)\n", maxLoss))
    cat(sprintf("  * Max Profit: $%.2f (if stock drops to $0)\n", maxProfit))
  }
  
  # Delta ITM probability proxy
  cat(sprintf("Probability Proxy: Delta indicates an approx. %.0f%% chance of expiring In-the-Money.\n", abs(delta) * 100))

  if (rentAnnualPct > 7) {
    cat("!!! WARNING: High Rent. Ensure target growth exceeds benchmark + rent.\n")
  }
  if (pctExtrinsic == 100) {
    cat("!!! NOTE: This option has 100% extrinsic value. If the stock does not move past the strike, the option will expire worthless.\n")
  }

  invisible(list(
    intrinsic       = intrinsic,
    extrinsic       = extrinsic,
    breakEven       = breakEven,
    rent_annual_pct = rentAnnualPct,
    leverage        = leverage,
    theta_daily     = thetaDaily,
    moneyness       = moneyness,
    pct_extrinsic   = pctExtrinsic,
    max_loss        = maxLoss,
    max_profit      = maxProfit
  ))
}
  
#' Compute the CAGR of a stock
#'
#' @description
#' `fin_stock_CAGR` uses stock info from Yahoo to work out the CAGR over time.
#'
#' @param priceSeries A price series using yahoo
#' @param from The date in the series to start from (blank = all)
#' @return - value
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()], [fin_tax_NI()], [fin_value_percent()]
#' @examples
#' \dontrun{
#' libs(c("quantmod", "ggplot2", "scales", "lubridate"))
#' getSymbols(c("NVDA"), from = "2010-01-01", to = Sys.Date())
#' startDate = "2016-01-01"
#' nvdaCagr = fin_stock_CAGR(NVDA, startDate)
#' }
fin_stock_CAGR<- function(priceSeries, from = "1900-01-01") {
	# getSymbols(c("NVDA"), from = "2010-01-01", to = Sys.Date())
	tickerName = deparse(substitute(priceSeries))   # this is the magic line you wanted
  data = data.frame(
    Date  = zoo::index(priceSeries),
    Price = as.numeric(quantmod::Cl(priceSeries))
  )
  if(missing(from)){
	  startDate   = zoo::index(priceSeries[1,])
  } else {
	  startDate   = as.Date(from)
  }
	dataFromWhen = data[data$Date >= startDate, ]
  startPrice  = dataFromWhen$Price[1]
  endPrice    = utils::tail(dataFromWhen$Price, 1)
  yearsPassed = as.numeric(difftime(max(dataFromWhen$Date), startDate, units = "days")) / 365.25
  cagr        = (endPrice / startPrice) ^ (1 / yearsPassed) - 1
  
  cat(sprintf("%s: %.1f%% CAGR ($%.2f --> $%.2f over %.2f years)\n", tickerName, cagr * 100, startPrice, endPrice, yearsPassed))
  
  tmp = list(
    cagr       = cagr,
    startPrice = startPrice,
    endPrice   = endPrice,
    years      = yearsPassed,
    data       = data
  )
  invisible(tmp)
}

#' Work the valuation of a company
#'
#' @description
#' `fin_stock_valuation` uses the revenue, operating margin, expenses and PE to compute a market capitalization.
#' Better to use a more powerful online site.
#'
#' @details
#' Revenue is multiplied by opmargin to get a gross profit. From this the proportion specified in `expenses` is subtracted 
#' and the resulting earnings turned into a price via the `PE`
#' 
#' @param revenue Revenue of the company
#' @param opmargin Margin on operating revenue
#' @param expenses Additional fixed costs
#' @param PE of the company
#' @param symbol Currency
#' @param use reporting values in "B" (billion) or "M" (millions)
#' @return - value
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()], [fin_tax_NI()], [fin_value_percent()]
#' @examples
#' fin_stock_valuation(rev=7e9, opmargin=.1, PE=33)
#' # Market cap =  $18,480,000,000
#' # (Based on PE= 33, operating Income of $0.70 B, and net income =$0.56B
#'
fin_stock_valuation <- function(revenue=6e6*30e3, opmargin=.08, expenses=.2, PE=30, symbol = "$", use = c("B", "M")) {
	use = match.arg(use)
	if(use=="B"){
		divisor=1e9
	} else {
		divisor=1e6
	}
	operatingIncome = revenue * opmargin
	netIncome = operatingIncome *(1-expenses)
	marketCap = netIncome*PE
	class(marketCap) = 'money'; attr(marketCap, 'symbol') = symbol
	class(netIncome) = 'money'; attr(netIncome, 'symbol') = symbol
	class(operatingIncome) = 'money'; attr(operatingIncome, 'symbol') = symbol
	
	cat("Market cap = ", print(marketCap, cat=F))
	cat("\n(Based on PE= ", PE, ", operating Income of ", print(operatingIncome/divisor, cat=F), " ", use, ", and net income =", print(netIncome/divisor, cat=F), use, "\n", sep = "")

	invisible(marketCap)
}


#' Compute the future value and gain of an investment
#'
#' @description
#' fin_stock_target takes a current and fair value, as well as a cost of capital, and returns the expected gain.
#'
#' @param current The current market value of the instrument
#' @param fair The user's estimated fair value.
#' @param ticker A label for printing
#' @param capital The cost of capital (defaults to .15)
#' @param verb Verbose or concise (FALSE)
#' @return - expected gain
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()]
#' @examples
#' fin_stock_target(114,fair=140, ticker="NVDA", capital=.15, verb=TRUE)
#' # NVDA  return =  41 %
#' # delta (fair-current)= $ 26 
#' # growth = $ 21 
#' # expected gain = $ 47 
#' # future value (final) = $ 161 
#' 
#' fin_stock_target(24, 130, ticker="SMMT")
#' # SMMT  return =  523 %
#' 
#' fin_stock_target(24, 75, ticker="SMMT", verb=TRUE)
#' # SMMT  return =  259 %
#' # delta (fair-current)= $ 51 
#' # growth = $ 11.25 
#' # expected gain = $ 62.25 
#' # future value (final) = $ 86.25 
#' 
#' fin_stock_target(750, 1000, ticker="LLY", verb=TRUE)
#' # LLY  return =  53 %
#' # delta (fair-current)= $ 250
#' # growth = $ 150 
#' # expected gain = $ 400 
#' # future value (final) = $ 1150 
#'
fin_stock_target <- function(current=89, fair=140, ticker = "NVDA", capital=.15, verb = FALSE) {
	delta  = (fair-current)      
	growth = fair*capital        
	expectedGain = (growth+delta)
	final  = fair*(1+capital)    
	cat(ticker, " return = ", round(((final/current)-1)*100, 0), "%\n")
	if(verb){
		cat("delta (fair-current)= $", delta, "\n")
		cat("growth = $", growth, "\n")
		cat("expected gain = $", expectedGain, "\n")
		cat("future value (final) = $", final, "\n")
	}
	invisible(expectedGain)
}

#' Calculate Compound Annual Growth Rate (CAGR)
#'
#' @description
#' Calculates the constant, period-over-period growth rate required for an
#' investment to grow from a beginning value to an ending value over a
#' specified number of periods.
#'
#' The Compound Annual Growth Rate (CAGR) is computed as:
#'  CAGR = (End value/Start value)^(1/t) - 1
#' 
#' Where \emph{t} is the number of years (periods).
#'
#' @param beginningValue Starting value of investment
#' @param endingValue Ending value of investment
#' @param numYears Number of periods (e.g., years) elapsing from begin to end
#' @param digits rounding the returned value (default = 3)
#'
#' @return A numeric value representing the Compound Annual Growth Rate as a 
#'   decimal (e.g., 0.096 for 9.6%).
#'
#' @note This function includes input validation and will `stop()` with an error
#'   if any inputs are non-numeric or non-positive.
#'
#' @family financial functions
#' @export
#'
#' @examples
#' rate = fin_value_CAGR(beginningValue = 100, endingValue = 190, numYears = 7)
#' print(rate)
#' 
#' # --- Example with a Loss ---
#' fin_value_CAGR(100, 50, 5) 
#'
#' # --- Formatting as Percentage ---
#' percent = paste0(round(rate * 100, 2), "%")
#' print(percent)
fin_value_CAGR <- function(beginningValue, endingValue, numYears, digits=3) {
  # Ensure inputs are numeric
  if (!is.numeric(beginningValue) || !is.numeric(endingValue) || !is.numeric(numYears)) {
    stop("All inputs must be numeric.")
  }
  # Ensure values are valid
  if (beginningValue <= 0 || endingValue <= 0 || numYears <= 0) {
    stop("Inputs must be positive values.")
  }
  # Calculate the rate
  cagr = (endingValue / beginningValue)^(1 / numYears) - 1
  return(round(cagr, digits))
}

#' Compute the value of a principal & annual deposits at a compound interest over a number of years
#' @description
#' Allows you to determine the final value of an initial `principal` (with optional 
#' periodic `deposits`), over a number of years (`yrs`) at a given rate of `interest`.
#' Principal and deposits are optional. You control compounding periods each year (n) and whether deposits occur at the beginning or end of the year.
#' The function outputs a nice table of annual returns, formats the total using a user-settable currency `symbol`. Can also `report` using a web table.
#' 
#' *notes*: Graham valuation: fair P/E = 9 + (1.5 * growth%). e.g.  $INTEL fair P/E = 9+.5*3 = 10.5 up to  9+2*10 = 29
#' Can move the weighting between a conservative .5 and an optimistic 2 (in terms of how long the growth will last and how low the hurdle rate is)
#' 
#' 
#' @param principal The initial investment at time 0 (default 100)
#' @param deposits Optional periodic additional investment each *year*.
#' @param interest Annual interest rate (default .05)
#' @param inflate How much to inflate deposits over time (default 0)
#' @param yrs Duration of the investment (default 10).
#' @param n Compounding intervals per year (default 12 (monthly), use 365 for daily)
#' @param when Deposits made at the "beginning" (of each year) or "end"
#' @param symbol Currency symbol to embed in the result.
#' @param report "markdown" or "html", 
#' @param table Whether to print a table of annual returns (default TRUE)
#' @param largest_with_cents Default = 0
#' @param baseYear Default = current year (for table row labels)
#' @param final if set (default = NULL), returns the rate required to turn principal into final after yrs (principal defaults to 1)
#' @param deflate Final capital is inflation adjusted when inflation is non zero (default TRUE).
#' @return - Value of balance after yrs of investment.
#' @export
#' @family financial functions
#' @seealso - [umx_set_dollar_symbol()], [fin_value_percent()], [fin_tax_NI()], [fin_stock_valuation()]
#' @references - <https://en.wikipedia.org/wiki/Compound_interest>
#' @examples
#' # 1. Value of a principal after yrs years at 5% return, compounding monthly.
#' # Report in browser as a nice table of annual returns and formatted totals.
#' fin_value_interest(principal = 5000, interest = 0.05, rep= "html")
#'
#' # Report as a nice markdown table
#' fin_value_interest(principal = 5000, interest = 0.05, yrs = 10)
#'
#' umx_set_dollar_symbol("$")
#' # 2 What rate is needed to increase principal to final value in yrs time?
#' fin_value_interest(1, final = 1.4, yrs=5)
#' fin_value_interest(principal = 50, final=200, yrs = 5)
#'
#' # 3. What's the value of deposits of $100/yr after 10 years at 7% return?
#' fin_value_interest(0, deposits = 100, interest = 0.07, yrs = 10, n = 12)
#'
#' # 4. What's the value of $20k + $100/yr over 10 years at 7% return?
#' fin_value_interest(principal= 20e3, deposits= 100, interest= .07, yrs= 10, symbol="$")
#'
#' # 5. What is $10,000 invested at the end of each year for 5 years at 6%?
#' fin_value_interest(deposits = 10e3, interest = 0.06, yrs = 5, n=1, when= "end")
#'
#' # 6. What will $20k be worth after 10 years at 15% annually (n=1)?
#' fin_value_interest(deposits=20e3, interest = 0.15, yrs = 10, n=1, baseYear=1)
#' # $466,986
#'
#' # manual equivalent
#' sum(20e3*(1.15^(10:1))) # 466985.5
#'
#' # 7. Annual (rather than monthly) compounding (n=1)
#' fin_value_interest(deposits = 100, interest = 0.07, yrs = 10, n=1)
#' 
#' # 8 Interest needed to increase principal to final value in yrs time.
#' fin_value_interest(principal = 100, final=200, yrs = 5)
#'
fin_value_interest <- function(principal = 100, deposits = 0, inflate = 0, interest = 0.05, yrs = 10, final= NULL, n = 12, when = "beginning", symbol = NULL, largest_with_cents = 0, baseYear= as.numeric(format(Sys.time(), "%Y")), table = TRUE, report= c("markdown", "html"), deflate = TRUE){
	report = match.arg(report)
	if(is.null(symbol)){symbol = umx_set_dollar_symbol(silent=TRUE)}
	if(principal==0){
		caption= paste0("Compounding ", bucks(deposits, symbol, cat=TRUE), " deposits over ", yrs, " years at ", interest*100, "% interest with ", inflate*100, "% inflation.")
	} else {
		caption= paste0("Compounding ", bucks(principal, symbol, cat=TRUE), " principle plus ", bucks(deposits, symbol, cat=TRUE), " annual deposits, ", interest * 100, "% interest and ", inflate*100, "% inflation.")
	}

	if(inflate != 0){
		deposits = c(deposits, rep(deposits, times = yrs-1) *(1+inflate)^c(1:(yrs-1)))
	}else{
		deposits = rep(deposits, times = yrs)
	}
	if(!is.null(final)){
		# final = prin*(1+rate)^y
		if(principal==0){ principal=1 }
		return((final/principal)^(1/(yrs))-1)
		# rate is the years root of (final *prin?)
	}

	# 1. compute compounding rate per unit time n (allowing for zero interest so 1.0)
	rate = ifelse(interest==0, 1, 1+(interest/n))

	tableOut = data.frame(Year = NA, Deposits = NA, Interest = NA, Total_Deposits = NA, Total_Interest = NA, Total = scales::dollar(principal, prefix = symbol, largest_with_cents = 0))
	balance  = principal
	totalDeposits = 0
	totalInterest = 0
	for (yr in 1:yrs) {
		# 1. Compute compounding rate per unit time n (allowing for zero interest so 1.0)
		if(when == "beginning"){
			# Deposits at the beginning of each year
			thisInterest = ((balance + deposits[yr]) * rate^n) - (balance + deposits[yr])
		} else {
			# Deposits at the end of the year
			thisInterest = (balance * rate^n) - balance
		}
		totalDeposits = (totalDeposits + deposits[yr])
		totalInterest = (totalInterest + thisInterest)
		balance       = (balance + deposits[yr] + thisInterest)
		thisRow = c(Year=yr+baseYear, Deposit= deposits[yr], Interest = thisInterest, Total_Deposit = totalDeposits, Total_Interest = totalInterest, Total = balance)
		thisRow = c(thisRow[1], scales::dollar(thisRow[-1], prefix = symbol, largest_with_cents = largest_with_cents))
		tableOut = rbind(tableOut, thisRow)
	}
	if(table){
		# principal = 0, deposits = 0, inflate = 0, interest = 0.05, yrs
		umx_print(tableOut, justify = "right", caption = caption, report=report)
	}

	if(length(deposits)==1){
		# 2. compute compounded value of the principal (initial deposit)
		Compound_interest_for_principal = principal* rate^(n*yrs)

		# 3. compute compounded value of the deposits

		if(interest==0){
			Future_value_of_a_series = deposits * yrs
		} else {
			# beginning: A = PMT * (((1 + r/n)^(nt) - 1) / (r/n))
			# end      : A = PMT * (((1 + r/n)^(nt) - 1) / (r/n)) * (1+r/n)
			if(when == "beginning"){
				# deposits at the beginning of each year
				periods = (yrs:1)*n
				Future_value_of_a_series = sum(deposits*(rate^periods))
			} else {
				# deposits at the end of the year
				periods = ((yrs-1):1)*n
				Future_value_of_a_series = sum(deposits*(rate^periods)) + (1*deposits)
			}
		}

		Total =  Compound_interest_for_principal+ Future_value_of_a_series
	} else {
		Total = balance
	}
	class(Total) = 'money'
	attr(Total, 'symbol') = symbol
	if(deflate){
		return(Total/ (1+inflate)^yrs)
	} else {
		return(Total)
	}
}


#' Compute UK NI given annual Earnings.
#'
#' @description
#' Employees pay contributions at 12%% on annual earnings between GBP 9,568 and GBP 50,270. Above that you pay at 2%%. 
#' Employers pay at 13.8%% on all annual earnings of more than GBP 8,840, although there are different thresholds 
#' for those under the age of 21 and for apprentices under the age of 25.
#'
#' @param annualEarnings Employee annual earnings.
#' @param symbol Currency symbol to embed in the result.
#' @return - NI
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()], [fin_value_percent()], [fin_stock_valuation()]
#' @references - <https://www.telegraph.co.uk/tax/tax-hacks/politicians-running-scared-long-overdue-national-insurance-overhaul/>
#' @examples
#' fin_tax_NI(42e3)
#' fin_tax_NI(142000)
#'
fin_tax_NI <- function(annualEarnings, symbol = "\u00A3") {
	if(annualEarnings < 50270){
		employee = .12 * max(0, (annualEarnings- 9568))
	} else {
		employee = (.12 * (annualEarnings- 9568)) + (.02 * (annualEarnings-50270))
	}
	employer = .138 * max((annualEarnings - 8840), 0)

	Total = employer + employee
	class(Total) = 'money'
	attr(Total, 'symbol') = symbol
	cat(paste0("Employer pays ", bucks(employer, symbol = symbol, cat = FALSE), ", and employee pays ", bucks(employee, symbol = symbol, cat=FALSE),
	 ". So ", round((employer+employee)/annualEarnings*100, 2),	" % total!\n")
	 )
	return(Total)
}

#' Justified P/E Ratio
#'
#' Compute the Justified Price-to-Earnings (P/E) ratio for a stock using a single-stage or two-stage (high-growth + ROIC) Gordon Growth Model.
#'
#' @details
#' Justified P/E represents the fundamental value multiple that a stock should trade at based on its payout ratio (or ROIC reinvestment),
#' expected growth rate (\eqn{g}), and required rate of return / discount rate (\eqn{r}).
#'
#' **Single-Stage Model** (when \code{yearsHighGrowth = 0}):
#' For stable-growth companies where \eqn{r > g}:
#' \deqn{\text{Justified Trailing P/E} = \frac{\text{Payout Ratio} \times (1 + g)}{r - g}}
#' \deqn{\text{Justified Leading P/E} = \frac{\text{Payout Ratio}}{r - g}}
#'
#' **Two-Stage Model** (when \code{yearsHighGrowth > 0}):
#' For growth companies where high-growth rate \eqn{g_{\text{high}}} may exceed the cost of capital (\eqn{r}),
#' payout ratio during high growth is derived from Return on Invested Capital (\eqn{\text{ROIC}}):
#' \deqn{\text{Payout Ratio} = 1 - \frac{g}{\text{ROIC}} + \text{buybackYield}}
#' Cash flows during the \code{yearsHighGrowth} period are discounted at rate \eqn{r}. After \code{yearsHighGrowth},
#' growth transitions to \code{terminalGrowth} (\eqn{g_{\text{term}} < r}), and the terminal value is discounted back to present value.
#'
#' **Typical Values & Interpretation**:
#' Typical justified P/E ratios for mature, stable-growth companies generally fall between 12 and 25.
#' High-growth companies with high ROIC (e.g. 30%+ ROIC and 20% growth) justify 20x–30x+ P/E multiples.
#'
#' **Worked Example ($NVDA)**:
#' Suppose Nvidia ($NVDA) has high growth of 20% (\eqn{g = 0.20}) for 5 years, high ROIC of 30% (\eqn{\text{ROIC} = 0.30}),
#' cost of equity of 10% (\eqn{r = 0.10}), and long-term terminal growth of 3% (\eqn{g_{\text{term}} = 0.03}).
#' The justified P/E is calculated as:
#' \code{fin_stock_justifiedPE(EPS = 2.70, growthRate = 0.20, discountRate = 0.10, ROIC = 0.30, yearsHighGrowth = 5, terminalGrowth = 0.03)}
#'
#' @param dividend Dividend per share (\eqn{D_0} for trailing, \eqn{D_1} for leading). Default = 0.80. Ignored if \code{ROIC} is specified.
#' @param EPS Earnings per share (\eqn{E_0} for trailing, \eqn{E_1} for leading). Default = 2.00.
#' @param growthRate Sustainable growth rate (\eqn{g}), as a decimal (e.g. 0.20 for 20%). Default = 0.06.
#' @param discountRate Required rate of return / cost of equity (\eqn{r}), as a decimal (e.g. 0.10 for 10%). Default = 0.10.
#' @param buybackYield Net share repurchase yield added to payout ratio, as a decimal (e.g. 0.015 for 1.5%). Default = 0.
#' @param ROIC Return on Invested Capital, as a decimal (e.g. 0.30 for 30%). If specified, payout ratio during high growth is derived as \eqn{1 - g/\text{ROIC}}.
#' @param yearsHighGrowth Duration of high-growth phase in years (default = 0 for single-stage model). If \eqn{g \ge r} and \code{yearsHighGrowth == 0}, defaults to 5 years.
#' @param terminalGrowth Long-term terminal growth rate after high-growth phase, as a decimal (e.g. 0.03 for 3%). Default = 0.03.
#' @param type Character string indicating \code{"trailing"} (default) or \code{"leading"} P/E.
#' @return Numeric justified P/E ratio.
#' @export
#' @family financial functions
#' @references
#' * Gordon, M. J. (1962). *The Investment, Financing, and Valuation of the Corporation*. R. D. Irwin.
#' * Pinto, J. E., Henry, C., Robinson, T. R., & Stowe, J. D. (2020). *Equity Asset Valuation* (4th ed.). Wiley.
#' * Mauboussin, M. J., & Rappaport, A. (2021). *Expectations Investing: Reading Stock Prices for Better Returns*. Columbia University Press.
#' @seealso - [fin_value_interest()], [fin_value_percent()], [fin_tax_NI()]
#' @examples
#' # Example 1: Standard trailing Justified P/E
#' fin_stock_justifiedPE(dividend = 0.8, EPS = 2.0, growthRate = 0.06, discountRate = 0.10)
#' 
#' # Example 2: Two-Stage growth model for 20% growth company with 30% ROIC for 5 years
#' fin_stock_justifiedPE(EPS = 2.70, growthRate = 0.20, discountRate = 0.10, 
#'                        ROIC = 0.30, yearsHighGrowth = 5, terminalGrowth = 0.03)
#' 
fin_stock_justifiedPE <- function(dividend = 0.80, EPS = 2.00, growthRate = 0.06, discountRate = 0.10, buybackYield = 0, ROIC = NULL, yearsHighGrowth = 0, terminalGrowth = 0.03, type = c("trailing", "leading")) {
	type = match.arg(type)

	if (EPS <= 0) {
		stop("Polite note: EPS must be positive to compute a justified P/E ratio.")
	}

	# Auto-switch to two-stage model if growthRate >= discountRate and yearsHighGrowth == 0
	if (growthRate >= discountRate && yearsHighGrowth == 0) {
		yearsHighGrowth = 5
	}

	if (yearsHighGrowth > 0) {
		if (terminalGrowth >= discountRate) {
			stop("Polite note: terminalGrowth (", terminalGrowth, ") must be strictly less than discountRate (", discountRate, ").")
		}

		# High-growth payout ratio
		if (!is.null(ROIC) && ROIC > 0) {
			if (growthRate > ROIC) {
				payoutHigh = buybackYield
			} else {
				payoutHigh = (1 - (growthRate / ROIC)) + buybackYield
			}
		} else {
			payoutHigh = (dividend / EPS) + buybackYield
		}
		payoutHigh = max(0, min(1, payoutHigh))

		# Terminal payout ratio (assuming ROIC fades to discountRate or standard terminal payout)
		roicTerm = if (!is.null(ROIC)) max(ROIC, discountRate) else discountRate
		payoutTerm = max(0, min(1, 1 - (terminalGrowth / roicTerm) + buybackYield))

		# Calculate Present Value of High Growth Phase (normalized E0 = 1)
		pvHigh = 0
		currentE = 1
		for (t in 1:yearsHighGrowth) {
			currentE = currentE * (1 + growthRate)
			cf_t = currentE * payoutHigh
			pvHigh = pvHigh + (cf_t / ((1 + discountRate)^t))
		}

		# Terminal value at end of yearsHighGrowth
		eN = currentE
		peTerminal = (payoutTerm * (1 + terminalGrowth)) / (discountRate - terminalGrowth)
		pvTerminal = (eN * peTerminal) / ((1 + discountRate)^yearsHighGrowth)

		justifiedPE = pvHigh + pvTerminal

	} else {
		if (discountRate <= growthRate) {
			stop("Polite note: discountRate (k = ", discountRate, ") must be strictly greater than growthRate (g = ", growthRate, ") for a single-stage model.")
		}

		if (!is.null(ROIC) && ROIC > 0) {
			payoutRatio = (1 - (growthRate / ROIC)) + buybackYield
		} else {
			payoutRatio = (dividend / EPS) + buybackYield
		}
		payoutRatio = max(0, min(1, payoutRatio))

		if (type == "trailing") {
			justifiedPE = (payoutRatio * (1 + growthRate)) / (discountRate - growthRate)
		} else {
			justifiedPE = payoutRatio / (discountRate - growthRate)
		}
	}

	return(justifiedPE)
}

#' Open a ticker in yahoo finance.
#'
#' @description
#' Open a stock ticker, currently in yahoo finance
#'
#' @param ticker A stock symbol to look up, e.g., "OXY"
#' @param exchange Stock exchange code (default = "NASDAQ").
#' @param provider Financial data provider site ("GOOGLE" or "YAHOO", default = "GOOGLE").
#' @return - Open a ticker in a finance site online
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()], [fin_value_percent()], [fin_tax_NI()]
#' @examples
#' # Open $NVDA in google, MRVL in yahoo finance.
#' \dontrun{
#' fin_stock_ticker("NVDA")
#' fin_stock_ticker("MRVL", provider= "YAHOO")
#' }
fin_stock_ticker <- function(ticker= "NVDA", exchange = "NASDAQ", provider= c("GOOGLE", "YAHOO")) {
	provider = match.arg(provider)
	if(provider ==	"GOOGLE"){
		url =paste0("https://www.google.com/finance/quote/", ticker, ":", exchange)
	} else {
		url =paste0("https://finance.yahoo.com/quote/", ticker)
	}
	browseURL(url, browser = getOption("browser"))
}

#' NZ FIF tax offset: NAV-neutral leverage (FDR on the whole pile)
#'
#' Fair Dividend Rate tax is applied to **opening equity plus assets bought
#' with loan funds**.
#'
#' Neutral loan / opening equity:
#' \deqn{L/E = (f t) / (r - i(1-t) - f t)}{L/E = (f t) / (r - i(1-t) - f t)}
#'
#' When \eqn{f = i}{f = i} (both 5%), the leftover on the loan collapses to
#' \eqn{r - i}{r - i}: interest deduction pays FDR on the borrowed slice, and the
#' remaining spread pays FDR on the original book.
#'
#' \deqn{z = \frac{\bar{x} - \mu}{\sigma/\sqrt{n}}}{z = (xbar - mu)/(sigma/sqrt(n))}
#'
#' LTV on the plot is loan / opening equity, not loan / total assets.
#'
#' @param portfolioValue Opening FIF value (1 April), before the new loan.
#' @param marginRate IBKR (or other) annual margin rate (e.g. 0.05).
#' @param expectedReturn Expected annual return of the asset (e.g. 0.12).
#' @param taxRate Marginal tax rate (e.g. 0.38).
#' @param fifRate FDR deemed rate (default 0.05).
#' @return A ggplot of net annual impact vs LTV. Invisibly, a list with
#'   `loan`, `assets`, `ltvOpening`, `fdrDrag`, `leftoverOnLoan`.
#' @export
#' @family financial functions
#' @seealso [fin_value_interest()], [fin_tax_NI()], [fin_value_percent()]
#' @examples
#' # $3.8m opening, 5% IBKR, 12% expected, 38% tax -> ~$1.03m loan, ~$4.83m assets
#' fin_tax_FIF(portfolioValue = 3.8e6, marginRate = 0.05, expectedReturn = 0.12, taxRate = 0.38)
fin_tax_FIF <- function(portfolioValue=1.e6, marginRate=.056, expectedReturn = .12, taxRate = .38, fifRate = 0.05) {
	fdrDrag = fifRate * taxRate
	# Return on borrowed dollar, after interest, tax shield, and FDR on that dollar
	leftoverOnLoan = expectedReturn - marginRate * (1 - taxRate) - fdrDrag
	if (leftoverOnLoan <= 0) {
		stop("Polite note: expectedReturn (", expectedReturn,
			") cannot cover after-tax margin (", marginRate * (1 - taxRate),
			") plus FDR drag (", fdrDrag,
			"). Leverage cannot NAV-neutralise the pile.")
	}
	# L / opening equity
	ratioNavNeutral = fdrDrag / leftoverOnLoan
	loanNeutral = portfolioValue * ratioNavNeutral
	assetsNeutral = portfolioValue + loanNeutral

	maxRange = min(0.85, max(ratioNavNeutral * 2, 0.05))
	ltvRange = seq(0, maxRange, length.out = 100)
	netImpact = vapply(ltvRange, function(ltv) {
		loan = portfolioValue * ltv
		loan * leftoverOnLoan - portfolioValue * fdrDrag
	}, numeric(1))
	plotDf = data.frame(loanRatio = ltvRange, netGainLoss = netImpact)

	p = ggplot(plotDf, aes(x = loanRatio, y = netGainLoss))
	p = p + geom_line(color = "#2c3e50", linewidth = 1.2)
	p = p + geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c")
	p = p + geom_vline(xintercept = ratioNavNeutral, linetype = "dotted", color = "#27ae60")
	p = p + scale_y_continuous(labels = scales::dollar)
	p = p + scale_x_continuous(labels = scales::percent)
	p = p + labs(
		title = "NZ FIF: net impact (FDR on equity + borrowed assets)",
		subtitle = paste0(
			"Opening ", scales::dollar(portfolioValue),
			" | Neutral loan ", scales::dollar(loanNeutral),
			" | Assets ", scales::dollar(assetsNeutral)
		),
		x = "Loan / opening equity",
		y = "Net annual gain/loss vs FDR on the pile"
	)
	p = p + annotate("label", x = ratioNavNeutral, y = 0,
		label = paste0("NAV-neutral at ", round(ratioNavNeutral * 100, 1), "% of opening"),
		fill = "white", alpha = 0.8)
	p = p + theme_minimal()

	cat("=== NZ FIF (FDR on the whole pile) ===\n")
	cat("Opening:       ", scales::dollar(portfolioValue), "\n", sep = "")
	cat("Neutral loan:  ", scales::dollar(loanNeutral),
		"  (", round(ratioNavNeutral * 100, 1), "% of opening)\n", sep = "")
	cat("Assets:        ", scales::dollar(assetsNeutral), "\n", sep = "")
	cat("FDR drag:      ", round(fdrDrag * 100, 2), "% of the pile\n", sep = "")
	cat("Leftover/loan: ", round(leftoverOnLoan * 100, 2),
		"%  (return - after-tax interest - FDR)\n", sep = "")

	print(p)
	invisible(list(
		loan = loanNeutral,
		assets = assetsNeutral,
		ltvOpening = ratioNavNeutral,
		fdrDrag = fdrDrag,
		leftoverOnLoan = leftoverOnLoan
	))
}

#' Work the carry cost of a house
#'
#' @description
#' `fin_value_CarryCost` uses the purchase price, holding expenses, appreciation, and opportunity cost to compute a carrying cost for a house purchase.
#'
#' @param property_cost Purchase price
#' @param appreciation rate of property increase
#' @param QQQ Opportunity cost of leaving money in the markets
#' @param rent_saved But now you have to rent somewhere
#' @param interest Cost of borrowing
#' @param rates Council rates per year at t=1 (absolute).
#' @param insurance The cost of property owners insurance per year at t=1 (absolute).
#' @param maintenance New kitchen roof etc. If <1, treated as rate of property_cost at t=1; if >=1, treated as absolute per year at t=1.
#' @param years Holding time (integer >=1).
#' @param inflation Annual inflation applied to rent, rates, insurance and maintenance (default .025). Set 0 to recover flat model. Property and QQQ remain compound totals.
#' @param verbose Logical; if TRUE, print a one-line per-year schedule when years <= 20.
#' @return Invisibly the total net cost of buying (scalar). When verbose is TRUE, also returns a schedule data.frame as attribute "schedule".
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()], [fin_tax_NI()], [fin_value_percent()]
#' @examples
#' fin_value_CarryCost(property_cost=1.2e6)
#' fin_value_CarryCost(property_cost=1.1e6, appreciation = .035, QQQ=.15, years=10)
#' fin_value_CarryCost(property_cost=1.2e6, inflation=0) # flat, recovers pre-inflation total
#'
fin_value_CarryCost <- function(property_cost, appreciation = .02, QQQ = .14, rent_saved = .04, interest = .06, rates = 5000, insurance = 2000, maintenance = .015, years = 5, inflation = .025, verbose = TRUE){
  # base annual at t=1 (flat reference)
  rent0       = property_cost * rent_saved
  interest0   = property_cost * interest
  if (maintenance < 1) {
    maintenance0 = property_cost * maintenance
  } else {
    maintenance0 = maintenance
  }
  QQQgains  = (property_cost * (1+QQQ)^years) - property_cost
  propAprec = property_cost * ((1+appreciation)^years)
  propAprec = (propAprec*.97) - property_cost # 3% sale cost
  # per-year carry with inflation on rent/rates/insurance/maintenance; interest fixed (opportunity on price)
  annualCarry = numeric(years)
  rentAnnual = numeric(years)
  ratesAnnual = numeric(years)
  insuranceAnnual = numeric(years)
  maintenanceAnnual = numeric(years)
  for (t in 1:years) {
    infFactor = (1+inflation)^(t-1)
    rentAnnual[t]       = rent0 * infFactor
    ratesAnnual[t]      = rates * infFactor
    insuranceAnnual[t]  = insurance * infFactor
    maintenanceAnnual[t]= maintenance0 * infFactor
    annualCarry[t]      = (interest0 + ratesAnnual[t] + insuranceAnnual[t] + maintenanceAnnual[t]) - rentAnnual[t]
  }
  totalCarry = sum(annualCarry)
  flatCarry  = (interest0 + rates + insurance + maintenance0 - rent0) * years
  netnetCostOfBuying = totalCarry + QQQgains - propAprec
  # for reporting: keep original variable names for dollar formatting at t=1
  rent_saved  = rent0
  interest    = interest0
  maintenance = maintenance0
  Carry_Cost  = annualCarry[1]

  if((Carry_Cost/property_cost) > .015){
  	cat("Polite note: Carry Cost over the 1.5% threshold: **too high**\n\n")
  }
  cat(
	  "Purchase Price = ", dollar(as.numeric(property_cost) , prefix = "$"), "\n",
	  dollar(as.numeric(interest) , prefix = "$"),    "interest + ",
	  dollar(as.numeric(rates)    , prefix = "$"),    "rates + ",
	  dollar(as.numeric(insurance), prefix = "$"),    "insurance + ",
	  dollar(as.numeric(maintenance) , prefix = "$"), "maintenance - ",
	  dollar(as.numeric(rent_saved)  , prefix = "$"), "rent_saved (t=1)\n",
	  "Annual carry cost (t=1) = ", dollar(as.numeric(interest+ rates + insurance + maintenance -rent_saved), prefix = "$"), "\n",
	  "Assumed appreciation: QQQ ", QQQ*100, "% p.a., property ", appreciation*100, "% p.a. (net ", dollar(as.numeric(propAprec), prefix = "$"), " total after 3% sale cost over ", years, " years)\n",
	  "Inflation on rent/rates/insurance/maintenance: ", inflation*100, "% p.a.\n",
	  "Total carry = ", dollar(as.numeric(totalCarry), prefix = "$"), ")\n",
	  "Missed market gains  = ", dollar(as.numeric(QQQgains), prefix = "$"), " total over ", years, " years\n",
	  "Net-net cost of Buying = ", dollar(as.numeric(netnetCostOfBuying), prefix = "$"), " total over ", years, " years\n"
  )
  if(isTRUE(verbose) && years <= 20 && years > 1){
    cashflowTable = data.frame(
      Year        = 1:years,
      Carry       = dollar(as.numeric(annualCarry[1:years]), accuracy = 1),
      Rent        = dollar(as.numeric(rentAnnual[1:years]), accuracy = 1),
      Rates       = dollar(as.numeric(ratesAnnual[1:years]), accuracy = 1),
      Insurance   = dollar(as.numeric(insuranceAnnual[1:years]), accuracy = 1),
      Maintenance = dollar(as.numeric(maintenanceAnnual[1:years]), accuracy = 1)
    )  
    print(knitr::kable(cashflowTable, align = "r"))
  }

  schedule = data.frame(year=1:years, carry=annualCarry, rent=rentAnnual, rates=ratesAnnual, insurance=insuranceAnnual, maintenance=maintenanceAnnual)
  attr(netnetCostOfBuying, "schedule") = schedule
  attr(netnetCostOfBuying, "totalCarry") = totalCarry
  attr(netnetCostOfBuying, "flatCarry") = flatCarry
  invisible(netnetCostOfBuying)
}

#' Compute the net present value of a future income stream.
#' @description
#' `fin_stock_valuation` uses the revenue, operating margin, expenses and PE to compute a market capitalization.
#' Better to use a more powerful online site.
#' @details
#' Revenue stream is discounted back to a present day cash amount which is equivalent.
#' @param income Value of expected recurring payment
#' @param discount_rate Percent return to discount against (.05 = 5%)
#' @param periods How many periods the stream delivers, e.g., 25 years of pension.
#' @param symbol Currency symbol to use
#' @return - value
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()], [fin_tax_NI()], [fin_value_percent()]
#' @examples
#' fin_value_NPV(27e3, .05, 25)
#'
fin_value_NPV <- function(income=27e3, discount_rate=.05, periods = 25, symbol = umx_set_dollar_symbol(silent=TRUE)) {	
	cashflows   = rep(income, periods)
	timePeriods = seq(1, periods)
	discount_factors = 1/(1+discount_rate)^timePeriods
	present_values = cashflows*discount_factors
	pv = sum(present_values)
	cat("\nBased on a discount rate of ", discount_rate*100, "%, an income of ", bucks(income, symbol, cat=TRUE), " for ", periods, " years, has a net present value of \n", sep="")
	cat("\n", bucks(pv, symbol))
	invisible(pv)
}

#' Compute the percent change needed to return to the original value after percent off (or on).
#'
#' @description
#' Determine the percent change needed to "undo" an initial percent change. Has a plot function as well.
#' If an amount of $100 has 20% added, what percent do we need to drop it by to return to the original value?
#' 
#' `fin_value_percent(20)` yields $100 increased by 20% = $120 (Percent to reverse = -17%)
#' 
#' @param percent Change in percent (enter 10 for 10%, not 0.1)
#' @param value Principal
#' @param symbol value units (default = "$")
#' @param digits Rounding of results (default 2 places)
#' @param plot Whether to plot the result (default TRUE)
#' @param logY Whether to plot y axis as log (TRUE)
#' @return - new value and change required to return to baseline.
#' @export
#' @family financial functions
#' @seealso - [fin_value_interest()]
#' @examples
#' # Percent needed to return to original value after 10% taken off
#' fin_value_percent(-10)
#' # Percent needed to return to original value after 10% added on
#' fin_value_percent(10)
#' # Percent needed to return to original value after 50% off 34.50
#' fin_value_percent(-50, value = 34.5)
fin_value_percent <- function(percent, value= 100, symbol = "$", digits = 2, plot = TRUE, logY = TRUE) {
	percent  = percent/100
	newValue = value * (1 + percent)
	percent_to_reverse = (value/newValue) - 1
	class(newValue) = 'percent'
	attr(newValue, 'oldValue') = value
	attr(newValue, 'percent')  = percent
	attr(newValue, 'digits')   = digits
	attr(newValue, 'symbol')   = symbol
	attr(newValue, 'percent_to_reverse') = percent_to_reverse

	if(plot){
		plot(newValue, logY = logY)
	}else{
		return(newValue)
	}
}

#' Print a percent object
#'
#' Print method for "percent" objects: e.g. [umx::fin_value_percent()].
#' @param x percent object.
#' @param ... further arguments passed to or from other methods.
#' @return - invisible
#' @seealso - [umx::fin_value_percent()]
#' @method print percent
#' @export
#' @examples
#' # Percent needed to return to original value after 10% off
#' fin_value_percent(-10)
#' # Percent needed to return to original value after 10% on
#' fin_value_percent(10)
#'
#' # Percent needed to return to original value after 50% off 34.50
#' fin_value_percent(-50, value = 34.5)
#'
print.percent <- function(x, ...) {
	if(!is.null(attr(x, 'digits')) ){
		digits = attr(x, 'digits')
	}
	oldValue = round(attr(x, 'oldValue'), digits)
	percentChange  = attr(x, 'percent')
	symbol   = attr(x, 'symbol')
	percent_to_reverse = round(attr(x, 'percent_to_reverse'), digits)
	dir = ifelse(percentChange < 0, "decreased", "increased")

	cat(symbol, oldValue, " ", dir , " by ", percentChange*100, "% = ", symbol, x, " (Percent to reverse = ", percent_to_reverse*100, "%)", sep="")
}

#' Plot a percent change graph
#'
#' Plot method for "percent" objects: e.g. [umx::fin_value_percent()].
#' @param x percent object.
#' @param ... further arguments passed to or from other methods.
#' @return - invisible
#' @seealso - [umx::fin_value_percent()]
#' @method plot percent
#' @export
#' @examples
#' # Percent needed to return to original value after 10% off
#' fin_value_percent(-10)
#' # Percent needed to return to original value after 10% on
#' tmp = fin_value_percent(10)
#' plot(tmp)
#'
#' # Percent needed to return to original value after 50% off 34.50
#' fin_value_percent(-50, value = 34.5, logY = FALSE)
#'
plot.percent <- function(x, ...) {
	tmp = list(...) # pull logY if passed in
	logY = tmp$logY
	symbol   = attr(x, 'symbol')
	digits   = attr(x, 'digits')
	oldValue = round(attr(x, 'oldValue'), digits)
	percentChange  = attr(x, 'percent')	
	percent_to_reverse = round(attr(x, 'percent_to_reverse'), digits)
	dir = ifelse(percentChange < 0, "decreased", "increased")
	fnReversePercent <- function(x) {
		# 1/(1+.1)
		percentOn = x/100
		newValue = (1 + percentOn)
		percent_to_reverse = 1-(1/newValue)
		return(-percent_to_reverse*100)
	}
	if(percentChange > 0){
		p = ggplot(data.frame(x = c(0, 90)), aes(x))
		lab = paste0(round(percentChange*100, 2), "% on = ", round(percent_to_reverse * 100, 2), "% off", sep = "")
		labXpos = 50
		labYpos = -20
		logY = FALSE
	} else {
		p = ggplot(data.frame(x = c(-90, 0)), aes(x))
		lab = paste0(round(percentChange*100, 2), "% off = ", round(percent_to_reverse * 100, 2), "% on", sep = "")
		labXpos = -50
		labYpos = 700
	}
	if(is.null(logY)||!(logY)){
		p = p + ggplot2::scale_y_continuous(n.breaks = 8) + ggplot2::scale_x_continuous(n.breaks = 10)
		p = p + cowplot::draw_label(lab, vjust = 1, hjust = .5, x = labXpos, y = labYpos, color= "grey")
		# hor & vert
		p = p + ggplot2::geom_segment(x = percentChange*100, xend=-100, y=percent_to_reverse*100, yend=percent_to_reverse*100, alpha=.5, color = "lightgrey")
		p = p + ggplot2::geom_segment(x = percentChange*100, xend=percentChange*100, y=-10, yend=percent_to_reverse*100, alpha=.5, color = "lightgrey")
	} else {
		p = p + ggplot2::scale_y_continuous(n.breaks = 8, trans="log10") + ggplot2::scale_x_continuous(n.breaks = 10) 
		p = p + cowplot::draw_label(lab, vjust = 1, hjust = .5, x = labXpos, y = log10(labYpos), color= "grey")
		# hor & vert
		p = p + ggplot2::geom_segment(x = percentChange*100, xend=-100             , y= log10(percent_to_reverse*100), yend= log10(percent_to_reverse*100), alpha=.5, color = "lightgrey")
		p = p + ggplot2::geom_segment(x = percentChange*100, xend=percentChange*100, y= -10, yend= log10(percent_to_reverse*100), alpha= .5, color = "lightgrey")
	}
	p = p + ggplot2::stat_function(fun = fnReversePercent, color= "lightblue")
	p = p + labs(x = "Percent change", y = "Percent change to reverse", title = paste0(round(percentChange*100, 2), "% ", ifelse(percentChange>0, "on ", "off "), oldValue, " = ", (1+percentChange)*oldValue))
	p = p + cowplot::theme_cowplot(font_size = 11)
	print(p)
	cat(symbol, oldValue, " ", dir , " by ", percentChange*100, "% = ", symbol, x, " (Percent to reverse = ", percent_to_reverse*100, "%)", sep="")
	invisible(p)
}

#' Set the symbol for money
#'
#' Set umx_set_dollar_symbol (used in e.g. [fin_value_interest()]
#'
#' @param umx.dollar.symbol symbol for money calculations.
#' @param silent If TRUE, no message will be printed.
#' @return - Current umx.dollar.symbol
#' @export
#' @family Get and set
#' @examples
#' library(umx)
#' umx_set_dollar_symbol() # show current state
#' old = umx_set_dollar_symbol(silent=TRUE) # store existing value
#' fin_value_interest(100)
#' umx_set_dollar_symbol(old)    # reinstate
umx_set_dollar_symbol <- function(umx.dollar.symbol = NULL, silent = FALSE) {
	if(is.null(umx.dollar.symbol)) {
		if(!silent){ message("Current format is ", omxQuotes(getOption("umx.dollar.symbol"))	) }
		invisible(getOption("umx.dollar.symbol"))
	} else {
		options("umx.dollar.symbol" = umx.dollar.symbol)
	}
}

#' Print a money object
#'
#' @description Print function for "money" objects, e.g. [fin_value_interest()].
#'
#' @aliases bucks print
#' @param x money object.
#' @param symbol Default prefix if not set.
#' @param big.mark option defaulting to ","
#' @param decimal.mark option defaulting to "."
#' @param trim option defaulting to TRUE
#' @param largest_with_cents option defaulting to 1e+05
#' @param negative_parens option defaulting to "hyphen"
#' @param ... further arguments passed to or from other methods. also cat =F to return string
#' @return - invisible
#' @seealso - [umx::fin_value_percent()], [umx::fin_value_interest()], [scales::dollar()]
# #' @family print
#' @export
#' @examples
#' bucks(100 * 1.05^32)
#' fin_value_interest(deposits = 20e3, interest = 0.07, yrs = 20)
#'
bucks <- function(x, symbol = umx_set_dollar_symbol(silent=TRUE), big.mark = ",", decimal.mark = ".", trim = TRUE, largest_with_cents = 1e+05, negative_parens = c("hyphen", "minus", "parens"), ...) {
	dot.items = list(...) # grab all the dot items cat
	cat = ifelse(is.null(dot.items[["cat"]]), TRUE, dot.items[["cat"]])
	if(is.null(dot.items[["cat"]])){
		cat = TRUE
	} else {
		cat = FALSE
		dot.items[["cat"]] = NULL
	}

	if(!is.null(attr(x, 'symbol')) ){
		symbol = attr(x, 'symbol')
	}
	formatted = scales::dollar(as.numeric(x), prefix = symbol, big.mark = big.mark, decimal.mark = decimal.mark, trim =trim, largest_with_cents = largest_with_cents, style_negative = negative_parens, ...)
	if(cat){
		cat(formatted)
	} else {
		formatted
	}
}

#' @export
#' @method print money
print.money <- bucks

