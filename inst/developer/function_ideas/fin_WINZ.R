libs(ggplot2, dplyr, tidyr, scales)

fin_WINZ = function(capital = 301e3, baseCosts = (46 * 365), income = 32000, years = 4, fdrRate = 0.05, pirRate = 0.175, winzExemption = 1267, marketReturn = 0.15, inflationRate = 0.031) {  
  yearVector = 0:years
  capitalVector = numeric(years + 1)
  capitalVector[1] = capital
  
  currentCapital = capital
  
  for (i in 1:years) {
    # 1. Compound the Metlifecare room premium by inflation
    currentCosts = baseCosts * (1 + inflationRate)^(i - 1)
    
    # 2. Calculate statutory/deemed income
    fdrIncome = currentCapital * fdrRate
    
    # 3. Calculate tax and WINZ offsets
    taxPaid = fdrIncome * pirRate
    # 4. Aggregate total capital drain (Room + WINZ Shortfall + PIE Tax)
    totalOutflow = currentCosts + taxPaid
    
    # 5. Calculate actual market NAV growth
    marketGrowth = currentCapital * marketReturn
    
    # 6. Reconcile end of year capital
    currentCapital = currentCapital + marketGrowth - totalOutflow
    capitalVector[i + 1] = currentCapital
  }
	realisedReturn = ((currentCapital/capital)^(1/years)) -1
	labGraph = paste0(round(realisedReturn, 3)*100, "% realisedReturn. Final value", scales::dollar(currentCapital) )

  plotData = data.frame(year = yearVector, capitalValue = capitalVector)
  
  p = ggplot(plotData, aes(x = year, y = capitalValue))
  p = p + geom_line(color = "steelblue", linewidth = 1.2)
  p = p + geom_point(color = "darkred", size = 2.5)
	p = p + scale_y_continuous(labels = dollar_format(prefix = "$"), n.breaks = 10)
  p = p + labs(
    title = "Estate NAV Trajectory: US Growth PIE vs WINZ/Care Depletion", 
    subtitle = paste("Market Return", marketReturn * 100, "%,", inflationRate * 100, "% premium room inflation.\n", labGraph),
    x = "Years in Care", 
    y = "Net Asset Value (NZD)"
  )
  p = p + theme_minimal()
  
  print(p)
	return(realisedReturn)
}

fin_WINZ(291e3, baseCosts = (46 * 365), income = 32000, years = 5, marketReturn = 0.15)


#' Plot earnings yield or price evolution under varying EPS growth assumptions
#'
#' Generates a line plot showing how earnings yield (E[t]/P[0]) or normalized price
#' (P[t]/P[0]) evolves over time for 10 evenly spaced constant annual EPS growth rates.
#' Legend levels are forced into strict ascending numerical order (no alphabetical sort).
#' When `abscissa = "Price"` the y-axis shows the price multiple assuming the P/E ratio
#' stays constant forever (price compounds exactly at the EPS growth rate).
#'
#' @param initialPE Numeric scalar. Starting price-to-earnings ratio (e.g. 30).
#' @param lowGrowth Numeric scalar. Lowest annual EPS growth rate to test (default 0.05).
#' @param highGrowth Numeric scalar. Highest annual EPS growth rate to test (default 0.30).
#' @param treasuryYield Numeric scalar. Risk-free rate plotted as a horizontal reference
#'   (only shown when `abscissa = "Yield"`; default 0.046).
#' @param timeHorizon Integer. Number of years forward to plot (default 5).
#' @param abscissa Character. One of `"Yield"` (default) or `"Price"`. Controls
#'   which quantity appears on the y-axis.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Default: earnings yield view
#' plotYieldDynamics(initialPE = 30, lowGrowth = .05, highGrowth = .25)
#'
#' # Price-multiple view (constant P/E assumption)
#' plotYieldDynamics(initialPE = 30, lowGrowth = .05, highGrowth = .25, abscissa = "Price")
#'
#' @export
plotYieldDynamics(initialPE = 30, lowGrowth = .07, highGrowth = .25, abscissa = "Yield")
plotYieldDynamics(initialPE = 35, lowGrowth = .07, highGrowth = .30, abscissa = "Yield")
plotYieldDynamics(initialPE = 25, lowGrowth = .20, highGrowth = .30, abscissa = "Price")
plotYieldDynamics = function(initialPE, lowGrowth = .05, highGrowth = .30, treasuryYield = .046, timeHorizon = 5, abscissa = c("Yield", "Price")) {
  abscissa      = match.arg(abscissa)
  years         = 0:timeHorizon
  growthRates   = round(seq(lowGrowth, highGrowth, length.out = 10), 2)
  startingYield = 1 / initialPE

  plotData = expand.grid(year = years, growthRate = growthRates)

  # growthLabel with explicit ascending order (prevents alphabetical legend)
  plotData$growthLabel = factor(
    paste0(plotData$growthRate * 100, "%"),
    levels = paste0(growthRates * 100, "%")
  )

  if (abscissa == "Yield") {
    plotData$value = startingYield * (1 + plotData$growthRate)^plotData$year
    y_label        = expression("Yield (" * E[t] / P[0] * ")")
    main_title     = "Earnings Yield on Initial Purchase Price Over Time"
    y_formatter    = scales::percent_format(accuracy = 1)
  } else {  # Price: constant-PE assumption → price compounds exactly at EPS growth rate
    plotData$value = (1 + plotData$growthRate)^plotData$year   # P[0] normalized to 1
    y_label        = "Price Multiple (P[t] / P[0])"
    main_title     = "Price Evolution (Constant P/E Assumption)"
    y_formatter    = scales::number_format(accuracy = 0.01)
  }

  p = ggplot(plotData, aes(x = year, y = value, color = growthLabel)) +
    geom_line(linewidth = 1.2) +
    scale_y_continuous(labels = y_formatter) +
    scale_x_continuous(breaks = years) +
    labs(title = main_title, subtitle = paste0("Starting P/E: ", initialPE), x = "Years Since Purchase", y = y_label, color = "Annual EPS Growth") +
    theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))

  # treasury reference line only makes sense for the Yield view
  if (abscissa == "Yield") {
    p = p + geom_hline(yintercept = treasuryYield, color = "red", linetype = "dotted", linewidth = 1.2)
  }
  p
}

# Example execution matching today's environment:
# Assuming a starting P/E of 30 and a 2-Year Treasury yield of 4.6%
# plotYieldDynamics(30, lowGrowth = .05, highGrowth = .30)


Memory on the other hand. $MU MC currently $800B.
Bear case: companies buy back 1/3 of their shares in the next 2-3 years. PE stays around 10.
Base case, Demand increases at 50%/yr out to 2030. Prices follow. Stock re-prices doubles.
Bull case: Everything cars, tractors, robots gets 280GB of HBM. SK, MU, and Samsung each become $4T behemoths.

Narrative control vs. information:
Elon saying transformers will be a bottleneck = signal.
Elon saying Anthropic never had any route to success = narrative control/belief inception.

“The top stocks to buy”
“Crash is coming”
“Smart money is moving”

It feels like insight, but its a just noise: a headline. It is pressure to act.

Those who listen get hurt. They start chasing. Drive others to chase. Can't survive alone.

Patience is one of the few real advantages investors have.