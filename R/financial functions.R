# =======================
# = Financial utilities =
# =======================

#' Work the valuation of a company
#'
#' @description
#' `fin_valuation` uses the revenue, operating margin, expenses and PE to compute a market capitalization.
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
#' @family Miscellaneous Functions
#' @seealso - [fin_interest()], [fin_NI()], [fin_percent()]
#' @md
#' @examples
#' fin_valuation(rev=7e9, opmargin=.1, PE=33)
#' # Market cap =  $18,480,000,000
#' # (Based on PE= 33, operating Income of $0.70 B, and net income =$0.56B
#'
fin_valuation <- function(revenue=6e6*30e3, opmargin=.08, expenses=.2, PE=30, symbol = "$", use = c("B", "M")) {
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

#' Compute the net present value of a future income stream
#'
#' @description
#' `fin_valuation` uses the revenue, operating margin, expenses and PE to compute a market capitalization.
#' Better to use a more powerful online site.
#'
#' @details
#' Revenue stream is discounted back to a present day cash amount which is equivalent.
#' 
#' @param cashflow Value of expected recurring payment
#' @param discount Percent return to discount against (.05 = 5%)
#' @param periods How many periods the stream delivers, e.g., (90-65) for 25 of a pension.
#' @param PE of the company
#' @param symbol Currency
#' @param use reporting values in "B" (billion) or "M" (millions)
#' @return - value
#' @export
#' @family Miscellaneous Functions
#' @seealso - [fin_interest()], [fin_NI()], [fin_percent()]
#' @md
#' @examples
#' fin_net_present_value(27e3, .05, 25)
#'
fin_net_present_value <- function(income=27e3, discount_rate=.05, periods = 25, symbol = NULL) {
	if(is.null(symbol)){symbol = umx_set_dollar_symbol(silent=TRUE)}
	
	cashflows   = rep(income, periods)
	timePeriods = seq(1, periods)
	discount_factors = 1/(1+discount_rate)^timePeriods
	present_values = cashflows*discount_factors
	pv = sum(present_values)
	cat("\nBased on a discount rate of ", discount_rate*100, "%, an income of ", bucks(income, symbol, cat=TRUE), " for ", periods, " years, has a net present value of \n", sep="")
	cat("\n", bucks(pv, symbol))

	invisible(pv)
}

#' Compute the future value and gain of an investment
#'
#' @description
#' fin_expected takes a current and fair value, as well as a cost of capital, and returns the expected gain.
#'
#' @param current The current market value of the instrument
#' @param fair The user's estimated fair value.
#' @param capital The cost of capital (defaults to .15)
#' @return - expected gain
#' @export
#' @family Miscellaneous Functions
#' @seealso - [fin_interest()]
#' @md
#' @examples
#' fin_expected(15, 45)
fin_expected <- function(current, fair, capital=.15) {
	delta  = (fair-current)      ; cat("delta (fair-current)= $", delta, "\n")
	growth = fair*capital        ; cat("growth = $", growth, "\n")
	expectedGain = (growth+delta); cat("expected gain = $", expectedGain, "\n")
	final  = fair*(1+capital)    ; cat("future value (final) = $", final, "\n")
	cat("return = ", round(((final/current)-1)*100, 2), "%\n")
	return(expectedGain)
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
#' @return - Value of balance after yrs of investment.
#' @export
#' @family Miscellaneous Functions
#' @seealso - [umx_set_dollar_symbol()], [fin_percent()], [fin_NI()], [fin_valuation()]
#' @references - <https://en.wikipedia.org/wiki/Compound_interest>
#' @md
#' @examples
#' \dontrun{
#' # 1. Value of a principal after yrs years at 5% return, compounding monthly.
#' # Report in browser as a nice table of annual returns and formatted totals.
#' fin_interest(principal = 5000, interest = 0.05, rep= "html")
#' }
#'
#' # Report as a nice markdown table
#' fin_interest(principal = 5000, interest = 0.05, yrs = 10)
#'
#' umx_set_dollar_symbol("£")
#' # 2 What rate is needed to increase principal to final value in yrs time?
#' fin_interest(1, final = 1.4, yrs=5)
#' fin_interest(principal = 50, final=200, yrs = 5)
#'
#' # 3. What's the value of deposits of $100/yr after 10 years at 7% return?
#' fin_interest(0, deposits = 100, interest = 0.07, yrs = 10, n = 12)
#'
#' # 4. What's the value of $20k + $100/yr over 10 years at 7% return?
#' fin_interest(principal= 20e3, deposits= 100, interest= .07, yrs= 10, symbol="$")
#'
#' # 5. What is $10,000 invested at the end of each year for 5 years at 6%?
#' fin_interest(deposits = 10e3, interest = 0.06, yrs = 5, n=1, when= "end")
#'
#' # 6. What will $20k be worth after 10 years at 15% annually (n=1)?
#' fin_interest(deposits=20e3, interest = 0.15, yrs = 10, n=1, baseYear=1)
#' # $466,986
#'
#' # manual equivalent
#' sum(20e3*(1.15^(10:1))) # 466985.5
#'
#' # 7. Annual (rather than monthly) compounding (n=1)
#' fin_interest(deposits = 100, interest = 0.07, yrs = 10, n=1)
#' 
#' # 8 Interest needed to increase principal to final value in yrs time.
#' fin_interest(principal = 100, final=200, yrs = 5)
#'
fin_interest <- function(principal = 100, deposits = 0, inflate = 0, interest = 0.05, yrs = 10, final= NULL, n = 12, when = "beginning", symbol = NULL, largest_with_cents = 0, baseYear= as.numeric(format(Sys.time(), "%Y")), table = TRUE, report= c("markdown", "html")){
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
		return((final/principal)^(1/(yrs+1))-1)
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
	return(Total)
}


#' Compute NI given annual Earnings.
#'
#' @description
#' Employees pay contributions at 12%% on annual earnings between £9,568 and £50,270. Above that you pay at 2%%. 
#' Employers pay at 13.8%% on all annual earnings of more than £8,840, although there are different thresholds 
#' for those under the age of 21 and for apprentices under the age of 25.
#'
#' @param annualEarnings Employee annual earnings.
#' @param symbol Currency symbol to embed in the result.
#' @return - NI
#' @export
#' @family Miscellaneous Functions
#' @seealso - [fin_interest()], [fin_percent()], [fin_valuation()]
#' @references - <https://www.telegraph.co.uk/tax/tax-hacks/politicians-running-scared-long-overdue-national-insurance-overhaul/>
#' @md
#' @examples
#' fin_NI(42e3)
#' fin_NI(142000)
#'
fin_NI <- function(annualEarnings, symbol = "\u00A3") {
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

#' Print a money object
#'
#' @description Print function for "money" objects, e.g. [fin_interest()].
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
#' @seealso - [umx::fin_percent()], [umx::fin_interest()], [scales::dollar()]
#' @md
# #' @family print
#' @export
#' @examples
#' bucks(100 * 1.05^32)
#' fin_interest(deposits = 20e3, interest = 0.07, yrs = 20)
#'
bucks <- function(x, symbol = "$", big.mark = ",", decimal.mark = ".", trim = TRUE, largest_with_cents = 1e+05, negative_parens = c("hyphen", "minus", "parens"), ...) {
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

#' Compute the percent change needed to return to the original value after percent off (or on).
#'
#' @description
#' Determine the percent change needed to "undo" an initial percent change. Has a plot function as well.
#' If an amount of $100 has 20% added, what percent do we need to drop it by to return to the original value?
#' 
#' `fin_percent(20)` yields $100 increased by 20% = $120 (Percent to reverse = -17%)
#' 
#' @param percent Change in percent (enter 10 for 10%, not 0.1)
#' @param value Principal
#' @param symbol value units (default = "$")
#' @param digits Rounding of results (default 2 places)
#' @param plot Whether to plot the result (default TRUE)
#' @param logY Whether to plot y axis as log (TRUE)
#' @return - new value and change required to return to baseline.
#' @export
#' @family Miscellaneous Functions
#' @seealso - [fin_interest()]
#' @md
#' @examples
#' # Percent needed to return to original value after 10% taken off
#' fin_percent(-10)
#'
#' # Percent needed to return to original value after 10% added on
#' fin_percent(10)
#'
#' # Percent needed to return to original value after 50% off 34.50
#' fin_percent(-50, value = 34.5)
fin_percent <- function(percent, value= 100, symbol = "$", digits = 2, plot = TRUE, logY = TRUE) {
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
#' Print method for "percent" objects: e.g. [umx::fin_percent()]. 
#'
#' @param x percent object.
#' @param ... further arguments passed to or from other methods.
#' @return - invisible
#' @seealso - [umx::fin_percent()]
#' @md
#' @method print percent
#' @export
#' @examples
#' # Percent needed to return to original value after 10% off
#' fin_percent(-10)
#' # Percent needed to return to original value after 10% on
#' fin_percent(10)
#'
#' # Percent needed to return to original value after 50% off 34.50
#' fin_percent(-50, value = 34.5)
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
#' Plot method for "percent" objects: e.g. [umx::fin_percent()]. 
#'
#' @param x percent object.
#' @param ... further arguments passed to or from other methods.
#' @return - invisible
#' @seealso - [umx::fin_percent()]
#' @md
#' @method plot percent
#' @export
#' @examples
#' # Percent needed to return to original value after 10% off
#' fin_percent(-10)
#' # Percent needed to return to original value after 10% on
#' tmp = fin_percent(10)
#' plot(tmp)
#'
#' # Percent needed to return to original value after 50% off 34.50
#' fin_percent(-50, value = 34.5, logY = FALSE)
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
	# fnReversePercent(-.1)
	fnReversePercent <- function(x) {
		# 1/(1+.1)
		percentOn = x/100
		newValue = (1 + percentOn)
		percent_to_reverse = 1-(1/newValue)
		return(-percent_to_reverse*100)
	}
	# x range	= -100 (%) to +500 (%)?
	# y = -100 to +200?
	# y range	= -100 to +200?
	if(percentChange>0){
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
	p = p + labs(x = "Percent change", y = "Percent change to reverse", title = paste0(percentChange*100, " percent ", ifelse(percentChange>0, "on ", "off "), oldValue, " = ", (1+percentChange)*oldValue))
	# p = p + ggplot2::geom_area() can't do with stat fun ...

	# p = p + cowplot::draw_label("\u2B55", hjust=0, vjust=1, x = percentChange*100, y = percent_to_reverse*100, color = "lightblue")

	if(umx_set_plot_use_hrbrthemes(silent = TRUE)){
		# p = p + hrbrthemes::theme_ipsum()
		p = p + hrbrthemes::theme_ft_rc()
	} else {
		# p = p + ggplot2::theme_bw()
		p = p + cowplot::theme_cowplot(font_size = 11)
	}

	
	print(p)
	cat(symbol, oldValue, " ", dir , " by ", percentChange*100, "% = ", symbol, x, " (Percent to reverse = ", percent_to_reverse*100, "%)", sep="")
	invisible(p)
}

#' Justified P/E Ratio
#'
#' @description
#' Compute the Justified P/E of a stock.
#' Justified P/E = ( (DPS / EPS) * (1 + g)) / (k – g)
#' DPS is the dividend per share, EPS is the earnings per share,
#' g is the sustainable growth rate, and k is the required rate of return.
#' @param Dividend The dividend.
#' @param EPS The Earnings per Share.
#' @param growthRate The growth rate.
#' @param discountRate Your chosen discount rate.
#' @param basePE The base PE.
#' @param yrs Years.
#' @return - A PE that is justified for this stock.
#' @export
#' @family Miscellaneous Functions
#' @seealso - [fin_interest()], [fin_percent()], [fin_NI()]
#' @md
#' @examples
#' # fin_JustifiedPE(Dividend= .8, EPS = 2, growthRate = .06, discountRate = .1)
#' 
fin_JustifiedPE <- function(Dividend= .02, EPS = 1, growthRate = .08, discountRate = .12, basePE= 20, yrs=10) {
	paste0("Based on growth (", growthRate*100, "% expected growth for ", yrs, " years and a base P/E of ",
	basePE, "), the justified P/E would be: ", (growthRate * yrs) + basePE )
	
   # ((0.4 * 2) * (1 + 0.06)) / (0.1 - 0.06)
   # ((Dividend/EPS) * (1 + growthRate)) / (k-growthRate)
   # Justified P/E Ratio = 16.8
}

#' Open a ticker in yahoo finance.
#'
#' @description
#' Open a stock ticker, currently in yahoo finance
#'
#' @param ticker A stock symbol to look up, e.g., "OXY"
#' @return - Open a ticker in a finance site online
#' @export
#' @family Miscellaneous Functions
#' @seealso - [fin_interest()], [fin_percent()], [fin_NI()]
#' @md
#' @examples
#' # Open $INTC in yahoo finance.
#' \dontrun{
#' fin_ticker("INTC")
#' }

fin_ticker <- function(ticker= "INTC") {
	url =paste0("https://finance.yahoo.com/quote/", ticker)
	# https://www.google.com/finance/quote/SWBI:NASDAQ
	browseURL(url, browser = getOption("browser"))
}
