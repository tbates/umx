#    fin_cyclical_model: no visible global function definition for ‘lines’
#    fin_cyclical_model: no visible global function definition for ‘legend’
#    Undefined global functions or variables:
#      legend lines
#    Consider adding
#      importFrom("graphics", "legend", "lines")
#    to your NAMESPACE file.
#' Toy cyclical-commodity earnings model
#'
#' Constant-elasticity demand and supply with market clearing every year: demand is \eqn{Qd = (1+demand)^t * P^-elas_d}, supply is \eqn{Qs = (1+supply)^t * P^elas_s}, so the clearing price is \eqn{P_t = ((1+demand)/(1+supply))^(t/(elas_d+elas_s))} with \eqn{P_0 = Q_0 = 1}. Costs are fixed plus a constant variable cost per unit, calibrated so the `t0` margin equals `margin` and `fixed_share` of `t0` cost is fixed.
#'
#' Price moves scale with \eqn{1/(elas_d + elas_s)}: inelastic markets turn small supply/demand growth gaps into huge price swings, and fixed costs turn those into hyper-elastic earnings. Elasticities are magnitudes: demand elasticity -0.3 becomes \eqn{elas_d = 0.3}.
#'
#' @param supply Annual proportional increase in the supply; `supply = .2` is 20%/yr.
#' @param demand Annual proportional increase in the demand.
#' @param margin Earnings margin at t0 (revenue normalised to 1).
#' @param elas_d Demand elasticity magnitude; may be vector, in which case combinations are simulated and overlaid.
#' @param elas_s Magnitude of supply elasticity; may be vector, in which case combinations are simulated and overlaid.
#' @param fixed_share Share of t0 total cost that is fixed (the rest is variable cost per unit).
#' @param years Number of years to simulate (t = 0..years).
#' @param plot.it Draw the price-path and earnings-path panels?
#' @return The full scenario paths, invisibly, as a data.frame with columns year, elas_d, elas_s, price, quantity, revenue, earnings.
#' @examples
#' # HBM boom: demand +35%/yr vs supply +15%. Watch the d=0.2 line detach.
#' fin_cyclical_model(supply = .15, demand = .35, margin = .5, elas_d = c(.2, .5), elas_s = .15, years = 8)
#' # The bust: supply +25%/yr, demand +5%. Inelasticity cuts both ways.
#' fin_cyclical_model(supply = .25, demand = .05, margin = .6, elas_d = .3, elas_s = c(.1, .4), years = 8)
fin_cyclical_model = function(supply = .2, demand = 0, margin = .5, elas_d = c(.2, .4, .8), elas_s = .15, fixed_share = .5, years = 10, plot.it = TRUE) {
	stopifnot(margin < 1, elas_d > 0, elas_s > 0, fixed_share >= 0, fixed_share <= 1, years >= 1)
	scen = expand.grid(elas_d = elas_d, elas_s = elas_s)
	if (nrow(scen) > 9) warning("plotting ", nrow(scen), " elasticity scenarios; consider fewer")
	t = 0:years
	out = do.call(rbind, lapply(seq_len(nrow(scen)), function(i) {
		ed = scen$elas_d[i]; es = scen$elas_s[i]
		price = ((1 + demand) / (1 + supply))^(t / (ed + es))
		qty = (1 + supply)^t * price^es
		rev = price * qty
		fc = fixed_share * (1 - margin)
		vc = (1 - fixed_share) * (1 - margin)
		earn = rev - fc - vc * qty
		data.frame(year = t, elas_d = ed, elas_s = es, price = price, quantity = qty, revenue = rev, earnings = earn)
	}))
	if (plot.it) {
		lab = paste0("d=", scen$elas_d, " s=", scen$elas_s)
		cols = seq_len(nrow(scen))
		op = par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 1)); on.exit(par(op), add = TRUE)
		plot(NA, xlim = range(t), ylim = range(out$price), xlab = "year", ylab = "price (P0 = 1)", main = "Price path")
		for (i in seq_len(nrow(scen))) { d = out[out$elas_d == scen$elas_d[i] & out$elas_s == scen$elas_s[i], ]; lines(d$year, d$price, col = cols[i], lwd = 2) }
		legend("topleft", legend = lab, col = cols, lwd = 2, bty = "n", cex = .8)
		plot(NA, xlim = range(t), ylim = range(out$earnings), xlab = "year", ylab = "earnings (R0 = 1)", main = "Earnings path")
		abline(h = 0, lty = 3, col = "grey50")
		for (i in seq_len(nrow(scen))) { d = out[out$elas_d == scen$elas_d[i] & out$elas_s == scen$elas_s[i], ]; lines(d$year, d$earnings, col = cols[i], lwd = 2) }
	}
	invisible(out)
}
