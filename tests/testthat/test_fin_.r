# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxPath.r") 
# test_package("umx")
test_that("fin_* works", {	
	require(umx)
	expect_equal(fin_CAGR(beginningValue = 100, endingValue = 190, numYears = 7), 0.096)
	expect_equal(fin_CAGR(beginningValue = 100, endingValue = 50, numYears = 5), -0.129)
	expect_equal(fin_CAGR(beginningValue = 100, endingValue = 50, numYears = 5, digits = 2), -0.13)
	
	# Error: Inputs must be positive
	expect_error(fin_CAGR(beginningValue =  0   , endingValue = 50, numYears =  5), "Inputs must be positive values")
	expect_error(fin_CAGR(beginningValue = 25   , endingValue = 50, numYears = -1), "Inputs must be positive values")
	expect_error(fin_CAGR(beginningValue = "100", endingValue = 50, numYears = -1), "All inputs must be numeric")
	# fin_stock_justifiedPE tests
	# Trailing: (0.4 * 1.06) / (0.10 - 0.06) = 0.424 / 0.04 = 10.6
	expect_equal(fin_stock_justifiedPE(dividend = 0.8, EPS = 2.0, growthRate = 0.06, discountRate = 0.10, type = "trailing"), 10.6)
	# Leading: 0.4 / (0.10 - 0.06) = 10
	expect_equal(fin_stock_justifiedPE(dividend = 0.8, EPS = 2.0, growthRate = 0.06, discountRate = 0.10, type = "leading"), 10.0)
	
	# Multi-stage ROIC test: 20% growth, 30% ROIC for 5 years, 10% discount rate, 3% terminal growth
	peTwoStage = fin_stock_justifiedPE(EPS = 2.70, growthRate = 0.20, discountRate = 0.10, ROIC = 0.30, yearsHighGrowth = 5, terminalGrowth = 0.03)
	expect_equal(round(peTwoStage, 2), 22.64)

	# Auto-switch to 2-stage when growthRate >= discountRate
	peAuto = fin_stock_justifiedPE(growthRate = 0.20, discountRate = 0.10, ROIC = 0.30)
	expect_true(peAuto > 0)

	# Error handling
	expect_error(fin_stock_justifiedPE(terminalGrowth = 0.12, discountRate = 0.10, yearsHighGrowth = 5), "terminalGrowth")
	expect_error(fin_stock_justifiedPE(EPS = 0), "EPS must be positive")
})



