# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxCLPM_names_groups.R")

require(umx)
q = function(x) suppressMessages(suppressWarnings(x))
data(docData)
dt = docData[2:9]
dn = dt
names(dn) = c(paste0("varA1_W", 1:4), paste0("varA2_W", 1:4))
zyg = ifelse(grepl("^MZ", docData$zygosity), "MZ", "DZ")

test_that("umxCLPM selects X and Y by name, and names waves with sep", {
	pos = q(umxCLPM(waves = 4, model = "Hamaker2015", data = dt))
	expect_equal(pos$name, "Hamaker2015")
	expect_true(all(c("X_W1", "Y_W4") %in% pos@manifestVars))

	dn$other = 1 # unused columns are ignored when selecting by name
	byName = q(umxCLPM(waves = 4, X = "varA1", Y = "varA2", model = "Hamaker2015", data = dn))
	expect_equal(byName$output$fit, pos$output$fit)
	expect_true(all(c("varA1_W1", "varA2_W4") %in% byName@manifestVars))

	sketch = umxCLPM(waves = 3, X = "a", Y = "b", sep = "_t", model = "Heise1969")
	expect_setequal(sketch@manifestVars, c(paste0("a_t", 1:3), paste0("b_t", 1:3)))

	expect_error(umxCLPM(waves = 4, X = "varA1", model = "Heise1969", data = dn), "give both X and Y")
	expect_error(umxCLPM(waves = 4, model = "Heise1969", data = dt[, 1:7]), "make 8")
})

test_that("umxCLPM fits a multigroup model from a group column", {
	g = dn
	g$zyg = zyg
	mg = q(umxCLPM(waves = 4, X = "varA1", Y = "varA2", model = "Hamaker2015", data = g, group = "zyg", batteries = NULL))
	expect_setequal(names(mg@submodels), c("Hamaker2015_MZ", "Hamaker2015_DZ"))

	# All parameters free across groups: -2LL is the sum of the separate group fits
	sepFit = sum(sapply(c("MZ", "DZ"), function(z) {
		q(umxCLPM(waves = 4, X = "varA1", Y = "varA2", model = "Hamaker2015", data = dn[zyg == z, ], batteries = NULL))$output$fit
	}))
	expect_equal(mg$output$fit, sepFit, tolerance = 1e-4)

	# Without group =, extra columns are ignored and the model is single-group
	d2 = dn
	d2$.group = zyg
	m2 = q(umxCLPM(waves = 4, X = "varA1", Y = "varA2", model = "Hamaker2015", data = d2, batteries = NULL))
	expect_equal(length(m2@submodels), 0)
	single = q(umxCLPM(waves = 4, X = "varA1", Y = "varA2", model = "Hamaker2015", data = dn, batteries = NULL))
	expect_equal(m2$output$fit, single$output$fit)

	expect_error(q(umxCLPM(waves = 4, X = "varA1", Y = "varA2", model = "Hamaker2015", data = g, group = "zyg", batteries = "thresholds")), "not yet supported with groups")
	expect_error(umxCLPM(waves = 4, X = "varA1", Y = "varA2", model = "Hamaker2015", data = g, group = "nope"), "nope")
})

test_that("umxCLPM names a default model sketch after its type", {
	sketch = umxCLPM(waves = 3, X = "a", Y = "b", sep = "_t")
	expect_equal(sketch$name, "Hamaker2015")
	expect_error(umxCLPM(waves = 3, X = "a", Y = "b", sep = "_t", model = "nope"), "should be one of")
})
