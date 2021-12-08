# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxBuild_run_modify.r") 
# test_package("umx")

context("umx Build_run_modify Functions")

test_that("umxEquate works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	# By default, umxEquate just equates master and slave labels: doesn't run model
	m2 = umxEquate(m1, a = "G_to_x1", b = "G_to_x2", name = "Eq x1 x2 loadings")
	
	# Set autoRun = TRUE and comparison = TRUE to run and output a comparison
	m2 = umxEquate(m1, a = "G_to_x1", b = "G_to_x2", autoRun = TRUE, comparison = TRUE, name = "Eq_x1_x2")
	
	# rename the equated paths
	m2 = umxEquate(m1, a = "G_to_x1", b = "G_to_x2", newlabels = "equated", autoRun = TRUE, comparison = TRUE, name = "Eq_x1_x2")
	expect_true(parameters(m2)$name[1] == "equated")

})
