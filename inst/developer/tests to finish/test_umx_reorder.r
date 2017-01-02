# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_reorder.r") 
# 
# testthat::test_package("umx")

test_that("umx_reorder works for different values", {
	require(OpenMx)
	oldData = cov(myFADataRaw[,paste0("x",1:6)], use = "pairwise.complete.obs")
	allNames = colnames(oldData)
	expect_equal(umx_reorder(old = oldData, newOrder = allNames), oldData)

	# smaller set
	oldData = cov(myFADataRaw[,paste0("x",1:6)], use = "pairwise.complete.obs")
	newData = cov(myFADataRaw[,paste0("x",1:4)], use = "pairwise.complete.obs")
	allNames = colnames(newData)
	expect_equal(umx_reorder(old = oldData, newOrder = allNames), newData)

	# different order
	newData = cov(myFADataRaw[,paste0("x",c(4,2,3,1))], use = "pairwise.complete.obs")
	allNames = colnames(newData)
	expect_equal(umx_reorder(old = oldData, newOrder = allNames), newData)

	# =====================
	# = what about n = 1? =
	# =====================
	oldData = cov(mtcars)
	newData = cov(mtcars[,"hp", drop = FALSE])
	allNames = colnames(newData)
	expect_equal(umx_reorder(old = oldData, newOrder = allNames), newData)

})
