# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_has_means.r") 
# 
# test_package("umx")

require(umx)
data(demoOneFactor)
latents  = c("G")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM", 
	manifestVars = manifests, latentVars = latents, 
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)

testthat::expect_false(umx_has_means(m1))
m2 <- mxModel(m1, 
	mxPath(from = "one", to = manifests),
	mxData(demoOneFactor, type = "raw")
)
testthat::expect_true(umx_has_means(m2))
m2 = umxRun(m2, setLabels = TRUE, setValues = TRUE)
testthat::expect_true(umx_has_means(m2))

m3 <- mxModel(m2, name = "row_of_NA", mxData(rbind(demoOneFactor, NA), type = "raw"))
umx_time(c(mxRun(m2), mxRun(m3)), "%H:%M:%OS2")


