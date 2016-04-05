# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_confint.OpenMx.r") 
# 
# test_package("umx")
test_that("confint works for different inputs", {
require(OpenMx)
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
m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
m2 = confint(m1) # default: CIs added, but user prompted to set run = TRUE
m2 = confint(m2, run = TRUE) # CIs run and reported
m1 = confint(m1, parm = "G_to_x1", run = TRUE) # Add CIs for asymmetric paths in RAM model, report them, save m1 with this CI added
m1 = confint(m1, parm = "A", run = TRUE) # Add CIs for asymmetric paths in RAM model, report them, save m1 with mxCIs added
confint(m1, parm = "existing") # request existing CIs (none added yet...)

	expect_match(umx_APA_pval(p = .04, min = .05), "< 0.05")
	expect_match(umx_APA_pval(1.23E-3, addComparison = T), "= 0.001")
	expect_equal(umx_APA_pval(1.23E-3, addComparison = F), 0.001)
	expect_equal(umx_APA_pval(c(1.23E-6, .5))    ,  c("< 0.001", "0.500") )
	expect_equal(umx_APA_pval(c(1.23E-3, .5), addComparison = F), c(0.001, 0.500) )
	expect_equal(umx_APA_pval(c(1.23E-3, .5), addComparison = T), c("= 0.001", "= 0.500") )
})
