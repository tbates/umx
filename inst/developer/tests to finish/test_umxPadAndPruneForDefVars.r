# library(testthat); library(devtools)
# test_file("~/bin/umx/tests/testthat/test_umxPadAndPruneForDefVars.r")
# 
# test_package("umx")
require(umx)
x1 = c(1:3,NA,5:10)
x2 = c(1:3,NA,5:10)
x3 = c(1:3,NA,5:9, NA)
d1 = c(1:3,NA,5:9, NA)
d2 = c(1:3,NA,5:9, NA)*3
d3 = c(1:3,NA,5:9, NA)*10

df = data.frame(x1, x2, x3, d1, d2, d3)
defVars = c("d1", "d2", "d3")
vars    = c("x1", "x2", "x3")
df
testEF = df
testEF[4 , defVars] = 99
testEF[10, defVars] = c(5.125, 15.375, 51.25)
ef = umxPadAndPruneForDefVars(df, vars, defVars, highDefValue= 99, suffixes = c(""))

test_that("umxPadAndPruneForDefVars works", {	
	testthat::expect_equivalent(ef, testEF, "umxPadAndPruneForDefVars failed")
})
