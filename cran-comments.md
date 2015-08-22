## Resubmission
This is a resubmission (of a New submission).

In this version I have addressed each request as follows:

## Possibly mis-spelled words in DESCRIPTION:
 OpenMx (10:48)
 umx (4:8, 11:43)

*Response*: These are correctly spelled package names.

## Package has a VignetteBuilder field but no prebuilt vignette index.

*Response*: removed VignetteBuilder field

## Found the following (possibly) invalid URLs:
 URL: "http://www.github.com/tbates/umx"
   From: man/umx.Rd
   Message: Invalid URI scheme

*Response*: Fixed (removed quotes)

## The Title field is just the package name: provide a real title.

*Response*: changed to "Helper functions for Structural Equation Modelling in OpenMx"

## Undefined global functions or variables:
I added 
 importFrom("graphics", "plot")
 importFrom("methods", "as", "getSlots", "is", "slotNames")
 importFrom("stats", "C", "aggregate", "as.formula", "complete.cases",
            "confint", "cor", "cov", "cov.wt", "cov2cor", "df", "lm",
            "logLik", "na.exclude", "na.omit", "pchisq", "pf", "qchisq",
            "qnorm", "quantile", "residuals", "rnorm", "runif", "sd",
            "setNames", "var")
 importFrom("utils", "combn", "data", "flush.console", "read.table",
            "txtProgressBar")

My DESCRIPTION Imports field contains 'methods'

Best wishes, tim

## Test environments
* local OS X install, R version 3.2.2 (2015-08-14)
* 64-bit Linux (Scientific Linux 6), R 3.2.0
* Win x86_64-w64-mingw32, R Under development (unstable) (2015-08-20 r69138)

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:
* checking R code for possible problems ... NOTE
  umxACE: no visible binding for global variable ‘MZ.objective’
  
These have all been scrutunized and consist only of valid 
OpenMx Algebra symbols

## Downstream dependencies

There are currently no downstream dependencies for this package.