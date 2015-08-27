## Resubmission
This is a resubmission (of a New submission).

In this version I have addressed each request as follows:

* Checked Possibly mis-spelled words in DESCRIPTION: OpenMx & umx
 * These are correctly spelled package names.
* Removed VignetteBuilder field
* Corrected invalid URL "http://www.github.com/tbates/umx" (removed quotes)
* Provided a real title: "Helper functions for Structural Equation Modelling in OpenMx"
* now import all methods from base packages e.g. ("graphics", "plot")
 importFrom("methods", "as", "getSlots", "is", "slotNames")
 importFrom("stats", "C"); 
 importFrom("utils", "combn", "data", "flush.console", "read.table", "txtProgressBar")

Best wishes, tim

## Test environments
* local OS X install, R version 3.2.2 (2015-08-14)
* Win x86_64-w64-mingw32, R Under development (unstable) (2015-08-20 r69138)
* 64-bit Linux (Scientific Linux 6), R 3.2.0

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:
* checking R code for possible problems ... NOTE
  umxACE: no visible binding for global variable ‘MZ.objective’
  
These have all been scrutunized and consist only of valid 
OpenMx Algebra symbols

## Downstream dependencies

There are currently no downstream dependencies for this package.