## Changes
I would appreciate if you can please upload this new version of umx (with over 60 new features, fixes and enhancements) .

The package likely can't be tested on El Capitan, as CRAN currently lacks a binary build of OpenMx for El Cap.

However, it runs perfectly on the other platforms, and on local builds of OpenMx under El Capitan.

Requiring umx to be checked against OpenMx on El Cap on CRAN is preventing users and students from using the
any of the new features and fixes implemented since R 3.4.0...

We continue to communicate with CRAN/Simon to find out  why we can build OpenMx locally, and on travis,
but it is failing for the El Cap MacOS binary under the new CRAN custom tool chain.

## Test environments
* OS X 10.13.0, R version 3.4.2
* Win R Under development (unstable) (2017-09-12 r73242)
* 64-bit Linux (Scientific Linux 6), R 3.4.2

## R CMD check results
* No ERRORs, No WARNINGs, No NOTES

## Downstream dependencies
* No downstream dependencies.

Many thanks!
Tim