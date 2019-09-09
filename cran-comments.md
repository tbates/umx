## Changes
On the actual CRAN check (as opposed to me running R CMD) I got one NOTE about files in the check directory.
In response, I turned-off a bunch of existing examples. Should be clear now.

There was a false alarm NOTE about unused imported space. This was apparently checking commented-out code. Anyhow, I deleted the comments now.
There was an ERROR attempting to run under Solaris. But OpenMx doesn't run under Solaris and never has for 4 years.
It runs fine on *nix, Win and MacOS.

Over 80 new features and improvements

## Test environments
* OS X 10.14.0, R version  3.6.1 (2019-07-05)
* Win R (unstable) (2019-08-15 r76992)
* 64-bit Linux (Scientific Linux 6), R 3.6.1

## R CMD check results
* No WARNINGs, ERRORs or NOTEs

## Downstream dependencies
* No downstream dependencies.

Many thanks!
Tim