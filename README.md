# umx

[![Build Status](https://travis-ci.org/tbates/umx.svg?branch=master)](https://travis-ci.org/tbates/umx)
![Github commits](https://img.shields.io/github/commits-since/tbates/umx/latest.svg?colorB=green)
[![cran version](http://www.r-pkg.org/badges/version/umx)](https://cran.r-project.org/package=umx)
[![Monthly Downloads](http://cranlogs.r-pkg.org/badges/umx)](http://cranlogs.r-pkg.org/badges/umx)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/umx)](http://cranlogs.r-pkg.org/badges/grand-total/umx)
[![Rdoc](http://www.rdocumentation.org/badges/version/umx)](http://www.rdocumentation.org/packages/umx)
[![DOI](https://img.shields.io/badge/doi-10.1017/thg.2019.2-yellow.svg?style=flat)](https://doi.org/10.1017/thg.2019.2)
[![License](https://img.shields.io/cran/l/umx.svg)](https://cran.r-project.org/package=umx)

[Roadmap](https://github.com/tbates/umx/labels/enhancement), and [Tutorials](https://tbates.github.io) (let me know what you'd like, or perhaps a book?)

`umx` is a [structural equation modeling](https://en.wikipedia.org/wiki/Structural_equation_modeling) package designed to make SEM easier, from building, to modifying and reporting. Please cite as: `citation("umx")`:

Timothy C. Bates, Michael C. Neale, Hermine H. Maes, (2019). umx: A library for Structural Equation and Twin Modelling in R. *Twin Research and Human Genetics*, **22**, 27-41. [DOI:10.1017/thg.2019.2](https://doi.org/10.1017/thg.2019.2)

`umx` includes high-level functions for complex models such as multi-group twin models, as well as graphical model output.

Install it from CRAN:

```splus
install.packages("umx")
library(umx)
?umx
```

Most functions have extensive and practical examples (even figures for the twin models): so USE THE HELP :-).

See what is on offer with '?umx'. There are online tutorials at  [tbates.github.io](http://tbates.github.io).

`umx` stands for "user" OpenMx functions. It provides over 100 functions, but most importantly the high-level `umxRAM` functions that makes path-based SEM in R straightforward from model specification to `plot`, along with a suite of high-level twin modelling functions. These are supported by dozens of low-level functions automating activities such as labeling, setting start values etc., and helping with data-wrangling, plotting etc.

Some highlights include:

1. Building Path Models
	* `umxRAM()` *# mxModel with smart `data =` parameter, automatic labels, start, run, plot, and auto-discovery of manifests and latents from the paths you write*
	* `umxPath()` *# write paths but with one-word settings to set `var` , `mean` `cov`, `fixedAt` 'v.m.' = and more.*
2. Reporting output
	* `umxSummary(model)` # *A fit summary designed for journal reporting (Χ², p, CFI, TLI, & RMSEA). Optionally show the path loadings*
	* `plot(model, std=T, digits = 3, file = "name")` # *Graphical, model in your browser! or edit in programs like Omnigraffle*
3. Modify models
	* `umxModify()` *# Modify and run a model. You can add objects, drop or add paths, including by wildcard label matching), re-name the model, re-run, and even return the comparison. All in 1 line *
	* `parameters(m1, "below", .1, pattern="_to_"))` *# A powerful assistant to get labels and values from a model (e.g. all 'to' params, below .1 in value)*
4. Twin modeling!
	* `umxACE` *# Twin ACE modeling with aplomb* paths are labeled! Works with `plot()` and `umxSummary`!
	* `umxCP`, `umxIP`, `umxGxE`, `umxCP`, `umxGxEbiv`, `umxSexLim`  …
	* ![umxACE](https://github.com/tbates/umx/blob/master/man/figures/ACEunivariate.png)
5. Easy-to-remember options
	* `umx_set_cores()`
	* `umx_set_optimizer()`
6. Many more miscellaneous helpers e.g.
	* `umx_time(model1, model2)` reports and compares run times in a compact programmable format (also "start" and "stop" a timer)
	* `umxHetcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*
	* Dozens more: Check out the "family links" in `?umx` and in any help file!

Feel free to use, and submit code and requests via Github. Tell your friends! Publish more good science :-)


For thrill-seekers and collaborators only: the bleeding-edge development version is here:

```splus
install.packages("devtools")
library("devtools")
install_github("tbates/umx")
library("umx")
?umx
```
