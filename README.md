# umx
![alt text](https://zenodo.org/badge/5184/tbates/umx.svg)

umx is a structural equation modelling package designed to make SEM easier to build, modify, and report.

It includes high-level functions for complex models such as multi-group twin models, as well as for graphical model output.

Grab it from CRAN with

```splus
install.packages("umx")
library(umx)
?umx
```
The best way to learn what is on offer is to use the help ('?umx' works) and the online tutorial: [tbates.github.io](http://tbates.github.io)

umx stands for "user" OpenMx functions. umx contains over 100 functions which build on
[OpenMx](http://openmx.psyc.virginia.edu), and are designed to automate activities such as labelling, setting start values etc.,
and also provides umxRAM and umxPath functions that make [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in R straightforward.

Some highlights include:

1. Building Path Models
	* `umxRAM()` *# mxModel with smart `data =` parameter, no need to specify type = "RAM", + auto-discovery of manifests and latents from the paths you write*
	* `umxPath()` *# mxPath on steroids: one-word settings to set `var` , `mean` `cov`, `fixedAt` 'v.m.' = and more. Great time-saver!*
2. Reporting output
	* `umxSummary(model)` # *A model fit designed for journal reporting (Χ², p, CFI, TLI, & RMSEA). Optionally show the path loadings*
	* `plot(model, std=T, digits = 3, file = "name")` # *Graphical, editable output of model in your browser!*
3. Modify models
	* `umxModify()` *# Modify and run a model. You can add objects, drop or add paths, including by regular-expression label matching), re-name the model, re-run, and even return the comparison. All in 1 line *
	* `umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T)` *# A powerful assistant to get labels from a model. like `omxGetParameters` but uses regular expressions.*
4. Twin modeling!
	* `umxACE()` *# Twin ACE modeling with aplomb* paths are labeled! Works with `plot()` and `umxSummary`!
	* `umxCP()`, `umxIP()`, `umxGxE()`, `umxCP()`…
	* ![umxACE](https://github.com/tbates/umx/blob/master/man/figures/ACE.png)
5. Easy-to-remember options
	* `umx_set_cores()`
	* `umx_set_optimizer()`
6. Many more miscellaneous Helpers and smart expert helpers e.g.
	* `umx_time(model)`  *# Report the time taken by a model in a compact programable format*
	* `umxStart()` *# Add plausible start values to RAM or matrix models, or matrices: **very** helpful*
	* `umxLabel()` *# Add systematic labels to paths ("var1_to_var2") and matrix cells ("a_r1c1"). This is great for setting, equating and dropping paths by label!*
	* `umxHcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*

Dozens more (?umx to see them all). Check out the "family links" in any help file also!

Feel free to use, and submit code and requests via Github. Tell your friends! Publish more good science :-)

For thrill-seekers and collaborators only: the bleeding-edge development version is here:

```splus
install.packages("devtools")
library("devtools")
install_github("tbates/umx")
library("umx")
?umx
```
