# umx

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

1. Building Models
	* `umxRAM()` *# mxModel with an R-like `data =` parameter, no need to specify type = "RAM", + auto-discovery of manifests and latents from the paths you write*
	* `umxPath()` *# mxPath on steroids: one-word settings to set `var` , `mean` `cov`, `fixedAt` and more. Great time-saver!*
	* `umxACE()` *# 1-line function handling twin ACE modeling with aplomb* paths are labeled, and works with `plot()` and `umxSummary`
2. Reporting output
	* `umxSummary(model)` # *Get a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA). Optionally show the path loadings*
	* `plot(model, std=TRUE, precision=3, dotFilename="name")` # *Create a graphical representation of models (outputs a [GraphViz](http://www.graphviz.org/Gallery.php) file)*
	* `umx_time(m1)`  *# Report the time taken by a model in a compact programable format*
3. Modify models
	* `umxReRun()` *# Modify and run a model. You can add objects, drop or add paths, including by regular-expression label matching), re-name the model, re-run, and even return the comparison. All in 1 line *
	* `umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T)` *# A powerful assistant to get labels from a model. like `omxGetParameters` but uses regular expressions.*
	* `umxMI()` *# Report the top n modification indices*
	* `umxAdd1()` *# add parameters and return a table of the effect on fit*
	* `umxDrop1()` *# Drop parameters and return a table of the effect on fit*
4. Twin models
	* umxACE, umxCP, umxIP
	* ![umxACE](https://github.com/tbates/umx/blob/master/man/figures/ACE.png)
5. Lower level helpers, data helpers, etc.
	* `umxStart()` *# Add plausible start values to RAM or matrix models, or matrices: **very** helpful*
	* `umxLabel()` *# Add systematic labels to paths ("var1_to_var2") and matrix cells ("a_r1c1"). This is great for setting, equating and dropping paths by label!*
	* `umxHcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*
6. Helpers for easy-to-forget option names
	* `umx_get_optimizer()`
	* `umx_set_optimizer()`

Dozens more (?umx to see them all). Check out the "family links" in any help file also!

Feel free to use, and improve: Log suggestions using the Github comments. Tell your friends! Get awesome publications :-)

You can install the development version from github:

```splus
install.packages("devtools")
library("devtools")
install_github("tbates/umx")
library("umx")
?umx
```
