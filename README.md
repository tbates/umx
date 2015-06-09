# umx

The umx package provides functions which build on the foundation of OpenMx() to support a range of strucutral equation modelling tasks.
These include all stages from building models rapidly, to plotting output graphically.

umx stands for "user" OpenMx functions. It consists of a large (around 100 functions) library of helper-functions building on
[OpenMx](http://openmx.psyc.virginia.edu), and designed to help users 
doing [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in R.

You can install the most recent version from github as follows

```splus
install.packages("devtools")
library("devtools")
install_github("tbates/umx")
library("umx")
?umx
# on windows you might need
# install_github("umx", username = "tbates", args = "--no-multiarch")
# On the newest version of devtools, this will work
# install_github("tbates/umx")
```

The umx package has a wide range of functions (about 100) to get things done in SEM. The best way to learn about what is on offer is to install, and use the help, starting with '?umx'.
Some highlights include:

1. Building Models
	* `umxRAM()` *# mxModel with an R-like `data =` parameter, no need to specify type = "RAM", + auto-discovery of manifests and latents from the paths you write*
	* `umxPath()` *# mxPath on steroids: one-word settings for `var` , `mean` `cov`, `fixedAt` and more. Great time-saver!*
	* `umxACE()` *# 1-line function handling twin ACE modeling with aplomb* paths are labeled, and works with `plot()` and `umxSummary`
3. Reporting output
	* `umxSummary(model)` # *Get a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA). Optionally show the path loadings*
	* `plot(model, std=TRUE, precision=3, dotFilename="name")` # *Create a graphical representation of models (outputs a [GraphViz](http://www.graphviz.org/Gallery.php) file)*
	* `umx_time(fit1)`  *# Report the time taken by a model in a compact programable format*
3. Modify models
	* `umxReRun()` *# Modify and run a model. You can add objects, drop or add paths, including by regular-expression label matching), re-name the model, re-run, and even return the comparison. All in 1 line *
	* `umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T)` *# A powerful assistant to get labels from a model. like `omxGetParameters` but uses regular expressions.*
	* `umxMI()` *# Report the top n modification indices*
	* `umxAdd1()` *# add parameters and return a table of the effect on fit*
	* `umxDrop1()` *# Drop parameters and return a table of the effect on fit*
4. Lower level helpers, data helpers, etc.
	* `umxStart()` *# Add plausible start values to RAM or matrix models, or matrices: **very** helpful*
	* `umxLabel()` *# Add systematic labels to paths ("var1_to_var2") and matrix cells ("a_r1c1"). This is great for setting, equating and dropping paths by label!*
	* `umxHcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*
4. Miscellaneous helpers
	* `umx_get_optimizer()`
	* `umx_set_optimizer()`

Dozens more (?umx to see them all). Check out the "family links" in any help file also!

Feel free to use, and improve: Log suggestions using the Github comments! Tell your friends! Get awesome publications and cite umx!

