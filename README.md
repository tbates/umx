# umx
umx stands for "user" mx function, and is a library of helper functions for doing [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in [OpenMx](http://openmx.psyc.virginia.edu).

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
	* `umxPath()` *# mxPath on steroids: one-parameter settings for `var=` , `mean =` `cov=` and more. Great time-saver in RAM modelling*
	* `umxACE()` *# 1-line twin modeling*
	* `umxStart()` *# Add sane start values to RAM or matrix models, or matrices: **very** helpful*
	* `umxLabel()` *# Add systematic labels to paths and matrix cells. This is great for setting, equating and dropping paths by label!*
3. Reporting output
	* `umxSummary(model)` # *Get a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA). Optionally show the path loadings*
	* `plot(model, std=TRUE, precision=3, dotFilename="name")` # *Create a graphical representation of a RAM model (outputs a [GraphViz](http://www.graphviz.org/Gallery.php) file)*
	* `umx_get_time(fit1)`  *# Report the time taken by a model in a compact friendly, programable format*
3. Modify models
	* `umxReRun()` *# Modify and run a model. You can add objects, drop or add paths, including by regular expression label matching), re-name the model, re-run, and even return the comparison all in 1 line *
	* `umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T)` *# A powerful assistant to get labels from a model. like `omxGetParameters` but uses regular expressions.*
	* `umxMI()` *# Report the top n modification indices*
	* `umxAdd1()` *# add parameters and return a table of the effect on fit*
	* `umxDrop1()` *# Drop parameters and return a table of the effect on fit*
4. Data and package helpers
	* `umxHcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*

Feel free to use, and improve: Log suggestions here using the Github comments, wiki, or git.

### Contributors
* [Tim Bates](tim.bates@ed.ac.uk)
* [Michael Culbertson]() (modification index [code](http://openmx.psyc.virginia.edu/thread/1019) (based on functions in [John Fox's](http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf) [SEM](http://cran.r-project.org/web/packages/sem) package))
