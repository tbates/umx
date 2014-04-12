---
layout: post
title: "Overview of umx functions: Quickly survey all the umx helpers available to you"
date: 2014-04-11 14:50
comments: true
categories: models

---

<p>umx adds helpers to <a href="http://openmx.psyc.virginia.edu">OpenMx</a> that take care of repetitive tasks like labeling and start values, and which streamlines the work flow of model comparison and graphing, while not sacrificing power.</p>

<p>The umx package has a range of useful helpers to help you get things done in SEM. This list is not comprehensive: install the package to learn more</p>

# umx
umx stands for "user" mx function, and is a library of helper functions for doing [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in [OpenMx](http://openmx.psyc.virginia.edu).

You can install the most recent version from github as follows

```S
install.packages("devtools")
library("devtools")
install_github("umx", username = "tbates")
library("umx")
?umx
# on windows you might need
# install_github("umx", username = "tbates", args = "--no-multiarch")
# On the newest version of devtools, this will work
# install_github("tbates/umx")
```
The umx package has a range of useful helpers to help you get things done in SEM. This list is not comprehensive: install the package to learn more

1. Building Models
	* `umxStart()` *# Add sane start values to a model: **very** helpful*
	* `umxLabel()` *# Add labels to paths: Labels allow you to set, equate, and drop paths by label!*
	* `umxLatent()` *# Helper for building formative and reflective latent variables from their manifest indicators*
2. Run models
	* `umxRun()` *# Use in place of mxRun to: set labels, starts, compute saturated for raw data, run model until it returns green*
	* `umxReRun()` *# Modify a model (drop paths etc), run, and even return the comparison all in 1 line*
3. Reporting output
	* `umxSummary(model)` # *Get a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA)*
	* `umxPlot(fit1, std=T, precision=3, dotFilename="name")` # *Create a graphical representation of a RAM model (outputs a [GraphViz](http://www.graphviz.org/Gallery.php) file)*
	* `umxTime(fit1)`  *# Report the time taken by a model in a compact friendly, programable format*
3. Modify models
	* `umxMI()` # Report the top n modification indices
	* `umxAdd1()` # add parameters and return a table of the effect on fit
	* `umxDrop1()` # Drop parameters and return a table of the effect on fit
	* `umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T)` *# A powerful assistant to get labels from a model. like `omxGetParameters` but uses regular expressions.*
	* `umxReRun()` *# re-run a model: Quickly drop or free parameters, rename the model, and re-run...*
4. Data and package helpers
	* `umxHcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*
	* `lower2full(lower.no.diag, diag=F, byrow=F)`  *# Create a full matrix from a lower matrix of data*
	* `umxUpdateOpenMx(bleedingEdge = FALSE, loadNew = TRUE)` *# Update the OpenMx package*
	* `umxSaturated(model)` *# Create a saturated model when raw data are being used. *
		* `summary(model, SaturatedLikelihood = model_sat$Sat, IndependenceLikelihood = model_sat$Ind)`
		* **nb**:* Saturated solutions are not computable for definition variables and some other models.

Feel free to use, and improve: Log suggestions here using the Github comments, wiki, or git.

### Contributors
* [Tim Bates](tim.bates@ed.ac.uk)
* [Michael Culbertson]() (modification index [code](http://openmx.psyc.virginia.edu/thread/1019) (based on functions in [John Fox's](http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf) [SEM](http://cran.r-project.org/web/packages/sem) package))
* [Ryne Estabrook]() ([code](http://openmx.psyc.virginia.edu/thread/718) to Standardize RAM models)
