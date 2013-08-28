# umx
umx stands for "user" mx function, and is a library of helper functions for doing [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in [OpenMx](http://openmx.psyc.virginia.edu).

You can install the most recent version from github as follows

```S
install.packages("devtools")
library("devtools")
install_github("umx", username = "tbates")
library("umx")
?umx

```
<!-- source("http://timbates.wdfiles.com/local--files/start/umx.lib.R") -->

The umx package has helpers supporting:

1. Building Models
	* `umxStart()` *# Add sane start values to a model: **very** helpful*
	* `umxLabel()` *# Add labels to paths: Labels allow you to set, equate, and drop paths by label!*
	* `umxLatent()` *# Helper for building formative and reflective latent variables from their manifest indicators*
2. Run models
	* `umxRun()` *# Use in place of mxRun to: compute saturated for raw data, run model until it returns green, turn off features that slow model evaluation, like the Hessian.*
3. Reporting output
	* `umxSummary(model)` # *Get a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA)*
	* `umxGraph_RAM(fit1, std=T, precision=3, dotFilename="name")` # *Create a graphical representation of a RAM model (outputs a [GraphViz](http://www.graphviz.org/Gallery.php) file)*
	* `umxStandardizeModel(model, return = "model")` #* standardize a RAM model*
		* **nb**:* `summary()` computes standardized paths for RAM models, but this function is still useful, as `omxGraphviz()` does not, and the function `.standardizeRAMModel()` is not exposed in OpenMx.
	* `umxTime(fit1)`  *# Report the time taken by a model in a compact friendly, programable format*
	* `umxSaturated(model)` *# Create a saturated model when raw data are being used. *
		* `summary(model, SaturatedLikelihood = model_sat$Sat, IndependenceLikelihood = model_sat$Ind)`
		* **nb**:* Saturated solutions are not computable for definition variables and some other models.
3. Modify models
	* `umxMI_top()` # Report the top n modification indices
		* `umxMI()` *# Get modification indices for a model*
	* `umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T)` *# A powerful assistant to get labels from a model. like `omxGetParameters` but uses regular expressions.*
	* `umxReRun(lastFit, dropList = NA, regex = NA, free = F, value = 0, freeToStart = NA, newName = NA, verbose = F, intervals = F)`
4. Data and package helpers
	* `umxHcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*
	* `lower2full(lower.no.diag, diag=F, byrow=F)`  *# Create a full matrix from a lower matrix of data*
	* `umxUpdateOpenMx(bleedingEdge = FALSE, loadNew = TRUE)` *# Update the OpenMx package*

Feel free to use, and improve: Log suggestions here using the Github comments, wiki, or git.

### Contributors
* [Tim Bates](tim.bates@ed.ac.uk)
* [Michael Culbertson]() (modification index [code](http://openmx.psyc.virginia.edu/thread/1019) (based on functions in [John Fox's](http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf) [SEM](http://cran.r-project.org/web/packages/sem) package))
* [Ryne Estabrook]() ([code](http://openmx.psyc.virginia.edu/thread/718) to Standardize RAM models)
