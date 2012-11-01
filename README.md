# umx
umx stands for "user" mx function, and is a library of helper functions for doing [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in [OpenMx](http://openmx.psyc.virginia.edu).

It currently contains functions to support:

1. Report results more easily
	* **umxReportFit**(*model*) # *report a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA)*
	* **umxStandardizeRAM**(*model*) # * Standardize a RAM model*
		* **nb**: *summary() computes standardized paths for RAM models, but this function is still useful, as omxGraphviz() does not, and the function standardizeRAMModel() is not exposed in OpenMx.*
	* **umxSaturated**(*model*) # *Allows computations of RMSEA and TLI etc for raw data models
		* This helper computes the saturated and independence models in the raw data case. These can be then be fed to `summary()` & `umxReportFit()`
2. Modification Indices for improving model fit
	* **umxMI**(*model*, *vector*=T) # *return modification indices for a model*
	* **umxMI_top**(*fit*, *numInd*=5, *typeToShow*="add", *descending*=T) # *report the top n indices*
3. Data processing helpers
	* **umxHcor**(data, use="pairwise.complete.obs") # *compute heterochoric (pair-wise appropriate) correlations for mixed data types*
4. Run models faster
	* **umxTryHarder**(*model*)
		* Sometimes models will code Red, and just need to be re-run from their now closer-to-good starting values. umxTryHard() replaces mxRun, allowing a model to run as many as n times until it returns green. This function also turns off features that slow model evaluation (like the Hessian) which are not-needed in all cases. Ideally, mxRun() might support this multiple-run option itself.
5. Utilities
	* **umxUpdateOpenMx**(bleedingEdge = FALSE, loadNew = TRUE) # *update your copy of OpenMx library*
	* **umxTime**(fit1) # *report the time taken by a model in a compact friendly, programmable format*


Feel free to use, and improve! Log suggestions here using the Github comments, wiki, or git.

### Contributors
* [Tim Bates](tim.bates@ed.ac.uk)
* [Michael Culbertson]() (modification index [code](http://openmx.psyc.virginia.edu/thread/1019) (based on functions in [John Fox's](http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf) [SEM](http://cran.r-project.org/web/packages/sem) package))
* [Ryne Estabrook]() ([code](http://openmx.psyc.virginia.edu/thread/718) to Standardize RAM models)
