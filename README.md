# umx

umx stands for "user mx", and is a library of helper functions for doing [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in [OpenMx](http://openmx.psyc.virginia.edu).

It currently contains functions to support:

1. RAM model Modification Indices
	* **umxMI**(*model*, *vector*=T) # return modification indices for a model
	* **umxMI_top**(*fit*, *numInd*=5, *typeToShow*="add") # report the top n indices
2. Reporting helpers
	* **umxReportFit**(*model*) # report a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA)
	* **umxStandardizeRAM**(*model*) # standardize a RAM model


Feel free to use, and improve!
Log suggestions here using the Github comments and 
### Contributors
* [Tim Bates](tim.bates@ed.ac.uk)
* [Michael Culbertson]() (modification index [code](http://openmx.psyc.virginia.edu/thread/1019) (based on functions in [John Fox's](http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf) [SEM](http://cran.r-project.org/web/packages/sem) package))
* [Ryne Estabrook]() ([code](http://openmx.psyc.virginia.edu/thread/718) to Standardize RAM models)


