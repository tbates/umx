# umx
umx stands for "user" mx function, and is a library of helper functions for doing [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in [OpenMx](http://openmx.psyc.virginia.edu).

It currently contains functions to support:

1. RAM model Modification Indices
	* `umxMI(model, vector = T)`  *# Return modification indices for a model*
	* `umxMI_top(fit, numInd = 5, typeToShow = "add", descending = T)` # Report the top n indices
2. Reporting helpers
	* `umxReportFit(model)` # *report a brief summary of model fit, similar to a journal report (Χ², p, CFI, TLI, & RMSEA)*
	* `umxGraph_RAM(fit1, std=T, precision=3, dotFilename="name")` # *Create a graphical representation of a RAM model (outputs a [GraphViz](http://www.graphviz.org/Gallery.php) file)*
	* `umxStandardizeModel(model, return = "model")` #* standardize a RAM model*
		* **nb**:* `summary()` computes standardized paths for RAM models, but this function is still useful, as `omxGraphviz()` does not, and the function `.standardizeRAMModel()` is not exposed in OpenMx.
	* `umxTime(fit1)`  *# Report the time taken by a model in a compact friendly, programable format*
	* `umxSaturated(model)` *# Create a saturated model when raw data are being used. *
		* `summary(model, SaturatedLikelihood = model_sat$SaturatedLikelihood, IndependenceLikelihood = model_sat$IndependenceLikelihood)`
		* **nb**:* Saturated solutions are not computable for definition variables and some other models.

3. Speed and efficiency helpers
	* `umxTryHard(model)`
		* Sometimes models will code Red, and just need to be re-run from their now closer-to-good starting values. `umxTryHard()` replaces mxRun, allowing a model to run as many as n times until it returns green. This function also allows turning off features that slow model evaluation, like the Hessian, and which are not-needed in all cases. Ideally, mxRun() might support this option itself.
4. Data processing helpers
	* `umxHcor(data, use = "pairwise.complete.obs")` *# Compute appropriate pair-wise correlations for mixed data types.*
	* `lower2full(lower.no.diag, diag=F, byrow=F)`  *# Create a full matrix from a lower matrix of data*
5. Utilities to update models
	* `umxUpdateOpenMx(bleedingEdge = FALSE, loadNew = TRUE)` *# Update the OpenMx package*
	* `umxGetLabels(model, regex = "as_r_2c_[0-9]", free = T)` *# A powerful assistant to get labels from a model. Can use regular expressions.*
	* `umxReRun(lastFit, dropList=NA, regex=NA, free=F, value=0, freeToStart=NA, newName=NA, verbose=F, intervals=F)`

Feel free to use, and improve! Log suggestions here using the Github comments, wiki, or git.

Load it up like this:
<br />
<div style="color: #a7a8a7; font-family: Menlo; font-size: 15px;">
<span style="color: #d77c79;">url</span><span style="color: black;"> </span><span style="color: #9ac9c4;">&lt;-</span><span style="color: black;"> </span><span style="color: #c2c77b;">"https://raw.github.com/tbates/umx/master/umx.lib.R"</span><span style="color: black;"><br />
</span><span style="color: #93b2ca;">source_https</span><span style="color: black;"> </span><span style="color: #9ac9c4;">&lt;-</span><span style="color: black;"> </span><span style="color: #c0a7c7;">function</span><span style="color: black;">(</span><span style="color: #d77c79;">u</span><span style="color: black;">, </span><span style="color: #e6a472;">unlink.tmp.certs</span><span style="color: black;"> </span><span style="color: #9ac9c4;">=</span><span style="color: black;"> </span><span style="color: #e6a472;">F</span><span style="color: black;">) {<br />
&nbsp; &nbsp; </span># read script lines from website using a security certificate<br />
<span style="color: black;">&nbsp; &nbsp; require(</span><span style="color: #d77c79;">RCurl</span><span style="color: black;">)<br />
&nbsp; &nbsp; </span><span style="color: #c0a7c7;">if</span><span style="color: black;">(</span><span style="color: #9ac9c4;">!</span><span style="color: black;">file.exists(</span><span style="color: #c2c77b;">"cacert.pem"</span><span style="color: black;">)){<br />
&nbsp; &nbsp; &nbsp; &nbsp; download.file(</span><span style="color: #e6a472;">url</span><span style="color: black;"> </span><span style="color: #9ac9c4;">=</span><span style="color: black;"> </span><span style="color: #c2c77b;">"http://curl.haxx.se/ca/cacert.pem"</span><span style="color: black;">, </span><span style="color: #e6a472;">destfile</span><span style="color: black;"> </span><span style="color: #9ac9c4;">=</span><span style="color: black;"> </span><span style="color: #c2c77b;">"cacert.pem"</span><span style="color: black;">)<br />
&nbsp; &nbsp; }<br />
&nbsp; &nbsp; </span><span style="color: #d77c79;">script</span><span style="color: black;"> </span><span style="color: #9ac9c4;">&lt;-</span><span style="color: black;"> RCurl</span><span style="color: #c0a7c7;">::</span><span style="color: black;">getURL(</span><span style="color: #d77c79;">u</span><span style="color: black;">, </span><span style="color: #e6a472;">followlocation</span><span style="color: black;"> </span><span style="color: #9ac9c4;">=</span><span style="color: black;"> </span><span style="color: #e6a472;">T</span><span style="color: black;">, </span><span style="color: #e6a472;">cainfo</span><span style="color: black;"> </span><span style="color: #9ac9c4;">=</span><span style="color: black;"> </span><span style="color: #c2c77b;">"cacert.pem"</span><span style="color: black;">)<br />
&nbsp; &nbsp; </span><span style="color: #c0a7c7;">if</span><span style="color: black;">(</span><span style="color: #d77c79;">unlink.tmp.certs</span><span style="color: black;">) unlink(</span><span style="color: #c2c77b;">"cacert.pem"</span><span style="color: black;">)<br />
&nbsp; &nbsp; </span># parse lines and evaluate in the global environement<br />
<span style="color: black;">&nbsp; &nbsp; eval(parse(</span><span style="color: #e6a472;">text</span><span style="color: black;"> </span><span style="color: #9ac9c4;">=</span><span style="color: black;"> </span><span style="color: #d77c79;">script</span><span style="color: black;">), </span><span style="color: #e6a472;">envir</span><span style="color: black;"> </span><span style="color: #9ac9c4;">=</span><span style="color: black;"> .</span><span style="color: #d77c79;">GlobalEnv</span><span style="color: black;">)<br />
}<br />
source_https(</span><span style="color: #d77c79;">url</span><span style="color: black;">) </span># Using unlink.tmp.certs = T will delete the security certificates text file that source_https downloads</div>

### Contributors
* [Tim Bates](tim.bates@ed.ac.uk)
* [Michael Culbertson]() (modification index [code](http://openmx.psyc.virginia.edu/thread/1019) (based on functions in [John Fox's](http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf) [SEM](http://cran.r-project.org/web/packages/sem) package))
* [Ryne Estabrook]() ([code](http://openmx.psyc.virginia.edu/thread/718) to Standardize RAM models)
