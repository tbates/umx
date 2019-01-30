# Upcoming in next version... umx 3.0.0 January 2018, R 3.5.2
* NEW: `umxRAM` supports WLS, UWLS, DWLS models!
* NEW: `umxACE` supports WLS, UWLS, DWLS models!
* NEW: `umxACEv` supports WLS, UWLS, DWLS models
* NEW: `umxCP` supports WLS, UWLS, DWLS models
* NEW: `xmu_check_variance` checks data for minVar (default > 0.1) and comparable scale for variables (maxVarRatio default = 500)
	* `umx_set_data_variance_check` getter/setter for variance checking data
* DATA: Added Fischbein (1977) weight data (weight of 66 females record at 6 6mth intervals. `data(Fischbein_wt)`
* FIX: `umxValues` to work more reliably with data with no means.
* IMPROVED: `umx_read_lower` examples and input checking.

# umx 2.9.9 December 2018, R 3.5.2
* Compatibility with OpenMx changes.
* NEW: `umxSummarizeTwinData` to create summary tables for papers using twin Data
* IMPROVED: `umxRAM`: support definition variables to some extent in umxRAM
* IMPROVED: `umxMatrix` advice user when they specify `umxMatrix("me", 1,1)`
* IMPROVED: `umxModify` nicer free-parameter report as default output with no changes requested (calls parameters)
* IMPROVED: `umxRAM` preserve definition variables in data
* IMPROVED: `umxRAM` preserve definition variables in data
* IMPROVED: `plot` strip_zero more reliable
* INCOMPATIBLE: `plot` dropped deprecated showMeans and showFixed from plot after 3 years (use means= and fixed = )
* INCOMPATIBLE: removed deprecated suffix parameter from `umx_scale_wide_twin_data`

# umx 2.9.0 December 2018, R 3.5.1: More love
* INCOMPATIBLE: Support for `suffix` as a synonym for `sep` removed after 3-years of deprecation warnings.
* FIXED: `umxSummary`for `ACEv` models: CI now works - (was 'a' not 'A').
* NEW: `xmu_safe_run_summary` Runs a model safely, optionally summarizing and comparing it to a base model.
* IMPROVED: All models now support `tryHard` as a parameter!!
* IMPROVED: `autoRun` option in more places.
* IMPROVED: `plot` functions now have `strip_zero` option (default = strip leading zeroes from parameter estimates)
* IMPROVED: `umx_standardize_ACEv` now working!
* IMPROVED: Models use `xmu_safe_run_summary` so even bad models are returned for the user to diagnose them.
* IMPROVED: Omnigraffle stencil improved. Use to draw diagrams.
* IMPROVED: `umxRAM` now labels paths when no data are provided (simulation of sketch modes).
* IMPROVED: `umx_score_scale` can cope with a single item.
* IMPROVED: `xmu_safe_run_summary` don't error on code red non-run models.
* IMPROVED: `xmu_twin_check` tells user how to rename selVars and sep when sep is not provided, but is enforced.
* IMPROVED: `umx_as_numeric` allows user to select which columns to convert.
* IMPROVED `umxSimplex` start values now flexible and robust (was hard coded for 4-times).
* IMPROVED `xmu_simplex_corner` Takes a full values list (not just 1).
* IMPROVED: `umxAPA` uses `.Last.value` as default input if none provided.
* IMPROVED: `umx_make` supports `what = "rhub"`.
* IMPROVED: `umxlong2wide` now allows user to retain only desired twin IDs.
* IMPROVED: Help-file model diagrams use pdf for pdf output.
* MINOR: `xmu_mean_var_starts` uses "expMean_" as the name for means cells (was "mean").
* MODIFY: xmu_safe_run_summary don't return bVector (already available)
* HELP: `umxACE` help diagrams improved with clearer ACE_matrix picture.
* HELP: `umxRAM` help gained an example of sketch-mode and diagram.
* HELP: `umx_scale` help file improved
* HELP: new pdfs for images in pdf help; remove _ from image names
* HELP: Examples cleaned up


# umx 2.8.5 October 2018, R 3.5.1: More love
* NEW: `xmu_make_mxData` function to make mxData functions out of dataframes (and also drop variables from cov/cor dataframes.
* NEW: SEstyle confidence information for CP and IP `plot`s.
* IMPROVED: `umxCompare` gains ability to output Weight AIC conditional model probabilities! h/t @mNivard
* IMPROVED: `umxReduce` returns AIC weight-based probability of being best model.
* IMPROVED `umxReduce.MxModelGxE` returns best model (invisibly).
* IMPROVED: `umxReduce` for GxE, don't run no A and noC models with moderation in place
* IMPROVED: `namez` given a list of models will return the names of each.
* IMPROVED: `namez` allow global replace.
* IMPROVED: `umx_lower2full` can now take a data.frame.
* IMPROVED: `umx_aggregate` can now open tables in a web browser.
* IMPROVED: `install.OpenMx` can now install a package file if selected in the Finder (MacOS only)
* IMPROVED: `install.OpenMx` now works with windows.
* IMPROVED: `umx_print` more robust to non table input.
* IMPROVED: `xmu_make_mxdata` more robust to non dataframe input.
* IMPROVED `umx_apply` robustness.
* IMPROVED: `umx_simplex_corner` can take a numeric to set matrix size.
* IMPROVED: `umx_long2wide` warn if twinID (order) has too many levels; improved help; @md
* BUG: `umxAPA` test parameter had no effect.
* HELP: Better links, text, layout, etc., umxCP/IP HELP figures.

# umx 2.8.2 June 2018, R 3.5.0: Simplex or s__t sticks
* FIXED: `umxSexLim` now works for univariate tests (thanks to Michael Zakharin for reporting!)
* FIXED: `umxSummary` Corrected Chi^2 df for RAM models.
* NEW: `umxCP` handles ordinal, continuous and binary data
  * Note: old version still available as `umxCPold`
  * Note: new version also lowerbounds specifics at 0 by default.
* NEW: `umxCPplot` re-written to allow arbitrary labels and correlated factors.
* NEW helper functions: umx_graphviz_rank, umx_mat2dot
* NEW: `umx_set_optimization_options` function to set "mvnRelEps" tolerances which impact, especially, ordinal model.
* REFACTOR: new twin model creation helper will help ensure consistency and improve multiple twin functions
* IMPROVED: `umx_is_class` reports classes if not given a set of classes to check
* IMPROVED: `umx_scale(verbose=TRUE)` now lists the vars it didn't touch as well as those it didn't (also formatted better)
* IMPROVED: `umx_make_TwinData`  can set mean and SD of data
* IMPROVED: `umx_simplex_corner` can take a numeric to set matrix size
* FIXED: `umx_standardize_CP` was ignoring existing CIs
* DELETED: `umxAlgebra` This appears broken in some circumstances?
* HELP:  verbose option for twin helper; better feedback;

# umx 2.6.5 May 2018, R 3.5.0: Poly gets a cracker
* NEW: Beta versions of `umx_polychoric`, `umx_polypairwise`, and `umx_polytriowise`
* NEW: `umxSimplex`twin model!
  * `umxSummary` for simplex
  * `plot support for simplex
  * `iqdat`longitudinal IQ twin data for simplex modeling
* NEW: `namez` alias for the ever-useful `umx_names` function.
* NEW: `umxAlgebra` (just allows name first).
* FIX: `install.OpenMx` URLs for NPSOl and travis builds of OpenMx
* BREAK: Following reports of problems in RStudio, changed umx class names to remove the period character from MxModel derivative classes
 * This will **break** hard-coded calls, for instance  to `umxSummary.MxModel.ACE` instead of `umxSummary`. 
  * "Just" replace with `umxSummary.MxModelACE` # i.e. delete the last period in the name... Sorry :-(
* IMPROVED: `parameters` now returns helpful near-match names as suggestions on not finding actual matches.
* IMPROVED: `parameters` now handles vectors of regular selections
* IMPROVED: `umx_print` now handles vectors.
* IMPROVED: `umxConfint` "smart" feature to request only free algebra cells for models I understand (umxCP in the first instance)
* IMPROVED: `namez` can handle a model summary (accesses names() of the summary)
* IMPROVED: `umxGxE` added option to control plot colors
* IMPROVED: `umxPlotCP` can handle >9 manifests [issue](https://github.com/tbates/umx/issues/47)
* HELP: `umxGxE` example improved
* HELP: More related-function links in help files.
* HELP: Better error messages.
* HELP: Help titles, parameter documentation.
* INTERNAL: `xmu_simplex_corner` helper function for simplex modelling.
* INTERNAL: `umx_make_sql` improved 

# umx 2.4.0 March 21 2018, R 3.4.4 post Boulder Workshop CRAN edition)
* NEW: `umxIP` nFac now allows specifying different numbers of a, c, and e factors!
* NEW: `umxACEv` plot works (shows correlated factors).
* NEW: `umxACEv` standardize mostly functional.
* NEW: `umxEFA` can now take a formula to select variables.
* IMPROVED: `umxIP` and CP examples improved (3-variables, GFF data).
* IMPROVED: `umxIP` *reporting* of `as`, `cs`, `es` simplified into compact stack.
* IMPROVED: `umxCP` *reporting* of `as`, `cs`, `es` simplified into compact stack.
* IMPROVED: `umxPlotCP` labelling of variables improved (catching more _Tn).
* IMPROVED: `umxEFA` reports factor correlation matrix (thanks @ConorDolan)!
* IMPROVED: `umxEFA` can return a loadings object.
* IMPROVED: `umx_make`not supports spelling check.
* IMPROVED: `umxACEv` diagonal now unbounded by default (which is it's whole purpose)
* IMPROVED: `umxACEv` got major improvements to help file.
* IMPROVED: `umxSummaryCP` got ability to print to html.
* IMPROVED: `plot` made more robust to unexpected labels (now that umxModify can make newLabels)!
* MINOR: `umx_names` Added GFF examples.
* DROPPED: deleted deprecated showStd and showMeans parameters from plot ACE.
* BUGFIX: `umxSummaryIP` was assuming nFac = 1

# umx 2.0.2 (Feb 12 2018, R 3.4.3 Higher goals edition)
* NEW: `umxSummary` works with multi-group models!
	* (make these with `umxSuperModel`)
* NEW: `umxModify` supports multiple find strings and replace strings!
* IMPROVED: `umxAPA` supports glm, more robust input checking.
* FIXED: `umxEquate` allows vectors of labels.
* DOCS: `umxGetParameters` has better help.
* DOCS: `umxHetCor` docs now note is will return a pos-def version if given a cov matrix. 
* DOCS: `umxAPA` now has t-test and glm examples
* DROPPED: `umx_install_OpenMx` (use `install.OpenMx`)

# umx 2.0.0 (January 29 2018, R 3.4.3 Birthday edition)
* BETA: `umxSexLim` multivariate twin sex-limitation.
* BETA: `umxGxE_biv` bivariate Gene-environment interaction model.
* BETA: `umx_make_TwinData` Can now make bivariate moderated twin data.
* BETA: Beginning a `umxGroup` function as part of measurement invariance support.
* NEW: `umx_stack` Slightly more powerful version of base stack.
* NEW: `umx_array_shift` Shifts an item off the beginning of a list.
* IMPROVED: `umxRAM` can take lists of paths as input (contributed by @bwiernik).
* IMPROVED: `umxModify` can write newlabels.
* IMPROVED: `umxAPA` can back-out an SE if given b and CI.
* IMPROVED: Help pages and examples improved and reorganized.
* DROPPED: `umxReRun` (use `umxModify`)
* DROPPED: support for `suffix` parameter (use `sep` instead)

# umx 1.9.2 (December 2017, R 3.4.2)
* *TIP*: Filter paths by estimated size with `umx_parameters(model, "below", .1)`
* NEW:  `tmx_genotypic_effect` Part of a suite of teaching functions for biometric genetics!

# umx 1.9.1 (November 2017, R 3.4.2)
* *TIP*: `?umxRAM` has a sketch mode. just set `data` to a list of manifest variable names.
* NEW: `umxSuperModel` function to automate multiple-group model creation.
* NEW `?umxACEv` Variance component version of ACE. (beta)
* NEW: CIs in output tables!
* NEW `?umxACEcov_fixed` Beta1: Handles main effects of covariates in the means of continuous variables.
* NEW `umxModify` can now equate parameters (set master = ), update is slave set of labels.
	* As a result, umxEquate now doesn't autoRun by default. Let me know if this is a pain.
* NEW: Dataset on general family functioning in twins. ?GFF
* NEW: `optimiser` parameter for umxACE and other twin models: Set the optimizer in your model code!
* NEW: `umx_is_numeric` boolean check if variables in a dataframe are numeric.
* NEW: `umx_is_class` boolean check if variables in a dataframe match a desired class.
* NEW: `umx_twin_check` internal function for common input checks.
* NEW: `umx_set_plot_file_suffix(c("gv", "dot")` function (to control the filename used for figures)/
* FIXED: `umxACE` was broken with large cov inputs h/t @NathanGillespie.
* FIXED: `umxAggregate` now actually supports user functions as input...
* BEGINNING: to add `umxACEv` variance components method
* IMPROVED: `umxPath(Cholesky = )` method: supports labels, bounds, lbound at 0.
* IMPROVED: `umxPath(Cholesky = )` method: return one statement instead of a list for clarity.
* IMPROVED: `umxACE` and `umxReduce.ACE` run intervals
* IMPROVED: `umxReduce` works better with `umxACE` models.
* IMPROVED: `umxEFA` Supports `minManifests`.
* IMPROVED: `umxEFA` Works better when returning a single factor score.
* IMPROVED: `umxParameters` now supports `digits`.
* IMPROVED: `umxSummary` "inline" reporting now includes AIC.
* IMPROVED: `umx_aggregate` works  with factor input/
	* `umx_aggregate(sex_T1 ~ zyg_2grp, data = x)`
* IMPROVED: `umx_long2wide` can `passalong` variables.
* IMPROVED: `umx_wide2long` much more powerful
* IMPROVED: `umx_msg` supports dataframes
* IMPROVED: `umxACE` gains ability to set plot format: `format = "graphviz" or "DiagrammeR"`
* IMPROVED: `umx_set_auto_plot` take TRUE/FALSE as input.
* IMPROVED: `umx_names`: added a replace option to munge the names
* IMPROVED: `umxRAM` in "sketch mode" now plots models automatically.
* IMPROVED: `install.OpenMx` from UVa, travis, or open travis build page.
* IMPROVED: `umx_cor` automatically drops non-numeric columns.
* IMPROVED: `umxCI` now supports setting interval and one- or two-sided type (h/t @Conor Dolan).
* INTERNAL: Switch from `R2HTML` to `xtable`
* MINOR: `umxParameters` now supports non-run models

# umx 1.8.0 (September 2017, R 3.4.1 (Single Candle)
* *TIP*: `?umxAPA` can format lots of things for you: from dataframes to p-values - try it out! Let me know what you'd like.
* NEW `?umx_parameters` function, report parameter estimates, filtering by name and value!
	* aliased as `umxParameters`
	* `umx_parameters(model, "below", .1, "loading")`
* NEW `?umx_long2wide`: Merges long data on famID, for an unlimited number of individuals in a family (twinIDs).
* NEW `?umx_wide2long`: Takes a wide df (currently limited to 2 per family), & returns a long-format version.
* FIXED: Removed superfluous print call from `umx_MakeTwinData`.
* IMPROVED: html tables by switching to `xtable` (`R2HTML` is abandoned & strips decimals from AIC, ignores c(nsmall)...
* IMPROVED: Plotting: Models now plot by default. Turn off with `umx_set_auto_plot(FALSE)`.
* IMPROVED `forEach` option in `umxPath`. This is a one.headed version of "unique.pairs".
* IMPROVED: Output: Twin models now show summary when `autoRun`.
* IMPROVED: Graphics: `umxGxE` plot colors now use the universal ACE -> RGB scheme.
* IMPROVED: `umxACEcov` Now tries to detect violations: Age and Sex are good examples.
* IMPROVED: Stability: bound diag to 0 by default in `umxACE`.
* IMPROVED: Stability: Stop on covs that are identical across twins in `umxACEcov`!
* IMPROVED: Better default model name for  `umxACEcov`;
* IMPROVED: More helpful default core list for `umx_check_parallel` (1 and n-1);
* IMPROVED: Output formatting of times in `umx_check_parallel`
* IMPROVED: `umxReduce` does a much better job reporting automated umxGxE model reduction.
* IMPROVED: `umxAPA` handles lme models.
* IMPROVED: `umxPrint` error messages.
* DOCUMENT `logic.MxModel` now has an Rd page.
* CHANGE: Showing Rg (genetic correlation) table is now off by default for umx twin model summaries.
* CHANGE: `confint`. OpenMx now supports this, so I dropped it from umx (added alias to `umxConfint`).
* CHANGE: `anova`. OpenMx now supports, this so I dropped it from `umx`.
* CHANGE: `umx_fix_first_loadings` now skips latents with fixed variance.

# umx 1.7.5 (April 04 2017, R 3.3.3 Another Canoe)
* *TIP*: In twin models, mzData and dzData can contain unused variables (umx removes them for you!)
* NEW: `umxVersion` (returns mxVersion() + umx version).
* IMPROVED: `umxACE` start values much better for univariate models.
* NEWFEATURES: `umx_make_TwinData`
	* Use variance input
	* Option to set seed
	* Add varName to datasets
	* nThresh to threhold the created variables, and return as mxFactors
	* Surface the empirical parameter of mvrnorm
	* More flexible: Leave out any one of A, C, or E.
	* omit nDZPairs to get equal numbers
	* Option to allow A+C+E ≠ 1
	* Full set of examples
	* NOTE: `umx_make_TwinData` uses **variance** input. This was ambiguous previously.
	* Docs much improved.
* NEWFEATURES: `install.OpenMx` can install UVA parallel, travis latest (on mac) or opening the travis list.
	* BUGFIX: `install.OpenMx` had a broken URL, now corrected.
* NEWFEATURES: `umxRAM` can now be used in "sketch" mode, to plot demos without data: just add the list of manifests as a character string to `data`.
	```splus
	m1 <- umxRAM("test", data = paste0("itemC", 1:4),
		umxPath(unique.bivariate = paste0("itemC", 1:4)),
		umxPath(var="X")
	)
	plot(m1)
	```
* NEWFEATURES: `umx_cont_2_quantiles` now allows returning cut points, better examples, level_names
	* BUGFIX: `umx_cont_2_quantiles` lowest threshold was empty
* NEWFEATURES: `umxAPA` now reports mode for factor data
* NEWFEATURES: Allow `umxLabel` to rename the returned model
* IMPROVED: `sep` is now the preferred separator (synonym for suffix in umxACE, umxCP, umxIP)
* DOCUMENT: Examples use zygosity string from twinData instead of numeric code
* DOCUMENT: Fix typo in `umxPath` docs
* INCOMPATIBLE: renamed `umx_cov_diag` to umx_var

# umx 1.5.5 (March 20 2017, R 3.3.3 "Another Canoe")
* NEW: `umx_write_to_clipboard`
* NEW: `umx_r_test`
* IMPROVED: Add autoRun option to umxModify.
* FIXED: Switch away from CSOLNP in umxCP example.
* ROLLBACK: No longer generically alter CSOLNP tolerance.

# umx 1.5.1 (March 05 2017 R 3.3.2)
* IMPROVED: `umxCP` example for improved compatibility with OpenMx 2.7
* IMPROVED: umx twin models now set a better (less likely to fail) precision for CSOLNP
* IMPROVED: improved help files; document forms parameter for `umxPath`
* IMPROVED: handling list inputs
* IMPROVED: behavior of `confint`()
* IMPROVED: formatting of `umxAPA` table output
* ADD: "check" option to `umx_make`
* FIXED: umxpath(a, forms= ) arrows = 2 bug

# umx 1.5.0 (December 2016)
* Features + OpenMx 1.7.3 compatibility release
* IMPROVED: `umxRAM` returns invisibly
* IMPROVED: `umx_aggregate` can summarise multiple DVs in a table
* IMPROVED: `umx_aggregate` can return a formatted table (kable = TRUE)
* IMPROVED: `umxAPA` returns lower-triangle of data.frame correlations (by default)
* REMOVED: `umxSE` now included in OpenMx 2.7.0!
* FIXED: `umx_set_plot_format` uses silent = TRUE internally where needed

# umx 1.4.9 (December 2016)
* Bug fix
* FIXED: `umx_make_TwinData` fix bug in DZ moderator code and A+C computation
* IMPROVED: more functionality hints in man page titles

# umx 1.4.5
* November 2016 "Usability is queen/Sincere Pumpkin Patch"
* IMPROVED: Reorganize function families to increase clarity
* IMPROVED: `umx_set` functions now show legal options by default
* NEW: `umx_set` functions have silent option
* FIX secure http URLs

# umx 1.4.0
* Mid October 2016 "New features and improvements"
* BUG FIX: `umxFitIndices` RMR and SRMR (h/t Brenton Wiernik!)
* NEW: `umxMatrix` with default labels and name as 1st parameter 
* NEW: Definition variable umxPath type, e.g.
	* `umxPath(defn = "def", labels = "data.age")`
* IMPROVED: More concise feedback
* IMPROVED: `umxEquate` supports autoRun and compare
* IMPROVED: `umx_names` can, as a convenience, take string vectors
* RENAME: `umxGetOpenMx` aliased to `install.OpenMx` or `umx_install_OpenMx`
* IMPROVED: `umxFitIndices` additional fit-indices and now automatically computes refModels if needed.
* IMPROVED: `umxRAM` remove_unused_manifests no longer updates manifestVars + leaves variable in data by default.
* RENAME: `plot` "showFixed" deprecated in favour of easier to type "fixed"

# umx 1.3.0
* R 3.3.1 companion: "Bug in your Hair" August 2016
* NEW: `umxPath` allows `unique.pairs` connection
 * Useful for growth curve models
* IMPROVED: `umxRAM` handles suffix labels
	* Useful for models embedded in multi-group 
* IMPROVED: `umxSummary` to show parameters from first model in multi-group
	* Begin to support multi-group in umxSummary
* IMPROVED: twin model figures tweaked
* CHANGE showStd= to std= in most remaining cases
	* Might need to update your code!
* CHANGE: `umx_show_options` to `umx_get_options`
	* Might need to update your code!
* FIX: `umx_check_parallel` typo
* FIX: `plot` calls from summary
* REMOVE: dependency on non-condensed slots

# umx 1.2.8
* R 3.3.0 companion: "Very compatible" May 2016
* NEW: `umxRAM` can now build and run Joint Continuous Ordinal models. Ta da!!
* NEW: `umx_show_options` function to help users learn the options available
* BETA: `umxACE` can now implement a form of left-censoring.
* BETA: `umxThresholdMatrix` can now implement a form of left-censoring.
* IMPROVE: `umxRAM` & `umxRAM2Ordinal` can turn off refModel computation with `refModels = FALSE`;
* IMPROVE: `umx_show` can show thresholds
* IMPROVED: `umx_is_MxModel` can take a list: `listOK` option
* IMPROVED: `umx_check_parallel` options for rowwiseParallel and numberSubjects
* CHANGE: I changed "suffix" to "sep" in several places for consistency: this will break
  some low level code: nearly all of it mine, and fixed. But might affect some users!!
* UPDATE: `umx_time` and `umx_check_parallel` to work with new S4 list deprecation
* UPDATE: shorter run-time on 2sls code

# umx 1.2.7
* May 2016 stable release
* ADDED: `umxTwoStage` FIML-based Instrumental variable and Mendelian Randomization (beta)
* ADDED: `umx_make_MR_data` to simulate SNP-based Mendelian randomization data (beta)
* ADDED: `umx_set_condensed_slots(TRUE|FALSE)`: Get and set matrix compression
* ADDED: `umx_get_OpenMx` (download OpenMP/parallel version of OpenMx with NPSOL)
* ADDED: `umx_cont_2_quantiles(x, nlevels = )`
* ADDED: `umx_factor` alias to umxFactor
* FIXED: `plot(resid = "none")` was not suppressing plotting residuals
* IMPROVED: `umxFactor` now handles dataframes and allows non-ordered output
* IMPROVED: `umx_CI` more flexible, now allows removing CIs
	* Old code will break: replace "add" with "which"
* IMPROVED: `umx_time` handling of no input arguments
* IMPROVED: `umx_pb_note` better feedback and setup
* IMPROVED: loadings() generic
* CHANGE: `umxModify` - if free is a string, it will be used as input and the found paths will be freed.
* CHANGE: `umxModify` - show summary as well when comparison = `TRUE`
* CHANGE: `plot` defaults for `std` and `showFixed` changed h/t @MikeNeale
* CHANGE: `umx_scale_wide_twin_data` argument names for consistency: "suffix" "data"
* CHANGE: `umx_residualize` ditto
* UPDATE: Readme.md

# umx 1.2.5 (April 2016 R 3.2.5 Very, Very Secure Dishes)
* Bug fix release
* FIX: $models bug in `umxStandardizeACE`
* EXPAND: test suite

# umx 1.2.0 (April 2016 R 3.2.4 Very Secure Dishes)
* NEW: `plot` using DiagrammeR!
* NEW: Choose plot format!
	* `umx_set_plot_format("DiagrammeR")`
	* `umx_set_plot_format("graphviz")`
* NEW: Choose table format!
	* `umx_set_table_format("markdown")`
	* `umx_set_table_format("latex")`
* NEW: set plotting by default: `umx_set_auto_plot(TRUE)` 
* NEW: Exploratory factor analysis! with `umxEFA` (beta!)
* NEW `loadings`() support for `umxEFA`
* CHANGE:  .dot --> .gv suffix for plotting (more compatible)
* CHANGE: To $ from @ accessors for OpenMx compression compatibility
* BETTER: Help! ACECov figure, ... many other improvements
* ENHANCED: `umx_print`, `umx_show`, `umxCompare`
* REQUIRE knitr, DiagrammeR
* INCREASE minimum R / OpenMx to  >= 3.2.0 / >= 2.5.0

# umx 1.1.5
* March 2016 Boulder Workshop feedback updates
* NEW: `plot` now works on Windows and Unix as well as OS X! (h/t @mhunter)
* CHANGE: `umxReRun` to `umxModify` (h/t @hmaes)
* BETTER: compatibility with OpenMx 2.5.2

# umx 1.1.1
* Feb 2016 R 3.2.3 Wooden Christmas Tree edition
* NEW: auto-run now the default for all models!
	* Added option to switch this: options("umx_auto_run" = TRUE)
* NEW: `plot` should generate pdfs for Windows and Unix users now
* NEW: `umxACEcov` support for ACE with covariates
* NEW: `umx_make_TwinData` added to support simulation studies
* BETTER: `umx_check_parallel` now returns timing information
* BETTER: `umx_time` now supports "start" and "stop")
* BETTER: `umxAPA` can format more objects in APA style, inc just a p-value)
* BETTER: `summaryAPA` can now summarise a data.frame with mean, sd, and correlations
* BETTER: `umxLabel` now preserves existing labels in umxRAM models: labels = is now saved
* BETTER: Help improvements (wording, seealso etc)
* DEEP: Removed dependency on formula.tools

# umx 1.1.0 (December 2015, R 3.2.3/Wooden Christmas-Tree update
* NEW: `vcov`() added
* NEW: Begin support for sex-lim twin models
* NEW: New data file for sex-limitation
* IMPROVED: `umxRAM` can take existing model as input (very useful)
* IMPROVED: `umxRAM` reports model fit and comparison (if old model exists)
* IMPROVED: `umxPath` now lbounds var@0 for v.m.
* IMPROVED: `umx_residualize`  can now take multiple DVs
* IMPROVED: `umxMI` enhanced to cope with more situations
* IMPROVED: `umxSummary` more resilient with missing CIs (#ht Nathan Gillespie!)
* IMPROVED: `summaryAPA` now handles data as well as lm
* IMPROVED: Help for twin functions
* IMPROVED: Various new function features
* IMPROVED: `umx_print` resilient to 0-row input
* IMPROVED: summary helpers (e.g. `umx_fun_mean_sd`)
* IMPROVED: consistency in parameter names (minor backward incompatibility)
* DEEP: Replacing @ with $ accessor begun

# umx 1.0.0
* NEW `umxCP` twin models!  + plot(), umxSummary()
* NEW `umxIP` twin models!  + plot(), umxSummary() 
* NEW `umxGxE` twin models! + plot(), umxSummary() 
* FIXED `umxPath` limitation where "to" was not being set.
* SUPPORT Circles as well as lines for residual variance.
* IMPROVE Help text. Thanks Mike Neale esp.!
* IMPROVE Organization of functions into @families in the help.
* IMPROVE Example for `umx_lower2full`
* DROPPED umxRAM functionality to set endogenous, exogenous, and latent traits
* DROPPED Deprecated a dozen functions.

# umx 0.50.0 Likely R 3.1.3 (Smooth Sidewalk) ~ 2015-03-09
* First CRAN release! WOot!

# umx 0.49.0
* IMPROVE Edge cases work
* IMPROVE Improving help files and feedback
* IMPROVE Rationalising fn names into @families

# umx 0.45-beta
* Feature complete
* Bug squashing
* Optimising fn names for memorability and typing

# umx 0.40-beta
* Beta level!
* Standard `confint`(); `plot`() etc functions implemented
* Adding umxRAM() and umxPath shortcuts

# umx 0.30-alpha
* Alpha release
* 90% feature complete
* Lots of name rationalisation

# umx 0.25-alpha
* Alpha release
* 80% feature complete
* Function prefix reflects usage:: umxMajor, umx_utility_function, xmuNotForUser