## dev umx 4.2.00

## umx 4.15.0
* NEW: `umx_merge_randomized_columns` wide-2-mingled (rather than wide2long) to unify data from people assigned to different conditions.
* NEW: `umxDiscTwin` Discordant twin test for causal effects.
* NEW: `umxDiscTwin` Graphical output for publication.
* NEW: `umxPlot` ease customized/journal-close, e.g. y~x, -> x y plot, with r^2.
* IMPROVED: `umxAPA` t-test reporting.
* IMPROVED: `umx_score_scale` gains error and proportion.
* IMPROVED: `umx_score_scale` report only omega total when 1 factor extracted.
* IMPROVED: `umx_read_prolific_demog` gains functionality.
* IMPROVED: `umxMR`: gained `tryHard'
* IMPROVED: `umxGxEwindow`: gained `tryHard`, smarter print control.
* IMPROVED: `umxGxEwindow`: removes un-needed columns.
* IMPROVED: `umxGxE` checks that `selDefs` are numeric (closes #196, h/t @salvatoj)
* IMPROVED: `umxRun`: gained `summary` control.
* IMPROVED: `umx_read_prolific_demog` more control, better defaults, and more reliable summary table.
* IMPROVED: `umx_score_scale` cleaner output for 1 factor case with omega.
* FIXED: `xmu_twin_print_means` now takes >1 covariate h/t @Eren Kafadar #201
* FIXED: `xmu_safe_run_summary` now takes refModels = FALSE h/t @lf-araujo #200
* FIXED: `umxACEcov` plot and summary working (needed to filter covs from DVs).
* FIXED: `xmu_twin_print_means` handles more than 1 covariate and improves labeling.
* FIXED: `umx_score_scale` handles NA correctly.
* SAFE CODING: `umxPath` stops when `v*m*` is used with `firstAt` or `fixedAt`.
* SAFE CODING: `umxAlgebra` warns that it isn't functional.
* MINOR: `umx_print` call fixed where was being called with file.
* HELP: `umxCP` help `.svg` figure instead of `.png`` (smaller).
* HELP: `umxMR` Remove unsupported 'weights' parameter.
* ADMIN: copyright, tests, Rd improvements.

## umx 4.10.50
* December 2021 R 4.1.1 (2021/11/30)
* IMPROVED: `umxDoC` can take data (#ht @lf-araujo )
* IMPROVED: `umxDoC` plot Rd figure includes specifics.
* IMPROVED: `umxPower` search plot with fixed N has effect size on the x axis.
* IMPROVED: `umxGxE` plot legend location works for `ggplot`.
* IMPROVED: `umx_score_scale` gains omega reliability.
* IMPROVED: CI printing more robust (e.g. when they fail).
* BUG: `umx_score_scale` bug in handling new `mapstrings`.
* FIXED: `umxDoC` plot.
* FIXED: `omxLocateParameters` call was breaking `umxSummary`.
* FIXED: `xmu_safe_run_summary` supports `intervals` parameter.
* FIXED: `umxSummary` works with `umxSuperModel` now that paths have types.
* FIXED: `umx_make_twinData` works with custom names
* FIXED: `umx_make_twinData` no longer shifts means now that mean and sd parameters are available.
* NEW: helpers to `umxGetModel`, `umxGetLatents`, and `umxGetManifests

## umx 4.10.10
* November 2021 R 4.1.1 (2021/11/30)
* NEW: `umxPower` can plot power search space! (beta!)
* NEW: `noNAs()` helper returning complete rows of a data.frame.
* NEW: `umxThresholdMatrix` can handle one value columns
* IMPROVED: `umxPath` gains `v0m.`
* IMPROVED: `umxPower` parameters ==1 for power search with fixed n
* IMPROVED: `umxPower` ref
* IMPROVED: `power.ACE.test` can run ADE model
* IMPROVED: `power.ACE.test` Clarify which of power, effect, and n are fixed or being estimated.
* IMPROVED: `umxReduce.ACE` gains tryHard  (compute ACE/ADE/CE/AE, outputs a table an also AIC-based probability weights for which is the preferable model)
* IMPROVED: `umxReduce` tests familiality (E model)
* IMPROVED: `umxReduce(ACE)` includes the parameters of the reduced models. #ht @nick martin
* IMPROVED: `umxSummarizeTwinData` takes an age column and notes when it can’t be found
* IMPROVED: `umx_score_scale` can score items with character labels
* IMPROVED: `umx_score_scale` gains the ability to report Cronbach alpha
* CHANGED: `umx_score_scale` changed default `na.rm = TRUE`.
* IMPROVED: `umxAPA` gains OR output for logit models
* IMPROVED: `tmx_show` handles matrices
* IMPROVED: `umx_rename` better names for deprecated parameters
* IMPROVED: `umxRun` has tryHard (not a very useful function)
* IMPROVED: `umxCompare` can silently return a data.frame for further modification
* IMPROVED: `umxCP` warning to not use `correlatedACE`
* IMPROVED: `umxAPA` (invisibly) returns the standardized model for lm
* IMPROVED: `bucks` more control for formatting money
* IMPROVED: `libs` now install.packages() if not found
* IMPROVED: `umx_make_TwinData` shifts data to be positive if it not being scaled
* IMPROVED: `grViz()` in place of print(DiagrammR())` (should work better with knitr)
* CHANGE: `fin_interest` more memo/type-able parameter name for inflation
* CHANGE: `umxRun` defaults to tryHard= "yes"
* FIX: `umxPower` supports multiple path drop.
* HELP: `FishersMethod` Rd ref
* HELP: `umxCP` ref Martin, N. G., & Eaves, L. J. (1977). The Genetical Analysis of Covariance Structure. *Heredity*, **38**, 79-95.
* HELP: `deg2rad` Rd better in html
* HELP: `umxRAM` sketch mode help
* HELP: `umxCP` more refs for Rd
* HELP: improvements

# Releases

## umx 4.9.0
* July 2021 R 4.1.0 (2021-06-27)
* NEW: `umxACEv` and `umxACE` work with `nSib = 3`!
* BETA: `umxCP` `correlatedACE` was not being filtered out of the expectation #hat-tip @micanzach But still working on what the right choices are here...
* CHANGED:  `umxCP` defaults to tryHard = "yes"
* REMOVED: `umxRAM` deprecated parameter `show` after 4 years deprecation
* FIXED: `tmx_show` works for M matrix, closed  #163
* FIXED: `kable` and `kbl` get format passed in at all calls
* IMPROVED: `umx_make_TwinData` supports `nSib` > 2 for MZ/DZr mode
* IMPROVED: `umxAPA` more use of cat to print (easier to read and copy)
* IMPROVED:  `umx_open_CRAN_page` printing if installed version if found
* IMPROVED: `umxAPA` more use of cat to print (easier to read and copy)
* IMPROVED: `fin_interest` compute effective interest
* IMPROVED: `xmuTwinSuper_NoBinary` update for nSib > 2
* MINOR: `kable` formats (pandoc "pipe" and "simple" added, "pandoc" removed)
* MINOR: `add_footnote` in place of `footnote`...
* HELP:  `umxPath` and `umxRAM` help
* HELP:  better organization of functions/families
* xmu: `xmuTwinUpgradeMeansToCovariateModel` can handle nSib=3
* xmu: `xmu_twin_make_def_means_mats_and_alg` helper
* xmu: `xmu_start` improved closes #158

## umx 4.8.0
* June 2021 R 4.1.0 (2021-06-01)
* NEW: `umxPlot` now detects and plots Pearson-Aitken selection paths (solid red headless).
* NEW: `umxPlotFun` plot functions without hassles. e.g `umxPlotFun("sin(x)")`
* NEW: `umxModelNames` to get model and submodel names from a model.
* NEW: `libs` as a short cut for `multiple library()` calls
  * e.g. `libs("x", "y", c("a", "b"))`
* NEW: `fin_valuation` uses revenue, margin, expenses and PE to compute a market cap.
* IMPROVED: `fin_interest` shorter parameter name `dinflate` and other improvements.
* IMPROVED: `umxACEv` Tables now include markdown-compatible table captions.
* IMPROVED: `umxAPA` reports R^2 for `lm` (in addition to beta & CI[] for each parameter)
* IMPROVED: `umxSummary` sorts RAM parameters by type (factor loading etc.) and can filter means and residuals for ease of use.
* IMPROVED: `umxSummary` gives desired levels for good TLI and RMSEA.
* IMPROVED: `umxSuperModel` checks for duplicate names in incoming model names and makes them unique.
* IMPROVED: `umxCompare` Maintain compatibility with new OpenMx changes to `mxCompare` (esp. for WLS).
* IMPROVED: `umx_read_lower` accept names for lower diag.
* IMPROVED: `umxCP` `correlatedACE` leaves the covariances fixed@0.
* IMPROVED: `umxACE` added `A_std` closes #151
* IMPROVED: `umxAPA` pretty printing.
* IMPROVED: `umxPlotFun` check for character input.
* NEW: `xmu_summary_RAM_group_parameters` sort and filter RAM model parameters for ease of understanding.
* MINOR: `umxFactor` now used in all examples involving factor making.

## umx 4.3.0
* January 2021 R 4.0.3 (2020-10-10)
* NEW `plot` can output to pdf, png, svg!
* NEW `umx_set_plot_format` adds pdf, png, svg plot output (in addition to DiagrammeR and graphviz)
* NEW: `umx_lower.tri` helper to get lower.tri values in 1 call
* NEW: `umx_set_plot_use_hrbrthemes` control
* NEW: `plot.percent` plot support for percent change questions
* IMPROVED: `umxSummary` now reports algebra values when present in RAM models closes #70
* IMPROVED: `umxSummary` tables have pandoc captions (see #145)
* IMPROVED: `umxCompare` table output and plain-English summary printing (cat)
* IMPROVED: `umxCP` practical workflows for correlated ACE
* IMPROVED: `fin_compound_interest` now outputs a table of annual returns
* IMPROVED: `umxAPA` uses cat to improve usability of summary data
* HELP: `umxCP` Document that correlatedA works on a, c and e, allowing factors to correlate.
* BETA: `umxGxEbiv` "lboundACE" @ 0 by default
* FIXED: `umxSimplex` example
* FIXED: `umxGxEbiv` fix nonfunctional lboundACE and lboundM (thanks @mcneale)
* FIXED: `umxGxEbiv` selVars order was imposed on expectation dimnames but didn't match the cov matrix due to bespoke nature of `expCov` algebras...

## umx 4.2.5
* December 2020 R 4.0.3 (2020-10-10)
* NEW: `tmx_show` display model matrices on the web, with color-coded free/false, values + roll-over labels.
* NEW: `harmonic_mean` with support for weights.
* HELP: `harmonic_mean` Example averaging speeds across distance.
* NEW: `geometric_mean`
* NEW: `fin_compound_interest`
* NEW: `fin_percent`
* IMPROVED: `umxPlotGxE` Gains CI ribbons (closes #96)
* IMPROVED: `umxPlotGxE` Defaults to use ggplot and now returns the graphs for further decoration (closes #139)
* IMPROVED: `umxPlotGxE` Can now plot user-specified moderator ranges (closes #96)
* IMPROVED: `umxPlotIP` Now relies on matrix cells, not their labels (closes #61)
* IMPROVED: `umxACE` Returns group models invisibly #134 @khusmann.
* IMPROVED: `plot` Now allows "ortho" "compound" and "polyline" spline control.
* IMPROVED: `umx_set_optimization_options` Can get and set "Parallel diagnostics"
* IMPROVED: `tmx_show` Smart about not printing NULL matrices.
* IMPROVED: 'umxEquate` Gained a `newlabels` parameter for the equated paths (rather than one path adopting the label of the other)
* IMPROVED: `umx_score_scale` Supports `minManifests` for factor scores
* IMPROVED: `umx_open_CRAN_page` Can now install a package, as well as open the CRAN page for the package.
* IMPROVED: `umx_rename_file` Better feedback about matches and files checked.
* IMPROVED: `umx_long2wide` More resilient to zygosity column type.
* IMPROVED: `umxSummarizeTwinData` More resilient to name of age column
* IMPROVED: `plot` `strip_zero` option works for fixed (@) parameters
* IMPROVED: `umx_is_class` Handle non-df input.
* IMPROVED: `umxCI`: Gains `regex` parameter to support adding and removing CIs on labels matching a string pattern.
* IMPROVED: `umx_is_class` Handles non-df input.
* IMPROVED: `umx_set_table_format` Defaults to "markdown" on load.
* FIXED: `umxPlotIP` Wasn't working with threshold models (didn't ignore thresh names for plot).
* FIXED: `umx_time` Was using time units for model 1 for all models... h/t @khusmann
* FIXED: `umxPlotACEv` Was not displaying half the variables in an MV model as NA.
* FIXED: `umx_set_cores` No longer use `imxGetNumThreads` which is hard-coded to not report number of threads.
* FIXED: Check for more than one named CI requested (closes #136).
* FIXED: `tmx_show` Has `html_font` parameter in help.
* WORKAROUND: 'umxSummary` Catches `mxRefModels` error for WLS models and politely informs user OpenMx doesn't support fit stats for WLS yet.
* BACKWARD: `umxThresholdMatrix` Changed parameter from selDVs to fullVarNames to prevent user error.
* NEW: `umx_read_prolific_demog` to read prolific academic demographics files.

## umx 4.1.0
* July 2020 R 4.0.0
* CHANGE: `umxRAM` Groups in multi-group models now independent by default (label style  "path_group" )
* FIXED: 'umxRAM` Group option failing to pass autoRun tryHard Merge pull request #134 @khusmann
* FIXED: `umxRAM` No longer demand new data if a model is given as the first parameter. Updates data if provided.
* IMPROVED: `umxACE` Return group models invisibly #134 @khusmann
* IMPROVED: `umxACEv` support for covariates enabled.
* IMPROVED: `umx_score_scale` gains the ability to return a factor score.
* IMPROVED: `umxEFA` returns scores by default when requested (used to require user to also set `return = "loadings"`  (thanks to user!)
* IMPROVED: `umx_r_test` handles case of overlapping variables, i.e., compare r(ab) to r(ac) 
* IMPROVED: `umxPlotACE` uses matrix cells, not labels to draw the plot (so now labels are arbitrary)
* IMPROVED: `umxIP` checks names of nFac for each of a, c, and e 
* IMPROVED: `umxSimplex` default sep is now "_T" instead of NULL (should do this throughout?) 
* IMPROVED: `residuals` can now suppress 0s - helpful for large tables
* IMPROVED: `xmu_dot_mat2dot` can cope with 1-variable name lists
* DEVELOPER `umxValues` -> `xmuValues` clean up namespace 
* DEVELOPER `umxLabels`  -> `xmuLabels` clean up namespace
* FIXED: `parameters`: std wasn't being used.
* INFO:  TLI /CFI vs. RMSEA
* HELP:  `umxEFA` examples.
* HELP:  `umxTwinMaker`.


## umx 4.0.0

* May 2020 R 4.0.0
* FIXED: `umx_score_scale` Fix a bad design decision where I was storing the reversed items in the data returned to the user. Worked for 1 run, but not repeats...
* FIXED: `umx_long2wide` now looks for zygosity in more than just twin 1 h/t @nathangillespie for catching this case.
* NEW: Covariates in twin models for ordinal and continuous manifests!
	* `m1 = umxACE(selVars = "wt", selCovs= "age", sep="_T")`
* NEW: `umxTwinMaker`: Make twin models by specifying *just the within-twin paths for one twin*
	* umxTwinMaker handles making twin2, variance components covariances, MZ and DZ groups, makes a `umxSuperModel` and runs it!
	* `plot()` and "std= TRUE" work for any model now! 
* NEW: `umxDoCp` Path-based Direction of causation model.
* IMPROVED: `umxSummaryACE` prints a means table!
	* including for intercepts only, no means model, and intercepts + covariate betas.
	* All RAM and twin models gain this also.
* IMPROVED `plot` for `umxACEv` now prints model name on the diagram.
* IMPROVED `plot` for `umxACE` now uses tips for printing.
* IMPROVED: `FishersMethod`: accepts p-values for meta-analysis in "..." argument.
* IMPROVED: `namez` handles class(list) objects, returning $ components.
* IMPROVED: `parameters` supports standardizing parameters (closes #92)	.
* IMPROVED: `power.ACE.test` now prints the parameter and value when testing power (closes #111).
* IMPROVED: `umxCompare` includes Delta-AIC.
* IMPROVED: `umxCP` plot prints model name.
* IMPROVED: `umxGxE` code to detect mismatched moderators
* IMPROVED: `umxGxE` code to include co-twin moderator in means model
* IMPROVED: `umxGxE` simplified algebra
* IMPROVED: `umxIP` gains report option.
* IMPROVED: `umxIP` plot prints model name.
* IMPROVED: `umxSummaryGxEbiv` working, with a note that the whole function is beta. h/t @MikeNeale.
* IMPROVED: `umxThresholdMatrix` re-implemented `l_u_bound` - wasn't working for some time h/t @NathanGillespie.
* IMPROVED: `umxThresholdMatrix` supports leaving all thresholds free with `method= "allFree"`	h/t @NathanGillespie.
* IMPROVED: `umx_check_names` returns TRUE when no names are entered.
* IMPROVED: `umx_check` can concatenate extra messages, and is by default polite with messages.
* IMPROVED: `umx_check` handles ... items better
* IMPROVED: `umx_is_ordered` can process mxData as well as df.
* IMPROVED: `umx_make_TwinData` has dzAr.
* IMPROVED: `umx_make_twin_data_nice` can turn numeric zyg column into human readable "MZMM" etc.
* IMPROVED: `umx_print` gains option to append to existing file.
* IMPROVED: `umx_print` gains option to make tables sort-able.
* IMPROVED: `umx_print` gains option to output content in "both" md and html.
* IMPROVED: `umx_print` supports report to html.
* IMPROVED: `umx_rename` now uses "from=, to=" format for renaming columns.
* IMPROVED: `umx_rename` was using old/replace  instead of from/to (and so generating a polite note).
* IMPROVED: `umx_residualize` now prints the betas (and CIs) of the residualization it performs.
* IMPROVED: `umx_score_scale` checks input ranges
* IMPROVED: `umx_score_scale` tell user what the max response is for reversed items if they leave this blank.
* IMPROVED: `umx_score_scale` Warns user if there are NAs when "score"  is totals.
* IMPROVED: `umx_rename_file`: Perl mode is TRUE (allows lookahead/behind features; default list pattern is NULL.
* IMPROVED: `umx_rename_file` now ignores file suffix by default.
* IMPROVED: `umx_rot` can NA the last value.
* IMPROVED: `umx_var` upgrades input to df (e.g. if you feed it a column, it now works).
* IMPROVED: `xmu_data_missing` can `hint=` to user which dataset had missingness.
* IMPROVED: `xmu_make_mxData` handles dropping rows, alerting about variance ratios.
* IMPROVED: `xmu_make_mxData` now handles cov matrix input (added `numObs` parameter).
* IMPROVED: better labels for output tables.
* IMPROVED: standardize on expMean as name of means matrix even when it's a local algebra.
* IMPROVED: twin Models print out means now.
* HELP: `umxACE` improved tutorial, more tips * IMPROVED `plot` for `umxACE` now prints model name on the diagram.
* HELP: `umxACE` covariate examples
* HELP: `umx_explode_twin_names` links for find-ability
* HELP: `umxACE` residualize examples match
* NEW: `umxRenameMatrix` wrap up the multiple lines of code needed to modify a matrix in OpenMx.
* NEW: `umx_str_chars` return selected characters of a string.
* NEW: `umxTwinUpgradeMeansToCovariateModel` function to add def-based covariates to means in umx twin models.
* NEW: `xmu_data_missing` helper to drop rows with missing def vars or stop and alert the user to do this.
* NEW: `xmu_extract_column` to allow mxData
* NEW: `xmu_twin_add_WeightMatrices` helper.
* NEW: `xmuDefMean` helper
* NEW: `xmuDefBetas` helper
* NEW `xmu_twin_upgrade_selDvs2SelVars` helper.
* NEW: `xmu_make_TwinSuperModel` handles selCovs
* REPLACE: `parallel::detectCores` --> ` imxGetNumThreads` (as OpenMx does).

## umx 3.1.0
* 13 March 2020 R 4.0.0 compatibility
* BETA: `plot` for `umxTwinMaker` models!!
* NEW: data for example with direction of causation work on umxSummary for DoC
* NEW: `xmu_path2twin`  twin model helper for `umxPath` users 
* NEW: `xmu_path_regex` helper for making twin models for `umxPath` users
* NEW: `umx_file_load_pseudo` to load and process non-transmitted genome files for the method we developed in Bates, ... Gillespie, N. A. (2018). The Nature of Nurture: Using a Virtual-Parent Design to Test Parenting Effects on Children's Educational Attainment in Genotyped Families. Twin Res Hum Genet, 21(2), 73-83. doi:10.1017/thg.2018.11
* NEW: `xmu_data_missing` helper to drop rows with missing def vars or stop and alert the user to do this
* NEW: `xmu_make_TwinSuperModel` handles selCovs
* IMPROVED: `umxFitIndices` Many new indices! @bwiernik
* IMPROVED: `umxPath(v.m0 = )` now uses values (if provided) to set starts for variance
* IMPROVED: `umx_check_parallel` use all cores (instead of half) by default
* IMPROVED: `umxPlotACE` variable labels (trimmed off _T1 like `umxPlotCP` does
* IMPROVED: `umxDoCp` bound e/es above zero
* IMPROVED: `umxDoCp` start with all variance as specific
* IMPROVED: `power.ACE.test`
* IMPROVED: `umxLabel` 100x speedup for RAM and Matrix model labeling!
* IMPROVED: `xmuLabel_RAM_Model` 100x speedup
* IMPROVED: `xmu_safe_run_summary` has smarter default for `std` (model summary default)
* IMPROVED: `xmu_dot_mat2dot` now accepts algebra input begin umxPlot for DoC
* IMPROVED: `umx_check` handles ... items better
* IMPROVED: `xmu_make_mxData` handles dropping rows, alerting about variance ratios
* BUGFIX: `power.ACE.test` no longer fails when estimating power at a fixed n
* HELP: `umxSummary`: Improve Rd file
* HELP: `umxACE` covariate examples
* HELP: `umx_explode_twin_names` links for find-ability


## umx 3.0.6
* Feb 2020 R 4.0.0 compatibility
* BETA: `umxDoc` example but summary and plot TODO
* NEW: `umxRAM2Lav` export umx to lavaan
* IMPROVED: `xmu_safe_run_summary` has smarter default for `std` (model summary default)
* IMPROVED: `umx_score_scale`: implemented score = "mean" and score = "max" 
* IMPROVED: `umx_score_scale`: better documentation 
* IMPROVED: `umxGxE` gains dzAr and dzCr control so ADE is possible as well as assortative mating
* IMPROVED: `xmu_twin_check` works with mxData * IMPROVED: `xmu_twin_starts` works with mxData
* IMPROVED: `xmu_make_top_twin` works with mxData
* IMPROVED: `xmu_twin_check` support mxData (was just data.frame input)
* IMPROVED: `umx_long_2_wide`: gains some notes on processing TWINID into famID and twinID
* REPAIR: `umxSummary` handles std=NULL as documented (don't show parameter table)
* RENAME:  `umx_nice_data` --> `umx_make_twin_data_nice` with future improvements planned to cope with non numeric twin schemes.

## umx 3.0.5
* December 2019 R 4.0.0 compatibility
* BUG FIXED: `umx_is_ordered` was broken by previous fix to support the un-tidyverse mucking with dataframes... Now fixed
* BETA: `umxRAM` supports multi-group via `group = "column"
* IMPROVED: `umx_score_scale` now works with binary (0-1) items
* IMPROVED: `umx_time` gains "lap" (report elapsed time without restarting timer)
* IMPROVED: `umx_time` behavior with no input - now prints the current time and date
* IMPROVED: `umxFactorScores` can now `return` SEs
* IMPROVED: `umxFactorScores` allows abbreviated type names
* IMPROVED: `umx_reorder` can operate without verifying data (ignore precision digits in triangles of cov matrices)
* IMPROVED: `residuals` can re-order variables for ease of checking patterns of residuals
* IMPROVED: `umxPath(defn=` Definition vars now auto-named if labels not given closes #107
* IMPROVED: `umxACEv` uses default name "ADEv" (instead of "ACEv") if user sets `dzCr = .25`
* IMPROVED: `umxAPA` only uses italic Beta when std = TRUE
* IMPROVED: `umxRAM` now respects `umx_set_silent`
* CLEANUP: More removal of unused and obscure functions: 
* CLEANUP: `umxRAM2Ordinal` to `xmuRAM2Ordinal`
* CLEANUP: `umx_fix_latents` and `umx_fix_first_loadings` to encourage user to be more explicit in their modeling.
* CLEANUP: Reorganized function groupings to be easier to navigate.


## umx 3.0.0
* August 2019 R 3.6.1 "Action of the Toes"
* This release has major new features in beta including support for lavaan syntax, AND a over 80 additional improvements clean-out/cleanup of old functions and parameters that impede getting learning and using `umx`: Think of it like `ggplot2` version 2.

* ALPHA: `umxRAM` lavaan string syntax support!
* ALPHA: `umxRAM` multi-group models with `group="column"`!
* BETA: `power.ACE.test` and examples. (beta)
* BETA: `umxPower` and examples (beta).
* BETA: `umxRotate` Rotate the factor loadings in `umxCP` models.
* NEW: `umxCP` and `umxIP` can take `data` and create `MZ` and `DZ` datasets.
  * Preparation for expansion to 5-group models.
* NEW: `deg2rad` `rad2deg` utility  functions
* NEW: `FishersMethod` To combine p-values.
* NEW: `oddsratio`.
* NEW: `SE_from_p` helper to get SE from b and p, or get a p from CI.
* NEW: `umxMendelianRandomization` alias to umxTwoStage.
* NEW: `umx_nice_data` converts your twinData to the standard format (zyg = zygosity, "_T1" , "_T2" suffixing of twin variable names).
* NEW: `umx_select_valid` to replace values in one column with those in another, if first column is NA.
* NEW: `umx_set_silent` preference (option) for other functions to listen too and choose how much junk to print to console. Like a global verbose.
* NEW: `xmu_safe_run_summary` can listen to `silent` to turn off summary and progress from models - use when running big simulation loops.
* IMPROVED: `namez` has a better default action (call `names` )
* IMPROVED: `oddsratio` teaches about limitations; supports odds-format as input (closes #102)
* IMPROVED: `plot` can do `pathLabels = "labels"` to show the labels for paths
* IMPROVED: `plot` for `umxIP` supports `means=TRUE`
* IMPROVED: `power.ACE.test` now reports searches with fixed n nicely
* IMPROVED: `umxAPA` now returns its output, rather than printing it. makes for easier consumption in programmatic uses.
* IMPROVED: `umxAPA` supports `cor.test` and `t.test`
* IMPROVED: `umxConfint` uses smart confidence intervals (just the free standardized parameters) for umxCP models.
* IMPROVED: `umxEFA` can suppress printing when `umx_set_silent(TRUE)` #103
* IMPROVED: `umxEFA` now has an option about reporting the summary fit statistics of a model (default is FALSE) see #103
* IMPROVED: `umxGxE` removed border from legend (obscures plot to no benefit)
* IMPROVED: `umxGxE` supports digits (rounding for tables)
* IMPROVED: `umxlav2RAM` catch means
* IMPROVED: `umxPlotCP` can show (non-zero) fixed paths closes #97
* IMPROVED: `umxReduce.GxE` Don't try and drop means moderation.
* IMPROVED: `umxReduceGxE` gains a `tryHard` option
* IMPROVED: `umxReduceGxE` more rational set of reductions - means obey principle of marginality.
* IMPROVED: `umxSummaryGxE` prints parameter table and SEs as well as the interaction plot.
* IMPROVED: `umx_cor` quieter, more informative
* IMPROVED: `umx_is_class` robust to being given a tibble (closes #101 Might need to re-open if there are more cases of where toggling drop from TRUE to FALSE interferes with data types)
* IMPROVED: `umx_make_raw_from_cov` can add names to generated data
* IMPROVED: `umx_make_TwinData` bivariate parameters (e.g., `aMod`) --> `bivAmod` for more clarity (closes #78)
* IMPROVED: `umx_make_TwinData` can now simulate D i.e., A, C, D & E
* IMPROVED: `umx_make_TwinData` More robust
* IMPROVED: `umx_move_file` supports wildcards (closes #83)
* IMPROVED: `umx_rename` examples
* IMPROVED: `umx_rename` reorder parameters, call x-> `data`, deprecate `grep` in favor of `regex` for consistency,
* IMPROVED: `umx_scale_wide_data` gains twins parameter
* IMPROVED: `umx_set_data_variance_check`: set default `minvar` to .1
* IMPROVED: `umx_set_silent` returns old value
* IMPROVED: `umx_var` robustness + support ordinal variables.
* IMPROVED: `xmu_dot_move_ranks` set min, max or same to "" to take these ranks out of the diagram - Aids tricky layouts. (closes #84)
* IMPROVED: `xmu_make_mxData` drop duplicates from manifests list
* IMPROVED: `xmu_make_mxData` handles manipulating a 1-column mxData input
* IMPROVED: `xmu_safe_run_summary` supports digits (rounding for tables)
* CHANGED: `power.ACE.test` now uses semantic labels ("a", "c" ) and can test a dropped after c. Better feedback for boundDiag
* CHANGED: `umx_show` -> `tmx_show`.
* CHANGED: `umx_set_optimization_options` to `umx_set_mvn_optimization_options` for user clarity.
* CHANGED: `umx_default_option` to `xmu_match.arg` as programming aids are moved into xmu space.
* CHANGE: `umx_aggregate` now uses df as default data.
* CHANGED: `showEstimates` -> `show` across all functions: makes learning/usage more consistent.
* CHANGED: `umxGetParameters` doesn't anchor search strings to front of label, nor suffix with digit label.
* DROPPED: `umx_drop_ok` - orphan function - just use `umxCompare`
* DROPPED: `umxEval` - broken anyway - just use `mxEval`
* FIX: `mxPath` `v0m0` , `v.m0` now use labels (if two provided)
* FIXED: `umxACE` standardizes model in summary
* DEPRECATED: The parameter to show standardized parameters is universally `std=TRUE` (`show = ` no longer works).
* REMOVE: `umxIPold`
* NAMESPACE clean up to make it easier for users to focus on functions they (rather than devs) will use
  * umxCovData -> xmu_DF_to_mxData_TypeCov
  * xmu_model_needs_means -> xmu_check_needs_means
  * umx_swap_a_block -> xmu_data_swap_a_block
  * umxDescribeDataWLS -> xmu_describe_data_WLS
  * umx_cov2raw -> umx_make_raw_from_cov
  * umx_make_bin_cont_pair_data -> xmu_make_bin_cont_pair_data
  * umxPadAndPruneForDefVars -> xmu_PadAndPruneForDefVars
* HELP: @md links
* HELP `plot` Better explanation of graphing: can still be improved
* HELP `umx_as_numeric`:better examples
* HELP: `umxMendelianRandomization` nice figure
* HELP: `NEWS` layout improved for R 3.6.0
* HELP: `Rd` doi link, spell-check,
* HELP: `umxEFA` notes it can use formulas, e.g. umxEFA(~v1+v2+v3, data)
* HELP: `NEWS` updated
* HELP: `GxEbiv` Improved help page
* HELP: `umxPower` gains text and a nice figure


## umx 2.15.0
* May 2019, R 3.6.0 "Planting of a Tree"
* **tip**: Try new `plot` `splines` = T|F and `min`= , `max` = `same`=  controls to layout your graphs!
* NEW: `umxLav2RAM`: detect `ngroup`, and a `umxSuperModel` of groups.
* NEW: `plot.MxRAMModel` plots multiple groups (on separate pages)!
* NEW: `plot` gains control of object clustering with `max` `min` and `same`.
* NEW: `plot` select whether to allow splines with `spline=T\F`
* NEW: `plot` opt paths out of layout constraints!
* IMPROVED: `plot` now labels plots with the group name.
* IMPROVED: `umx_time` Handles stop without prior start. New `now` option returns current date.
* FIX: `uxmSummary` the annoying "umxSummary not defined" error should be fixed!
* FIX: `umxSimplex`
* HELP: `umxLav2RAM`: multiple group example in help.
* Minor
  * NEW: `umx_set_separator` allows Europeans to use ; in place of comma. 
  * NEW: `xmu_lavaan_process_group`
  * NEW: `xmu_clean_label`
  * NEW: `xmu_name_from_lavaan_str` helper

## umx 2.10.0
* April 2019, R 3.5.3 "Great Truth"
* CITATION: `citation("umx")` Our paper on umx is out! 
  * Bates, Neale and Maes (2019). umx: twin and path-based structural equation modeling in R. *Twin Research and Human Genetics*. **22**, 27-41. doi: [10.1017/thg.2019.2](https://doi.org/10.1017/thg.2019.2)
* **tip**: Try `?umxExamples` to see the example code from Bates, Neale and Maes (2019).
* NEW: `umxSexLim` multivariate sex-limitation working.
* NEW: `umxSexLim` Gained nonlinear, linear, and homogeneity modes
* NEW: `umxSexLim` Gained examples.
* NEW: `umxRAM` supports WLS, UWLS, DWLS models!
* NEW: `umxACE` supports WLS, UWLS, DWLS models!
* NEW: `umxPlotCP` Supports CIs on the diagram!
* NEW: `umx_make_top_twin_models` increases the robustness and improve-ability of all twin scripts.
* NEW: umx_set_data_variance_check to set the criteria for warning users about excessively small variance or divergence of variance.
* NEW: `xmu_dot_define_shapes` builds the latents and manifest shape definitions (should be xmu_)
* NEW: `xmu_check_variance` checks data for minVar (default > 0.1) and comparable scale for variables (maxVarRatio default = 500)
	* `umx_set_data_variance_check` get/set variance tolerance.
* NEW: `umx` is checked on [travis](https://travis-ci.org/tbates/umx/builds).
* NEW: [github](https://github.com/tbates/umx) has build, doi, etc info boxes! 
* NEW: `plot` allows user control over splines, as well as max, min, same positioning.
* NEW: `xmu_dot_move_ranks`.
* NEW: `xmu_dot_rank_string`.
* CHANGED: default for stripping zeros is reversed for plot.
* CHANGED: `namez` options for collapse get easier to short-cut names.
* DATA: `data(Fischbein_wt)` weight of 66 females record over six 6-month intervals (from Fischbein (1977) weight data).
* BETA: `umxRAM2` Beginning support for lavaan syntax strings.
* BETA: Adding `type` (WLS) added to twin models...
* BETA: Universal weighted rows support for all twin models
* FIXED: `umxValues` to work more reliably with data with no means.
* FIXED: `umx_rename` Didn't give correct values when `test = TRUE` for non-grep searches (drop NAs)
* FIXED: `umxSexLim` match.arg `tryHard`
* FIXED: use of `selVars` vs. selDVs across different functions (search for "# New-style build-block" to see these changes
* IMPROVED: `umxSuperModel` can take a list of models.
* IMPROVED: `tryHard` - allow "yes" for default mxTryHard across all twin and RAM models.
* IMPROVED: `umxPlotCP` revamped - now not dependent on labels! (old function now `umxPlotCPold`)
* IMPROVED: `xmu_model_needs_means`
* IMPROVED: `umxDescribeDataWLS` to check whether the form of WLS requested will have means of not.
* IMPROVED: `umx_make`: use check_win_devel; set working director for release; spelling check
* IMPROVED: `xmu_dot_mat2dot` add `model`, `toLabel` and `fromLabel` + `fromType` & `toType`
* IMPROVED: `xmu_dot_mat2dot` example
* IMPROVED: `xmu_safe_run_summary` now copes with warnings in models, summary, compare etc. (as it should have)
* IMPROVED: `umxModify` now supports find and replace labels with grep (not just find with grep and set free/value)
* IMPROVED: `xmu_assemble_twin_supermodel` no longer take bVector (uses existence of weights instead)
* IMPROVED: `umxModify` now obeys `verbose` option
* IMPROVED: `umx_time` reports % change between model times
* IMPROVED: `umIP` help
* IMPROVED: `umx_make` now supports quick-make as default
* IMPROVED: `tmx_show` printing.
* IMPROVED: `umx_score_scale` help and examples.
* IMPROVED: `umxModify` now supports find and replace labels with grep (not just find with grep and set free/value).
* IMPROVED: `umxModify` now obeys verbose.
* IMPROVED: `umxValues` and elsewhere more robust to new data types.
* IMPROVED: `umx_read_lower` examples and input checking.
* IMPROVED: `xmu_cell_is_on` now has lower and upper triangle with or without diagonal included
* IMPROVED: `xmu_dot_mat2dot` now can get parameter values with CIs
* IMPROVED: `umxRAM` feedback when no data provided
* RENAME: `xmu_make_top_twin` lose "models" for shorter name.
* RENAME: `tmx_show` (was `umx_show` - it is mostly a teaching function
* RENAME: `xmu_dot_define_shapes`
* RENAME: `xmu_dot_rank`
* RENAME: `umx_mat2dot` --> `xmu_dot_mat2dot`
* RENAME: `umx_APA_model_CI` --> `xmu_get_CI`
* REMOVED: `umx_add_variances` old function not used and not very safe.
* CLEANUP: Removed `thresholds` option from several functions (always doing deviation-based, WLS isn't thresholds, and left-censored will be implemented differently)


## umx 2.9.9
* December 2018, R 3.5.2
* Compatibility with OpenMx changes.
* NEW: `umxSummarizeTwinData` to create summary tables for papers using twin Data.
* IMPROVED: `umxRAM`: support definition variables to some extent in umxRAM.
* IMPROVED: `umxMatrix` advice user when they specify `umxMatrix("me", 1,1)`.
* IMPROVED: `umxModify` nicer free-parameter report as default output with no changes requested (calls parameters).
* IMPROVED: `umxRAM` preserve definition variables in data.
* IMPROVED: `plot` strip_zero more reliable.
* INCOMPATIBLE: `plot` dropped deprecated showMeans and showFixed from plot after 3 years (use `means=` and `fixed =` ).
* INCOMPATIBLE: `umx_scale_wide_twin_data` dropped support for deprecated suffix parameter.

## umx 2.9.0
* December 2018, R 3.5.1: "More love"
* INCOMPATIBLE: Support for `suffix` as a synonym for `sep` removed after 3-years of deprecation warnings.
* FIXED: `umxSummary`for `ACEv` models: CI now works - (was 'a' not 'A').
* NEW: `xmu_safe_run_summary` Runs a model safely, optionally summarizing and comparing it to a base model.
* IMPROVED: All models now support `tryHard` as a parameter!!
* IMPROVED: `autoRun` option in more places.
* IMPROVED: `plot` functions now have `strip_zero` option (default = strip leading zeroes from parameter estimates)
* IMPROVED: `umx_standardize_ACEv` now working!
* IMPROVED: Models use `xmu_safe_run_summary` so even bad models are returned for the user to diagnose them.
* IMPROVED: OmniGraffle stencil improved. Use to draw diagrams.
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
* MODIFY: `xmu_safe_run_summary` don't return bVector (already available)
* HELP: `umxACE` help diagrams improved with clearer ACE_matrix picture.
* HELP: `umxRAM` help gained an example of sketch-mode and diagram.
* HELP: `umx_scale` help file improved
* HELP: new pdfs for images in pdf help; remove _ from image names
* HELP: Examples cleaned up


## umx 2.8.5
* October 2018, R 3.5.1: "More love"
* NEW: `xmu_make_mxData` function to make mxData functions out of dataframes (and also drop variables from cov/cor dataframes.
* NEW: SE style confidence information for CP and IP `plot`s.
* IMPROVED: `umxCompare` gains ability to output Weight AIC conditional model probabilities! h/t @mNivard
* IMPROVED: `umxReduce` returns AIC weight-based probability of being best model.
* IMPROVED `umxReduce.MxModelGxE` returns best model (invisibly).
* IMPROVED: `umxReduce` for GxE, don't run no-A and no-C models with moderation in place.
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

## umx 2.8.2
* June 2018, R 3.5.0: Simplex or s**t sticks
* FIXED: `umxSexLim` now works for univariate tests (thanks to Michael Zakharin for reporting!)
* FIXED: `umxSummary` Corrected Chi^2 df for RAM models.
* NEW: `umxCP` handles ordinal, continuous and binary data
  * Note: old version still available as `umxCPold`
  * Note: new version also lower-bounds specifics at 0 by default.
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

## umx 2.6.5
* May 2018, R 3.5.0: "Poly gets a cracker"
* NEW: Beta versions of `umx_polychoric`, `umx_polypairwise`, and `umx_polytriowise`
* NEW: `umxSimplex`twin model!
  * `umxSummary` for simplex
  * `plot support for simplex
  * `iqdat`longitudinal IQ twin data for simplex modeling
* NEW: `namez` alias for the ever-useful `umx_names` function.
* NEW: `umxAlgebra` (just allows name first).
* FIX: `install.OpenMx` URLs for NPSOL and travis builds of OpenMx
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
* INTERNAL: `xmu_simplex_corner` helper function for simplex modeling.
* INTERNAL: `umx_make_sql` improved 

## umx 2.4.0 Post-Boulder Workshop CRAN edition
* March 21 2018, R 3.4.4
* NEW: `umxIP` nFac now allows specifying different numbers of a, c, and e factors!
* NEW: `umxACEv` plot works (shows correlated factors).
* NEW: `umxACEv` standardize mostly functional.
* NEW: `umxEFA` can now take a formula to select variables.
* IMPROVED: `umxIP` and CP examples improved (3-variables, GFF data).
* IMPROVED: `umxIP` *reporting* of `as`, `cs`, `es` simplified into compact stack.
* IMPROVED: `umxCP` *reporting* of `as`, `cs`, `es` simplified into compact stack.
* IMPROVED: `umxPlotCP` labeling of variables improved (catching more _Tn).
* IMPROVED: `umxEFA` reports factor correlation matrix (thanks @ConorDolan)!
* IMPROVED: `umxEFA` can return a loadings object.
* IMPROVED: `umx_make`not supports spelling check.
* IMPROVED: `umxACEv` diagonal now unbounded by default (which is it's whole purpose)
* IMPROVED: `umxACEv` got major improvements to help file.
* IMPROVED: `umxSummaryCP` got ability to print to html.
* IMPROVED: `plot` made more robust to unexpected labels (now that `umxModify` can write `newlabels`)!
* MINOR: `umx_names` Added GFF examples.
* DROPPED: deleted deprecated showStd and showMeans parameters from plot ACE.
* BUG-FIX: `umxSummaryIP` was assuming nFac = 1

## umx 2.0.2
* Feb 12 2018, R 3.4.3 Higher goals edition
* NEW: `umxSummary` works with multi-group models!
	* (make these with `umxSuperModel`)
* NEW: `umxModify` supports multiple find strings and replace strings!
* IMPROVED: `umxAPA` supports `glm`, more robust input checking.
* FIXED: `umxEquate` allows vectors of labels.
* DOCS: `umxGetParameters` has better help.
* DOCS: `umxHetCor` docs now note is will return a pos-def version if given a cov matrix. 
* DOCS: `umxAPA` now has t-test and `glm` examples
* DROPPED: `umx_install_OpenMx` (use `install.OpenMx`)

## umx 2.0.0
* January 29 2018, R 3.4.3 Birthday edition
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

## umx 1.9.2
* December 2017, R 3.4.2
* *TIP*: Filter paths by estimated size with `umx_parameters(model, "below", .1)`
* NEW:  `tmx_genotypic_effect` Part of a suite of teaching functions for biometric genetics!

## umx 1.9.1
* November 2017, R 3.4.2
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
* IMPROVED: `umx_names`: added a find and replace option for names
* IMPROVED: `umxRAM` in "sketch mode" now plots models automatically.
* IMPROVED: `install.OpenMx` from University of Virginia, travis, or open travis build page.
* IMPROVED: `umx_cor` automatically drops non-numeric columns.
* IMPROVED: `umxCI` now supports setting interval and one- or two-sided type (h/t @Conor Dolan).
* INTERNAL: Switch from `R2HTML` to `xtable`
* MINOR: `umxParameters` now supports non-run models

## umx 1.8.0
* September 2017, R 3.4.1 "Single Candle"
* *TIP*: `?umxAPA` can format lots of things for you: from dataframes to p-values - try it out! Let me know what you'd like.
* NEW `?umx_parameters` function, report parameter estimates, filtering by name and value!
	* aliased as `umxParameters`
	* `umx_parameters(model, "below", .1, "loading")`
* NEW `?umx_long2wide`: Merges long data on famID, for an unlimited number of individuals in a family (twinIDs).
* NEW `?umx_wide2long`: Takes a wide df (currently limited to 2 per family), & returns a long-format version.
* FIXED: Removed superfluous print call from `umx_MakeTwinData`.
* IMPROVED: html tables by switching to `xtable` (`R2HTML` is abandoned, strips decimals from AIC, etc.)
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

## umx 1.7.5
* April 04 2017, R 3.3.3 "Another Canoe"
* *TIP*: In twin models, mzData and dzData can contain unused variables (umx removes them for you!)
* NEW: `umxVersion` (returns `mxVersion` + information on umx).
* IMPROVED: `umxACE` start values much better for univariate models.
* NEW: `umx_make_TwinData`
	* Use variance input
	* Option to set seed
	* Set`varName`
	* `nThresh` to threshold the created variables, and return as mxFactors
	* Surface the `empirical` parameter of mvrnorm
	* More flexible: Leave out any one of A, C, or E.
	* omit `nDZPairs` to get equal numbers
	* Option to allow A+C+E ≠ 1
	* Full set of examples
	* NOTE: `umx_make_TwinData` uses **variance** input. This was ambiguous previously.
	* Docs much improved.
* NEW: `install.OpenMx` can install UVA parallel, travis latest (on mac) or opening the travis list.
	* BUG-FIX: `install.OpenMx` had a broken URL, now corrected.
* NEW: `umxRAM` can now be used in "sketch" mode, to plot demos without data: just add the list of manifests as a character string to `data`.
	```splus
	m1 <- umxRAM("test", data = paste0("itemC", 1:4),
		umxPath(unique.bivariate = paste0("itemC", 1:4)),
		umxPath(var="X")
	)
	plot(m1)
	```
* NEW: `umx_cont_2_quantiles` now allows returning cut points, better examples, level_names
	* BUG-FIX: `umx_cont_2_quantiles` lowest threshold was empty
* NEW: `umxAPA` now reports mode for factor data
* NEW: Allow `umxLabel` to rename the returned model
* IMPROVED: `sep` is now the preferred separator (synonym for suffix in umxACE, umxCP, umxIP)
* DOCUMENT: Examples use zygosity string from twinData instead of numeric code
* DOCUMENT: Fix typo in `umxPath` docs
* INCOMPATIBLE: renamed `umx_cov_diag` to umx_var

## umx 1.5.5
* March 20 2017, R 3.3.3 "Another Canoe"
* NEW: `umx_write_to_clipboard`
* NEW: `umx_r_test`
* IMPROVED: Add autoRun option to umxModify.
* FIXED: Switch away from CSOLNP in umxCP example.
* ROLLBACK: No longer generically alter CSOLNP tolerance.

## umx 1.5.1
* March 05 2017 R 3.3.2
* IMPROVED: `umxCP` example for improved compatibility with OpenMx 2.7
* IMPROVED: umx twin models now set a better (less likely to fail) precision for CSOLNP
* IMPROVED: improved help files; document forms parameter for `umxPath`
* IMPROVED: handling list inputs
* IMPROVED: behavior of `confint`()
* IMPROVED: formatting of `umxAPA` table output
* ADD: "check" option to `umx_make`
* FIXED: `umxPath(a, forms= , arrows = 2)` bug

## umx 1.5.0
* December 2016
* Features + OpenMx 1.7.3 compatibility release
* IMPROVED: `umxRAM` returns invisibly
* IMPROVED: `umx_aggregate` can summarize multiple DVs in a table
* IMPROVED: `umx_aggregate` can return a formatted table (kable = TRUE)
* IMPROVED: `umxAPA` returns lower-triangle of data.frame correlations (by default)
* REMOVED: `umxSE` now included in OpenMx 2.7.0!
* FIXED: `umx_set_plot_format` uses silent = TRUE internally where needed

## umx 1.4.9
* December 2016
* Bug fix
* FIXED: `umx_make_TwinData` fix bug in DZ moderator code and A+C computation
* IMPROVED: more functionality hints in man page titles

## umx 1.4.5
* November 2016 "Usability is queen/Sincere Pumpkin Patch"
* IMPROVED: Reorganize function families to increase clarity
* IMPROVED: `umx_set` functions now show legal options by default
* NEW: `umx_set` functions have silent option
* FIX secure http URLs

## umx 1.4.0
* Mid October 2016 "New features and improvements"
* BUG FIX: `umxFitIndices` RMR and SRMR (h/t Brenton Wiernik!)
* NEW: `umxMatrix` with default labels and name as 1st parameter 
* NEW: Definition variable umxPath type, e.g.
	* `umxPath(defn = "def", labels = "data.age")`
* IMPROVED: More concise feedback
* IMPROVED: `umxEquate` supports autoRun and compare
* IMPROVED: `umx_names` can, as a convenience, take string vectors
* RENAME: `umxGetOpenMx` aliased to `install.OpenMx` or `umx_install_OpenMx`
* IMPROVED: `umxFitIndices` additional fit-indices and now automatically computes reference models if needed.
* IMPROVED: `umxRAM` remove_unused_manifests no longer updates manifestVars + leaves variable in data by default.
* RENAME: `plot` "showFixed" deprecated in favor of easier to type "fixed"

## umx 1.3.0
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

## umx 1.2.8
* R 3.3.0 companion: "Very compatible" May 2016
* NEW: `umxRAM` can now build and run Joint Continuous Ordinal models!!
* NEW: `umx_show_options` function to help users learn the options available
* BETA: `umxACE` can now implement a form of left-censoring.
* BETA: `umxThresholdMatrix` can now implement a form of left-censoring.
* IMPROVE: `umxRAM` & `umxRAM2Ordinal` can turn off reference models computation with `refModels = FALSE`;
* IMPROVE: `umx_show` can show thresholds
* IMPROVED: `umx_is_MxModel` can take a list: `listOK` option
* IMPROVED: `umx_check_parallel` options for row-wise parallel and number of subjects
* CHANGE: I changed "suffix" to "sep" in several places for consistency: this will break
  some low level code: nearly all of it mine, and fixed. But might affect some users!!
* UPDATE: `umx_time` and `umx_check_parallel` to work with new S4 list deprecation
* MINOR: `umxTwoStage` shorter run-time on 2-stage least squares 

## umx 1.2.7
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
* CHANGE: `umx_residualize` ditto.
* UPDATE: "Read me" file.

## umx 1.2.5
* April 2016 R 3.2.5 "Very, Very Secure Dishes"
* Bug fix release
* FIX: $models bug in `umxStandardizeACE`
* EXPAND: test suite

## umx 1.2.0
* April 2016 R 3.2.4 "Very Secure Dishes"
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

## umx 1.1.5
* March 2016 Boulder Workshop feedback updates
* NEW: `plot` now works on Windows and Unix as well as OS X! (h/t @mhunter)
* CHANGE: `umxReRun` to `umxModify` (h/t @hmaes)
* BETTER: compatibility with OpenMx 2.5.2

## umx 1.1.1
* Feb 2016 R 3.2.3 Wooden Christmas Tree edition
* NEW: auto-run now the default for all models!
	* Added option to switch this: options("umx_auto_run" = TRUE)
* NEW: `plot` should generate pdfs for Windows and Unix users now
* NEW: `umxACEcov` support for ACE with covariates
* NEW: `umx_make_TwinData` added to support simulation studies
* BETTER: `umx_check_parallel` now returns timing information
* BETTER: `umx_time` now supports "start" and "stop")
* BETTER: `umxAPA` can format more objects in APA style, inc just a p-value)
* BETTER: `summaryAPA` can now summarize a data.frame with mean, sd, and correlations
* BETTER: `umxLabel` now preserves existing labels in umxRAM models: labels = is now saved
* BETTER: Help improvements (wording, see-also etc)
* DEEP: Removed dependency on formula.tools

## umx 1.1.0
* December 2015, R 3.2.3 "Wooden Christmas-Tree"
* NEW: `vcov`() added
* NEW: Begin support for sex-lim twin models
* NEW: New data file for sex-limitation
* IMPROVED: `umxRAM` can take existing model as input (very useful)
* IMPROVED: `umxRAM` reports model fit and comparison (if old model exists)
* IMPROVED: `umxPath` now `lbounds` var@0 for v.m.
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

## umx 1.0.0
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

## umx 0.50.0
* March 2015 R 3.1.3, "Smooth Sidewalk" 
* First CRAN release!

## umx 0.49.0
* IMPROVE Edge cases work
* IMPROVE Improving help files and feedback
* IMPROVE Rationalizing function names into @families

## umx 0.45-beta
* Feature complete
* Bug squashing
* Optimizing function names for memorability and typing

## umx 0.40-beta
* Beta level!
* Standard `confint`(); `plot`() etc functions implemented
* Adding umxRAM() and umxPath shortcuts

## umx 0.30-alpha
* Alpha release
* 90% feature complete
* Lots of name rationalization

## umx 0.25-alpha
* Alpha release
* 80% feature complete
* Function prefix reflects usage:: camel-case for major functions, underscore for utility functions, "xmu" prefix, "not for end-user"