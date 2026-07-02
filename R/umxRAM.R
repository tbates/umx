#' Catches users typing umxModel instead of umxRAM.
#'
#' @description
#' Catches a common typo, moving from mxModel to umx.
#'
#' @param ... Anything. We're just going to throw an error.
#' @return None
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxRAM()], [OpenMx::mxModel()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>

#' @examples
#' \dontrun{
#' umxModel()
#' }
umxModel <- function(...) {
	stop("You probably meant umxRAM?, not umxModel?")
}

#' Build and run path-based SEM models
#'
#' @description
#' `umxRAM` expedites creation of structural equation models, still without doing invisible things to the model. It 
#' supports [umxPath()]. To support cross-language sharing and science learning, `umxRAM` also supports lavaan model strings.
#' 
#' Here's a path example that models miles per gallon (mpg) as a function of weight (wt) and engine displacement (disp)
#' using the widely used `mtcars` data set.
#' 
#' ```
#' m1 = umxRAM("tim", data = mtcars,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(v.m. = c("wt", "disp", "mpg"))
#' )
#' ```
#'
#' As you can see, most of the work is done by [umxPath()]. `umxRAM` wraps these paths up, takes the `data =` input, and 
#' then internally sets up all the labels and start values for the model, runs it, and calls [umxSummary()], and [plot.MxModel()].
#' 
#' Try it, or one of the several models in the examples at the bottom of this page.
#' 
#' A common error is to include data in the main list, a bit like
#' saying `lm(y ~ x + df)` instead of `lm(y ~ x, data = df)`.
#' 
#' **nb**: Because it uses the presence of a variable in the data to detect if a variable is latent or not, `umxRAM` needs data at build time.
#'
#' **String Syntax**
#' 
#' Here is an example using lavaan syntax (for more, see [umxLav2RAM()])
#' 
#' ```R
#' m1 = umxRAM("mpg ~ wt + disp", data = mtcars)
#' ```
#'
#' **Sketch mode**
#'
#' If you are at the "sketching" stage of theory consideration, `umxRAM` supports
#' setting data to a simple vector of manifest names.
#' As usual in `umxRAM`, any variables you refer to that are not in data are treated as latents.
#' 
#' ```R
#' m1 = umxRAM("sketch", data = c("A", "B"),
#' 	umxPath("C", to = c("A", "B"), values=.3),
#' 	umxPath("A", with = "B", values=.45),
#' 	umxPath(v.m. = c("A", "B")),
#' 	umxPath(v1m0 = "C")
#' )
#' plot(m1, means = FALSE)
#' ```
#' Will create this figure:
#' 
#' \if{html}{\figure{sketch.png}{options: alt="Figure: sketch.png"}}
#' \if{latex}{\figure{sketch.pdf}{options: width=7cm}}
#' 
#' @details
#' **WLS**
#' `umxRAM` supports WLS estimation via the `type` argument (`"WLS"`, `"DWLS"`, or `"ULS"`).
#'
#' **Important for ordinal data**: If your data contains ordered factors, `umxRAM` will
#' automatically create the necessary `mxThreshold` objects. You do not need to add them manually.
#'
#' For all-continuous data, use `allContinuousMethod` to control means modeling:
#' - `"cumulants"` (default): Faster. No means model.
#' - `"marginals"`: Includes means and supports missing data.
#' 
#' **Comparison for OpenMx users**
#' 
#' `umxRAM` differs from [OpenMx::mxModel()] in the following ways:
#' 
#' 1. You don't need to set type = "RAM".
#' 2. You don't need to list manifestVars (they are detected from path usage).
#' 3. You don't need to list latentVars (detected as anything in paths but not in \code{mxData}).
#' 4. You don't need to create mxData when you already have a data.frame.
#' 5. You add data with `data = ` (as elsewhere in R, e.g. [lm()]).
#' 6. You don't need to add labels: paths are automatically labelled "a_to_b" etc.
#' 7. You don't need to set start values, they will be done for you.
#' 8. You don't need to `mxRun` the model: it will run automatically, and print a summary.
#' 9. You don't need to run `summary`: with `autoRun=TRUE`, it will print a summary.
#' 10. You get a plot of the model with estimates on the paths, including multiple groups.
#' 11. Less typing: [umxPath()] offers powerful verbs to describe paths.
#' 12. Supports a subset of lavaan string input.
#'
#' **Start values**. Currently, manifest variable means are set to the observed means, 
#' residual variances are set to 80%  of the observed variance of each variable, 
#' and single-headed paths are set to a positive starting value (currently .9).
#' *note*: The start-value strategy is subject to improvement, and will be documented in the help for [umxRAM()].
#' 
#' **Comparison with other software**
#' 
#' Some SEM software does a lot of behind-the-scenes defaulting and path addition. 
#' If you want this, `umxRAM` can read lavaan strings so you can carry on that way.
#' 
#' **WLS, DWLS, and ULS in `umxRAM` vs lavaan**
#'
#' `umxRAM` supports three weighted least squares estimators via the `type` argument.
#' These control which weight matrix is used during estimation:
#'
#' \tabular{llll}{
#' **umx `type`** \tab **Weight matrix** \tab **Closest lavaan equivalent** \tab **Typical use** \cr
#' `"WLS"`        \tab Full weight matrix         \tab `estimator = "WLS"`          \tab Full-information WLS (computationally heavy) \cr
#' `"DWLS"`       \tab Diagonal weight matrix     \tab `estimator = "DWLS"` or the estimation step of `"WLSMV"` \tab Most common for ordinal data \cr
#' `"ULS"`        \tab Identity matrix            \tab `estimator = "ULS"`          \tab Simpler unweighted estimation \cr
#' }
#'
#' **Important notes:**
#' - In lavaan, `WLSMV` combines diagonal weighting (`DWLS`-style estimation) with a mean-and-variance adjusted test statistic.
#'   In `umx`/`OpenMx`, the weighting is controlled by `type`, while robust corrections to the chi-square and standard errors
#'   are handled via its Jacobian, and information matrix.
#' - When using ordinal data, make sure your variables are `ordered` factors. `umxRAM` will then automatically create the required `mxThreshold` objects.
#'
#' @param model A model to update (or set to string to use as name for new model)
#' @param data data for the model. Can be an [OpenMx::mxData()] or a data.frame
#' @param ... umxPaths, mxThreshold objects, etc.
#' @param group (optional) Column name to use for a multi-group model (default = NULL)
#' @param group.equal In multi-group models, what to equate across groups (default = NULL: all free)
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param suffix String to append to each label (useful if model will be used in a multi-group model)
#' @param name A friendly name for the model
#' @param type One of "Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param weight Passes weight values to mxData
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param std Whether to show standardized estimates, raw (NULL print fit only)
#' @param optimizer optionally set the optimizer (default NULL does nothing)
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param setValues Whether to generate likely good start values (Defaults to TRUE)
#' @param refModels pass in reference models if available. Use FALSE to suppress computing these if not provided.
#' @param independent Whether the model is independent (default = NA)
#' @param remove_unused_manifests Whether to remove variables in the data to which no path makes reference (defaults to TRUE)
#' @param verbose Whether to tell the user what latents and manifests were created etc. (Default = FALSE)
#' @param std.lv Whether to auto standardize latent variables when using string syntax (default = FALSE)
#' @param lavaanMode Defaults when building out string syntax default = "sem" (alternative is "lavaan", with very few defaults)
#' @param printTab (for string input, whether to output a table of paths (FALSE)
#' @return - [OpenMx::mxModel()]
#' @export 
#' @seealso [umxPath()], [umxSummary()], [plot()], [parameters()], [umxSuperModel()], [umxLav2RAM()]
#' @family Core Model Building Functions
#' @references - <https://tbates.github.io>, <https://github.com/tbates/umx>

#' @examples
#' \dontrun{
#' 
#' # ============================================
#' # = 1. Here's a simple example with raw data =
#' # ============================================
#' mtcars$litres = mtcars$disp/61.02
#' m1 = umxRAM("tim", data = mtcars,
#' 	umxPath(c("wt", "litres"), to = "mpg"),
#' 	umxPath("wt", with = "litres"),
#' 	umxPath(v.m. = c("wt", "litres", "mpg"))
#' )
#'
#' # 2. Use parameters to see the parameter estimates and labels
#' parameters(m1)
#'
#' # And umxSummary to get standardized parameters, CIs etc from the run model.
#' umxSummary(m1, std=TRUE)
#' # |name           | Std.Estimate| Std.SE|CI                   |
#' # |:--------------|------------:|------:|:--------------------|
#' # |wt_to_mpg      |        -0.54|   0.17|-0.54 [-0.89, -0.2]  |
#' # |disp_to_mpg    |        -0.36|   0.18|-0.36 [-0.71, -0.02] |
#' # |mpg_with_mpg   |         0.22|   0.07|0.22 [0.08, 0.35]    |
#' # |wt_with_wt     |         1.00|   0.00|1 [1, 1]             |
#' # |b1             |         0.89|   0.04|0.89 [0.81, 0.96]    |
#' # |disp_with_disp |         1.00|   0.00|1 [1, 1]             |
#' 
#' # 3. Of course you can plot the model
#' plot(m1)
#' plot(m1, std=TRUE, means=FALSE)
#' plot(m1, std = TRUE, means=FALSE, strip= TRUE, resid = "line")
#'
#' # ===============================================
#' # = lavaan string example (more at ?umxLav2RAM) =
#' # ===============================================
#' m1 = umxRAM(data = mtcars, "#modelName
#'  mpg ~ wt + disp")
#' 
#'
#' # =======================
#' # = A multi-group model =
#' # =======================
#'
#' mtcars$litres = mtcars$disp/61.02
#' m1 = umxRAM("tim", data = mtcars, group = "am",
#' 	umxPath(c("wt", "litres"), to = "mpg"),
#' 	umxPath("wt", with = "litres"),
#' 	umxPath(v.m. = c("wt", "litres", "mpg"))
#' )
#' # In this model, all parameters are free across the two groups.
#'
#' # ====================================
#' # = A cov model, with steps laid out =
#' # ====================================
#'
#' # *note*: The variance of displacement is in cubic inches and is very large.
#' # to help the optimizer, one might, say, multiply disp *.016 to work in litres
#' tmp = mtcars; tmp$disp= tmp$disp *.016
#'
#' # We can just give the raw data and ask for it to be made into type cov:
#' m1 = umxRAM("tim", data = tmp, type="cov",
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(var = c("mpg", "wt", "disp"))
#' )
#'
#' # (see ?umxPath for more nifty options making paths...)
#'
#' # =========================================
#' # = umxRAM can also accept mxData as data =
#' # =========================================
#' # For convenience, list up the manifests you will be using
#' 
#' selVars = c("mpg", "wt", "disp")
#' tmp = mtcars; tmp$disp= tmp$disp *.016
#' myCov = mxData(cov(tmp[, selVars]), type = "cov", numObs = nrow(mtcars) )
#'
#' m1 = umxRAM("tim", data = myCov,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(var = selVars)
#' )
#' 
#' 
#' # =======================
#' # = umxRAM supports WLS =
#' # =======================
#'
#' # 1. Run an all-continuous WLS model
#'  mw = umxRAM("raw", data = mtcars[, c("mpg", "wt", "disp")], 
#'		type = "WLS", allContinuousMethod = "cumulants",
#'  	umxPath(var = c("wt", "disp", "mpg")),
#'  	umxPath(c("wt", "disp"), to = "mpg"),
#'  	umxPath("wt", with = "disp")
#'  )
#' # 2. Switch to marginals to support means in WLS model
#'  mw = umxRAM("raw", data = mtcars[, c("mpg", "wt", "disp")], 
#'		type = "WLS", allContinuousMethod= "marginals",
#'  	umxPath(var = c("wt", "disp", "mpg")),
#'  	umxPath(c("wt", "disp"), to = "mpg"),
#'  	umxPath("wt", with = "disp")
#'  )
#'
#' ##########################
#' # Ordinal-data WLS model #
#' ##########################
#'
#' # 1. Generate Data
#' set.seed(12345)
#' n  = 800
#' f  = rnorm(n)
#' x1 = 0.75 * f + rnorm(n, sd = 0.66)
#' x2 = 0.80 * f + rnorm(n, sd = 0.60)
#' x3 = 0.70 * f + rnorm(n, sd = 0.71)
#' x4 = 0.85 * f + rnorm(n, sd = 0.53)
#' 
#' dat = data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
#' 
#' # Turn into ordered categorical (4 categories)
#' dat[] = lapply(dat, function(x) cut(x, breaks = 4, labels = FALSE))
#' dat[] = lapply(dat, ordered)
#' 
#' # 1. Model in lavaan
#' model = ' f =~ x1 + x2 + x3 + x4 '
#' lav1  = cfa(model, data = dat, ordered = names(dat), estimator = "WLSMV")
#' 
#' # 2. Model in umxRAM
#' # todo: Note if not clear: Data can be raw, or mxData()
#' # todo: implement m.v. as a synonym for v.m.?
#' m1 = umxRAM("WLS_umx", data = dat, type = "WLS", allContinuousMethod = "cumulants",
#'  # or "marginals"
#' 	umxPath("f", to = c("x1", "x2", "x3", "x4") ),
#' 	umxPath(v.m. = "f"),
#' 	umxPath(v.m. = c("x1", "x2", "x3", "x4"))
#' )
#'
#' # ===============================
#' # = Using umxRAM in Sketch mode =
#' # ===============================
#' # No data needed: just list variable names!
#' # Resulting model will be plotted automatically
#' m1 = umxRAM("what does unique pairs do, I wonder", data = c("A", "B", "C"),
#'	   umxPath(unique.pairs = c("A", "B", "C"))
#' )
#' 
#' m1 = umxRAM("ring around the roses", data = c("B", "C"),
#'	  umxPath(fromEach = c("A", "B", "C"))
#' )
#' 
#' m1 = umxRAM("fromEach with to", data = c("B", "C"),
#'	   umxPath(fromEach = c("B", "C"), to= "D")
#' )
#'
#' m1 = umxRAM("CFA_sketch", data = paste0("x", 1:4),
#' 	umxPath("g", to = paste0("x", 1:4)),
#' 	umxPath(var = paste0("x", 1:4)),
#' 	umxPath(v1m0 = "g")
#' )
#'
#' # =================================================
#' # = This is an example of using your own labels:  =
#' #   umxRAM will not over-ride them                =
#' # =================================================
#' m1 = umxRAM("tim", data = mtcars, type="cov",
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath(cov = c("wt", "disp"), labels = "b1"),
#' 	umxPath(var = c("wt", "disp", "mpg"))
#' )
#' omxCheckEquals(m1$S$labels["disp", "wt"], "b1") # label preserved
#' m1$S$labels
#'#      mpg             wt            disp
#'# mpg  "mpg_with_mpg"  "mpg_with_wt" "disp_with_mpg"
#'# wt   "mpg_with_wt"   "wt_with_wt"  "b1"
#'# disp "disp_with_mpg" "b1"          "disp_with_disp"
#' parameters(m1)
#'
#' # ===========
#' # = Weights =
#' # ===========
#' # !!! Not tested !!!
#' mtcars$litres = mtcars$disp/61.02
#' m1 = umxRAM("tim", data = mtcars, weight= "cyl",
#' 	umxPath(c("wt", "litres"), to = "mpg"),
#' 	umxPath("wt", with = "litres"),
#' 	umxPath(v.m. = c("wt", "litres", "mpg"))
#' )
#'
#' }
#'
umxRAM <- function(model = NA, ..., data = NULL, name = NA, group = NULL, group.equal = NULL, suffix = "", comparison = TRUE, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), weight = NULL, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), std = FALSE, refModels = NULL, remove_unused_manifests = TRUE, independent = NA, setValues = TRUE, optimizer = NULL, verbose = FALSE, std.lv = FALSE, lavaanMode = c("sem", "lavaan"), printTab = FALSE) {
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	# Check for data/model objects passed in ... before unlist() flattens them
	for (item in dot.items) {
		thisIs = class(item)[1]
		if (thisIs %in% c("data.frame", "matrix", "MxData")) {
			stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me a ", thisIs, " inside the path list. ",
				 "To include data in umxRAM, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
		} else if (thisIs == "MxModel") {
			stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me an MxModel inside the path list. ",
				 "umxRAM does not support nesting MxModels directly. If you wanted a multi-group model, see ?umxSuperModel.", call. = FALSE)
		}
	}
	dot.items = unlist(dot.items) # In case any dot items are lists of mxPaths, etc...
	type       = match.arg(type)
	tryHard    = match.arg(tryHard)
	lavaanMode = match.arg(lavaanMode)
	allContinuousMethod = match.arg(allContinuousMethod)

	if(!is.null(weight)){
		message("Polite note: Weight feature has not been tested: Models may have spurious fit, consider this feature alpha quality")
	}
	# if data provided check it isn't a tibble
	if(!is.null(data)){
		# avoid ingesting tibbles
		if(inherits(data, "tbl")){
			data = as.data.frame(data)
		}
	}

	# =================
	# = Set optimizer =
	# =================
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	if(!is.null(group)){
		if(!inherits(data, "data.frame")){
			stop("Currently, for multiple groups, data must be a raw data.frame so I can subset it into multiple groups. You gave me a ", omxQuotes(class(data)))
		}
	}

	# lavaan string style model
	if (is.character(model) && grepl(model, pattern = "(<|~|=~|~~|:=)")){
		# Process lavaanString: need to modify so that all the RAM options are processed: 
		# type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
		# show
		# suffix
		# refModels = NULL
		# comparison
		# allContinuousMethod
		# remove_unused_manifests
		model = umxLav2RAM(model = model, data = data, type = type, group = group, group.equal = group.equal, std.lv = std.lv, name = name, 
					lavaanMode = lavaanMode, autoRun = autoRun, tryHard = tryHard, printTab = printTab)
		return(model)
	}


	# umxPath-based model
	if(typeof(model) == "character"){
		if(is.na(name)){
			name = model
		} else {
			stop("If model is set to a string, don't pass in name as well...")
		}
	} else {
		if(umx_is_RAM(model)){
			# message("Updating existing model")
			if(is.na(name)){
				name = model$name
			}
			if(is.null(data)){
				newModel = mxModel(model, dot.items, name = name)
			} else {
				if(umx_is_MxData(data)){
					newModel = mxModel(model, dot.items, data, name = name)
				} else {
					stop("Polite note: I don't know how to convert raw data into mxData to update your model - can you please do that for me and try again?")
				}
			}
			# if(setValues){
			# 	newModel = xmuValues(newModel)
			# }
			newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
			return(newModel)
		} else {
			stop("First item must be either an existing model or a name string. You gave me a ", typeof(model))
		}
	}

	umx_check(!is.null(data), "stop", "In umxRAM, you must set 'data = '. If you're building a model with no data, use mxModel")

	foundNames = c()
	defnNames = c()
	for (thisItem in dot.items) {
		if(!is.list(thisItem)){
			# Sometimes we get a list, so expand everything to a list.
			thisItem = list(thisItem)
		}
		for (i in seq_along(thisItem)) {
			thisIs = class(thisItem[[i]])[1]
			if(thisIs == "MxPath"){
				foundNames = append(foundNames, c(thisItem[[i]]$from, thisItem[[i]]$to))
				tmp = namez(thisItem[[i]]$labels, "data\\.")
				if(length(tmp) > 0){
					defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement= "\\1"))
				}
			} else {
				if(thisIs == "MxThreshold"){
					# MxThreshold detected
				} else if(umx_is_MxMatrix(thisItem[[i]])){
					# matrix labels might refer to definition variables
					tmp = namez(thisItem[[i]]$labels, "data\\.")
					if(length(tmp) > 0){
						defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement= "\\1"))
					}
				} else if (isS4(thisItem[[i]]) && grepl("^Mx", thisIs) && !thisIs %in% c("MxModel", "MxData")) {
					# Valid OpenMx S4 object (MxConstraint, MxAlgebra, MxCI, etc.) - no path/matrix-label actions needed
				} else {
					if (thisIs %in% c("data.frame", "matrix", "MxData")) {
						stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me a ", thisIs, " inside the path list. ",
							 "To include data in umxRAM, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
					} else if (thisIs == "MxModel") {
						stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me an MxModel inside the path list. ",
							 "umxRAM does not support nesting MxModels directly. If you wanted a multi-group model, see ?umxSuperModel.", call. = FALSE)
					} else {
						stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me a ", thisIs, " which is not supported inside the RAM path list.", call. = FALSE)
					}
				}
			}			
		}
	}

	# ============================
	# = All dot.items processed  =
	# ============================

	# ====================================
	# = Find latentVars and manifestVars =
	# ====================================
	# Get names from data (forms pool of potential usedManifests)
	manifestVars = unique(na.omit(umx_names(data)))

	# Omit NAs from found names (empty "to =" can generate these spuriously)
	foundNames = unique(na.omit(foundNames))
	defnNames  = unique(na.omit(defnNames))
	
	if(length(defnNames)>0){
		# check'm if you've got'm
		umx_check_names(defnNames, data = data, message = "note: used as definition variable, but not present in data")
	}
	# Anything else used as a path, but not found in the data (and not a key word like "one") must be a latent
	latentVars = setdiff(foundNames, c(manifestVars, "one"))


	nLatent = length(latentVars)
	# Report which latents were created
	if (!umx_set_silent(silent=TRUE)) {
    	if(nLatent == 0){
			# message("No latent variables were created.\n")
			# latentVars = NA
	    } else if (nLatent == 1){
			message("A latent variable '", latentVars[1], "' was created. ")
	    } else {
      	  message(nLatent, " latent variables were created:", paste(latentVars, collapse = ", "), ". ")
    	}
	}

	# ===========================================================
	# = TODO handle user adding an mxThreshold object to umxRAM =
	# ===========================================================
	# This will be a model where things are not in the data and are not latent...
	
	# ======================================
	# = List up used and un-used Manifests =
	# ======================================
	# Used = all data columns present in found and not reserved, e.g. "one"
	unusedManifests = setdiff(manifestVars, c(foundNames, defnNames))

  # Include weight if it is passed
  if (!is.null(weight)) unusedManifests = setdiff(c(manifestVars, weight), c(foundNames, defnNames))

	if(remove_unused_manifests & length(unusedManifests) > 0){
		usedManifests = setdiff(intersect(manifestVars, foundNames), "one")
    if (!is.null(weight)) {
        myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
            defnNames, verbose = verbose, weight = weight)
    } else {
        myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
            defnNames, verbose = verbose)
    }
	} else {
		# keep everything
		usedManifests = setdiff(manifestVars, defnNames)
		myData = xmu_make_mxData(data= data, type = type, verbose = verbose, manifests = usedManifests, fullCovs = 
            defnNames)
	}
	# ==================
	# = Assemble model =
	# ==================

	newModel = do.call("mxModel", list(name = name, type = "RAM",
		manifestVars = usedManifests,
		latentVars  = latentVars,
		independent = independent, dot.items)
	)
	# ============
	# = Add data =
	# ============
	if (inherits(myData, "character")){
		# User is just running a trial model, with no data, but provided names for sketch mode
		newModel = xmuLabel(newModel, suffix = suffix)
		if(is.null(group)){
			if(autoRun && umx_set_auto_plot(silent = TRUE)){
				plot(newModel)
			}
			return(newModel)
		} else {
			# will be added to a super model, but no data needed/available to subset
		}
	}else{
		newModel = mxModel(newModel, myData)
		# note: if necessary (group), will be re-processed to add the required data below...
	}
	
	# ==========================
	# = Add means if necessary =
	# ==========================
	# Note: WLS data will be mxData(..., type = "raw") at this stage.
	# Add means if data are raw and means not requested by user
	needsMeans = xmu_check_needs_means(data = myData, type = type, allContinuousMethod = allContinuousMethod)
	if(needsMeans && is.null(newModel$matrices$M)){
		message("You have raw data, but no means model. I added\n",
		"mxPath('one', to = manifestVars)")
		newModel = mxModel(newModel, mxPath("one", usedManifests))
	}

	# =========================
	# = Labels and set values =
	# =========================
	suffix = ifelse(is.null(group), yes = suffix, no = paste0(suffix, "_GROUP"))
	newModel = xmuLabel(newModel, suffix = suffix)
	if(setValues){
		newModel = xmuValues(newModel, onlyTouchZeros = TRUE)
	}

	if(any(umx_is_ordered(myData$observed))){
		newModel = xmuRAM2Ordinal(newModel, verbose = TRUE)
	}

	# ==============================
	# = Add mxFitFunction to model =
	# ==============================
	if(type %in%  c('WLS', 'DWLS', 'ULS')) {
		# message("data treated as ", omxQuotes(type), ". allContinuousMethod = ", omxQuotes(allContinuousMethod))
		# Replace newModel fit functions
		# Still mxExpectationNormal and means not affected (either has or lacks means matrix already).
		newModel = mxModel(newModel, mxFitFunctionWLS(type= type, allContinuousMethod = allContinuousMethod) )
	}

	# =====================
	# = Handle group here =
	# =====================
	if(!is.null(group)){
		# 1. Go back to raw data and subset by "group" column
		# 2. Create new mxData,
		# 3. Add data to copy of the model and accumulate in list of models
		# 4. Add list of models to umxSuperModel
		modelList = list()
		groupCol  = data[, group]
		levelsOfGroup = unique(groupCol)
		# already computed above
		# unusedManifests = setdiff(manifestVars, foundNames)
		# usedManifests   = setdiff(intersect(manifestVars, foundNames), "one")
		# usedManifests = manifestVars
		for (thisLevelOfGroup in levelsOfGroup) {
			thisSubset = data[groupCol == thisLevelOfGroup, ]
			if(remove_unused_manifests & length(unusedManifests) > 0){
				myData = xmu_make_mxData(data = thisSubset, type = type, manifests = c(usedManifests, defnNames), verbose = FALSE)
			} else {
				myData = xmu_make_mxData(data= thisSubset, type = type, verbose = FALSE)
			}
			thisModel = mxModel(newModel, myData, name= paste0(name, "_", thisLevelOfGroup))

			if(!is.null(group.equal)){
				message("sorry, haven't implemented group.equal yet")
			}else{
				# replace "_GROUP$" with _thisLevelOfGroup
				thisModel = umxSetParameters(thisModel, regex= "_GROUP$", newlabels= paste0("_", thisLevelOfGroup))
			}

			modelList = c(modelList, thisModel)
		}
		return(umxSuperModel(name = name, modelList, autoRun = autoRun, tryHard = tryHard, std = std))
	}

	newModel = omxAssignFirstParameters(newModel)
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
	invisible(newModel)
}
