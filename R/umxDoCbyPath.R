#' Make a twin model from the model describing just one person
#'
#' @description
#' `xmu_path2twin` takes a collection of paths describing the model for 1 person
#' and returns a completed twin model. This consists of a [umxSuperModel()] containing
#' `MZ` and `DZ` [umxRAM()] models.
#'
#' Pass into `umxTwinMaker`:
#'
#' 1. A list of `paths` making up the twin 1 model
#' 2. In `t1_t2links`, a vector describing the component realtionships connecting twin 1 to twin2 (The default here is 1 and .5 for the a, and, 
#' 	for c and e are 1 and 0 in both groups, respectively.
#'
#' *Details*
#'
#' Some rules. All labels are expanded with a twin suffix: so "var1" -> "var1_T1" etc. so you
#' provide the person-model using just the base name (and tell umxTwinMaker how to expand it by providing a separator string).
#' 
#' Rule 2: The latent a, c, and e latent variables must be labelled to match the base name given in t1_t2links.
#' To avoid clashes, variables must not match the numbered variables in `t1_t2links`  - by default names like "a1" are reserved for ace.
#' 
#' @param name The name for the [superModel()] (Default "m1")
#' @param paths A vector of [umxPath()]s describing one person
#' @param t1_t2links base name (and values) of paths that covary between T1 and T2. Default: c('a'=c(1,.5), 'c'=c(1,1), 'e'=c(0,0))
#' @param mzData Data for MZ twins
#' @param dzData Data for DZ twins
#' @param sep The separator used to create twin 1 and 2 names (Default "_T")
#' @return - [umxSuperModel()]
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxRAM()], [umxSuperModel()]
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' # ================
#' # = An ACE model =
#' # ================
#' # Make the built-in twin data into a nice parsable set of names (with sep = "_T")
#' tmp= umx_make_twin_data_nice(data=twinData, sep="", zygosity="zygosity", numbering=1:2)
#' mzData = subset(tmp, zyg == 1)
#' dzData = subset(tmp, zyg == 3)
#'
#' twin1PathList = c(
#'	umxPath(v1m0 = c("a1", 'c1', "e1")),
#'	umxPath(c("a1", 'c1', "e1"), to = "wt", values=.2)
#')
#' tmp= umxTwinMaker(paths= twin1PathList, mzData = mzData, dzData= dzData, sep = "")
#' plot(tmp, means=FALSE)
#'
#' latents = paste0(rep(c("a", "c", "e"), each = 2), 1:2)
#' biv = c(
#'	umxPath(v1m0 = latents),
#'	umxPath(mean = c("wt", "ht")),
#'	umxPath(fromEach = c("a1", 'c1', "e1"), to = c("ht", "wt")),
#'	umxPath(c("a2", 'c2', "e2"), to = "wt")
#')
#' tmp= umxTwinMaker(paths= biv, mzData = mzData, dzData= dzData, sep = "")
#' plot(tmp, means=FALSE)
#'
umxTwinMaker <- function(name = "m1", paths, t1_t2links = list('a'=c(1, .5), 'c'=c(1, 1), 'e'=c(0, 0)), mzData = NULL, dzData= NULL, sep = "_T"){
	# TODO 
	# Ensure labels that might need equating of freeing across MZ/DZ or T1 T2
	# Check no manifests match the t1_t2links?
	# Check t1_t2links looks sane (list of 2-number objects)

	# Check all paths are paths
	for (i in length(paths)) {
		if(!class(paths[[i]]) == "MxPath"){
			stop("all elements of paths must be a umxPath. Item ", i, "seems to be class ", 
				omxQuotes(class(paths[[i]]) )
			)
		}
	}
	# 1. make twin 1 twin 2 versions of the paths
	Twin1Paths = xmu_path2twin(paths = paths, thisTwin = 1, sep = sep)	
	Twin2Paths = xmu_path2twin(paths = paths, thisTwin = 2, sep = sep)	

	# 2. Make MZ and DZ models with these paths and the right data
	MZ = umxRAM("MZ", c(Twin1Paths, Twin2Paths), data = mzData, autoRun=FALSE)
	DZ = umxRAM("DZ", c(Twin1Paths, Twin2Paths), data = dzData, autoRun=FALSE)

	# 3. Add a/c/e twin-covariance path(s)
	varNames = dimnames(MZ$S$values)[[1]]
	for (varComponent in names(t1_t2links)) {
		# varComponent = names(t1_t2links)[1]
		# TODO be smarter in here: use namez to pull what we will need: 
		thisSearch = paste0("^", varComponent, "[0-9]+",sep, "1$")
		allPathsOfComponent = namez(varNames, thisSearch)
		# strip a/c/e (varComponent) and suffix
		allPathsOfComponent = namez(allPathsOfComponent, pattern = paste0("^", varComponent, "([0-9]+)", sep, "[0-9]$"), replacement= "\\1")
		allPathsOfComponent = as.numeric(allPathsOfComponent)
		# parse to build i list,
		# iterate over it
		MZvalue = t1_t2links[[varComponent]][1]
		DZvalue = t1_t2links[[varComponent]][2]
		for (i in allPathsOfComponent) {
			T1 = paste0(varComponent, i, sep, "1")
			T2 = paste0(varComponent, i, sep, "2")
			MZ = mxModel(MZ, umxPath(T1, with = T2, free=FALSE, values = MZvalue))
			DZ = mxModel(DZ, umxPath(T1, with = T2, free=FALSE, values = DZvalue))
		}		
	}
	m1 = umxSuperModel(name, MZ, DZ)
	# 4. Equate means in auto-added means model
	# TODO: check bug where one is in manifestVars?
}

#' Re-name variables in umxPaths to twin versions
#'
#' @description
#' `xmu_path2twin` takes a collectionn of paths that use base variable names, 
#' and returns a model with twin names.
#'
#' @details
#' A path like `a to b` will be returned as `a_T1 to b_T1`.
#' @param paths A collection of paths using base variable names.
#' @param thisTwin The twin we are making (i.e., "_T1", or "_T2")
#' @param sep The separator (default "_T")
#' @return - list of relabeled paths
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxTwinMaker()], [umxRAM()]
#' @md
#' @examples
#' twin1PathList = c(
#'	umxPath(v1m0 = c("a1", 'c1', "e1")),
#'	umxPath(fromEach = c("a1", 'c1', "e1"), to = "NFC3", values=.2)
#')
#' xmu_path2twin(twin1PathList, thisTwin = 2)
#'
xmu_path2twin <- function(paths, thisTwin = 1, sep = "_T"){
	suffix = paste0(sep, thisTwin)
	for (i in 1:length(paths)){
		thisPath = paths[[i]]
		thisPath@from = xmu_path_regex(thisPath$from, "$", suffix)
		thisPath@to   = xmu_path_regex(thisPath$to  , "$", suffix)
		paths[[i]] = thisPath
	}
	paths
}

#' Re-name variables umxPaths to twin versions
#'
#' @description
#' `xmu_path2twin` takes a collection of [umxPath()]s (use base variable names), 
#' and returns a model for both twins (and using the expanded variable names).
#'
#' @details
#' A path like `a to b` will be returned as `a_T1 to b_T1`.
#' @param input vector of path labels
#' @param pattern = pattern to match and replace
#' @param replacement = replacement string
#' @param ignore Labels to ignore (reserved words like "one")
#' @return - renamed paths
#' @export
#' @family xmu internal not for end user
#' @seealso - [xmu_path2twin()], [umxTwinMaker()]
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' twin1PathList = c(
#'	umxPath(v1m0 = c("a1", 'c1', "e1")),
#'	umxPath(fromEach = c("a1", 'c1', "e1"), to = "NFC3", values=.2)
#')
#'
#' xmu_path_regex(c("a", "one", "b"), pattern = "$", replacement = "_T1")
#' # "a_T1" "one"  "b_T1"
xmu_path_regex <- function(input, pattern = NA, replacement = NA, ignore = "one"){
	if(any(is.na(input))){
		return(input)
	}else{
		for (i in 1:length(input)) {
			if(input[i] %in% ignore){
				# nothing to do
			} else {
				input[i] = namez(input[i], pattern = pattern, replacement = replacement)
			}
		}
		return(input)
	}
}

#' Make a direction of causation model based on umxPath statements
#'
#' @description
#' Makes a direction of causation model with [umxPath()] statements
#'
#' @details
#' See also [umxDoC()]
#' @param name = "DoC"
#' @param var1Indicators The indicators of trait 1
#' @param var2Indicators The indicators of trait 2
#' @param mzData The MZ twin dataframe
#' @param dzData The DZ twin dataframe
#' @param sep  (Default "_T")
#' @param causal (Default TRUE)
#' @param autoRun Default: getOption("umx_auto_run")_
#' @param intervals Whether to run intervals (Default FALSE)
#' @param tryHard  Defauly "no" (valid = "yes", "ordinal", "search")
#' @param optimizer Whether to set this for this run (Default no))
#' @return - [A direction of causation model with [umxPath()] statements.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDoC()]
#' @md
#' @examples
#' \dontrun{
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' mzData  = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData  = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#' var1 = paste0("varA", 1:3)
#' var2 = paste0("varB", 1:3)
#' 
#' umxDoCp(var1, var2, mzData= NULL, dzData= NULL, sep = "_T", causal= TRUE)
#' 
#' }
umxDoCp <- function(name = "DoC", var1Indicators, var2Indicators, mzData= NULL, dzData= NULL, sep = "_T", causal= TRUE, autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	# TODO: umxDoC add some name checking to avoid variables like "a1"
	tryHard = match.arg(tryHard)
	umx_check(is.logical(causal), "stop", "causal must be TRUE or FALSE")
	if(name == "DoC"){ name = ifelse(TRUE, "DoC", "Chol") }

	nSib    = 2
	nLat1   = length(var1Indicators) # measures for factor 1
	nLat2   = length(var2Indicators)
	nVar    = nLat1 + nLat2
	selVars = tvars(c(var1Indicators, var2Indicators), sep= sep)
	xmu_twin_check(selDVs= c(var1Indicators,var2Indicators), sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)
	mzData = xmu_make_mxData(mzData, manifests = selVars)
	dzData = xmu_make_mxData(dzData, manifests = selVars)

	var1asLats = paste0("var1as", 1:nLat1)
	var1csLats = paste0("var1cs", 1:nLat1)
	var1esLats = paste0("var1es", 1:nLat1)

	var2asLats = paste0("var2as", 1:nLat2)
	var2csLats = paste0("var2cs", 1:nLat2)
	var2esLats = paste0("var2es", 1:nLat2)
	
	# 1. append "_" to manifest names to tag them as things to be twin named: var1 _T1

	twin1PathList = list(
		# 1. ✓ Make latent ace and l1 &l2
		umxPath(v.m0 = c("a1", "a2"), values=.2), # free
		umxPath(v.m0 = c("c1", "c2"), values=.2), # free
		umxPath(v1m0 = c("e1", "e2")), # @1 to scale trait latents
		umxPath(v.m0 = c("l1", "l2")), # @ free

		# 2. ✓ Make ace specifics
		umxPath(v1m0 = var1asLats, var1csLats, var1esLats),
		umxPath(v1m0 = var2asLats, var2csLats, var2esLats),

		# 3. ✓ Add ace paths to each latent
		umxPath(c("a1", "c1", "e1"), to = "l1"),
		umxPath(c("a2", "c2", "e2"), to = "l2"),

		# 4. ✓ Make factor loadings from l1 and l2 onto manifests
		umxPath("l1", to = var1Indicators),
		umxPath("l2", to = var2Indicators),

		# 5. ✓ Load var1 ace specifics onto indicators
		umxPath(var1asLats, to = var1Indicators, values = .2),
		umxPath(var1csLats, to = var1Indicators, values = .2),
		umxPath(var1esLats, to = var1Indicators, values = .2),

		# 6. ✓ Load var2 ace specifics onto indicators
		umxPath(var2asLats, to = var2Indicators, values = .2),
		umxPath(var2csLats, to = var2Indicators, values = .2),
		umxPath(var2esLats, to = var2Indicators, values = .2),

		# 7. Generate the causal beta paths
		umxPath("l1", to = "l2", free=FALSE, labels = "a2b", values= 0),
		umxPath("l2", to = "l1", free=FALSE, labels = "b2a", values= 0)
	)

	# umxTwinMaker(T1Paths= twin1PathList, mzData= NULL, dzData= NULL, sep = "_T")
	
	MZ = mxModel("MZ",mzData,
		# 1. ✓ Make latent ace and l1 &l2
		umxPath(v.m0 = c("a1", "a2"), values=.2), # free
		umxPath(v.m0 = c("c1", "c2"), values=.2), # free
		umxPath(v1m0 = c("e1", "e2")), # @1 to scale trait latents
		umxPath(v.m0 = c("l1", "l2")), # free

		# 2. ✓ Make ace specifics
		umxPath(v1m0 = var1asLats, var1csLats, var1esLats),
		umxPath(v1m0 = var2asLats, var2csLats, var2esLats),

		# 3. ✓ Add ace paths to each latent
		umxPath(c("a1", "c1", "e1"), to = "l1"),
		umxPath(c("a2", "c2", "e2"), to = "l2"),

		# 4. ✓ Make factor loadings from l1 and l2 onto manifests
		umxPath("l1", to = var1Indicators),
		umxPath("l2", to = var2Indicators),

		# 5. ✓ Load var1 ace specifics onto indicators
		umxPath(var1asLats, to = var1Indicators, values = .2),
		umxPath(var1csLats, to = var1Indicators, values = .2),
		umxPath(var1esLats, to = var1Indicators, values = .2),

		# 6. ✓ Load var2 ace specifics onto indicators
		umxPath(var2asLats, to = var2Indicators, values = .2),
		umxPath(var2csLats, to = var2Indicators, values = .2),
		umxPath(var2esLats, to = var2Indicators, values = .2),

		# 7. Generate the causal beta paths
		umxPath("l1", to = "l2", free=FALSE, labels = "a2b", values= 0),
		umxPath("l2", to = "l1", free=FALSE, labels = "b2a", values= 0)	
		
		# TODO: Add twin 2...
		# TODO: add A<->A covariance
		# TODO: add C<->C covariance
	)

	# model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")) )

}