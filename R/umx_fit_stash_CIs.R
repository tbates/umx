#' Stash the CI values of a model as strings in the values of the model
#'
#' @description
#' Stash formatted CIs (e.g. ".1 [-.1, .3]") as strings overwriting the parameter values of the model.
#'
#' @details I might change this to a lookup-function that gets a CI string if one exists.
#'
#' @param model An \code{\link{mxModel}} to get CIs from.
#' @param digits rounding.
#' @param dropZeros makes strings for failed CIs?
#' @param stdAlg2mat treat std as algebra: stash in non std matrix.
#' @return - \code{\link{mxModel}}
#' @export
#' @family zAdvanced Helpers
#' @seealso - \code{\link{umxConfint}}, \code{\link{xmu_get_CI}}
#' @references - \url{https://github.com/tbates/umx}
#' @md
umx_stash_CIs <- function(model, digits = 3, dropZeros = FALSE, stdAlg2mat = TRUE) {
	# model = cp3h
	# TODO rationalize with xmu_get_CI
	if(!umx_has_CIs(model, "output")) {
		if(umx_has_CIs(model, "intervals")){
			stop("Please run the intervals first: mxConfint(... run= TRUE)")
		} else {
			stop("Please add and run some intervals using ?mxConfint()")
		}
	}
	# Get lower and upper CIs as a dataframe
	CIlist = data.frame(model$output$confidenceIntervals)
	CIlist$name = row.names(CIlist)
	if(dropZeros){
		# Drop rows fixed to zero
		CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0), ]
		# Discard rows named NA?
		CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
	}

	# Iterate over each CI
	nCIs = dim(CIlist)[1]
	for(n in 1:nCIs) { # n = 4
		thisCI = CIlist[n, ]
			# lbound    estimate  ubound     name
			# 0.4473444 0.5850266 0.6878915 top.a_cp[1,1]
		CIname = thisCI$name # CIname = "top.a_cp[1,1]" ; CIname = "cp_loadings_r1c1"

		# Make a CI report string "x[a,b]"
		CIparts  = umx_round(thisCI, digits)
		CIString = paste0(CIparts["estimate"], "[",CIparts["lbound"], ",", CIparts["ubound"], "]")
		if(umx_has_square_brackets(CIname)) {
			# Break out bracket address
			# TODO make robust to missing elements, like submodel
			# (was hard coded as top, but might not exist...)
			thisSub = sub("(.*)\\.([^\\.]*)\\[.*", replacement = "\\1", x = CIname) # (model).
			thisMat = sub(".*\\.([^\\.]*)\\[.*"  , replacement = "\\1", x = CIname) # .matrix[
			thisRow = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = CIname))
			thisCol = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = CIname))
		}else{
			# Convert CI label to bracket address
			tmp = omxLocateParameters(model, labels = CIname)
			#              label model      matrix row col     value lbound ubound
			# 1 cp_loadings_r1c1   top cp_loadings   1   1 0.5057673     NA     NA
			thisSub = tmp[1, "model"]
			thisMat = tmp[1, "matrix"]
			thisRow = tmp[1, "row"]
			thisCol = tmp[1, "col"]
			#' CIname = top.cp_loadings_std[1,1]
			#' thisSub = top; thisMat = cp_loadings_std; thisRow = 1; thisCol = 1
		}
		# umx_msg(CIname); # umx_msg(thisSub); umx_msg(thisMat); umx_msg(thisRow); umx_msg(thisCol)
		# thisMat = "cp_loadings_std"
		if(stdAlg2mat && sub(".*_(std)$", replacement = "\\1", x = thisMat)=="std"){
			# Assume _std is an algebra
			baseMat = sub("(.*)_std$", replacement = "\\1", x = thisMat)
			model@submodels[thisSub][[1]]@matrices[baseMat][[1]]$values[thisRow, thisCol] = CIString
		} else {
			model@submodels[thisSub][[1]]@matrices[thisMat][[1]]$values[thisRow, thisCol] = CIString
		}

		# model@submodels["top"][[1]]@matrices["a_cp"][[1]]$values[1, 1]
		# model@submodels[thisSub][[1]]@matrices[thisMat][[1]]$values[thisRow, thisCol] = CIString
		# model@submodels$top@matrices[thisMat][[1]]$values[thisRow, thisCol] = CIString
	}
	return(model)
}
