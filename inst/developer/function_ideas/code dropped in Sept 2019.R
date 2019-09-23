#' umxDrop1: Unfinished function to mimic drop1 in OpenMx
#'
#' Drops each free parameter (selected via regex), returning an [mxCompare()]
#' table comparing the effects. A great way to quickly determine which of several 
#' parameters can be dropped without excessive cost
#'
#' @param model An [mxModel()] to drop parameters from 
#' @param regex A string to select parameters to drop. leave empty to try all.
#' This is regular expression enabled. i.e., "^a_" will drop parameters beginning with "a_"
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of model comparisons
#' @export
#' @family Modify or Compare Models
#' @references - <https://www.github.com/tbates/umx>
#' @md
#' @examples
#' \dontrun{
#' umxDrop1(fit3) # try dropping each free parameters (default)  
#' # drop "a_r1c1" and "a_r1c2" and see which matters more.
#' umxDrop1(model, regex="a_r1c1|a_r1c2")
#' }
umxDrop1 <- function(model, regex = NULL, maxP = 1) {
	if(is.null(regex)) {
		toDrop = umxGetParameters(model, free = TRUE)
	} else if (length(regex) > 1) {
		toDrop = regex
	} else {
		toDrop = grep(regex, umxGetParameters(model, free = TRUE), value = TRUE, ignore.case = TRUE)
	}
	message("Will drop each of ", length(toDrop), " parameters: ", paste(toDrop, collapse = ", "), ".\nThis might take some time...")
	out = list(rep(NA, length(toDrop)))
	for(i in seq_along(toDrop)){
		tryCatch({
			message("item ", i, " of ", length(toDrop))
        	out[i] = umxModify(model, name = paste0("drop_", toDrop[i]), regex = toDrop[i])
		}, warning = function(w) {
			message("Warning incurred trying to drop ", toDrop[i])
			message(w)
		}, error = function(e) {
			message("Error occurred trying to drop ", toDrop[i])
			message(e)
		})
	}
	out = data.frame(umxCompare(model, out))
	out[out=="NA"] = NA
	suppressWarnings({
		out$p   = as.numeric(out$p) 
		out$AIC = as.numeric(out$AIC)
	})
	n_row = dim(out)[1] # 2 9
	sortedOrder = order(out$p[2:n_row])+1
	out[2:n_row, ] <- out[sortedOrder, ]
	good_rows = out$p < maxP
	good_rows[1] = T
	message(sum(good_rows)-1, "of ", length(out$p)-1, " items were beneath your p-threshold of ", maxP)
	return(out[good_rows, ])
}


#' umxAdd1
#'
#' Add each of a set of paths you provide to the model, returning a table of their effects on fit.
#'
#' @param model an [mxModel()] to alter
#' @param pathList1 a list of variables to generate a set of paths
#' @param pathList2 an optional second list: IF set paths will be from pathList1 to members of this list
#' @param arrows Make paths with one or two arrows
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of fit changes
#' @export
#' @family Modify or Compare Models
#' @references - <https://www.github.com/tbates/umx>
#' @md
#' @examples
#' \dontrun{
#' model = umxAdd1(model)
#' }
umxAdd1 <- function(model, pathList1 = NULL, pathList2 = NULL, arrows = 2, maxP = 1) {
	if ( is.null(model$output) ) stop("Provided model hasn't been run: use mxRun(model) first")
	# stop if there is no output
	if ( length(model$output) < 1 ) stop("Provided model has no output. use mxRun() first!")

	if(arrows == 2){
		if(!is.null(pathList2)){
			a = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			b = xmuMakeTwoHeadedPathsFromPathList(pathList2)
			a_to_b = xmuMakeTwoHeadedPathsFromPathList(c(pathList1, pathList2))
			toAdd = a_to_b[!(a_to_b %in% c(a,b))]
		}else{
			if(is.null(pathList1)){
				stop("best to set pathList1!")
				# toAdd = umxGetParameters(model, free = FALSE)
			} else {
				toAdd = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			}
		}
	} else if(arrows == 1){
		if(is.null(pathList2)){
			stop("pathList2 must not be empty for arrows = 1: it forms the target of each path")
		} else {
			toAdd = xmuMakeOneHeadedPathsFromPathList(pathList1, pathList2)
		}
	}else{
		stop("You idiot :-) : arrows must be either 1 or 2, you tried", arrows)
	}
	# TODO umxAdd1: fix count? or drop giving it?
	message("You gave me ", length(pathList1), "source variables. I made ", length(toAdd), " paths from these.")

	# Just keep the ones that are not already free... (if any)
	toAdd2 = toAdd[toAdd %in% umxGetParameters(model, free = FALSE)]
	if(length(toAdd2) == 0){
		if(length(toAdd[toAdd %in% umxGetParameters(model, free = NA)] == 0)){
			message("I couldn't find any of those paths in this model.",
				"The most common cause of this error is submitting the wrong model")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}else{
			message("I found (at least some) of those paths in this model, but they were already free")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}		
		stop()
	}else{
		toAdd = toAdd2
	}
	message("Of these ", length(toAdd), " were currently fixed, and I will try adding them")
	message(paste(toAdd, collapse = ", "))

	message("This might take some time...")
	flush.console()
	# out = data.frame(Base = "test", ep = 1, AIC = 1.0, p = 1.0); 
	row1Cols = c("Base", "ep", "AIC", "p")
	out = data.frame(umxCompare(model)[1, row1Cols])
	for(i in seq_along(toAdd)){
		# model = fit1 ; toAdd = c("x2_with_x1"); i=1
		message("item ", i, " of ", length(toAdd))
		tmp = omxSetParameters(model, labels = toAdd[i], free = TRUE, values = .01, name = paste0("add_", toAdd[i]))
		tmp = mxRun(tmp)
		mxc = umxCompare(tmp, model)
		newRow = mxc[2, row1Cols]
		newRow$AIC = mxc[1, "AIC"]
		out = rbind(out, newRow)
	}

	out[out=="NA"] = NA
	out$p   = round(as.numeric(out$p), 3)
	out$AIC = round(as.numeric(out$AIC), 3)
	out <- out[order(out$p),]
	if(maxP==1){
		return(out)
	} else {
		good_rows = out$p < maxP
		message(sum(good_rows, na.rm = TRUE), "of ", length(out$p), " items were beneath your p-threshold of ", maxP)
		message(sum(is.na(good_rows)), " was/were NA")
		good_rows[is.na(good_rows)] = T
		return(out[good_rows, ])
	}
}