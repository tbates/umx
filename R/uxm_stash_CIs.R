# Return a copy of the model, with values set to CI strings
uxm_stash_CIs <- function(model, dropZeros = FALSE) {
	if(!umx_has_CIs(model, "output")) {
		if(umx_has_CIs(model, "intervals")){
			stop("Please run the intervals first: mxConfint(... run= TRUE)")
		} else {
			stop("Please add and run some intervals using ?mxConfint()")
		}
	}
	# Get lower and upper CIs as a dataframe
	CIlist = data.frame(model$output$confidenceIntervals)
	if(dropZeros){
		# Drop rows fixed to zero
		CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
		# Discard rows named NA?
		CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
	}

	CIlist$CIname = row.names(CIlist)
	# Iterate over each CI
	labelList = imxGenerateLabels(model)	
	rowCount = dim(CIlist)[1]

	for(n in 1:rowCount) { # n = 1
		thisName = row.names(CIlist)[n] # thisName = "cp_loadings_r1c1"
		CIname = CIlist$CIname[n]
		# convert label to [bracket] style
		if(!umx_has_square_brackets(thisName)) {
			nameParts = labelList[which(row.names(labelList) == thisName),]
			CIlist$CIname[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
		}

		thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = CIname) # .matrix[
		thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = CIname))
		thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = CIname))
		CIparts    = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
		thisString = paste0(CIparts[1], " [",CIparts[2], ", ",CIparts[3], "]")

		if(grepl("^a", thisMatrixName)) {
			a_CI[thisMatrixRow, thisMatrixCol] = thisString
		} else if(grepl("^c", thisMatrixName)){
			c_CI[thisMatrixRow, thisMatrixCol] = thisString
		} else if(grepl("^e", thisMatrixName)){
			e_CI[thisMatrixRow, thisMatrixCol] = thisString
		} else{
			stop(paste("Illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
		}
	}
	# TODO Check the merge of a_, c_ and e_CI INTO the output table works with more than one variable
	# TODO umxSummaryACE: Add option to use mxSE
	# print(a_CI)
	# print(c_CI)
	# print(e_CI)
	Estimates = data.frame(cbind(a_CI, c_CI, e_CI), row.names = rowNames, stringsAsFactors = FALSE)
	names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
	Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
	if(report == "html"){
		# depends on R2HTML::HTML
		R2HTML::HTML(Estimates, file = "tmpCI.html", Border = 0, append = F, sortableDF = T); 
		umx_open("tmpCI.html")
	}
	CI_Fit = model
	CI_Fit$top$a$values = a_CI
	CI_Fit$top$c$values = c_CI
	CI_Fit$top$e$values = e_CI
}
