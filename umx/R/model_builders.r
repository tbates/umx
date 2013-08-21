# https://github.com/hadley/devtools/wiki/Philosophy
# setwd("~/bin/umx/umx"); devtools::document(); devtools::install(); devtools::load_all()
# devtools::check()
# devtools::dev_help("umxStart")

# ==================
# = Model Builders =
# ==================

umxCFA <- function(name="", latents, data, report =c("shortTable", "shortLine", "long")){
	# umxCFA(name="myFirstCFA", latents="g", data = myFAdata)
	manifests <- names(data)
	m1 <- mxModel(name, type="RAM",
		manifestVars = manifests,
		latentVars   = latents,
		# Factor loadings
		mxPath(from = latents, to = manifests),
		mxPath(from = manifests, arrows = 2), # manifest residuals 
		mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
		mxData(cov(data), type="cov", numObs = nrow(data))
	)
	m1 = mxRun(m1); 
	if(report == "shortTable") {
		umxReportFit(m1, report = "table");
	} else if(report == "shortLine"){
		umxReportFit(m1, report = "line");
	} else if (report == "long"){
		umxSummary(m1, report = "")
	} else {
		message("Bad setting for report, you said", report, "but only \"shortTable\", \"shortLine\", and \"long\" are valid")
	}
	invisible(m1)
}
